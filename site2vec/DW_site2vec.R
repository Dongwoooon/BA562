library(dplyr)
library(data.table)
library(wordVectors)

setwd('J:/data/BA562/final/2_Data')
load('s2v_train_all.Rdata')

###### Read data
cs <- read.csv("train_profiles.csv", stringsAsFactors = F)
cs <- cs %>% mutate(AGE_GROUP = substr(GROUP,2,3)) %>% select(-c(AGE,RESIDENCE))
cs$CUS_ID <- as.numeric(as.character(cs$CUS_ID))
cls <- read.delim("train_clickstreams.tab", stringsAsFactors = F)

###### Convert data.frame to data.table for fast computing
cs.dt <- data.table(cs, key="CUS_ID")
cls.dt <- data.table(cls, key="CUS_ID")
md.dt <- merge(cs.dt, cls.dt)
md.dt<-md.dt%>%select(-RESIDENCE)


###### Make sites sentences
fgen <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GENDER][1] #x번의 성별 저장
 
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x의 모든 카테고리(중복 허용) 저장
  fitems <- itemfreq[itemfreq >= t] #t번 이상 사용한 카테고리 분류 
  act <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 

  act <- sapply(act, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  set.seed(1)

  as.vector((sapply(1:20, function(x) c(grp, sample(act))))) #성별+뻥튀기
}

fage <- function(x, t) {
  grp <- md.dt[CUS_ID==x, AGE_GROUP][1] #x의 나이 저장
  
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x번의 모든 카테고리(중복 허용) 저장
  fitems <- itemfreq[itemfreq >= t] #t번 이상 사용한 카테고리 분류 
  act <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  
  act <- sapply(act, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  set.seed(1)
  
  as.vector((sapply(1:20, function(x) c(grp, sample(act))))) #성별+뻥튀기
}

items_gen <- unlist(sapply(cs.dt$CUS_ID, fgen, 2))
items_age <- unlist(sapply(cs.dt$CUS_ID, fage, 2))

write.table(items_gen, "items_gen_site.txt", eol = " ", quote = F, row.names = F, col.names = F)
write.table(items_age, "items_age_site.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Train site2vec model
set.seed(1)
model_gen = train_word2vec("items_gen_site.txt","vec_gen_site.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=10, force = T)
model_age = train_word2vec("items_age_site.txt","vec_age_site.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=10, force = T)

##### Predict (train data)
gender<-unique(md.dt$GENDER)
age<-unique(md.dt$AGE_GROUP)

g <- function(x, t) {
  itemfreq <- table(cls.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  
  sim_gen <- cosineSimilarity(model_gen[[ract, average=T]], model_gen[[gender, average=F]]) #ract의 평균과 남/여 사이 거리 추출
  sim_age <- cosineSimilarity(model_age[[ract, average=T]], model_age[[age, average=F]]) #ract의 평균과 남/여 사이 거리 추출
  
  return(c(x, sim_gen, sim_age))
}

pred<-as.data.frame(t(sapply(unique(cls.dt[,CUS_ID]), g, 5)))
colnames(pred)<-(c("CUS_ID","d_M","d_F","d_40","d_30","d_20"))
pred$d_M <- abs(pred$d_M)  #distance - 제거
pred$d_F <- abs(pred$d_F)
pred$d_20 <- abs(pred$d_20)    
pred$d_30 <- abs(pred$d_30)
pred$d_40 <- abs(pred$d_40)

pred<-pred %>% 
  mutate(p_M=d_M/(d_M+d_F),p_F=d_F/(d_M+d_F),p_20=d_20/(d_20+d_30+d_40),p_30=d_30/(d_20+d_30+d_40),
         p_40=d_40/(d_20+d_30+d_40))

pred <- pred %>% filter(is.na(p_20)==F) %>% select(CUS_ID,p_M,p_F,p_20,p_30,p_40)

cs <- select(cs,-c(GROUP))
cs_pred <- merge(cs,pred,by="CUS_ID",all.x=T, all.y=F)   #2500 ID와 붙이기 
get_age_prob <- function(x,i){
  if(x==20){
    return(cs_pred[i,6])
  }
  else if(x==30){
    return(cs_pred[i,7])
  }
  else{
    return(cs_pred[i,8])
  }
}
get_gen_prob <- function(x,i){
  if(x=='남자'){
    return(cs_pred[i,4])
  }
  else{
    return(cs_pred[i,5])
  }
}

cs_pred$prob_5<- mapply(get_age_prob,cs_pred$AGE_GROUP,1:nrow(cs_pred)) #참값에 해당하는 예측값
cs_pred$prob_6 <- mapply(get_gen_prob,cs_pred$GENDER,1:nrow(cs_pred)) #참값에 해당하는 예측값

s2v_train_all <- cs_pred
save(s2v_train_all,file="s2v_train_all.Rdata")

s2v_age_train <- select(s2v_train_all,c(CUS_ID,prob_5))
save(s2v_age_train,file="s2v_age_train.Rdata")

s2v_gender_train <- select(s2v_train_all,c(CUS_ID,prob_6))
save(s2v_gender_train,file="s2v_gender_train.Rdata")

prb <- s2v_age_train$prob_5
summary(prb)
hist(prb)

##### Predict (test data)
cls <- read.delim("test_clickstreams.tab", stringsAsFactors = F)
cls.dt <- data.table(cls, key="CUS_ID")

pred<-as.data.frame(t(sapply(unique(cls.dt[,CUS_ID]), g, 5)))
colnames(pred)<-(c("CUS_ID","d_M","d_F","d_40","d_30","d_20"))
pred$d_M <- abs(pred$d_M)  #distance - 제거
pred$d_F <- abs(pred$d_F)
pred$d_20 <- abs(pred$d_20)    
pred$d_30 <- abs(pred$d_30)
pred$d_40 <- abs(pred$d_40)

pred<-pred %>% 
  mutate(p_M=d_M/(d_M+d_F),p_F=d_F/(d_M+d_F),p_20=d_20/(d_20+d_30+d_40),p_30=d_30/(d_20+d_30+d_40),
         p_40=d_40/(d_20+d_30+d_40))

pred <- pred %>% filter(is.na(p_20)==F) %>% select(CUS_ID,p_M,p_F,p_20,p_30,p_40)
s2v_age_test <- select(pred,c(CUS_ID,p_20,p_30,p_40))
s2v_gender_test <- select(pred,c(CUS_ID,p_M,p_F))
save(s2v_age_test,file="s2v_age_test.Rdata")
save(s2v_gender_test,file="s2v_gender_test.Rdata")
