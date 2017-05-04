library(dplyr)
library(data.table)
library(wordVectors)

setwd('J:/data/BA562/final/2_Data')
dic = read.csv('keyword_dict.csv',stringsAsFactors=F)
load("train_str.Rdata")     # train.str
load("test_str.Rdata")      # test.str

tr.train <- merge(train.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   ## 검색어 중 dictionary에 있는 단어에 대한 관측치만 남긴 것
tr.test <- merge(test.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   
tr.train$CUS_ID <- as.numeric(as.character(tr.train$CUS_ID))
tr.test$CUS_ID <- as.numeric(as.character(tr.test$CUS_ID))

#### Convert data.frame to data.table for fast computing
#### cs data
cs <- read.csv("cs_merge.csv",stringsAsFactors=F) %>%
  select(CUS_ID, GENDER, AGE, GROUP)
cs <- cs %>% mutate(AGE_GROUP = substr(GROUP,2,3)) %>% select(-AGE)

cs.dt <- data.table(cs, key="CUS_ID")       ### key를 지정해줌 / key를 통한 연산을 빠르게 가능 
tr.dt <- data.table(tr.train, key="CUS_ID")
md.dt <- merge(cs.dt, tr.dt)

md.dt$QRY <- as.character(md.dt$QRY)
md.dt$GROUP <- as.character(md.dt$GROUP)
######### search keyword를 잘 grouping 해주어서 분석해야함. 노가다로 
# Make sites sentences
fgen <- function(x) {
  grp <- md.dt[CUS_ID==x, GENDER][1]               ###cus_id=x 인 애의 gender
  act <- unique(md.dt[CUS_ID==x, QRY])       ###1년치의 대분류 수준의 검색어를 뽑아라 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act를 20번 뻥튀기를 한 후에 맨 앞에 성별 정보를 붙이는 과정 . 데이터가 작으니까 20번 뻥튀기하는거야 (random order) / 총 4000sentence
}

fage <- function(x) {
  grp <- md.dt[CUS_ID==x, AGE_GROUP][1]               ###cus_id=x 인 애의 age
  act <- unique(md.dt[CUS_ID==x, QRY])       ###1년치의 대분류 수준의 검색어를 뽑아라 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act를 20번 뻥튀기를 한 후에 맨 앞에 성별 정보를 붙이는 과정 . 데이터가 작으니까 20번 뻥튀기하는거야 (random order) / 총 4000sentence
}

fgrp <- function(x) {
  grp <- md.dt[CUS_ID==x, GROUP][1]               ###cus_id=x 인 애의 group
  act <- unique(md.dt[CUS_ID==x, QRY])       ###1년치의 대분류 수준의 검색어를 뽑아라 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act를 20번 뻥튀기를 한 후에 맨 앞에 성별 정보를 붙이는 과정 . 데이터가 작으니까 20번 뻥튀기하는거야 (random order) / 총 4000sentence
}
### 이게 원래는 순서가 고려되는거임. 따라서 데이터가 크다면 sampling하지 말고 순서 고려해서 하면 검색 순서도 고려돼서 반영

items_gen <- unlist(sapply(unique(md.dt[,CUS_ID]), fgen))     ###unique(md.dt[,cus_id]) cus_id 열만 뽑으라는 소리임 // 고객마다 접속 사이트 수 다르니까 unlist해서 vector로 만들었음  // 이거 dpylr하면 20배 이상 걸림 
items_age <- unlist(sapply(unique(md.dt[,CUS_ID]), fage))
items_grp <- unlist(sapply(unique(md.dt[,CUS_ID]), fgrp))

write.table(items_gen, "items_gen.txt", eol = " ", quote = F, row.names = F, col.names = F)    ### 얘는 본래 큰 데이터로 쓰기 때문에 기본 가정이 하드디스크에 저장된 파일 불러와서 사용할거임 // eol: end of line을 구분하는 것 없이 쭉 이어지게 
write.table(items_age, "items_age.txt", eol = " ", quote = F, row.names = F, col.names = F)
write.table(items_grp, "items_grp.txt", eol = " ", quote = F, row.names = F, col.names = F)

# Train site2vec model
model_gen = train_word2vec("items_gen.txt","vec_gen.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=0, force = T)
model_age = train_word2vec("items_age.txt","vec_age.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=0, force = T)
model_grp = train_word2vec("items_grp.txt","vec_grp.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=0, force = T)
######차원 수     / 내 컴퓨터의 core수 / window는 전 후 몇개 할지 / negative sample은 default로 하기 (5임), 데이터 적을 경우 5 이상으로 돌려야함)

# Explore the model
for (v in unique(md.dt[,GENDER])) print(closest_to(model_gen, v, n=11))  #######남자에 가까운 벡터, 여자에 가까운 벡터 나옴 10개씩 
for (v in unique(md.dt[,AGE_GROUP])) print(closest_to(model_age, v, n=11))
for (v in unique(md.dt[,GROUP])) print(closest_to(model_grp, v, n=11))

model_gen[[unique(md.dt[,GENDER]), average=F]] %>% plot(method="pca")     ####남자, 여자 위치를 그래프에 표시 
model_age[[unique(md.dt[,AGE_GROUP]), average=F]] %>% plot(method="pca") 
model_grp[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca") 

##### Predict age (train data)
age<-unique(md.dt$AGE_GROUP)

p_age <- function(x, t) {
  itemfreq <- table(tr.dt[CUS_ID==x, QRY]) #x의 QRY를 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 keyword 문자열 저장 

  sim_age <- cosineSimilarity(model_age[[ract, average=T]], model_age[[age, average=F]])
  
  return(c(x, sim_age))
}

pred_age<-as.data.frame(t(sapply(unique(tr.dt[,CUS_ID]), p_age, 5)))
colnames(pred_age)<-(c("CUS_ID","d_40","d_20","d_30"))
pred_age$d_20 <- abs(pred_age$d_20)   #distance - 제거 
pred_age$d_30 <- abs(pred_age$d_30)
pred_age$d_40 <- abs(pred_age$d_40)

pred_age<-pred_age %>% 
  mutate(p_20=d_20/(d_20+d_30+d_40),p_30=d_30/(d_20+d_30+d_40),p_40=d_40/(d_20+d_30+d_40))
pred_age <- pred_age %>% filter(is.na(p_20)==F) %>% select(CUS_ID,p_20,p_30,p_40)

cs_age <- select(cs,c(CUS_ID,AGE_GROUP))
cs_pred <- merge(cs_age,pred_age,by="CUS_ID",all.x=T, all.y=F)   #2500 ID와 붙이기 
get_prob <- function(x,i){
  if(x==20){
    return(cs_pred[i,3])
  }
  else if(x==30){
    return(cs_pred[i,4])
  }
  else{
    return(cs_pred[i,5])
  }
}
cs_pred$prob <- mapply(get_prob,cs_pred$AGE_GROUP,1:nrow(cs_pred)) #참값에 해당하는 예측값
w2v_age_train <- cs_pred
w2v_age_train_all <- w2v_age_train
names(w2v_age_train_all)[6] <- 'prob_4'
save(w2v_age_train_all,file="w2v_age_train_all.Rdata")

w2v_age_train <- select(w2v_age_train,c(CUS_ID,prob))
names(w2v_age_train)[2] <- 'prob_4'
save(w2v_age_train,file="w2v_age_train.Rdata")

prb <- w2v_age_train$prob_4
prb <- prb[is.na(prb)==F]
summary(prb)
hist(prb)

##### Predict age (test data)
tr.dt <- data.table(tr.test, key="CUS_ID")
pred_age<-as.data.frame(t(sapply(unique(tr.dt[,CUS_ID]), p_age, 5)))
colnames(pred_age)<-(c("CUS_ID","d_40","d_20","d_30"))
pred_age$d_20 <- abs(pred_age$d_20)
pred_age$d_30 <- abs(pred_age$d_30)
pred_age$d_40 <- abs(pred_age$d_40)

pred_age<-pred_age %>% 
  mutate(p_20=d_20/(d_20+d_30+d_40),p_30=d_30/(d_20+d_30+d_40),p_40=d_40/(d_20+d_30+d_40))
w2v_age_test <- pred_age %>% filter(is.na(p_20)==F) %>% select(CUS_ID,p_20,p_30,p_40)

cs_test <- read.csv("test_cs_merge.csv",stringsAsFactors=F) %>%
  select(CUS_ID)
w2v_age_test <- merge(cs_test,w2v_age_test,by="CUS_ID",all.x=T, all.y=F)
save(w2v_age_test,file="w2v_age_test.Rdata")

h <- w2v_age_train_all[is.na(w2v_age_train_all$prob_4)==F,-1]
