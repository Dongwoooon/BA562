install.packages("topicmodels")

library(dplyr)
library(data.table)
library(wordVectors)
library(topicmodels)
library(tm)

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
  select(CUS_ID, GROUP)

cs.dt <- data.table(cs, key="CUS_ID")       ### key를 지정해줌 / key를 통한 연산을 빠르게 가능 
tr.dt <- data.table(tr.train, key="CUS_ID")
md.dt <- merge(cs.dt, tr.dt)

md.dt$QRY <- as.character(md.dt$QRY)
md.dt$GROUP <- as.character(md.dt$GROUP)
md.dt <- select(md.dt, -Freq)
##### search keyword를 잘 grouping 해주어서 분석해야함 -> LDA 사용
ftopic <- function(x) {
  act <- unique(md.dt[CUS_ID==x, QRY])       ### x의 1년치의 대분류 수준 검색어를 뽑아라 
  return(act)   ### sentence를 return
}
lda_doc <- sapply(unique(md.dt[,CUS_ID]), ftopic)
lda_doc <- as.vector(sapply(lda_doc, paste, collapse = " "))
corp <- Corpus(VectorSource(lda_doc))
dtm <- DocumentTermMatrix(corp)
lda_model <- LDA(dtm,50,method = "Gibbs",control = list(seed=1,burnin=500,iter=30))
topic <- terms(lda_model,15)
write.csv(topic,'topic.csv',row.names = F)
### 한글이라서 안 됨 ㅎㅎ

### Make ketword sentences
fgrp <- function(x) {
  grp <- md.dt[CUS_ID==x, GROUP][1]               ###cus_id=x 인 애의 group
  act <- unique(md.dt[CUS_ID==x, QRY])       ### 키워드를 뽑아라 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act를 20번 뻥튀기를 한 후에 맨 앞에 성별 정보를 붙이는 과정 . 데이터가 작으니까 20번 뻥튀기하는거야 (random order) / 총 4000sentence
}
### 이게 원래는 순서가 고려되는거임. 따라서 데이터가 크다면 sampling하지 말고 순서 고려해서 하면 검색 순서도 고려돼서 반영
items_grp <- unlist(sapply(unique(md.dt[,CUS_ID]), fgrp))

write.table(items_grp, "items_grp.txt", eol = " ", quote = F, row.names = F, col.names = F)

# Train site2vec model
model_grp = train_word2vec("items_grp.txt","vec_grp.bin",vectors=300,threads=4,window=5,cbow=1,negative_samples=10,iter=5,force = T)

######차원 수     / 내 컴퓨터의 core수 / window는 전 후 몇개 할지 / negative sample은 default로 하기 (5임), 데이터 적을 경우 5 이상으로 돌려야함)

# Explore the model
model_grp <- read.binary.vectors("vec_grp.bin") # reload the pre-trained word2vec model 
for (v in unique(md.dt[,GROUP])) print(closest_to(model_grp, v, n=31))

model_grp[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca") 

##### Predict group (train data)
grp <- unique(md.dt$GROUP)

p_grp <- function(x, t) {
  itemfreq <- table(md.dt[CUS_ID==x, QRY]) #x의 QRY를 저장
  answer <- cs.dt[x,2]
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 keyword 문자열 저장

  sim_grp <- cosineSimilarity(model_grp[[ract, average=T]], model_grp[[grp, average=F]])
  
  return(c(x,answer,sim_grp))
}

pred_grp<-as.data.frame(t(sapply(unique(md.dt[,CUS_ID]), p_grp, 5)))
colnames(pred_grp)<-c("CUS_ID","GROUP",grp)
pred_grp <- pred_grp %>% filter(is.na(M30)==F)
absdist <- apply(pred_grp[3:8],2,as.numeric)
pred_grp <- cbind(pred_grp[1:2],apply(absdist,2,abs))

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
#### 포기
'pred_age<-pred_age %>% 
  mutate(p_20=d_20/(d_20+d_30+d_40),p_30=d_30/(d_20+d_30+d_40),p_40=d_40/(d_20+d_30+d_40))
pred_age <- pred_age %>% filter(is.na(p_20)==F) %>% select(CUS_ID,p_20,p_30,p_40)

cs_age <- select(cs,c(CUS_ID,AGE_GROUP))
cs_pred <- merge(cs_age,pred_age,by="CUS_ID",all.x=T, all.y=F)   #2500 ID와 붙이기 

cs_pred$prob <- mapply(get_prob,cs_pred$AGE_GROUP,1:nrow(cs_pred)) #참값에 해당하는 예측값
w2v_age_train_all <- cs_pred
names(w2v_age_train_all)[6] <- prob_4
save(w2v_age_train_all,file="w2v_age_train_all.Rdata")

w2v_age_train <- select(w2v_age_train_all,c(CUS_ID,prob_4))
save(w2v_age_train,file="w2v_age_train.Rdata")

prb <- w2v_age_train_all$prob_4
prb <- prb[is.na(prb)==F]
prb1 <- summary(prb)
hist1<- hist(prb)'
