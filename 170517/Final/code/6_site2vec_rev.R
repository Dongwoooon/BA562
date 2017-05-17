library(dplyr)
library(data.table)
library(wordVectors)

setwd('J:/data/BA562/final/2_Data')

###### Read data
cs <- read.csv("train_profiles.csv", stringsAsFactors = F)
cs <- cs %>% select(c(CUS_ID,GROUP))
cs$CUS_ID <- as.numeric(as.character(cs$CUS_ID))
cls <- read.delim("train_clickstreams.tab", stringsAsFactors = F)

###### Convert data.frame to data.table for fast computing
cs.dt <- data.table(cs, key="CUS_ID")
cls.dt <- data.table(cls, key="CUS_ID")
md.dt <- merge(cs.dt, cls.dt)

model_grp <- read.binary.vectors("vec.bin") # reload the pre-trained word2vec model 
model_grp[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca") 
for (v in unique(md.dt[,GROUP])) print(closest_to(model_grp, v, n=11))

##### Predict (train data)
grp<-unique(md.dt$GROUP)

g <- function(x, t) {
  itemfreq <- table(cls.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  answer <- cs.dt[x,2]
  
  sim_grp <- cosineSimilarity(model_grp[[ract, average=T]], model_grp[[grp, average=F]]) #ract의 평균과 남/여 사이 거리 추출
  
  return(c(x,answer,sim_grp))
}

pred<-as.data.frame(t(sapply(unique(cls.dt[,CUS_ID]), g, 5)))
colnames(pred)<-c("CUS_ID","GROUP",grp)
absdist <- apply(pred[3:8],2,as.numeric)
pred <- cbind(pred[1:2],apply(absdist,2,abs))

get_prob <- function(x){
  a<-pred[x,3:8]
  sapply(a,function(i) i/sum(a))
}

probs <- cbind(pred[,1:2],t(mapply(get_prob,1:2500)))
probs$CUS_ID <- as.numeric(as.character(probs$CUS_ID))
probs <- probs[,-2]
write.csv(probs,'s2v_train.csv',row.names=F)

##### Predict (test data)
cls <- read.delim("test_clickstreams.tab", stringsAsFactors = F)
cls.dt <- data.table(cls, key="CUS_ID")

pred<-as.data.frame(t(sapply(unique(cls.dt[,CUS_ID]), g, 5)))
colnames(pred)<-c("CUS_ID","GROUP",grp)
absdist <- apply(pred[3:8],2,as.numeric)
pred <- cbind(pred[1:2],apply(absdist,2,abs))

probs <- cbind(pred[,1:2],t(mapply(get_prob,1:2500)))
probs$CUS_ID <- as.numeric(as.character(probs$CUS_ID))
probs <- probs[,-2]
write.csv(probs,'s2v_test.csv',row.names=F)
