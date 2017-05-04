rm(list=ls())

#install.packages("devtools")
library(devtools)
#devtools::install_github("bmschmidt/wordVectors")
#install.packages("data.table")
library(data.table)
library(wordVectors)
library(dplyr)
ls("package:wordVectors")



###### Read data
cs <- read.csv("train_profiles.csv", stringsAsFactors = F)
cls <- read.delim("train_clickstreams.tab", stringsAsFactors = F)
cls_test <- read.delim("test_clickstreams.tab", stringsAsFactors = F)


###### Convert data.frame to data.table for fast computing
cs.dt <- data.table(cs, key="CUS_ID")
cls.dt <- data.table(cls, key="CUS_ID")
cls_test.dt <- data.table(cls_test, key="CUS_ID")
md.dt <- merge(cs.dt, cls.dt)
md.dt<-md.dt%>%select(-RESIDENCE)

####################GENDER################################
###### Make sites sentences
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GENDER][1] #x번의 성별 저장
 
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x번의 모든 카테고리(중복 허용) 저장
  fitems <- itemfreq[itemfreq >= t] #t번 이상 사용한 카테고리 분류 
  act <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 

  act <- sapply(act, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  set.seed(1)

  as.vector((sapply(1:20, function(x) c(grp, sample(act))))) #성별+뻥튀기
}


items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Train site2vec model
set.seed(12345)
model = train_word2vec("items.txt","vec.bin",vectors=50,threads=4,window=5,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec.bin") #이미 만들어놓은 위의 파일 읽기.



# ##### Explore the model -> 눈으로 확인하는 용도
# for (v in unique(md.dt[,GENDER])) print(closest_to(model, v, n=10)) #각 GENDER의 연관성 높은 Top9 (10-1) 보여주기
# model[[unique(md.dt[,GENDER]), average=F]] %>% plot(method="pca") #남자/여자가 서로 얼마나 떨어져있는지 시각화.
# items.24 <- c(unique(md.dt[,GENDER]), unique(md.dt[CUS_ID==1, ACT_NM])) #남자/여자 및 CUS_ID가 검색한 distinct 클래스
# model[[items.24[1:10], average=F]] %>% plot(method="pca") #위의 CUS_ID가 검색한 것 중 앞 8(=10-2)개가 남자/여자 어디에 가까우지 시각화.
# 
# #아래 CUS_ID가 검색한 distinct 클래스가 남자/여자로부터 얼마나 떨어져 있는지 계산
# cosineSimilarity(model[[unique(md.dt[CUS_ID==24, ACT_NM]), average=T]], model[[c("남자","여자"), average=F]])
# cosineSimilarity(model[[unique(md.dt[CUS_ID==131, ACT_NM]), average=T]], model[[c("남자","여자"), average=F]])



##### Predict
g <- function(x, t) {
  itemfreq <- table(cls_test.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  
  sim <- cosineSimilarity(model[[ract, average=T]], model[[c("남자","여자"), average=F]]) #ract의 평균과 남/여 사이 거리 추출
  return(c(x, sim))
}
predict<-as.data.frame(t(sapply(unique(cls_test.dt[,CUS_ID]), g, 5)))
colnames(predict)<-(c("CUS_ID","d_Man","d_Woman"))
predict<-predict%>%
  mutate(p_Man=d_Man/(d_Man+d_Woman),p_Woman=d_Woman/(d_Man+d_Woman))
write.csv(predict,"click_site2vec.csv",row.names=FALSE) #distance 구한 결과를 모델에 적용해서 쓰거나, 비율 구해서 확률 바로 사용 




##### Evaluate with confusion matrix
h <- function(x, t) {
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  
  sim <- cosineSimilarity(model[[ract, average=T]], model[[c("남자","여자"), average=F]]) #ract의 평균과 남/여 사이 거리 추출
  
  return(names(which.max(sim[1,]))) #예측 결과중 최대 예측결과 (남자/여자)
}
ctab <- table(sapply(cs.dt$CUS_ID, h, 5), cs.dt$GENDER) #confusion matrix
ctab
sum(diag(ctab)) / nrow(cs.dt) #accuracy 추출.약 70.6%
nrow(cs.dt[GENDER=="남자",]) / nrow(cs.dt) #2500명중 실제 남자일 확률 0.6344


##############################GROUP###############################################
###### Make sites sentences
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1] #x번의 GROUP 저장
  
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x번의 모든 카테고리(중복 허용) 저장
  fitems <- itemfreq[itemfreq >= t] #t번 이상 사용한 카테고리 분류 
  act <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  
  act <- sapply(act, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  set.seed(1)
  
  as.vector((sapply(1:20, function(x) c(grp, sample(act))))) #group+뻥튀기
}


items3 <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items3, "items3.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Train site2vec model
set.seed(12345)
model = train_word2vec("items3.txt","vec3.bin",vectors=50,threads=4,window=5,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec3.bin") #이미 만들어놓은 위의 파일 읽기.


##### Predict
g <- function(x, t) {
  itemfreq <- table(cls_test.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  
  sim <- cosineSimilarity(model[[ract, average=T]], model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
  return(c(x, sim))
}
predict<-as.data.frame(t(sapply(unique(cls_test.dt[,CUS_ID]), g, 5)))
colnames(predict)<-(c("CUS_ID","d_M40","d_M30","d_F30","d_F40","d_F20","d_M20"))
predict<-predict%>%
  mutate(p_M40=d_M40/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20),
         p_M30=d_M30/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20),
         p_F30=d_F30/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20),
         p_F40=d_F40/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20),
         p_F20=d_F20/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20),
         p_M20=d_M20/(d_M40+d_M30+d_F30+d_F40+d_F20+d_M20))
write.csv(predict,"click_site2vec3.csv",row.names=FALSE) #distance 구한 결과를 모델에 적용해서 쓰거나, 비율 구해서 확률 바로 사용 




##### Evaluate with confusion matrix
h <- function(x, t) {
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM]) #x의 ACT_NM을 저장 
  fitems <- itemfreq[itemfreq >= t] #t번 이상 나온 변수를 추출 
  ract <- names(fitems) #위에서 뽑은 카테고리 이름 문자열 저장 
  ract <- sapply(ract, function(x) gsub(" ", "+", x)) #빈 공간을+합쳐서 하나의 단어처럼 만들기
  
  sim <- cosineSimilarity(model[[ract, average=T]], model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
  
  return(names(which.max(sim[1,]))) #예측 결과중 최대 예측결과
}
ctab <- table(sapply(cs.dt$CUS_ID, h, 5), cs.dt$GROUP) #confusion matrix
ctab
sum(diag(ctab)) / nrow(cs.dt) #accuracy 추출.약 70.6%
nrow(cs.dt[GROUP=="M30",]) / nrow(cs.dt) #2500명중 실제 M30일 확률 0.6344