# PAC derived variable
# Search Keyword
# Date: 2017.04.30
# http://blog.naver.com/hss2864/220980568640
# ------------------------------------------------------------------
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("reshape")
install.packages("KoNLP")      ##rjava 설치해야함
install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")
install.packages("data.table")

library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)
library(KoNLP)
library(data.table)       ### 이게 훨씬 빠름 dpylr보다 20배 // 큰 데이터 다룰때는 이게 더 좋음
library(wordVectors)

rm(list=ls())

setwd("D:\\비즈니스 모델링\\Predictive Analytics Challenge\\Challenge_170430")



#### test, train keyword cleaned by DW

train <- read.csv("train_keyword_cleaned.csv",stringsAsFactors=F)
test <- read.csv("test_keyword_cleaned.csv",stringsAsFactors=F)


#### Subtracting other characters
train$kor = gsub(pattern="[^가-힣]",replacement=" ",train$QRY_clean)
test$kor = gsub(pattern="[^가-힣]",replacement=" ",test$QRY_clean)

#  ^[[:digit:]]$ : digits ///  [^A-z] : english


#### Dictionary constructing / #### 동운이가 dictionary 만들기

dic <- read.csv("sample_dict.csv")

dic <- dic %>%
  select(-count)

names(dic)<-c("QRY")



#### train, test data keyword spliting
cutx <- function(x) {
  cut<-unlist(strsplit(x,' '))
  cut <- cut[!cut=='']
}
train$cut<-sapply(train$kor,cutx)
test$cut<-sapply(test$kor,cutx)

## 이상 아이디 제거
train <- train %>%
  select(CUS_ID, QRY_CNT, cut) %>%
  arrange(CUS_ID) %>%
  filter(is.na(QRY_CNT)==FALSE)

test <- test %>%
  select(CUS_ID, QRY_CNT, cut) %>%
  arrange(CUS_ID) %>%
  filter(is.na(QRY_CNT)==FALSE)

## 외국어 vector 찌꺼기 제거
no0<-function(x){
  if(identical(unlist(x),character(0))==T){
    x=1
  }
  else{
    x=0
  }
}
train$cut_dum<-mapply(no0,train$cut)
test$cut_dum<-mapply(no0,test$cut)

train <- train %>%
  filter(cut_dum==0) %>%
  select(-cut_dum)

test <- test %>%
  filter(cut_dum==0) %>%
  select(-cut_dum)


#### train, test data --> structure 바꾸는 함수 (1row 1keyword)
g <- function(x){
  CUS_ID=x[1]
  cut = rep(unlist(x[3]), x[2])
  dt = as.matrix(data.frame(CUS_ID=CUS_ID, QRY=cut))
  return(dt)
}

train.mat <- as.matrix(train,nrow=nrow(train),ncol=ncol(train))
test.mat <- as.matrix(test,nrow=nrow(test),ncol=ncol(test))

train.key <- apply(train.mat,1,g)
test.key <- apply(test.mat,1,g)

train.str <- NULL
test.str <- NULL

## 10450 sec
system.time(
for (i in 1:nrow(train)){
  train.str <- rbind(train.str,train.key[[i]])
}
## 27485 sec
)
system.time(
for (i in 1:nrow(test)){
  test.str <- rbind(test.str,test.key[[i]])
}
)


train.str <- as.data.frame(train.str,row.names = F)
test.str <- as.data.frame(test.str, row.names = F)

## write.csv(train.str,"train_keyword_seperated.csv",row.names = F)
## write.csv(test.str,"test_keyword_seperated.csv",row.names = F)

## save(train.str,file="train_str.Rdata")
## save(test.str,file="test_str.Rdata")




#### str, dic data join
load("train_str.Rdata")     # train.str
load("test_str.Rdata")      # test.str

tr.train <- merge(train.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   ## 검색어 중 dictionary에 있는 단어에 대한 관측치만 남긴 것
tr.test <- merge(test.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   
tr.train$CUS_ID <- as.numeric(tr.train$CUS_ID)
tr.test$CUS_ID <- as.numeric(tr.test$CUS_ID)



#### Convert data.frame to data.table for fast computing
#### cs data
cs <- read.csv("cs_merge.csv",stringsAsFactors=F) %>%
  select(CUS_ID, GENDER, AGE, GROUP)

cs.dt <- data.table(cs, key="CUS_ID")       ### key를 지정해줌 / key를 통한 연산을 빠르게 가능 
tr.dt <- data.table(tr, key="CUS_ID")
md.dt <- merge(cs.dt, tr.dt)

md.dt$QRY <- as.character(md.dt$QRY)
md.dt$GROUP <- as.character(md.dt$GROUP)
######### search keyword를 잘 grouping 해주어서 분석해야함. 노가다로 
# Make sites sentences
f <- function(x) {
  grp <- md.dt[CUS_ID==x, GROUP][1]               ###cus_id=x 인 애의 gender
  act <- unique(md.dt[CUS_ID==x, QRY])       ###1년치의 대분류 수준의 검색어를 뽑아라 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act를 20번 뻥튀기를 한 후에 맨 앞에 성별 정보를 붙이는 과정 . 데이터가 작으니까 20번 뻥튀기하는거야 (random order) / 총 4000sentence
}
### 이게 원래는 순서가 고려되는거임. 따라서 데이터가 크다면 sampling하지 말고 순서 고려해서 하면 검색 순서도 고려돼서 반영

items <- unlist(sapply(unique(md.dt[,CUS_ID]), f))     ###unique(md.dt[,cus_id]) cus_id 열만 뽑으라는 소리임 // 고객마다 접속 사이트 수 다르니까 unlist해서 vector로 만들었음  // 이거 dpylr하면 20배 이상 걸림 
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)    ### 얘는 본래 큰 데이터로 쓰기 때문에 기본 가정이 하드디스크에 저장된 파일 불러와서 사용할거임 // eol: end of line을 구분하는 것 없이 쭉 이어지게 

# Train site2vec model
model = train_word2vec("items.txt","vec.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=0, force = T)
##########################################   차원 수     / 내 컴퓨터의 core수 / window는 전 후 몇개 할지 / negative sample은 default로 하기 (5임), 데이터 적을 경우 5 이상으로 돌려야함)

# Explore the model
for (v in unique(md.dt[,GROUP])) print(closest_to(model, v, n=10))    #######남자에 가까운 벡터, 여자에 가까운 벡터 나옴 10개씩 
model[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca")     ####남자, 여자 위치를 그래프에 표시 

######### 돌려보면 남자들이 자주 찾는 사이트들, 여자들이 자주 찾는 사이트들이 비슷한 방향으로 해서 벡터 나옴. 
a<-unique(md.dt$GROUP)
cosineSimilarity(model[[unique(md.dt[CUS_ID==14, QRY]), average=T]], model[[a, average=F]])  
cosineSimilarity(model[[unique(md.dt[CUS_ID==16, QRY]), average=T]], model[[a, average=F]])  

### 이걸로 해당 id 집어넣으면 남자 벡터쪽에 가까운지 여자 벡터쪽에 가까운지 나옴. / 다만 완벽하게 예측은 못하니 튜닝을하고 참고를 해서 합쳐야지 
### 남자일 확률, 여자일 확률 나옴 


#### Challenge 할 때 참고 
unique(md.dt[cus_id==24, category3])   #### 이게 24번이 총 검색한 단어 다 있는거. 이거를 다 쓰지 말고 이거를 들여다보고 적당히 추려서 가공해서 사용하면 더 잘 나오겠지 







