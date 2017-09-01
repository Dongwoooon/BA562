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
cs <- read.csv("cs_merge_train_cut.csv",stringsAsFactors=F) %>%
  select(CUS_ID, GENDER, AGE, GROUP)
cs <- cs %>% mutate(AGE_GROUP = substr(GROUP,2,3)) %>% select(-AGE)

cs.dt <- data.table(cs, key="CUS_ID")       ### key를 지정해줌 / key를 통한 연산을 빠르게 가능 
tr.dt <- data.table(tr.train, key="CUS_ID")
md.dt <- merge(cs.dt, tr.dt)

md.dt$QRY <- as.character(md.dt$QRY)
md.dt$GROUP <- as.character(md.dt$GROUP)
md.dt <- select(md.dt, -Freq)
######### search keyword를 잘 grouping 해주어서 분석해야함. 노가다로 
# Make sites sentences
fgen <- function(x) {
  grp <- md.dt[CUS_ID==x, GENDER][1]               ###cus_id=x 인 애의 gender
  act <- unique(md.dt[CUS_ID==x, QRY])       
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    
}
fage <- function(x) {
  grp <- md.dt[CUS_ID==x, AGE_GROUP][1]               ###cus_id=x 인 애의 age
  act <- unique(md.dt[CUS_ID==x, QRY])      
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    
}
fgrp <- function(x){
  grp <- md.dt[CUS_ID==x, GROUP][1]               ###cus_id=x 인 애의 group
  act <- unique(md.dt[CUS_ID==x, QRY])      
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))   
}
items_gen <- unlist(sapply(unique(md.dt[,CUS_ID]), fgen))      
items_age <- unlist(sapply(unique(md.dt[,CUS_ID]), fage))
items_grp <- unlist(sapply(unique(md.dt[,CUS_ID]), fgrp))

write.table(items_gen, "items_gen.txt", eol = " ", quote = F, row.names = F, col.names = F)    
write.table(items_age, "items_age.txt", eol = " ", quote = F, row.names = F, col.names = F)
write.table(items_grp, "items_grp.txt", eol = " ", quote = F, row.names = F, col.names = F)

# Train site2vec model
model_gen = train_word2vec("items_gen.txt","vec_gen.bin",vectors=300,threads=3,window=5,cbow=1,negative_samples=10,iter=5,force = T)
model_age = train_word2vec("items_age.txt","vec_age.bin",vectors=300,threads=4,window=5,cbow=1,negative_samples=10,iter=5,force = T)
model_grp = train_word2vec("items_grp.txt","vec_grp.bin",vectors=300,threads=4,window=5,cbow=1,negative_samples=10,iter=5,force = T)


# Explore the model
for (v in unique(cs.dt[,GENDER])) print(closest_to(model_gen, v, n=31))   
for (v in unique(cs.dt[,AGE_GROUP])) print(closest_to(model_age, v, n=31))
for (v in unique(cs.dt[,GROUP])) print(closest_to(model_grp, v, n=31))