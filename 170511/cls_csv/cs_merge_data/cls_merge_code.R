# PAC 
# Version 1 (only clickstreams data)
# Date: 2017.04.26
# DY DW SJ
# ------------------------------------------------------------------



library(ggplot2)
library(dplyr)
library(caret)
library(ROCR)
library(C50)
library(e1071)
library(Epi)
library(randomForest)
library(xgboost)


rm(list=ls())

setwd("C:\\Users\\seokj_000\\Desktop\\Challenge_170511")


#### Merge custumer signature
## train data
cs.v1 <- read.csv("cs_others_train.csv")
cs.v2 <- read.csv("cs_visit_vars_train.csv")
cs.v3 <- read.csv("cs_websession_train.csv")
cs.v4 <- read.csv("cs_site2vec300_train.csv")
cs.v4 <- cs.v4 %>%
  select(-GROUP)

cs_merge_train <- cs.v1 %>%
  left_join(cs.v2) %>%
  left_join(cs.v3) %>%
  select(-RESIDENCE)

cs_merge_s2v_train <- cs.v1 %>%
  left_join(cs.v2) %>%
  left_join(cs.v3) %>%
  left_join(cs.v4) %>%
  select(-RESIDENCE)

rm(cs.v1,cs.v2,cs.v3,cs.v4)

## test data  // test data는 merge data에 GROUP 정보 없음
cs.v1 <- read.csv("cs_others_test.csv")
cs.v2 <- read.csv("cs_visit_vars_test.csv")
cs.v3 <- read.csv("cs_websession_test.csv")
cs.v4 <- read.csv("cs_site2vec300_test.csv")

cs_merge_test <- cs.v1 %>%
  left_join(cs.v2) %>%
  left_join(cs.v3)

cs_merge_s2v_test <- cs.v1 %>%
  left_join(cs.v2) %>%
  left_join(cs.v3) %>%
  left_join(cs.v4)

rm(cs.v1,cs.v2,cs.v3,cs.v4)


#### Replace NAs
cs_merge_train[is.na(cs_merge_train[,"wk_pat1"]) ,"wk_pat1"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"wk_pat2"]) ,"wk_pat2"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"wk_pat3"]) ,"wk_pat3"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"day_pat1"]) ,"day_pat1"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"day_pat2"]) ,"day_pat2"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"day_pat3"]) ,"day_pat3"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"month_pat1"]) ,"month_pat1"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"month_pat2"]) ,"month_pat2"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"month_pat3"]) ,"month_pat3"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat1"]) ,"time_pat1"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat2"]) ,"time_pat2"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat3"]) ,"time_pat3"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat_wk"]) ,"time_pat_wk"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat2_wk"]) ,"time_pat2_wk"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat3_wk"]) ,"time_pat3_wk"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat_we"]) ,"time_pat_we"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat2_we"]) ,"time_pat2_we"]<-"유형없음"
cs_merge_train[is.na(cs_merge_train[,"time_pat3_we"]) ,"time_pat3_we"]<-"유형없음"

cs_merge_test[is.na(cs_merge_test[,"wk_pat1"]) ,"wk_pat1"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"wk_pat2"]) ,"wk_pat2"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"wk_pat3"]) ,"wk_pat3"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"day_pat1"]) ,"day_pat1"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"day_pat2"]) ,"day_pat2"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"day_pat3"]) ,"day_pat3"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"month_pat1"]) ,"month_pat1"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"month_pat2"]) ,"month_pat2"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"month_pat3"]) ,"month_pat3"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat1"]) ,"time_pat1"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat2"]) ,"time_pat2"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat3"]) ,"time_pat3"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat_wk"]) ,"time_pat_wk"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat2_wk"]) ,"time_pat2_wk"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat3_wk"]) ,"time_pat3_wk"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat_we"]) ,"time_pat_we"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat2_we"]) ,"time_pat2_we"]<-"유형없음"
cs_merge_test[is.na(cs_merge_test[,"time_pat3_we"]) ,"time_pat3_we"]<-"유형없음"

cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"wk_pat1"]) ,"wk_pat1"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"wk_pat2"]) ,"wk_pat2"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"wk_pat3"]) ,"wk_pat3"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"day_pat1"]) ,"day_pat1"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"day_pat2"]) ,"day_pat2"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"day_pat3"]) ,"day_pat3"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"month_pat1"]) ,"month_pat1"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"month_pat2"]) ,"month_pat2"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"month_pat3"]) ,"month_pat3"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat1"]) ,"time_pat1"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat2"]) ,"time_pat2"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat3"]) ,"time_pat3"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat_wk"]) ,"time_pat_wk"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat2_wk"]) ,"time_pat2_wk"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat3_wk"]) ,"time_pat3_wk"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat_we"]) ,"time_pat_we"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat2_we"]) ,"time_pat2_we"]<-"유형없음"
cs_merge_s2v_train[is.na(cs_merge_s2v_train[,"time_pat3_we"]) ,"time_pat3_we"]<-"유형없음"

cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"wk_pat1"]) ,"wk_pat1"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"wk_pat2"]) ,"wk_pat2"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"wk_pat3"]) ,"wk_pat3"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"day_pat1"]) ,"day_pat1"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"day_pat2"]) ,"day_pat2"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"day_pat3"]) ,"day_pat3"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"month_pat1"]) ,"month_pat1"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"month_pat2"]) ,"month_pat2"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"month_pat3"]) ,"month_pat3"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat1"]) ,"time_pat1"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat2"]) ,"time_pat2"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat3"]) ,"time_pat3"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat_wk"]) ,"time_pat_wk"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat2_wk"]) ,"time_pat2_wk"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat3_wk"]) ,"time_pat3_wk"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat_we"]) ,"time_pat_we"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat2_we"]) ,"time_pat2_we"]<-"유형없음"
cs_merge_s2v_test[is.na(cs_merge_s2v_test[,"time_pat3_we"]) ,"time_pat3_we"]<-"유형없음"

for(i in 1:ncol(cs_merge_train)) cs_merge_train[is.na(cs_merge_train[,i]),i]<-0
for(i in 1:ncol(cs_merge_test)) cs_merge_test[is.na(cs_merge_test[,i]),i]<-0
for(i in 1:ncol(cs_merge_s2v_train)) cs_merge_s2v_train[is.na(cs_merge_s2v_train[,i]),i]<-0
for(i in 1:ncol(cs_merge_s2v_test)) cs_merge_s2v_test[is.na(cs_merge_s2v_test[,i]),i]<-0

which(is.na(cs_merge_train))
which(is.na(cs_merge_test))
which(is.na(cs_merge_s2v_train))
which(is.na(cs_merge_s2v_test))

write.csv(cs_merge_train,"cs_merge_train.csv",row.names=FALSE)
write.csv(cs_merge_test,"cs_merge_test.csv",row.names=FALSE)
write.csv(cs_merge_s2v_train,"cs_merge_s2v_train.csv",row.names=FALSE)
write.csv(cs_merge_s2v_test,"cs_merge_s2v_test.csv",row.names=FALSE)


rm(list=ls())
