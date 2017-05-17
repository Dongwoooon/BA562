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

#setwd("D:/2016~/전공/비즈니스모델링분석/PAC/Github/BA562/170512_importance_codes_data")


cs_s2v300_train <- read.csv("cs_site2vec300_train.csv")
cs_s2v300_train$GROUP <- substr(cs_s2v300_train$GROUP,1,3)
cs_s2v300_train$GROUP <- factor(cs_s2v300_train$GROUP)

cs_s2v300_test <- read.csv("cs_site2vec300_test.csv")
test_public <- read.csv("test_public.csv")
cs_1st_prediction<-read.csv("1등.csv")
cs_2nd_prediction<-read.csv("2등.csv")

#test_public을 train에 합치기
#############################
test_group <- test_public%>%
  mutate(GROUP=ifelse(F20.==1,"F20",
                      ifelse(F30==1,"F30",
                             ifelse(F40.==1,"F40",
                                    ifelse(M20.==1,"M20",
                                           ifelse(M30==1,"M30",
                                                  ifelse(M40.==1,"M40",NA)))))))%>%
  select(CUS_ID,GROUP)

a<-test_group$CUS_ID

cs_s2v300_test<-cs_s2v300_test%>%
  mutate(partition=ifelse(CUS_ID%in%a,1,0))

temp<-cs_s2v300_test%>%
  filter(partition==1)%>%
  select(-partition)

temp<-test_group%>%
  left_join(temp)

cs_s2v300_test<-cs_s2v300_test%>%
  filter(partition==0)%>%
  select(-partition)

cs_s2v300_train<-bind_rows(cs_s2v300_train,temp)
cs_s2v300_train$GROUP <- factor(cs_s2v300_train$GROUP)
#############################

#1등,2등 1497명 뽑기
#############################
cs_1st_prediction<-cs_1st_prediction%>%
  mutate(partition=ifelse(CUS_ID%in%a,1,0))
cs_2nd_prediction<-cs_2nd_prediction%>%
  mutate(partition=ifelse(CUS_ID%in%a,1,0))

cs_1st_prediction<-cs_1st_prediction%>%
  filter(partition==0)%>%
  select(-partition)
cs_2nd_prediction<-cs_2nd_prediction%>%
  filter(partition==0)%>%
  select(-partition)

rm(temp)
rm(test_group)
rm(test_public)
rm(a)

colnames(cs_1st_prediction) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
colnames(cs_2nd_prediction) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
write.csv(cs_1st_prediction,"1등_1497.csv",row.names = F)
write.csv(cs_2nd_prediction,"2등_1497.csv",row.names = F)
#############################


fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  summaryFunction=mnLogLoss)
# 
# site_vec_3=Rtsne(cs_s2v300_train[,-1:-2],dims=3)
# a<-cs_s2v300_train%>%
#   select(1,2)
# cs_s2v300_train<-as.data.frame(c(a,site_vec_3))
# 
# set.seed(1)
# model_C5.0Tree_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "C5.0Tree",
#                                           trControl = fitControl)
# set.seed(1)
# model_rf_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "rf",
#                                           trControl = fitControl)
# set.seed(1)
# model_glmnet_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "glmnet",
#                                           trControl = fitControl)
# set.seed(1)
# model_kknn_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "kknn",
#                                           trControl = fitControl)
set.seed(1)
model_pda_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
                                          data = cs_s2v300_train,
                                          preProcess = NULL,
                                          metric = "logLoss",
                                          method = "pda",
                                          trControl = fitControl)
# set.seed(1)
# model_slda_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "slda",
#                                           trControl = fitControl)
# set.seed(1)
# model_pam_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "pam",
#                                           trControl = fitControl)
# set.seed(1)
# model_pls_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
#                                           data = cs_s2v300_train,
#                                           preProcess = NULL,
#                                           metric = "logLoss",
#                                           method = "pls",
#                                           trControl = fitControl)


#### probability 뽑기
cs_s2v300_test <- cs_s2v300_test%>%
  arrange(CUS_ID)
CS<-cs_s2v300_test%>%
  select(CUS_ID)
# 
# C5.0Tree_s2v300_group_test=predict(model_C5.0Tree_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# glmnet_s2v300_group_test=predict(model_glmnet_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# kknn_s2v300_group_test=predict(model_kknn_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# pam_s2v300_group_test=predict(model_pam_s2v300_group300, newdata = cs_s2v300_test, type="prob")
pda_s2v300_group_test=predict(model_pda_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# pls_s2v300_group_test=predict(model_pls_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# rf_s2v300_group_test=predict(model_rf_s2v300_group300, newdata = cs_s2v300_test, type="prob")
# slda_s2v300_group_test=predict(model_slda_s2v300_group300, newdata = cs_s2v300_test, type="prob")

# 
# C5.0Tree_s2v300_group_test <- as.data.frame(c(CS,C5.0Tree_s2v300_group_test))
# glmnet_s2v300_group_test <- as.data.frame(c(CS,glmnet_s2v300_group_test))
# kknn_s2v300_group_test <- as.data.frame(c(CS,kknn_s2v300_group_test))
# pam_s2v300_group_test <- as.data.frame(c(CS,pam_s2v300_group_test))
pda_s2v300_group_test <- as.data.frame(c(CS,pda_s2v300_group_test))
# pls_s2v300_group_test <- as.data.frame(c(CS,pls_s2v300_group_test))
# rf_s2v300_group_test <- as.data.frame(c(CS,rf_s2v300_group_test))
# slda_s2v300_group_test <- as.data.frame(c(CS,slda_s2v300_group_test))
# 
# colnames(C5.0Tree_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(glmnet_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(kknn_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(pam_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
colnames(pda_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(pls_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(rf_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# colnames(slda_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# 
# write.csv(C5.0Tree_s2v300_group_test,"Ensemble_s2v300_300\\C5.0Tree_s2v300_test.csv",row.names = F)
# write.csv(glmnet_s2v300_group_test,"Ensemble_s2v300_300\\glmnet_s2v300_test.csv",row.names = F)
# write.csv(kknn_s2v300_group_test,"Ensemble_s2v300_300\\kknn_s2v300_test.csv",row.names = F)
# write.csv(pam_s2v300_group_test,"Ensemble_s2v300_300\\pam_s2v300_test.csv",row.names = F)
write.csv(pda_s2v300_group_test,"pda_s2v300_test_1497.csv",row.names = F)
# write.csv(pls_s2v300_group_test,"Ensemble_s2v300_300\\pls_s2v300_test.csv",row.names = F)
# write.csv(rf_s2v300_group_test,"Ensemble_s2v300_300\\rf_s2v300_test.csv",row.names = F)
# write.csv(slda_s2v300_group_test,"Ensemble_s2v300_300\\slda_s2v300_test.csv",row.names = F)