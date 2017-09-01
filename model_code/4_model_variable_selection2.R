# install.packages("caret")
# install.packages("ROCR")
# install.packages("C50")
# install.packages("e1071")
# install.packages("Epi")
# install.packages("randomForest")
# install.packages("xgboost")

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

setwd("D:\\비즈니스 모델링\\Challenge_170511")

cs_merge_train <- read.csv("cs_merge_train.csv")
cs_merge_s2v_train <- read.csv("cs_merge_s2v_train.csv")
cs_s2v300_train <- read.csv("cs_site2vec300_train.csv")


#### Model
cs_merge_train$GROUP <- substr(cs_merge_train$GROUP,1,3)
cs_merge_s2v_train$GROUP <- substr(cs_merge_s2v_train$GROUP,1,3)
cs_s2v300_train$GROUP <- substr(cs_s2v300_train$GROUP,1,3)

cs_merge_train$GROUP <- factor(cs_merge_train$GROUP)
cs_merge_s2v_train$GROUP <- factor(cs_merge_s2v_train$GROUP)
cs_s2v300_train$GROUP <- factor(cs_s2v300_train$GROUP)


fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction=mnLogLoss)


set.seed(1)

#### 1. cs_merge_train data로 먼저 rf, xgb 돌려서 variable selection하기
## Caret Random Forest 
model_rf_cls_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                              data = cs_merge_train, 
                              preProcess = NULL,
                              metric = "logLoss",
                              method = "rf",
                              trControl = fitControl)
model_rf_cls_group          ## 1.5198

## Caret xgboost
model_xgb_cls_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                data = cs_merge_train, 
                                preProcess = NULL,
                                metric = "logLoss",
                                method = "xgbTree",
                                trControl = fitControl)
model_xgb_cls_group
min(model_xgb_cls_group[[4]][8])   ## 1.497249

## feature selection / Using importance of models
importance_rf_cls_group <- varImp(model_rf_cls_group,scale=TRUE)  
importance_rf_cls_group

importance_xgb_cls_group <- varImp(model_xgb_cls_group,scale=TRUE)  
importance_xgb_cls_group

im_rf_cls_group = as.data.frame(importance_rf_cls_group[[1]])
im_xgb_cls_group = as.data.frame(importance_xgb_cls_group[[1]])

write.csv(im_rf_cls_group,"im_rf_cls_group.csv")
write.csv(im_xgb_cls_group,"im_xgb_cls_group.csv")


#### 2. cs_s2v300_train data
## Caret Random Forest 
model_rf_s2v300_group <- caret::train(GROUP ~ .-CUS_ID,
                                   data = cs_s2v300_train, 
                                   preProcess = NULL,
                                   metric = "logLoss",
                                   method = "rf",
                                   trControl = fitControl)
model_rf_s2v300_group

## Caret xgboost
model_xgb_s2v300_group <- caret::train(GROUP ~ .-CUS_ID,
                                    data = cs_s2v300_train, 
                                    preProcess = NULL,
                                    metric = "logLoss",
                                    method = "xgbTree",
                                    trControl = fitControl)
model_xgb_s2v300_group
min(model_xgb_s2v300_group[[4]][8])   ##

## feature selection / Using importance of models
importance_rf_s2v300_group <- varImp(model_rf_s2v300_group,scale=TRUE)  
importance_rf_s2v300_group

importance_xgb_s2v300_group <- varImp(model_xgb_s2v300_group,scale=TRUE)  
importance_xgb_s2v300_group

im_rf_s2v300_group = as.data.frame(importance_rf_s2v300_group[[1]])
im_xgb_s2v300_group = as.data.frame(importance_xgb_s2v300_group[[1]])

write.csv(im_rf_s2v300_group,"im_rf_s2v300_group.csv")
write.csv(im_xgb_s2v300_group,"im_xgb_s2v300_group.csv")


#### 3. cs_merge_s2v_train data
## Caret Random Forest 
model_rf_clss2v_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_s2v_train, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "rf",
                                      trControl = fitControl)
model_rf_clss2v_group

## Caret xgboost
model_xgb_clss2v_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                       data = cs_merge_s2v_train, 
                                       preProcess = NULL,
                                       metric = "logLoss",
                                       method = "xgbTree",
                                       trControl = fitControl)
model_xgb_clss2v_group
min(model_xgb_clss2v_group[[4]][8])

## feature selection / Using importance of models
importance_rf_clss2v_group <- varImp(model_rf_clss2v_group,scale=TRUE)  
importance_rf_clss2v_group

importance_xgb_clss2v_group <- varImp(model_xgb_clss2v_group,scale=TRUE)  
importance_xgb_clss2v_group

im_rf_clss2v_group = as.data.frame(importance_rf_clss2v_group[[1]])
im_xgb_clss2v_group = as.data.frame(importance_xgb_clss2v_group[[1]])

write.csv(im_rf_clss2v_group,"im_rf_clss2v_group.csv")
write.csv(im_xgb_clss2v_group,"im_xgb_clss2v_group.csv")







##########################################

cs_merge_train_cov <- cs_merge_train %>%
  select(-CUS_ID, -GROUP, -AGE, -GENDER,-wk_pat1,-wk_pat2,-wk_pat3,
         -day_pat1,-day_pat2,-day_pat3,-month_pat1,-month_pat2,-month_pat3,
         -time_pat1,-time_pat2,-time_pat3,-time_pat_wk,-time_pat2_wk,-time_pat3_wk,
         -time_pat_we,-time_pat2_we,-time_pat3_we)

covmat <- cov(cs_merge_train_cov)
low <- lower.tri(covmat, diag = FALSE)

model_glm_cls_group <- caret::train(GENDER ~ .-CUS_ID -GENDER -AGE,
                                       data = cs_merge_s2v_train, 
                                       preProcess = NULL,
                                       metric = "logLoss",
                                       method = "glm",
                                       trControl = fitControl)
model_glm_cls_group

##########################################




#### probability 뽑기 (train data)
cs_all2<-cs_all%>%
  arrange(CUS_ID)%>%
  select(-1:-4,-AGE_GROUP)

data<-cs_all%>%
  arrange(CUS_ID)%>%
  select(1:2,GROUP,AGE_GROUP)

rf_group_train=predict(model_rf_group, newdata = cs_all2, type="prob")
rf_group_train<-as.data.frame(c(data,rf_group_train))
a<-rf_group_train
rf_group_train<-a%>%
  select(1,3,5:10)
colnames(rf_group_train)<-c("CUS_ID","GROUP","F20-","F30","F40+","M20-","M30","M40+")
save(rf_group_train,file="rf_group_train.Rdata")


xgb_age_train=predict(model_xgb_agegroup, newdata = cs_all2, type="prob")
xgb_age_train<-as.data.frame(c(data,xgb_age_train))
b<-xgb_age_train
xgb_age_train<-b%>%
  select(1,4:7)
save(xgb_age_train,file="xgb_age_train.Rdata")


xgb_gender_train=predict(model_xgb_gender, newdata = cs_all2, type="prob")
xgb_gender_train<-as.data.frame(c(data,xgb_gender_train))
c<-xgb_gender_train
xgb_gender_train<-c%>%
  select(1,2,5,6)
colnames(xgb_gender_train)<-c("CUS_ID","GENDER","M","F")
xgb_gender_train<-xgb_gender_train%>%select(1,2,4,3)
save(xgb_gender_train,file="xgb_gender_train.Rdata")

# #probability 뽑기 (test data)
# 
# data<-cs_all%>%
#   arrange(CUS_ID)%>%
#   select(CUS_ID)
# 
# cs_all<-cs_all%>%
#   arrange(CUS_ID)%>%
#   select(-CUS_ID)
# 
# rf_group_test=predict(model_rf_group, newdata = cs_all, type="prob")
# rf_group_test<-as.data.frame(c(data,rf_group_test))
# colnames(rf_group_test)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
# save(rf_group_test,file="rf_group_test.Rdata")
#
# xgb_age_test=predict(model_xgb_agegroup, newdata = cs_all, type="prob")
# xgb_age_test<-as.data.frame(c(data,xgb_age_test))
# save(xgb_age_test,file="xgb_age_test.Rdata")
#
#
# xgb_gender_test=predict(model_xgb_gender, newdata = cs_all, type="prob")
# xgb_gender_test<-as.data.frame(c(data,xgb_gender_test))
# colnames(xgb_gender_test)<-c("CUS_ID","M","F")
# xgb_gender_test<-xgb_gender_test%>%select(1,3,2)
# save(xgb_gender_test,file="xgb_gender_test.Rdata")