# install.packages("caret")
# install.packages("ROCR")
# install.packages("C50")
# install.packages("e1071")
# install.packages("Epi")
# install.packages("randomForest")
# install.packages("xgboost")

library(ggplot2)
library(plyr)
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

cs_merge_train<-read.csv("cs_merge_train_cut.csv")

#### Model
cs_merge_train$GROUP <- substr(cs_merge_train$GROUP,1,3)

cs_merge_train$GROUP <- factor(cs_merge_train$GROUP)



fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction=mnLogLoss)


set.seed(1)

## Caret xgboost 60
cs_merge_train_60<-cs_merge_train%>%
  select(CUS_ID,GROUP,GENDER,AGE,횟수비율_쇼핑,p_v_.4,p_v_.12,시간비율_뉴스.미디어,p_v_.5,시간비율_게임,p_v_.9,p_v_.20,p_v_.19,wk_z2,횟수비율_사회.문화.종교,시간비율_온라인교육,시간비율_커뮤니티,시간비율_서비스,p_v_.1,cons_time9_we,mean_time_cnt,v_t_median,횟수비율_건강.의학,time5_z2_wk,nov_z2,day_coef_cnt,시간비율_문학.예술,interval,jan_z2,p_v_.3,횟수비율_비즈니스.경제,p_v_.16,s_v_mean,p_v_.17,v_pr_max,cons_cnt18_we,cons_num5,time0_z2_wk,시간비율_여행,s_prt_sd,cat_coef_visit,cons_time22,p_v_.13,cons_num13_we,sep_z,cons_num8_wk,s_pr_median,mean_time,횟수비율_인터넷.컴퓨터,cons_cnt9_wk,apr_z2,dec_z2,time8_z3,time13_z3,time12_z2_wk,sep_z2,time0_z_we,time9_z_wk,jun_z2,cons_num12_we,net_day,time22_z3_wk,cat_coef_cnt,cons_num18_wk)

model_xgb_cls_group60 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                    data = cs_merge_train_60, 
                                    preProcess = NULL,
                                    metric = "logLoss",
                                    method = "xgbTree",
                                    trControl = fitControl)
min(model_xgb_cls_group60[[4]][8])   ## 1.467697

## feature selection / Using importance of models
importance_xgb_cls_group60 <- varImp(model_xgb_cls_group60,scale=TRUE)  
im_xgb_cls_group60 = as.data.frame(importance_xgb_cls_group60[[1]])
write.csv(im_xgb_cls_group60,"im_xgb_cls_group60.csv")



## Caret xgboost 55
cs_merge_train_55<-cs_merge_train_60%>%
  select(-time22_z3_wk,-jun_z2,-sep_z2,-time9_z_wk,-cons_num12_we)
         
    
model_xgb_cls_group55 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_55, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group55[[4]][8])   ## 1.470592

## feature selection / Using importance of models
importance_xgb_cls_group55 <- varImp(model_xgb_cls_group55,scale=TRUE)  
im_xgb_cls_group55 = as.data.frame(importance_xgb_cls_group55[[1]])
write.csv(im_xgb_cls_group55,"im_xgb_cls_group55.csv")


## Caret xgboost 50
cs_merge_train_50<-cs_merge_train_55%>%
  select(-cons_num18_wk,-cons_cnt18_we,-dec_z2,-cons_cnt9_wk,-cat_coef_cnt)


model_xgb_cls_group50 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_50, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group50[[4]][8])   ## 1.465706

## feature selection / Using importance of models
importance_xgb_cls_group50 <- varImp(model_xgb_cls_group50,scale=TRUE)  
im_xgb_cls_group50 = as.data.frame(importance_xgb_cls_group50[[1]])
write.csv(im_xgb_cls_group50,"im_xgb_cls_group50.csv")


## Caret xgboost 45
cs_merge_train_45<-cs_merge_train_50%>%
  select(-cat_coef_visit,-p_v_.17,-apr_z2,-mean_time,-cons_num5)


model_xgb_cls_group45 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_45, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group45[[4]][8])   ## 1.458928

## feature selection / Using importance of models
importance_xgb_cls_group45 <- varImp(model_xgb_cls_group45,scale=TRUE)  
im_xgb_cls_group45 = as.data.frame(importance_xgb_cls_group45[[1]])
write.csv(im_xgb_cls_group45,"im_xgb_cls_group45.csv")
save(model_xgb_cls_group45,file="model_xgb_cls_group45.Rdata")


## Caret xgboost 40
cs_merge_train_40<-cs_merge_train_45%>%
  select(-time0_z_we,-시간비율_여행,-시간비율_문학.예술,-cons_num8_wk,-time12_z2_wk)


model_xgb_cls_group40 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_40, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group40[[4]][8])   ## 1.466215

## feature selection / Using importance of models
importance_xgb_cls_group40 <- varImp(model_xgb_cls_group40,scale=TRUE)  
im_xgb_cls_group40 = as.data.frame(importance_xgb_cls_group40[[1]])
write.csv(im_xgb_cls_group40,"im_xgb_cls_group40.csv")


## Caret xgboost 35
cs_merge_train_35<-cs_merge_train_40%>%
  select(-sep_z,-jan_z2,-p_v_.13,-time13_z3,-net_day)


model_xgb_cls_group35 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_35, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
model_xgb_cls_group35
min(model_xgb_cls_group35[[4]][8])   ## 1.463743

## feature selection / Using importance of models
importance_xgb_cls_group35 <- varImp(model_xgb_cls_group35,scale=TRUE)  
im_xgb_cls_group35 = as.data.frame(importance_xgb_cls_group35[[1]])
write.csv(im_xgb_cls_group35,"im_xgb_cls_group35.csv")


## Caret xgboost 30
cs_merge_train_30<-cs_merge_train_35%>%
  select(-횟수비율_건강.의학,-nov_z2,-s_pr_median,-time5_z2_wk,-cons_num13_we)


model_xgb_cls_group30 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_30, 
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "xgbTree",
                                      trControl = fitControl)
model_xgb_cls_group30
min(model_xgb_cls_group30[[4]][8])   ## 1.465955

## feature selection / Using importance of models
importance_xgb_cls_group30 <- varImp(model_xgb_cls_group30,scale=TRUE)  
im_xgb_cls_group30 = as.data.frame(importance_xgb_cls_group30[[1]])
write.csv(im_xgb_cls_group30,"im_xgb_cls_group30.csv")