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

## Caret xgboost
cs_merge_train_60<-cs_merge_train%>%
  select(CUS_ID,GROUP,GENDER,AGE,횟수비율_쇼핑,p_v_.4,p_v_.12,시간비율_뉴스.미디어,p_v_.5,시간비율_게임,p_v_.9,p_v_.20,p_v_.19,wk_z2,횟수비율_사회.문화.종교,시간비율_온라인교육,시간비율_커뮤니티,시간비율_서비스,p_v_.1,cons_time9_we,mean_time_cnt,v_t_median,횟수비율_건강.의학,time5_z2_wk,nov_z2,day_coef_cnt,시간비율_문학.예술,interval,jan_z2,p_v_.3,횟수비율_비즈니스.경제,p_v_.16,s_v_mean,p_v_.17,v_pr_max,cons_cnt18_we,cons_num5,time0_z2_wk,시간비율_여행,s_prt_sd,cat_coef_visit,cons_time22,p_v_.13,cons_num13_we,sep_z,cons_num8_wk,s_pr_median,mean_time,횟수비율_인터넷.컴퓨터,cons_cnt9_wk,apr_z2,dec_z2,time8_z3,time13_z3,time12_z2_wk,sep_z2,time0_z_we,time9_z_wk,jun_z2,cons_num12_we,net_day,time22_z3_wk,cat_coef_cnt,cons_num18_wk)

model_xgb_cls_group60 <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                                    data = cs_merge_train_60, 
                                    preProcess = NULL,
                                    metric = "logLoss",
                                    method = "xgbTree",
                                    trControl = fitControl)
model_xgb_cls_group60
min(model_xgb_cls_group60[[4]][8])   ## 

## feature selection / Using importance of models
importance_xgb_cls_group60 <- varImp(model_xgb_cls_group60,scale=TRUE)  
im_xgb_cls_group60 = as.data.frame(importance_xgb_cls_group60[[1]])



write.csv(im_xgb_cls_group,"im_xgb_cls_group.csv")

