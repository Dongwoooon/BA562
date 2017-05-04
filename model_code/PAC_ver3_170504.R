# PAC 
# Version 3 (only clickstreams data)
# Date: 2017.05.04
# DY DW SJ
# ------------------------------------------------------------------

#cs_all[is.na(cs_all[,i]) ,i]<-0

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

setwd("C:/Users/Dongyoun/Documents")


#### Data final munging
cs_all <- read.csv("cs_merge.csv",stringsAsFactors = TRUE)
#cs_all <- read.csv("test_cs_merge.csv",stringsAsFactors = TRUE)

cs_all <- cs_all %>%
  filter(CUS_ID!=1201 | CUS_ID!=1270 |CUS_ID!=2276 |CUS_ID!=2426) %>%      # 관측치 이상 아이디들 (time이 없음))
  select(-wk_ratio1, -we_ratio1, -wk_ratio2, -we_ratio2, -wk_ratio3, -we_ratio3,
         -mon_time:-day_pat3, -jan_ratio:-dec_ratio,-jan_ratio2:-dec_ratio2, -jan_ratio3:-dec_ratio3,
         -cons_num0_ratio:-cons_num22_ratio,-cons_time0_ratio:-cons_time22_ratio,-cons_cnt0_ratio:-cons_cnt22_ratio,
         -cons_num0_ratio_wk:-cons_num22_ratio_wk,-cons_time0_ratio_wk:-cons_time22_ratio_wk,-cons_cnt0_ratio_wk:-cons_cnt22_ratio_wk,
         -cons_num0_ratio_we:-cons_num22_ratio_we,-cons_time0_ratio_we:-cons_time22_ratio_we,-cons_cnt0_ratio_we:-cons_cnt22_ratio_we,
         -we_z,-we_z2,-we_z3)


#### Model
## factor
cs_all$GROUP <- factor(cs_all$GROUP)
cs_all$GENDER <- factor(cs_all$GENDER)
cs_all <- cs_all %>%
  mutate(AGE_GROUP = substr(GROUP,2,3))

cs_all$AGE_GROUP <- paste("p_", cs_all$AGE_GROUP, sep="")
cs_all$AGE_GROUP <- factor(cs_all$AGE_GROUP)

## fitControl
fitControl <- trainControl(## 10-fold CV                                        
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 2)


#### 변수 개수 바꿔가면서 가장 적합한 변수 선정하기
#rf_group
set.seed(1)
cs_all.rf_group <- cs_all %>%
  select(GROUP,
         cons_num18,시간비율_학문,p_v_.5,day_coef_visit,sum_time,횟수_쇼핑,cons_num12_we,p_v_.21,소모시간_커뮤니티,p_v_.12,
         횟수_서비스,횟수_뉴스.미디어,jan_time,jan_z3,we_cnt,nov_z2,소모시간_건강.의학,소모시간_학문,횟수_정치.행정,횟수_건강.의학,
         cons_time13_we,time8_z3,시간비율_뉴스.미디어,p_v_.11,wk_z2,p_v_.1,aug_time,apr_time,p_v_.8,횟수비율_엔터테인먼트,
         소모시간_뉴스.미디어, time9_z3_we,time9_z2,시간비율_사회.문화.종교,v_t_max)

# final: 상위 변수 35개
model_rf_group <- caret::train(GROUP~.,
                               data = cs_all.rf_group, 
                               method = "rf",
                               trControl = fitControl)
#model_rf_group


#xgb_gender
set.seed(1)
cs_all.xgb_gender <- cs_all %>%
  select(GENDER,
         n_v_.11,p_v_.5,횟수_쇼핑,횟수비율_정치.행정,p_v_.21,시간비율_여행,v_t_max,oct_day,횟수_게임,
         횟수_인터넷.컴퓨터,시간비율_뉴스.미디어,n_v_.5,time12_z3_we,interval,시간비율_인터넷.컴퓨터,소모시간_교육.학원,소모시간_뉴스.미디어,n_v_.9,
         p_v_.8,횟수_뉴스.미디어,mean_cnt,시간비율_게임,jun_z2,oct_z,p_v_.12,소모시간_학문,
         cons_time12_we,sum_cnt,time18_z2_we,시간비율_교육.학원,cons_time5,cons_cnt8_we,cons_time18_wk,aug_time,mar_z,횟수비율_쇼핑,
         p_v_.18,p_v_.1,cons_num12_we,cons_cnt22_wk,n_v_.8,dec_z3,cons_time0,횟수_문학.예술,시간비율_정보통신.IT)

# final: 상위 변수 45개
model_xgb_gender <- caret::train(GENDER ~ .,
                                 data = cs_all.xgb_gender, 
                                 method = "xgbTree",
                                 trControl = fitControl
)
#model_xgb_gender
#max(model_xgb_gender[[4]][8])


#xgb_age
set.seed(1)
cs_all.xgb_agegroup <- cs_all %>%
  select(AGE_GROUP,
         시간비율_뉴스.미디어,소모시간_뉴스.미디어,p_v_.5,cons_cnt9_wk,wk_z,횟수_정보통신.IT,we_day,dec_time,v_t_median,may_cnt,
         p_v_.12,time13_z3_we,apr_time,횟수비율_건강.의학,시간비율_정보통신.IT,횟수비율_정치.행정,시간비율_커뮤니티,cons_cnt8_wk,apr_z3,dec_z2,
         jun_cnt,time8_z,may_time,dec_z3,p_v_.21)

# final: 상위 변수 25개
model_xgb_agegroup <- caret::train(AGE_GROUP~.,
                                   data = cs_all.xgb_agegroup, 
                                   method = "xgbTree",
                                   trControl = fitControl
)
#model_xgb_agegroup
#max(model_xgb_agegroup[[4]][8])


#probability 뽑기 (train data)
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
  select(1,5:10)
colnames(rf_group_train)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
save(rf_group_train,file="rf_group_train.Rdata")


xgb_age_train=predict(model_xgb_agegroup, newdata = cs_all2, type="prob")
xgb_age_train<-as.data.frame(c(data,xgb_age_train))
b<-xgb_age_train
xgb_age_train<-b%>%
  select(1,5:7)
save(xgb_age_train,file="xgb_age_train.Rdata")


xgb_gender_train=predict(model_xgb_gender, newdata = cs_all2, type="prob")
xgb_gender_train<-as.data.frame(c(data,xgb_gender_train))
c<-xgb_gender_train
xgb_gender_train<-c%>%
  select(1,5,6)
colnames(xgb_gender_train)<-c("CUS_ID","M","F")
xgb_gender_train<-xgb_gender_train%>%select(1,3,2)
save(xgb_gender_train,file="xgb_gender_train.Rdata")

# #probability 뽑기 (test data)
# 
# data<-cs_all%>%
#   arrange(CUS_ID)%>%
#   select(CUS_ID)
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