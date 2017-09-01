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
         -we_z,-we_z2,-we_z3,-s_v_min:-s_prt_sd)


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

# #### Caret Random Forest /// GROUP으로 살펴보는 것이 더 정확도 높음 
# model_rf_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE -AGE_GROUP,
#                                data = cs_all, 
#                                method = "rf",
#                                trControl = fitControl)
# model_rf_group
# 
# 
# #### Caret xgboost /// AGE_GROUP, GENDER 따로해서 곱하는 것이 더 높게 나
# model_xgb_gender <- caret::train(GENDER ~ .-CUS_ID -GROUP -AGE -AGE_GROUP,
#                                  data = cs_all, 
#                                  method = "xgbTree",
#                                  trControl = fitControl
# )
# model_xgb_gender
# 
# model_xgb_agegroup <- caret::train(AGE_GROUP ~ .-CUS_ID -GROUP -AGE -GENDER,
#                                    data = cs_all, 
#                                    method = "xgbTree",
#                                    trControl = fitControl
# )
# model_xgb_agegroup
# 
# #### feature selection / Using importance of models
# importance_rf_group <- varImp(model_rf_group,scale=TRUE)  
# importance_rf_group
# 
# importance_xgb_agegroup <- varImp(model_xgb_agegroup,scale=TRUE)  
# importance_xgb_agegroup
# 
# importance_xgb_gender <- varImp(model_xgb_gender,scale=TRUE)
# importance_xgb_gender
# 
# im_rf_group = as.data.frame(importance_rf_group[[1]])
# im_xgb_agegroup = as.data.frame(importance_xgb_agegroup[[1]])
# im_xgb_gender = as.data.frame(importance_xgb_gender[[1]])
# 
# write.csv(im_rf_group,"im_rf_group.csv")
# write.csv(im_xgb_agegroup,"im_xgb_agegroup.csv")
# write.csv(im_xgb_gender,"im_xgb_gender.csv")


#### 변수 개수 바꿔가면서 가장 적합한 변수 선정하기
#rf_group
"p_v_.5,횟수비율_쇼핑,p_v_.4,시간비율_뉴스.미디어,횟수_쇼핑,mean_time_cnt,p_v_.11,v_t_median,p_v_.20,소모시간_게임,
횟수비율_뉴스.미디어,시간비율_게임,n_v_.11,p_v_.9,interval,v_pr_max,p_v_.1,p_v_.12,시간비율_제조,횟수비율_제조,
시간비율_쇼핑,cat_coef_visit,p_v_.17,횟수비율_건강.의학,n_v_.4,소모시간_제조,횟수비율_생활.가정.취미,횟수비율_금융.부동산,jan_z2,mon_coef_time,
시간비율_금융.부동산,시간비율_생활.가정.취미,v_pr_sd,횟수비율_게임,횟수비율_온라인교육,시간비율_온라인교육,횟수비율_인터넷.컴퓨터,v_t_max,day_coef_visit,p_v_.21,
시간비율_인터넷.컴퓨터,p_v_.10,p_v_.13,nov_z,dec_z3,횟수비율_커뮤니티,mon_coef_visit,시간비율_서비스,p_v_.19,cat_coef_time,
소모시간_뉴스.미디어,dec_z2,net_day,nov_z2,p_v_.18
"
set.seed(1)
cs_all.rf_group <- cs_all %>%
  select(GROUP,
         p_v_.5,횟수비율_쇼핑,p_v_.4,시간비율_뉴스.미디어,횟수_쇼핑,mean_time_cnt,p_v_.11,v_t_median,p_v_.20,소모시간_게임,
         횟수비율_뉴스.미디어,시간비율_게임,n_v_.11,p_v_.9,interval,v_pr_max,p_v_.1,p_v_.12,시간비율_제조,횟수비율_제조,
         시간비율_쇼핑,cat_coef_visit,p_v_.17,횟수비율_건강.의학,n_v_.4,소모시간_제조,횟수비율_생활.가정.취미,횟수비율_금융.부동산,jan_z2,mon_coef_time,
         시간비율_금융.부동산,시간비율_생활.가정.취미,v_pr_sd,횟수비율_게임,횟수비율_온라인교육,시간비율_온라인교육,횟수비율_인터넷.컴퓨터,v_t_max,day_coef_visit,p_v_.21)

# final: 상위 변수 40개
model_rf_group <- caret::train(GROUP~.,
                               data = cs_all.rf_group, 
                               method = "rf",
                               trControl = fitControl)
model_rf_group


#xgb_gender
"횟수비율_쇼핑,횟수비율_제조,p_v_.12,횟수비율_게임,p_v_.5,p_v_.9,시간비율_제조,횟수_쇼핑,횟수비율_스포츠.레저,횟수비율_생활.가정.취미,
소모시간_게임,횟수비율_정치.행정,횟수비율_뉴스.미디어,시간비율_커뮤니티,mar_cnt,소모시간_문학.예술,time13_z2_wk,소모시간_생활.가정.취미,cons_time5_we,시간비율_온라인교육,
v_pr_sd,횟수_건강.의학,interval,p_v_.13,횟수비율_인터넷.컴퓨터,mon_coef_cnt,v_t_max,시간비율_뉴스.미디어,p_v_.20,time22_z_wk,
cat_coef_visit,cons_num13_wk,jan_z2,횟수_교육.학원,oct_z2,mean_time_cnt,p_v_.21,횟수_게임,시간비율_서비스,n_v_.4,
cons_time18_wk,v_t_sd,jan_z3,n_v_.11,횟수비율_사회.문화.종교,mar_z2,시간비율_생활.가정.취미,time5_z,p_v_.3,횟수비율_온라인교육,
횟수_정치.행정,n_v_.20,p_v_.19,횟수_사회.문화.종교,time18_z_we
"

set.seed(1)
cs_all.xgb_gender <- cs_all %>%
  select(GENDER,
         횟수비율_쇼핑,횟수비율_제조,p_v_.12,횟수비율_게임,p_v_.5,p_v_.9,시간비율_제조,횟수_쇼핑,횟수비율_스포츠.레저,횟수비율_생활.가정.취미,
         소모시간_게임,횟수비율_정치.행정,횟수비율_뉴스.미디어,시간비율_커뮤니티,mar_cnt,소모시간_문학.예술,time13_z2_wk,소모시간_생활.가정.취미,cons_time5_we,시간비율_온라인교육,
         v_pr_sd,횟수_건강.의학,interval,p_v_.13,횟수비율_인터넷.컴퓨터,mon_coef_cnt,v_t_max,시간비율_뉴스.미디어,p_v_.20,time22_z_wk,
         cat_coef_visit,cons_num13_wk,jan_z2,횟수_교육.학원,oct_z2,mean_time_cnt,p_v_.21,횟수_게임,시간비율_서비스,n_v_.4,
         cons_time18_wk,v_t_sd,jan_z3,n_v_.11,횟수비율_사회.문화.종교,mar_z2,시간비율_생활.가정.취미,time5_z,p_v_.3,횟수비율_온라인교육)

# final: 상위 변수 50개
model_xgb_gender <- caret::train(GENDER ~ .,
                                 data = cs_all.xgb_gender, 
                                 method = "xgbTree",
                                 trControl = fitControl
)
#model_xgb_gender
max(model_xgb_gender[[4]][8])


#xgb_age
"p_v_.4,p_v_.5,시간비율_뉴스.미디어,횟수비율_쇼핑,mean_time_cnt,p_v_.16,횟수_쇼핑,v_t_median,횟수비율_금융.부동산,p_v_.1,
횟수_서비스,p_v_.11,시간비율_온라인교육,횟수_커뮤니티,횟수_교육.학원,day_coef_visit,n_v_.11,cons_cnt8_we,횟수_생활.가정.취미,sep_day,
소모시간_정치.행정,횟수비율_문학.예술,mon_coef_time,v_pr_max,p_v_.12,n_v_.4,interval,p_v_.20,횟수비율_비즈니스.경제,시간비율_여행,
wk_z,wk_z3,소모시간_생활.가정.취미,wk_z2,횟수비율_사회.문화.종교,시간비율_게임,횟수비율_교육.학원,time0_z_we,소모시간_게임,cons_time13_we,
cons_num8_wk,jan_z2,time22_z_we,횟수비율_뉴스.미디어,p_v_.8,시간비율_금융.부동산,cons_time9,cons_time8_we,cons_num8,oct_z2,
n_v_.1,n_v_.9,cons_num13_we,cons_num12_wk,p_v_.3
"

set.seed(1)
cs_all.xgb_agegroup <- cs_all %>%
  select(AGE_GROUP,
         p_v_.4,p_v_.5,시간비율_뉴스.미디어,횟수비율_쇼핑,mean_time_cnt,p_v_.16,횟수_쇼핑,v_t_median,횟수비율_금융.부동산,p_v_.1,
         횟수_서비스,p_v_.11,시간비율_온라인교육,횟수_커뮤니티,횟수_교육.학원,day_coef_visit,n_v_.11,cons_cnt8_we,횟수_생활.가정.취미,sep_day,
         소모시간_정치.행정,횟수비율_문학.예술,mon_coef_time,v_pr_max,p_v_.12,n_v_.4,interval,p_v_.20,횟수비율_비즈니스.경제,시간비율_여행,
         wk_z,wk_z3,소모시간_생활.가정.취미,wk_z2,횟수비율_사회.문화.종교,시간비율_게임,횟수비율_교육.학원,time0_z_we,소모시간_게임,cons_time13_we,
         cons_num8_wk,jan_z2,time22_z_we,횟수비율_뉴스.미디어,p_v_.8)

# final: 상위 변수 45개
model_xgb_agegroup <- caret::train(AGE_GROUP~.,
                                   data = cs_all.xgb_agegroup, 
                                   method = "xgbTree",
                                   trControl = fitControl
)
#model_xgb_agegroup
max(model_xgb_agegroup[[4]][8])


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