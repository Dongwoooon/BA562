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

setwd("C:\\Users\\seokj_000\\Desktop\\Challenge_170504")


#### Data final munging
cs_all <- read.csv("cs_merge.csv",stringsAsFactors = TRUE)


cs_all <- cs_all %>%
  filter(CUS_ID!=1201 | CUS_ID!=1270 |CUS_ID!=2276 |CUS_ID!=2426) %>%      # 관측치 이상 아이디들 (time이 없음)
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
cs_all$AGE_GROUP <- factor(cs_all$AGE_GROUP)

## fitControl
fitControl <- trainControl(## 10-fold CV                                        
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 2)


set.seed(1)

#### Caret Random Forest /// GROUP으로 살펴보는 것이 더 정확도 높음 
model_rf_gender <- caret::train(GENDER ~ .-CUS_ID -GROUP -AGE -AGE_GROUP,
                         data = cs_all, 
                         method = "rf",
                         na.action = na.omit,
                         trControl = fitControl)
model_rf_gender

model_rf_agegroup <- caret::train(AGE_GROUP ~ .-CUS_ID -GROUP -AGE -GENDER,
                                data = cs_all, 
                                method = "rf",
                                na.action = na.omit,
                                trControl = fitControl)
model_rf_agegroup

model_rf_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE -AGE_GROUP,
                                data = cs_all, 
                                method = "rf",
                                na.action = na.omit,
                                trControl = fitControl)
model_rf_group


#### Caret xgboost /// AGE_GROUP, GENDER 따로해서 곱하는 것이 더 높게 나
model_xgb_gender <- caret::train(GENDER ~ .-CUS_ID -GROUP -AGE -AGE_GROUP,
                          data = cs_all, 
                          method = "xgbTree",
                          na.action = na.omit,
                          trControl = fitControl
)
model_xgb_gender

model_xgb_agegroup <- caret::train(AGE_GROUP ~ .-CUS_ID -GROUP -AGE -GENDER,
                          data = cs_all, 
                          method = "xgbTree",
                          na.action = na.omit,
                          trControl = fitControl
)
model_xgb_agegroup

model_xgb_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE -AGE_GROUP,
                                   data = cs_all, 
                                   method = "xgbTree",
                                   na.action = na.omit,
                                   trControl = fitControl
)
model_xgb_group


#### feature selection / Using importance of models
importance_rf_group <- varImp(model_rf_group,scale=TRUE)  
importance_rf_group

importance_xgb_agegroup <- varImp(model_xgb_agegroup,scale=TRUE)  
importance_xgb_agegroup

importance_xgb_gender <- varImp(model_xgb_gender,scale=TRUE)
importance_xgb_gender

im_rf_group = as.data.frame(importance_rf_group[[1]])
im_xgb_agegroup = as.data.frame(importance_xgb_agegroup[[1]])
im_xgb_gender = as.data.frame(importance_xgb_gender[[1]])

write.csv(im_rf_group,"1.csv")
write.csv(im_xgb_agegroup,"2.csv")
write.csv(im_xgb_gender,"3.csv")


#xgb_agegroup 상위 변수 45개
"시간비율_뉴스.미디어,소모시간_뉴스.미디어,p_v_.5,cons_cnt9_wk,wk_z,횟수_정보통신.IT,we_day,dec_time,v_t_median,may_cnt,
p_v_.12,time13_z3_we,apr_time,횟수비율_건강.의학,시간비율_정보통신.IT,횟수비율_정치.행정,시간비율_커뮤니티,cons_cnt8_wk,apr_z3,dec_z2,
jun_cnt,time8_z,may_time,dec_z3,p_v_.21,횟수_제조,v_t_max,cons_num9_we,cons_num9,dec_cnt,
시간비율_인터넷.컴퓨터,mean_time,n_v_.21,cons_time5_wk,cons_num9_wk,dec_z,jan_z3,횟수_여행,time12_z2,day_coef_visit,
time9_z2,cons_cnt8,jun_z3,n_v_.17,n_v_.5"


#xgb_gender 상위 변수 45개
"n_v_.11,s_prt_mean,p_v_.5,횟수_쇼핑,횟수비율_정치.행정,p_v_.21,시간비율_여행,v_t_max,oct_day,횟수_게임,
횟수_인터넷.컴퓨터,시간비율_뉴스.미디어,s_prt_max,n_v_.5,time12_z3_we,interval,시간비율_인터넷.컴퓨터,소모시간_교육.학원,소모시간_뉴스.미디어,n_v_.9,
p_v_.8,횟수_뉴스.미디어,s_pr_max,s_pr_median,mean_cnt,시간비율_게임,jun_z2,oct_z,p_v_.12,소모시간_학문,
cons_time12_we,sum_cnt,time18_z2_we,시간비율_교육.학원,cons_time5,cons_cnt8_we,cons_time18_wk,aug_time,mar_z,횟수비율_쇼핑,
p_v_.18,p_v_.1,cons_num12_we,cons_cnt22_wk,n_v_.8"


#### 변수 개수 바꿔가면서 가장 적합한 변수 선정하기
#rf_group
set.seed(1)
cs_all.rf_group <- cs_all %>%
  select(CUS_ID, GENDER, AGE, AGE_GROUP, GROUP,
         cons_num18,시간비율_학문,p_v_.5,day_coef_visit,sum_time,횟수_쇼핑,cons_num12_we,p_v_.21,소모시간_커뮤니티,p_v_.12,
         횟수_서비스,횟수_뉴스.미디어,jan_time,jan_z3,we_cnt,nov_z2,소모시간_건강.의학,소모시간_학문,횟수_정치.행정,횟수_건강.의학,
         cons_time13_we,time8_z3,시간비율_뉴스.미디어,p_v_.11,wk_z2,p_v_.1,aug_time,apr_time,p_v_.8,횟수비율_엔터테인먼트,
         소모시간_뉴스.미디어, time9_z3_we,time9_z2,시간비율_사회.문화.종교,v_t_max)

# final: 상위 변수 35개
model_rf_group <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE -AGE_GROUP,
                               data = cs_all.rf_group, 
                               method = "rf",
                               na.action = na.omit,
                               trControl = fitControl)
model_rf_group


#xgb_gender
set.seed(1)
cs_all.xgb_gender <- cs_all %>%
  select(CUS_ID, GENDER, AGE, AGE_GROUP, GROUP,
         n_v_.11,p_v_.5,횟수_쇼핑,횟수비율_정치.행정,p_v_.21,시간비율_여행,v_t_max,oct_day,횟수_게임,
         횟수_인터넷.컴퓨터,시간비율_뉴스.미디어,n_v_.5,time12_z3_we,interval,시간비율_인터넷.컴퓨터,소모시간_교육.학원,소모시간_뉴스.미디어,n_v_.9,
         p_v_.8,횟수_뉴스.미디어,mean_cnt,시간비율_게임,jun_z2,oct_z,p_v_.12,소모시간_학문,
         cons_time12_we,sum_cnt,time18_z2_we,시간비율_교육.학원,cons_time5,cons_cnt8_we,cons_time18_wk,aug_time,mar_z,횟수비율_쇼핑,
         p_v_.18,p_v_.1,cons_num12_we,cons_cnt22_wk,n_v_.8,dec_z3,cons_time0,횟수_문학.예술,시간비율_정보통신.IT)

# n_v_.11,s_prt_mean,p_v_.5,횟수_쇼핑,횟수비율_정치.행정,p_v_.21,시간비율_여행,v_t_max,oct_day,횟수_게임,
# 횟수_인터넷.컴퓨터,시간비율_뉴스.미디어,s_prt_max,n_v_.5,time12_z3_we,interval,시간비율_인터넷.컴퓨터,소모시간_교육.학원,소모시간_뉴스.미디어,n_v_.9,
# p_v_.8,횟수_뉴스.미디어,s_pr_max,s_pr_median,mean_cnt,시간비율_게임,jun_z2,oct_z,p_v_.12,소모시간_학문,
# cons_time12_we,sum_cnt,time18_z2_we,시간비율_교육.학원,cons_time5,cons_cnt8_we,cons_time18_wk,aug_time,mar_z,횟수비율_쇼핑,
# p_v_.18,p_v_.1,cons_num12_we,cons_cnt22_wk,n_v_.8


# final: 상위 변수 45개
model_xgb_gender <- caret::train(GENDER ~ .-CUS_ID -GROUP -AGE -AGE_GROUP,
                                 data = cs_all.xgb_gender, 
                                 method = "xgbTree",
                                 na.action = na.omit,
                                 trControl = fitControl
)
model_xgb_gender
max(model_xgb_gender[[4]][8])


#xgb_age
set.seed(1)
cs_all.xgb_agegroup <- cs_all %>%
  select(CUS_ID, GENDER, AGE, AGE_GROUP, GROUP,
         시간비율_뉴스.미디어,소모시간_뉴스.미디어,p_v_.5,cons_cnt9_wk,wk_z,횟수_정보통신.IT,we_day,dec_time,v_t_median,may_cnt,
         p_v_.12,time13_z3_we,apr_time,횟수비율_건강.의학,시간비율_정보통신.IT,횟수비율_정치.행정,시간비율_커뮤니티,cons_cnt8_wk,apr_z3,dec_z2,
         jun_cnt,time8_z,may_time,dec_z3,p_v_.21)

# final: 상위 변수 25개
model_xgb_agegroup <- caret::train(AGE_GROUP ~ .-CUS_ID -GROUP -AGE -GENDER,
                                   data = cs_all.xgb_agegroup, 
                                   method = "xgbTree",
                                   na.action = na.omit,
                                   trControl = fitControl
)
model_xgb_agegroup
max(model_xgb_agegroup[[4]][8])




###### Xgboost
# 1. training/test matrix 제작

cs_all_2 <- cs_all %>%
  select(-CUS_ID, -GENDER, -AGE, -GROUP)

cs_all_age <- cs_all %>%
  mutate(AGE_GROUP = substr(GROUP,2,3))%>%
  select(AGE_GROUP)

cs_all_gender <- cs_all %>%
  select(GENDER)

cs_all_group <- cs_all %>%
  select(GROUP)

cs_all_age$AGE_GROUP <- paste("C", cs_all_age$AGE_GROUP, sep="")
cs_all_age$AGE_GROUP <- factor(cs_all_age$AGE_GROUP)

######## xgboost: label must be numeric value starts with 0
levels(cs_all_group$GROUP) <- c(0,1,2,3,4,5)
cs_all_group$GROUP <- as.numeric(cs_all_group$GROUP)-1

### XGBoost supports only xgb.DMatrix, train matrix = predicting variable, label = predicted variable
train_matrix <- apply(cs_all_2, 2, as.numeric)
xgb_train_matrix <- xgb.DMatrix(data = as.matrix(train_matrix),label=cs_all_group$GROUP)

# 2. Training with gbtree
xgb_model <- xgb.train(data = xgb_train_matrix,
                       label = getinfo(xgb_train_matrix, "label"),
                       eta = 1, 
                       max.depth = 3, 
                       objective = "multi:softmax",
                       num_class = 6,
                       eval_metric = "logloss",
                       nround = 10 
)

### train parameters
'label: vector of response values
eta: control the learning rate, low eta = robust to overfitting, 
0 < eta < 1, default=0.3
max_depth: maximum depth of a tree. default=6
objective: learning task & corresponding learning objective,
default = reg:linear(continuous var.)
for binary, binary:logistic
for multiclass, multi:softmax
nround: number of trees used for model building
eval_metric: rmse, logloss, error, ...
early_stopping_rounds: pre-stopping, stop split if the best split have negative gain
'

features = colnames(train_matrix)
importance_matrix_1 <- xgb.importance(features, model = xgb_model)


### predict
pred_1 <- predict(xgb_model, xgb_train_matrix)
unique(pred_1)
sum(pred_1[1:6])



#### softmax: accuracy
actual = matrix(0,nrow=2500,ncol=6)
for (i in 1:nrow(cs_all_group)){
  actual[i,cs_all_group[[1]][i]+1] = 1
}
predicted = matrix(0,nrow=2500,ncol=6)
for (i in 1:length(pred_1)){
  predicted[i,pred_1[i]+1] = 1
}

A<-actual-predicted
sum(abs(A))/2



#### softprob: logloss
actual = matrix(0,nrow=2500,ncol=6)
for (i in 1:nrow(cs_all_group)){
  actual[i,cs_all_group[[1]][i]+1] = 1
}
predicted = matrix(pred_1,nrow=2005,ncol=6)


logLoss <- function(actual, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted,1-eps),eps)
  return(-1*sum(actual * log(predicted)) / nrow(actual))
}
logLoss(actual, predicted) 

