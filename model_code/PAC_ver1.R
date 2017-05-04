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

setwd("C:\\Users\\seokj_000\\Desktop\\Challenge_170426")


#### Merge custumer signature
cs.v1 <- read.csv("cs_others.csv")
cs.v2 <- read.csv("cs_visit_vars.csv")
cs.v3 <- read.csv("cs_websession.csv")


cs_all <- cs.v1 %>%
  left_join(cs.v2) %>%
  left_join(cs.v3) %>%
  select(-RESIDENCE)

cs_all[is.na(cs_all[,"wk_pat1"]) ,"wk_pat1"]<-"유형없음"
cs_all[is.na(cs_all[,"wk_pat2"]) ,"wk_pat2"]<-"유형없음"
cs_all[is.na(cs_all[,"wk_pat3"]) ,"wk_pat3"]<-"유형없음"
cs_all[is.na(cs_all[,"day_pat1"]) ,"day_pat1"]<-"유형없음"
cs_all[is.na(cs_all[,"day_pat2"]) ,"day_pat2"]<-"유형없음"
cs_all[is.na(cs_all[,"day_pat3"]) ,"day_pat3"]<-"유형없음"
cs_all[is.na(cs_all[,"month_pat1"]) ,"month_pat1"]<-"유형없음"
cs_all[is.na(cs_all[,"month_pat2"]) ,"month_pat2"]<-"유형없음"
cs_all[is.na(cs_all[,"month_pat3"]) ,"month_pat3"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat1"]) ,"time_pat1"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat2"]) ,"time_pat2"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat3"]) ,"time_pat3"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat_wk"]) ,"time_pat_wk"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat2_wk"]) ,"time_pat2_wk"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat3_wk"]) ,"time_pat3_wk"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat_we"]) ,"time_pat_we"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat2_we"]) ,"time_pat2_we"]<-"유형없음"
cs_all[is.na(cs_all[,"time_pat3_we"]) ,"time_pat3_we"]<-"유형없음"

for(i in 1:631) cs_all[is.na(cs_all[,i]),i]<-0

write.csv(cs_all,"cs_merge.csv",row.names=FALSE)

rm(list=ls())


#### Model
cs_all <- read.csv("cs_merge.csv",stringsAsFactors = TRUE)


cs_all$GROUP <- factor(cs_all$GROUP)

fitControl <- trainControl(## 10-fold CV                                        
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 2)


##### Only age group
#cs_all_age <- cs_all %>%
#  mutate(AGE_GROUP = substr(GROUP,2,3))
#
#cs_all_age$AGE_GROUP <- factor(cs_all_age$AGE_GROUP)




set.seed(1)

#### Caret Random Forest
model_rf <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                         data = cs_all, 
                         method = "rf",
                         na.action = na.omit,
                         trControl = fitControl)
model_rf

importance <- varImp(model_rf,scale=TRUE)  
importance


#### Caret xgboost
model_xgb <- caret::train(GROUP ~ .-CUS_ID -GENDER -AGE,
                          data = cs_all, 
                          method = "xgbTree",
                          na.action = na.omit,
                          trControl = fitControl
)

importance <- varImp(model_xgb,scale=TRUE)  
importance



###### Xgboost
# 1. training/test matrix ��??

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

