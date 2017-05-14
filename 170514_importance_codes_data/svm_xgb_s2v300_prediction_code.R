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

setwd("C:\\Users\\seokj_000\\Desktop\\Challenge_170514")


cs_s2v300_train <- read.csv("cs_site2vec300_train.csv")
cs_s2v300_train$GROUP <- substr(cs_s2v300_train$GROUP,1,3)
cs_s2v300_train$GROUP <- factor(cs_s2v300_train$GROUP)

cs_s2v300_test <- read.csv("cs_site2vec300_test.csv")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction=mnLogLoss)


#### xgb_cls_group
# cs_s2v300_train_85 <- cs_s2v300_train %>%
#   select(CUS_ID,GROUP,
#          V139,V242,V80,V63,V125,V71,V208,V267,V1,V175,V55,V62,V135,V294,V77,V86,
#          V28,V209,V150,V106,V75,V229,V182,V154,V56,V243,V142,V222,V143,V285,V264,
#          V249,V19,V130,V140,V207,V3,V73,V43,V53,V186,V183,V8,V149,V286,V116,V102,
#          V158,V57,V253,V88,V199,V200,V113,V281,V42,V213,V287,V234,V204,V160,V16,
#          V250,V7,V15,V24,V257,V266,V245,V126,V288,V251,V189,V37,V146,V216,V276,
#          V93,V233,V100,V50,V133,V246,V282,V155)
# 
# 
# cs_s2v300_test_85 <- cs_s2v300_test %>%
#   select(CUS_ID,
#          V139,V242,V80,V63,V125,V71,V208,V267,V1,V175,V55,V62,V135,V294,V77,V86,
#          V28,V209,V150,V106,V75,V229,V182,V154,V56,V243,V142,V222,V143,V285,V264,
#          V249,V19,V130,V140,V207,V3,V73,V43,V53,V186,V183,V8,V149,V286,V116,V102,
#          V158,V57,V253,V88,V199,V200,V113,V281,V42,V213,V287,V234,V204,V160,V16,
#          V250,V7,V15,V24,V257,V266,V245,V126,V288,V251,V189,V37,V146,V216,V276,
#          V93,V233,V100,V50,V133,V246,V282,V155)
# 
# 
set.seed(1)
model_svm_s2v300_group300 <- caret::train(GROUP~ . -CUS_ID,
                                      data = cs_s2v300_train,
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "svmRadial",
                                      trControl = fitControl)

model_svm_s2v300_group300

save(model_svm_s2v300_group300,file="model_svm_s2v300_group300.Rdata")


load("model_xgb_s2v300_group300.Rdata")
load("model_svm_s2v300_group300.Rdata")

#### probability 뽑기 (s2v300 을 활용한 svm, xgb 85개 변수로 predict하는 코드) 



CS <- cs_s2v300_test%>%
  arrange(CUS_ID)%>%
  select(CUS_ID)


svm_s2v300_group_test=predict(model_svm_s2v300_group300, newdata = cs_s2v300_test, type="prob")
svm_s2v300_group_test <- as.data.frame(c(CS,svm_s2v300_group_test))
colnames(svm_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")

xgb_s2v300_group_test=predict(model_xgb_s2v300_group, newdata = cs_s2v300_test, type="prob")
xgb_s2v300_group_test <- as.data.frame(c(CS,xgb_s2v300_group_test))
colnames(xgb_s2v300_group_test) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")

write.csv(svm_s2v300_group_test,"Ensemble_s2v300_300\\svm_s2v300_test.csv",row.names = F)
write.csv(xgb_s2v300_group_test,"Ensemble_s2v300_300\\xgb_s2v300_test.csv",row.names = F)








