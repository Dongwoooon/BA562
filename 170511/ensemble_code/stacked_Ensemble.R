####################################################
###    Model Ensemble: Stacked Generalization    ###
####################################################

##### Required packages
library(caret)
library(dplyr)
library(caretEnsemble)

setwd("D:/2016~/전공/비즈니스모델링분석/PAC/Github/BA562/cs_csv/R.data")

##### Read data
load("rf_group_train.Rdata")
load("xgb_group_train.Rdata")
load("rf_group_test.Rdata")
load("xgb_group_test.Rdata")


### Build meta-model
# Create stacked dataset
train_data <- rf_group_train%>%left_join(xgb_group_train,by=c("CUS_ID","GROUP"))
train_data$GROUP <- substr(train_data$GROUP,1,3)



stack.train.x <- train_data%>%select(-CUS_ID,-GROUP)
stack.train.y <- factor(train_data$GROUP)


control <- trainControl(method="repeatedcv", number=5, repeats=2, classProbs=TRUE, summaryFunction=mnLogLoss)
# using RF
set.seed(222)
meta.model <- caret::train(stack.train.x, stack.train.y,
                         method = "rf",
                         preProcess = NULL,
                         metric = "logLoss",
                         trControl = control)
print(meta.model$results)  # logloss=0.1290817

### Generate predictions
# Reuse the sub-models and meta-model to make predictions on the test dataset
test_data<-rf_group_test%>%left_join(xgb_group_test,by="CUS_ID")
test_data_id<-test_data%>%select(CUS_ID)
stack.test.x <- test_data%>%select(-CUS_ID)

stack.pred <- predict(meta.model, stack.test.x, type="prob")
stack.pred <- as.data.frame(c(test_data_id,stack.pred))
colnames(stack.pred)<-c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")
