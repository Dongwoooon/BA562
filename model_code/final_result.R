library(dplyr)


rm(list=ls())

setwd("C:\\Users\\seokj_000\\Desktop\\Challenge_170504")

setwd('J:/data/BA562/final/2_Data')


load("rf_group_test.Rdata")
load("rf_group_train.Rdata")
load("xgb_age_test.Rdata")
load("xgb_age_train.Rdata")
load("xgb_gender_test.Rdata")
load("xgb_gender_train.Rdata")


cs_all <- read.csv("cs_merge.csv")
cs_all <- cs_all %>%
  mutate(AGE_GROUP = substr(GROUP,2,3)) %>%
  select(CUS_ID, GROUP)

cs_all$CUS_ID = as.numeric(as.character(cs_all$CUS_ID))

A<-xgb_age_train%*%xgb_gender_train


zeros = as.data.frame(matrix(0,nrow=2500,ncol=7))
names(zeros) <- names(rf_group_train)


a=c(1,2)
b=c(3,5,7)

A <- merge(xgb_age_train,xgb_gender_train,by="CUS_ID")
temp <- A %>%
  mutate(F20 = p_20*F, F30 = p_30*F, F40 = p_40*F, M20 = p_20*M, M30 = p_30*M, M40 = p_40*M) %>%
  select(CUS_ID, F20, F30, F40, M20, M30, M40)
temp <- merge(cs_all,temp,by="CUS_ID")
names(temp) <- names(rf_group_train)
xgb_group_train_en <- temp

A <- merge(xgb_age_test,xgb_gender_test,by="CUS_ID")
temp <- A %>%
  mutate(F20 = p_20*F, F30 = p_30*F, F40 = p_40*F, M20 = p_20*M, M30 = p_30*M, M40 = p_40*M) %>%
  select(CUS_ID, F20, F30, F40, M20, M30, M40)
names(temp) <- names(rf_group_test)
xgb_group_test_en <- temp

get_prob <- function(x,i){
  if(x=='F20-'){
    return(temp[i,3])
  }
  else if(x=='F30'){
    return(temp[i,4])
  }
  else if(x=='F40+'){
    return(temp[i,5])
  }
  else if(x=='M20-'){
    return(temp[i,6])
  }
  else if(x=='M30'){
    return(temp[i,7])
  }
  else{
    return(temp[i,8])
  }
}

xgb_group_train_en$prob <- mapply(get_prob,temp$GROUP,1:nrow(temp))

temp <- rf_group_train
rf_group_train$prob <- mapply(get_prob,temp$GROUP,1:nrow(temp))

weight_prev_rf=mean(rf_group_train[,9])
weight_prev_xgb=mean(xgb_group_train_en[,9])

w_rf = weight_prev_rf/(weight_prev_rf+weight_prev_xgb)
w_xgb = weight_prev_xgb/(weight_prev_rf+weight_prev_xgb)


ensem_group_test <- w_xgb*rf_group_test + w_rf*xgb_group_test_en
ensem_group_test2 <- w_rf*rf_group_test + w_xgb*xgb_group_test_en

write.csv(ensem_group_test,"final_result.csv",row.names = FALSE)
write.csv(ensem_group_test2,"additive_result.csv",row.names = FALSE)








