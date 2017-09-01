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


cs_merge_test_cut <- read.csv("cs_merge_test_cut.csv")




cs_merge_test_cut_45 <- cs_merge_test_cut %>%
  select(CUS_ID,
         횟수비율_쇼핑,p_v_.4,p_v_.5,p_v_.12,p_v_.20,시간비율_게임,
         시간비율_뉴스.미디어,p_v_.9,s_v_mean,v_t_median,p_v_.3,
         시간비율_온라인교육,p_v_.19,wk_z2,p_v_.16,s_prt_sd,
         v_pr_max,mean_time_cnt,cons_time9_we,횟수비율_사회.문화.종교,
         interval,시간비율_서비스,nov_z2,횟수비율_건강.의학,day_coef_cnt,
         횟수비율_인터넷.컴퓨터,s_pr_median,횟수비율_비즈니스.경제,
         time5_z2_wk,cons_time22,time8_z3,jan_z2,time0_z2_wk,
         시간비율_커뮤니티,cons_num13_we,p_v_.13,sep_z,p_v_.1,net_day,
         time13_z3,time0_z_we,시간비율_여행,시간비율_문학.예술,cons_num8_wk,time12_z2_wk)





load("model_xgb_cls_group45.Rdata")

CS <- cs_merge_test_cut_45 %>%
  arrange(CUS_ID)%>%
  select(CUS_ID)



xgb_cls_group_test45=predict(model_xgb_cls_group45, newdata = cs_merge_test_cut_45, type="prob")
xgb_cls_group_test45 <- as.data.frame(c(CS,xgb_cls_group_test45))
colnames(xgb_cls_group_test45) <- c("CUS_ID","F20-","F30","F40+","M20-","M30","M40+")

write.csv(xgb_cls_group_test45,"Ensemble_s2v300_300\\svm_s2v300_test.csv",row.names = F)



