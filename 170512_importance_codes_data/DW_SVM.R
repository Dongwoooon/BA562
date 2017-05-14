library(caret)
library(ROCR)
library(dplyr)

setwd('J:/data/BA562/final/2_Data')

cs_merge_train <- read.csv("cs_merge_train.csv")
fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction=mnLogLoss)

#im_svm_cls_group
########################
set.seed(1)

cs_merge_train_sel <- cs_merge_train %>%
  select(CUS_ID, GENDER, AGE,GROUP,
         p_v_.4,횟수비율_쇼핑,p_v_.5,시간비율_뉴스.미디어,p_v_.12,횟수_쇼핑,횟수_제조,p_v_.19,시간비율_게임,n_v_.11,
           p_v_.11,cons_time8_we,소모시간_쇼핑,p_v_.1,s_prt_mean,횟수_생활.가정.취미,p_v_.9,횟수비율_스포츠.레저,p_v_.3,횟수비율_뉴스.미디어,
           횟수비율_게임,cons_num9_we,interval,시간비율_온라인교육,p_v_.20,소모시간_생활.가정.취미,횟수비율_사회.문화.종교,횟수비율_온라인교육,시간비율_금융.부동산,시간비율_제조,
           n_v_.2,mean_time_cnt,소모시간_게임,s_prt_sd,횟수비율_커뮤니티,횟수_금융.부동산,s_pr_sd,횟수_교육.학원,s_prt_median,v_pr_max,
           횟수비율_서비스,n_v_.4,s_v_mean,cat_coef_visit,횟수비율_건강.의학,시간비율_여행,n_v_.16,v_t_median,sep_ratio,cons_cnt18_we,
           mean_time,mon_ratio3,wk_ratio2,소모시간_스포츠.레저,횟수비율_교육.학원,v_t_max,day_coef_cnt,횟수비율_비즈니스.경제,nov_ratio2,jan_z2,
           시간비율_커뮤니티,p_v_.8,mon_day,fri_ratio3,cat_coef_cnt,wednes_ratio2,소모시간_뉴스.미디어,시간비율_사회.문화.종교,cons_num22,시간비율_서비스)

write.csv(cs_merge_train_sel,'cs_merge_train_sel.csv',row.names = F)

cs_merge_train_sel <- read.csv("cs_merge_train_sel.csv")
model_svm_cls_group70 <- caret::train(GROUP~ . -CUS_ID -GENDER -AGE,
                                      data = cs_merge_train_sel,
                                      preProcess = NULL,
                                      metric = "logLoss",
                                      method = "svmRadial",
                                      trControl = fitControl)

model_svm_cls_group70
