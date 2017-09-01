# PAC derived variable
# Web session
# Date: 2017.04.22
# Written by Seokjoon Yoon
# ------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)

rm(list=ls())

setwd("D:\\비즈니스 모델링\\Predictive Analytics Challenge")


#cls<-read.delim("train_clickstreams.tab",stringsAsFactors = T)
cls<-read.delim("test_clickstreams.tab",stringsAsFactors = T)

head(cls)


#### web session

cls_t <- cls %>%
  arrange(CUS_ID, TIME_ID) %>%
  select(CUS_ID,TIME_ID)


nrow=nrow(cls_t)
## 2011010100 기준으로 지난 시간 구하기
for(i in nrow){
  cls_t$Dur= 24*(ymd_h(cls_t$TIME_ID)-ymd_h("2011010100"))
}

cls_t$Dur <- as.numeric(cls_t$Dur)

cls_t.mat <- cls_t %>%
  select(CUS_ID,Dur)
cls_t.mat <- as.matrix(cls_t.mat)



nrow=nrow(cls_t.mat)
ncus=tail(cls_t.mat,n=1)[1,1]
wsess=1
counter=1
for (i in 2:nrow){
  if (cls_t.mat[i,1]-cls_t.mat[i-1,1]==0){
    if (cls_t.mat[i,2]-cls_t.mat[i-1,2]==0 | cls_t.mat[i,2]-cls_t.mat[i-1,2]==1)
      wsess[i]=counter
    else
      counter=counter+1
      wsess[i]=counter
  }
  else{
    counter=1
    wsess[i]=counter
  }
}
wsess=as.data.frame(wsess)



cls_a <- cls %>%
  arrange(CUS_ID, TIME_ID)

cls3=cbind(cls_a,wsess)

save(cls3,file="cls_w_websession_test.Rdata")



#### Derived variable from web session
# 1. s_v / number of website visits per web session
# 2. s_t / time per web session
# 3. s_pr / number of page requests per web session
# 4. s_prt / average time between two subsequent page requewsts during a web session
load("cls_w_websession_test.Rdata")

cs.websession <- cls3 %>%
  group_by(CUS_ID,wsess) %>%
  summarise(s_v=n(), s_t=sum(ST_TIME), s_pr=sum(SITE_CNT), s_prt=(sum(ST_TIME)/sum(SITE_CNT))) %>%
  group_by(CUS_ID) %>%
  summarise_each(funs(min,max,mean,median,sd), s_v, s_t, s_pr, s_prt)

write.csv(cs.websession,"cs_websession_test.csv",row.names=F)

