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


cls<-read.delim("train_clickstreams.tab",stringsAsFactors = T)

head(cls)



#### web session

cls2 <- cls %>%
  arrange(CUS_ID, TIME_ID)


nrow=nrow(cls2)
ncus=tail(cls2,n=1)$CUS_ID
wsess=1
counter=1
for (i in 2:nrow){
  if (cls2$CUS_ID[i]-cls2$CUS_ID[i-1]==0){
    if (ymd_h(cls2$TIME_ID[i])-ymd_h(cls2$TIME_ID[i-1])==0 | ymd_h(cls2$TIME_ID[i])-ymd_h(cls2$TIME_ID[i-1])==1)
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
cls3=cbind(cls2,wsess)


#### Derived variable from web session
# 1. s_v / number of website visits per web session
# 2. s_t / time per web session
# 3. s_pr / number of page requests per web session
# 4. s_prt / average time between two subsequent page requewsts during a web session
cs.websession <- cls3 %>%
  group_by(CUS_ID,wsess) %>%
  summarise(s_v=n(), s_t=sum(ST_TIME), s_pr=sum(SITE_CNT), s_prt=(sum(ST_TIME)/sum(SITE_CNT))) %>%
  group_by(CUS_ID) %>%
  summarise_each(funs(min,max,mean,median,sd), s_v, s_t, s_pr, s_prt)


