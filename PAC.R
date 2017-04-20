rm(list=ls())

#install.packages("caret")
#install.packages("lubridate")
#install.packages("reshape")
#install.packages("ROCR")
#install.packages("C50")
#install.packages("xgboost")
#install.packages("dplyr")
#install.packages("e1071")
library(dplyr)
library(lubridate)
library(reshape)
library(e1071)
library(caret)
library(ROCR)
library(C50)
library(xgboost)

cls<-read.delim("train_clickstreams.tab",stringsAsFactors = T)
sk<-read.delim("train_searchkeywords.tab",stringsAsFactors = F)



#데이터 전처리 과정


#sample 추출
#set.seed(1)
#cls<-cls%>%sample_n(20000)
#sk<-sk%>%sample_n(20000)


#변수 생성 with Click Stream
##총/평균 소모시간 및 카운트 수, 카운트 당 소모시간
cs.v1<-cls%>%
  group_by(CUS_ID)%>%
  summarize(sum_time=sum(ST_TIME),mean_time=mean(ST_TIME),
            sum_cnt=sum(SITE_CNT),mean_cnt=mean(SITE_CNT),
            mean_time_cnt=sum(ST_TIME)/sum(SITE_CNT))

##총 방문일수##as.Date 삭제해보기
cs.v2.0<-cls%>%
  mutate(TIME_ID2=ymd(as.Date(ymd_h(TIME_ID))))%>%
  group_by(CUS_ID,TIME_ID2)%>%
  summarize(n=n())

cs.v2<-cs.v2.0%>%
  group_by(CUS_ID)%>%
  summarize(net_day=n())

##(time base) 주중/주말형
cs.v3<-cls%>%
  mutate(wk_time=ifelse(wday(ymd_h(TIME_ID))%in%2:6,ST_TIME,0),we_time=ifelse(wday(ymd_h(TIME_ID))%in%c(1,7),ST_TIME,0))%>% 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_time,we_time)%>%
  mutate(wk_pat=ifelse(wk_time>=we_time*1.5,"주중형", 
                      ifelse(we_time>=wk_time*1.5,"주말형","유형없음")))

##(day base) 주중/주말형
cs.v4<-cs.v2.0%>%
  mutate(wk_day=ifelse(wday(TIME_ID2)%in%2:6,1,0),we_day=ifelse(wday(TIME_ID2)%in%c(1,7),1,0))%>% 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_day,we_day)%>%
  mutate(wk_pat2=ifelse(wk_day>=we_day*1.5,"주중형", 
                       ifelse(we_day>=wk_day*1.5,"주말형","유형없음")))

##(time base) 선호 요일 및 요일별 비율 (and 요일별당 최대시간)
cs.v5<-cls%>%
  mutate(mon_time=ifelse(wday(ymd_h(TIME_ID))==2,ST_TIME,0),
         tues_time=ifelse(wday(ymd_h(TIME_ID))==3,ST_TIME,0),
         wednes_time=ifelse(wday(ymd_h(TIME_ID))==4,ST_TIME,0),
         thurs_time=ifelse(wday(ymd_h(TIME_ID))==5,ST_TIME,0),
         fri_time=ifelse(wday(ymd_h(TIME_ID))==6,ST_TIME,0),
         satur_time=ifelse(wday(ymd_h(TIME_ID))==7,ST_TIME,0),
         sun_time=ifelse(wday(ymd_h(TIME_ID))==1,ST_TIME,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time,sun_time)%>%
  mutate(sum_time=sum(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time),
         dmax_time=max(c(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time)))%>%
  mutate(day_pat=ifelse((sun_time>=0.3*sum_time)&(sun_time==dmax_time),"일요일",
                        ifelse((mon_time>=0.3*sum_time)&(mon_time==dmax_time),"월요일",
                               ifelse((tues_time>=0.3*sum_time)&(tues_time==dmax_time),"화요일",
                                      ifelse((wednes_time>=0.3*sum_time)&(wednes_time==dmax_time),"수요일",
                                             ifelse((thurs_time>=0.3*sum_time)&(thurs_time==dmax_time),"목요일",
                                                    ifelse((fri_time>=0.3*sum_time)&(fri_time==dmax_time),"금요일",
                                                           ifelse((satur_time>=0.3*sum_time)&(satur_time==dmax_time),"토요일","유형없음"))))))))%>%
  mutate(mon_ratio=mon_time/sum_time*100,
         tues_ratio=tues_time/sum_time*100,
         wednes_ratio=wednes_time/sum_time*100,
         thurs_ratio=thurs_time/sum_time*100,
         fri_ratio=fri_time/sum_time*100,
         satur_ratio=satur_time/sum_time*100,
         sun_ratio=sun_time/sum_time*100)%>%
  select(-sum_time)

##(day base) 선호 요일 및 요일별 비율 (and 요일별당 최대횟수)
cs.v6<-cs.v2.0%>%
  mutate(mon_day=ifelse(wday(TIME_ID2)==2,1,0),
         tues_day=ifelse(wday(TIME_ID2)==3,1,0),
         wednes_day=ifelse(wday(TIME_ID2)==4,1,0),
         thurs_day=ifelse(wday(TIME_ID2)==5,1,0),
         fri_day=ifelse(wday(TIME_ID2)==6,1,0),
         satur_day=ifelse(wday(TIME_ID2)==7,1,0),
         sun_day=ifelse(wday(TIME_ID2)==1,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),mon_day,tues_day,wednes_day,thurs_day,fri_day,satur_day,sun_day)%>%
  mutate(sum_day=sum(sun_day,mon_day,tues_day,wednes_day,thurs_day,fri_day,satur_day),
         dmax_day=max(c(sun_day,mon_day,tues_day,wednes_day,thurs_day,fri_day,satur_day)))%>%
  mutate(day_pat2=ifelse((sun_day>=0.3*sum_day)&(sun_day==dmax_day),"일요일",
                        ifelse((mon_day>=0.3*sum_day)&(mon_day==dmax_day),"월요일",
                               ifelse((tues_day>=0.3*sum_day)&(tues_day==dmax_day),"화요일",
                                      ifelse((wednes_day>=0.3*sum_day)&(wednes_day==dmax_day),"수요일",
                                             ifelse((thurs_day>=0.3*sum_day)&(thurs_day==dmax_day),"목요일",
                                                    ifelse((fri_day>=0.3*sum_day)&(fri_day==dmax_day),"금요일",
                                                           ifelse((satur_day>=0.3*sum_day)&(satur_day==dmax_day),"토요일","유형없음"))))))))%>%
  mutate(mon_ratio2=mon_day/sum_day*100,
         tues_ratio2=tues_day/sum_day*100,
         wednes_ratio2=wednes_day/sum_day*100,
         thurs_ratio2=thurs_day/sum_day*100,
         fri_ratio2=fri_day/sum_day*100,
         satur_ratio2=satur_day/sum_day*100,
         sun_ratio2=sun_day/sum_day*100)%>%
  select(-sum_day)

##(time base) 선호 월 및 월별 비율 (and 월별당 최대시간)
cs.v7<-cls%>%
  mutate(jan_time=ifelse(month(ymd_h(TIME_ID))==1,ST_TIME,0),
         fab_time=ifelse(month(ymd_h(TIME_ID))==2,ST_TIME,0),
         mar_time=ifelse(month(ymd_h(TIME_ID))==3,ST_TIME,0),
         apr_time=ifelse(month(ymd_h(TIME_ID))==4,ST_TIME,0),
         may_time=ifelse(month(ymd_h(TIME_ID))==5,ST_TIME,0),
         jun_time=ifelse(month(ymd_h(TIME_ID))==6,ST_TIME,0),
         jul_time=ifelse(month(ymd_h(TIME_ID))==7,ST_TIME,0),
         aug_time=ifelse(month(ymd_h(TIME_ID))==8,ST_TIME,0),
         sep_time=ifelse(month(ymd_h(TIME_ID))==9,ST_TIME,0),
         oct_time=ifelse(month(ymd_h(TIME_ID))==10,ST_TIME,0),
         nov_time=ifelse(month(ymd_h(TIME_ID))==11,ST_TIME,0),
         dec_time=ifelse(month(ymd_h(TIME_ID))==12,ST_TIME,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time)%>%
  mutate(sum_time=sum(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time),
         mmax_time=max(c(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time)))%>%
  mutate(month_pat=ifelse((jan_time>=0.3*sum_time)&(jan_time==mmax_time),"1월",
                          ifelse((fab_time>=0.3*sum_time)&(fab_time==mmax_time),"2월",
                                 ifelse((mar_time>=0.3*sum_time)&(mar_time==mmax_time),"3월",
                                        ifelse((apr_time>=0.3*sum_time)&(apr_time==mmax_time),"4월",
                                               ifelse((may_time>=0.3*sum_time)&(may_time==mmax_time),"5월",
                                                      ifelse((jun_time>=0.3*sum_time)&(jun_time==mmax_time),"6월",
                                                             ifelse((jul_time>=0.3*sum_time)&(jul_time==mmax_time),"7월",
                                                                    ifelse((aug_time>=0.3*sum_time)&(aug_time==mmax_time),"8월",
                                                                           ifelse((sep_time>=0.3*sum_time)&(sep_time==mmax_time),"9월",
                                                                                  ifelse((oct_time>=0.3*sum_time)&(oct_time==mmax_time),"10월",
                                                                                         ifelse((nov_time>=0.3*sum_time)&(nov_time==mmax_time),"11월",
                                                                                                ifelse((dec_time>=0.3*sum_time)&(dec_time==mmax_time),"12월","유형없음")))))))))))))%>%
  mutate(jan_ratio=jan_time/sum_time*100,
         fab_ratio=fab_time/sum_time*100,
         mar_ratio=mar_time/sum_time*100,
         apr_ratio=apr_time/sum_time*100,
         may_ratio=may_time/sum_time*100,
         jun_ratio=jun_time/sum_time*100,
         jul_ratio=jul_time/sum_time*100,
         aug_ratio=aug_time/sum_time*100,
         sep_ratio=sep_time/sum_time*100,
         oct_ratio=oct_time/sum_time*100,
         nov_ratio=nov_time/sum_time*100,
         dec_ratio=dec_time/sum_time*100)%>%
  select(-sum_time)

##(day base) 선호 월 및 월별 비율 (and 월별당 최대시간)
cs.v8<-cs.v2.0%>%
  mutate(jan_day=ifelse(month(TIME_ID2)==1,1,0),
         fab_day=ifelse(month(TIME_ID2)==2,1,0),
         mar_day=ifelse(month(TIME_ID2)==3,1,0),
         apr_day=ifelse(month(TIME_ID2)==4,1,0),
         may_day=ifelse(month(TIME_ID2)==5,1,0),
         jun_day=ifelse(month(TIME_ID2)==6,1,0),
         jul_day=ifelse(month(TIME_ID2)==7,1,0),
         aug_day=ifelse(month(TIME_ID2)==8,1,0),
         sep_day=ifelse(month(TIME_ID2)==9,1,0),
         oct_day=ifelse(month(TIME_ID2)==10,1,0),
         nov_day=ifelse(month(TIME_ID2)==11,1,0),
         dec_day=ifelse(month(TIME_ID2)==12,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),jan_day,fab_day,mar_day,apr_day,may_day,jun_day,jul_day,aug_day,sep_day,oct_day,nov_day,dec_day)%>%
  mutate(sum_day=sum(jan_day,fab_day,mar_day,apr_day,may_day,jun_day,jul_day,aug_day,sep_day,oct_day,nov_day,dec_day),
         mmax_day=max(c(jan_day,fab_day,mar_day,apr_day,may_day,jun_day,jul_day,aug_day,sep_day,oct_day,nov_day,dec_day)))%>%
  mutate(month_pat2=ifelse((jan_day>=0.3*sum_day)&(jan_day==mmax_day),"1월",
                          ifelse((fab_day>=0.3*sum_day)&(fab_day==mmax_day),"2월",
                                 ifelse((mar_day>=0.3*sum_day)&(mar_day==mmax_day),"3월",
                                        ifelse((apr_day>=0.3*sum_day)&(apr_day==mmax_day),"4월",
                                               ifelse((may_day>=0.3*sum_day)&(may_day==mmax_day),"5월",
                                                      ifelse((jun_day>=0.3*sum_day)&(jun_day==mmax_day),"6월",
                                                             ifelse((jul_day>=0.3*sum_day)&(jul_day==mmax_day),"7월",
                                                                    ifelse((aug_day>=0.3*sum_day)&(aug_day==mmax_day),"8월",
                                                                           ifelse((sep_day>=0.3*sum_day)&(sep_day==mmax_day),"9월",
                                                                                  ifelse((oct_day>=0.3*sum_day)&(oct_day==mmax_day),"10월",
                                                                                         ifelse((nov_day>=0.3*sum_day)&(nov_day==mmax_day),"11월",
                                                                                                ifelse((dec_day>=0.3*sum_day)&(dec_day==mmax_day),"12월","유형없음")))))))))))))%>%
  mutate(jan_ratio2=jan_day/sum_day*100,
         fab_ratio2=fab_day/sum_day*100,
         mar_ratio2=mar_day/sum_day*100,
         apr_ratio2=apr_day/sum_day*100,
         may_ratio2=may_day/sum_day*100,
         jun_ratio2=jun_day/sum_day*100,
         jul_ratio2=jul_day/sum_day*100,
         aug_ratio2=aug_day/sum_day*100,
         sep_ratio2=sep_day/sum_day*100,
         oct_ratio2=oct_day/sum_day*100,
         nov_ratio2=nov_day/sum_day*100,
         dec_ratio2=dec_day/sum_day*100)%>%
  select(-sum_day)

##선호 시간대(00~07새벽형,08~11아침형,12~13점심형,14~16오후형,17~19저녁형,20~23밤형): 24시간 중 가장 오래 머무른 시간
cs.v9<-cls%>%
  mutate(cons_time0=ifelse(hour(ymd_h(TIME_ID))%in%0:7,ST_TIME,0),
         cons_time8=ifelse(hour(ymd_h(TIME_ID))%in%8:11,ST_TIME,0),
         cons_time12=ifelse(hour(ymd_h(TIME_ID))%in%12:13,ST_TIME,0),
         cons_time14=ifelse(hour(ymd_h(TIME_ID))%in%14:16,ST_TIME,0),
         cons_time17=ifelse(hour(ymd_h(TIME_ID))%in%17:19,ST_TIME,0),
         cons_time20=ifelse(hour(ymd_h(TIME_ID))%in%20:23,ST_TIME,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_time0,cons_time8,cons_time12,cons_time14,cons_time17,cons_time20)%>%
  mutate(sum_cons_time=sum(cons_time0,cons_time8,cons_time12,cons_time14,cons_time17,cons_time20),
         max_cons_time=max(c(cons_time0,cons_time8,cons_time12,cons_time14,cons_time17,cons_time20)))%>%
  mutate(time_pat=ifelse((cons_time0>=0.3*sum_cons_time)&(cons_time0==max_cons_time),"새벽형",
                         ifelse((cons_time8>=0.3*sum_cons_time)&(cons_time8==max_cons_time),"아침형",
                                ifelse((cons_time12>=0.3*sum_cons_time)&(cons_time12==max_cons_time),"점심형",
                                       ifelse((cons_time14>=0.3*sum_cons_time)&(cons_time14==max_cons_time),"오후형",
                                              ifelse((cons_time17>=0.3*sum_cons_time)&(cons_time17==max_cons_time),"저녁형",
                                                     ifelse((cons_time20>=0.3*sum_cons_time)&(cons_time20==max_cons_time),"밤형","유형없음")))))))%>%
  mutate(cons_time0_ratio=cons_time0/sum_cons_time*100,
         cons_time8_ratio=cons_time8/sum_cons_time*100,
         cons_time12_ratio=cons_time12/sum_cons_time*100,
         cons_time14_ratio=cons_time14/sum_cons_time*100,
         cons_time17_ratio=cons_time17/sum_cons_time*100,
         cons_time20_ratio=cons_time20/sum_cons_time*100)%>%
  select(-sum_cons_time)

##선호 시간대(00~07새벽형,08~11아침형,12~13점심형,14~16오후형,17~19저녁형,20~23밤형): 24시간 중 가장 오래 횟수가 많은 시간
cs.v10<-cls%>%
  mutate(cons_num0=ifelse(hour(ymd_h(TIME_ID))%in%0:7,1,0),
         cons_num8=ifelse(hour(ymd_h(TIME_ID))%in%8:11,1,0),
         cons_num12=ifelse(hour(ymd_h(TIME_ID))%in%12:13,1,0),
         cons_num14=ifelse(hour(ymd_h(TIME_ID))%in%14:16,1,0),
         cons_num17=ifelse(hour(ymd_h(TIME_ID))%in%17:19,1,0),
         cons_num20=ifelse(hour(ymd_h(TIME_ID))%in%20:23,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_num0,cons_num8,cons_num12,cons_num14,cons_num17,cons_num20)%>%
  mutate(sum_cons_num=sum(cons_num0,cons_num8,cons_num12,cons_num14,cons_num17,cons_num20),
         max_cons_num=max(c(cons_num0,cons_num8,cons_num12,cons_num14,cons_num17,cons_num20)))%>%
  mutate(time_pat2=ifelse((cons_num0>=0.3*sum_cons_num)&(cons_num0==max_cons_num),"새벽형",
                         ifelse((cons_num8>=0.3*sum_cons_num)&(cons_num8==max_cons_num),"아침형",
                                ifelse((cons_num12>=0.3*sum_cons_num)&(cons_num12==max_cons_num),"점심형",
                                       ifelse((cons_num14>=0.3*sum_cons_num)&(cons_num14==max_cons_num),"오후형",
                                              ifelse((cons_num17>=0.3*sum_cons_num)&(cons_num17==max_cons_num),"저녁형",
                                                     ifelse((cons_num20>=0.3*sum_cons_num)&(cons_num20==max_cons_num),"밤형","유형없음")))))))%>%
  mutate(cons_num0_ratio=cons_num0/sum_cons_num*100,
         cons_num8_ratio=cons_num8/sum_cons_num*100,
         cons_num12_ratio=cons_num12/sum_cons_num*100,
         cons_num14_ratio=cons_num14/sum_cons_num*100,
         cons_num17_ratio=cons_num17/sum_cons_num*100,
         cons_num20_ratio=cons_num20/sum_cons_num*100)%>%
  select(-sum_cons_num)

##접속 간격
cs.v11<-cs.v2.0%>%
  group_by(CUS_ID)%>%
  summarize(first_date=min(TIME_ID2),last_date=max(TIME_ID2),interval=ifelse(n()==1,NA,as.numeric((max(TIME_ID2)-min(TIME_ID2))/(n()-1))))

##클래스 다양성
cs.v12<-cls%>%
  distinct(CUS_ID,BACT_NM)%>%
  group_by(CUS_ID)%>%
  summarize(diversity=n())

##클래스 별 횟수 및 비율
cs.v13.0<-cls%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(class_num=sum(SITE_CNT))

cs.v13<-cs.v13.0%>%
  cast(CUS_ID~BACT_NM,value="class_num")
colnames(cs.v13)<-c("CUS_ID",paste("횟수",colnames(cs.v13[-1]),sep="_"))

cs.v14<-cs.v13.0%>%
  group_by(CUS_ID)%>%
  mutate(class_ratio=class_num/sum(class_num)*100)%>%
  select(CUS_ID,BACT_NM,class_ratio)%>%
  cast(CUS_ID~BACT_NM,value="class_ratio")
colnames(cs.v14)<-c("CUS_ID",paste("횟수비율",colnames(cs.v14[-1]),sep="_"))

##클래스 별 소모 시간 및 비율
cs.v15.0<-cls%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(class_time=sum(ST_TIME))

cs.v15<-cs.v15.0%>%
  cast(CUS_ID~BACT_NM,value="class_time")
colnames(cs.v15)<-c("CUS_ID",paste("소모시간",colnames(cs.v15[-1]),sep="_"))

cs.v16<-cs.v15.0%>%
  group_by(CUS_ID)%>%
  mutate(class_ratio2=class_time/sum(class_time)*100)%>%
  select(CUS_ID,BACT_NM,class_ratio2)%>%
  cast(CUS_ID~BACT_NM,value="class_ratio2")
colnames(cs.v16)<-c("CUS_ID",paste("시간비율",colnames(cs.v16[-1]),sep="_"))

##클래스 별 평균 접속 시간 대(00~07새벽형,08~11아침형,12~13점심형,14~16오후형,17~19저녁형,20~23밤형) (ex.점심시간에 게임 접속)
cs.v17<-cls%>%
  mutate(hour=hour(ymd_h(TIME_ID)))%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(mean_conn_hour=ifelse(n()>=5,ifelse(as.integer(mean(hour))%in%0:7,"새벽형",
                                                ifelse(as.integer(mean(hour))%in%8:11,"아침형",
                                                       ifelse(as.integer(mean(hour))%in%12:13,"점심형",
                                                              ifelse(as.integer(mean(hour))%in%14:16,"오후형",
                                                                     ifelse(as.integer(mean(hour))%in%17:19,"저녁형",
                                                                            ifelse(as.integer(mean(hour))%in%20:23,"밤형","Error")))))),"유형없음"))%>%
  select(CUS_ID,mean_conn_hour)




#변수 생성 with Search keyword
##검색량

##관심사별 비율

##관심사별 횟수

##관심사별 다양성
