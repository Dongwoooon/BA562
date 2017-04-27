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
cs<-read.csv("train_profiles.csv",stringsAsFactors = T)


#데이터 전처리 과정


#sample 추출
#set.seed(1)
#cls<-cls%>%sample_n(20000)
#sk<-sk%>%sample_n(20000)


#변수 생성 with Click Stream
##총/평균 소모시간 및 카운트 수, 카운트 당 소모시간
cs.v1<-cls%>%
  group_by(CUS_ID)%>%
  summarize(sum_time=sum(ST_TIME),mean_time=mean(ST_TIME), #총/평균 소모시간
            sum_cnt=sum(SITE_CNT),mean_cnt=mean(SITE_CNT), #총/평균 카운트 수
            mean_time_cnt=sum(ST_TIME)/sum(SITE_CNT)) #카운트 당 소모시간



##총 방문일수##as.Date 삭제해보기
cs.v2.0<-cls%>%
  mutate(TIME_ID2=ymd(as.Date(ymd_h(TIME_ID))))%>% #시간 데이터 날리기 
  group_by(CUS_ID,TIME_ID2)%>% #날짜별 구분 
  summarize(n=n())

cs.v2<-cs.v2.0%>%
  group_by(CUS_ID)%>%
  summarize(net_day=n()) #총 방문일수 



##(time base: 소모시간 기준으로 구분) 주중/주말형
cs.v3<-cls%>%
  mutate(wk_time=ifelse(wday(ymd_h(TIME_ID))%in%2:6,ST_TIME,0),we_time=ifelse(wday(ymd_h(TIME_ID))%in%c(1,7),ST_TIME,0))%>% #주중/주말 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_time,we_time)%>% #주중/ 주말 소모시간 합산 
  mutate(sum_time=wk_time+we_time)%>%
  mutate(wk_pat=ifelse(wk_time>=we_time*1.5,"주중형", 
                      ifelse(we_time>=wk_time*1.5,"주말형","유형없음")))%>% #60%이상 사용시 패턴 부여
  mutate(wk_ratio=wk_time/sum_time*100,
         we_ratio=we_time/sum_time*100)%>%
  select(-sum_time)



##(day base: 소모시간을 무시하고 방문한 날에 1값 부여) 주중/주말형
cs.v4<-cs.v2.0%>%
  mutate(wk_day=ifelse(wday(TIME_ID2)%in%2:6,1,0),we_day=ifelse(wday(TIME_ID2)%in%c(1,7),1,0))%>% 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_day,we_day)%>%
  mutate(sum_day=wk_day+we_day)%>%
  mutate(wk_pat2=ifelse(wk_day>=we_day*1.5,"주중형", 
                       ifelse(we_day>=wk_day*1.5,"주말형","유형없음")))%>%
  mutate(wk_ratio2=wk_day/sum_day*100,
         we_ratio2=we_day/sum_day*100)%>%
  select(-sum_day)

##number of page requests and percentage of total number of page requests during 주중/주말
cs.d2<-cls%>%
  mutate(wk_cnt=ifelse(wday(ymd_h(TIME_ID))%in%2:6,SITE_CNT,0),we_cnt=ifelse(wday(ymd_h(TIME_ID))%in%c(1,7),SITE_CNT,0))%>% #주중/주말 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_cnt,we_cnt)%>% #주중/ 주말 카운트 합산
  mutate(sum_cnt=wk_cnt+we_cnt)%>% #전체 카운트
  mutate(wk_pat3=ifelse(wk_cnt>=we_cnt*1.5,"주중형", 
                        ifelse(we_cnt>=wk_cnt*1.5,"주말형","유형없음")))%>% #60%이상 사용시 패턴 부여
  mutate(wk_ratio3=wk_cnt/sum_cnt*100,
         we_ratio3=we_cnt/sum_cnt*100)%>%
  select(-sum_cnt)


##(time base) 선호 요일 및 요일별 비율 (and 요일별당 최대시간)
cs.v5<-cls%>%
  mutate(mon_time=ifelse(wday(ymd_h(TIME_ID))==2,ST_TIME,0),
         tues_time=ifelse(wday(ymd_h(TIME_ID))==3,ST_TIME,0),
         wednes_time=ifelse(wday(ymd_h(TIME_ID))==4,ST_TIME,0),
         thurs_time=ifelse(wday(ymd_h(TIME_ID))==5,ST_TIME,0),
         fri_time=ifelse(wday(ymd_h(TIME_ID))==6,ST_TIME,0),
         satur_time=ifelse(wday(ymd_h(TIME_ID))==7,ST_TIME,0),
         sun_time=ifelse(wday(ymd_h(TIME_ID))==1,ST_TIME,0))%>% #일요일=1,...,토요일=7 요일 구분
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time,sun_time)%>% #요일 별 합산 소모시간
  group_by(CUS_ID)%>%
  mutate(sum_time=sum(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time), #전체 소모시간 
         dmax_time=max(c(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time)))%>% #전체 요일 중 최대 소모시간 
  #전체 시간 중 30% 이상 사용 및 최대 소모시간일 경우 패턴 부여 
  mutate(day_pat=ifelse((sun_time>=0.3*sum_time)&(sun_time==dmax_time),"일요일",
                        ifelse((mon_time>=0.3*sum_time)&(mon_time==dmax_time),"월요일",
                               ifelse((tues_time>=0.3*sum_time)&(tues_time==dmax_time),"화요일",
                                      ifelse((wednes_time>=0.3*sum_time)&(wednes_time==dmax_time),"수요일",
                                             ifelse((thurs_time>=0.3*sum_time)&(thurs_time==dmax_time),"목요일",
                                                    ifelse((fri_time>=0.3*sum_time)&(fri_time==dmax_time),"금요일",
                                                           ifelse((satur_time>=0.3*sum_time)&(satur_time==dmax_time),"토요일","유형없음"))))))))%>%
  #요일 별 소모시간 비율 계산 
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
  group_by(CUS_ID)%>%
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

##number of page requests and percentage of total number of page requests during 요일
cs.d1<-cls%>%
  mutate(mon_cnt=ifelse(wday(ymd_h(TIME_ID))==2,SITE_CNT,0),
         tues_cnt=ifelse(wday(ymd_h(TIME_ID))==3,SITE_CNT,0),
         wednes_cnt=ifelse(wday(ymd_h(TIME_ID))==4,SITE_CNT,0),
         thurs_cnt=ifelse(wday(ymd_h(TIME_ID))==5,SITE_CNT,0),
         fri_cnt=ifelse(wday(ymd_h(TIME_ID))==6,SITE_CNT,0),
         satur_cnt=ifelse(wday(ymd_h(TIME_ID))==7,SITE_CNT,0),
         sun_cnt=ifelse(wday(ymd_h(TIME_ID))==1,SITE_CNT,0))%>% #일요일=1,...,토요일=7 요일 구분
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),mon_cnt,tues_cnt,wednes_cnt,thurs_cnt,fri_cnt,satur_cnt,sun_cnt)%>% #요일 별 합산 카운트
  group_by(CUS_ID)%>%
  mutate(sum_cnt=sum(sun_cnt,mon_cnt,tues_cnt,wednes_cnt,thurs_cnt,fri_cnt,satur_cnt), #전체 카운트 
         dmax_cnt=max(c(sun_cnt,mon_cnt,tues_cnt,wednes_cnt,thurs_cnt,fri_cnt,satur_cnt)))%>% #전체 요일 중 최대 카운트  
  #전체 시간 중 30% 이상 사용 및 최대 카운트일 경우 패턴 부여 
  mutate(day_pat3=ifelse((sun_cnt>=0.3*sum_cnt)&(sun_cnt==dmax_cnt),"일요일",
                         ifelse((mon_cnt>=0.3*sum_cnt)&(mon_cnt==dmax_cnt),"월요일",
                                ifelse((tues_cnt>=0.3*sum_cnt)&(tues_cnt==dmax_cnt),"화요일",
                                       ifelse((wednes_cnt>=0.3*sum_cnt)&(wednes_cnt==dmax_cnt),"수요일",
                                              ifelse((thurs_cnt>=0.3*sum_cnt)&(thurs_cnt==dmax_cnt),"목요일",
                                                     ifelse((fri_cnt>=0.3*sum_cnt)&(fri_cnt==dmax_cnt),"금요일",
                                                            ifelse((satur_cnt>=0.3*sum_cnt)&(satur_cnt==dmax_cnt),"토요일","유형없음"))))))))%>%
  #요일 별 카운트 비율 계산 
  mutate(mon_ratio3=mon_cnt/sum_cnt*100,
         tues_ratio3=tues_cnt/sum_cnt*100,
         wednes_ratio3=wednes_cnt/sum_cnt*100,
         thurs_ratio3=thurs_cnt/sum_cnt*100,
         fri_ratio3=fri_cnt/sum_cnt*100,
         satur_ratio3=satur_cnt/sum_cnt*100,
         sun_ratio3=sun_cnt/sum_cnt*100)%>%
  select(-sum_cnt)



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
         dec_time=ifelse(month(ymd_h(TIME_ID))==12,ST_TIME,0))%>% #월 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time)%>%
  group_by(CUS_ID)%>%
  mutate(sum_time=sum(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time), #전체 소모시간 
         mmax_time=max(c(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time)))%>% #월중 최대 소모시간 
  #전체 시간 중 30% 이상 사용 및 최대 소모시간일 경우 패턴 부여   
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
  #월별 소모시간 비율 계산 
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
  group_by(CUS_ID)%>%
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

##number of page requests and percentage of total number of page requests during 월
cs.d0<-cls%>%
  mutate(jan_cnt=ifelse(month(ymd_h(TIME_ID))==1,SITE_CNT,0),
         fab_cnt=ifelse(month(ymd_h(TIME_ID))==2,SITE_CNT,0),
         mar_cnt=ifelse(month(ymd_h(TIME_ID))==3,SITE_CNT,0),
         apr_cnt=ifelse(month(ymd_h(TIME_ID))==4,SITE_CNT,0),
         may_cnt=ifelse(month(ymd_h(TIME_ID))==5,SITE_CNT,0),
         jun_cnt=ifelse(month(ymd_h(TIME_ID))==6,SITE_CNT,0),
         jul_cnt=ifelse(month(ymd_h(TIME_ID))==7,SITE_CNT,0),
         aug_cnt=ifelse(month(ymd_h(TIME_ID))==8,SITE_CNT,0),
         sep_cnt=ifelse(month(ymd_h(TIME_ID))==9,SITE_CNT,0),
         oct_cnt=ifelse(month(ymd_h(TIME_ID))==10,SITE_CNT,0),
         nov_cnt=ifelse(month(ymd_h(TIME_ID))==11,SITE_CNT,0),
         dec_cnt=ifelse(month(ymd_h(TIME_ID))==12,SITE_CNT,0))%>% #월 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),jan_cnt,fab_cnt,mar_cnt,apr_cnt,may_cnt,jun_cnt,jul_cnt,aug_cnt,sep_cnt,oct_cnt,nov_cnt,dec_cnt)%>%
  group_by(CUS_ID)%>%
  mutate(sum_cnt=sum(jan_cnt,fab_cnt,mar_cnt,apr_cnt,may_cnt,jun_cnt,jul_cnt,aug_cnt,sep_cnt,oct_cnt,nov_cnt,dec_cnt), #전체 카운트  
         mmax_cnt=max(c(jan_cnt,fab_cnt,mar_cnt,apr_cnt,may_cnt,jun_cnt,jul_cnt,aug_cnt,sep_cnt,oct_cnt,nov_cnt,dec_cnt)))%>% #월중 최대 카운트  
  #전체 시간 중 30% 이상 사용 및 최대 카운트일 경우 패턴 부여   
  mutate(month_pat3=ifelse((jan_cnt>=0.3*sum_cnt)&(jan_cnt==mmax_cnt),"1월",
                           ifelse((fab_cnt>=0.3*sum_cnt)&(fab_cnt==mmax_cnt),"2월",
                                  ifelse((mar_cnt>=0.3*sum_cnt)&(mar_cnt==mmax_cnt),"3월",
                                         ifelse((apr_cnt>=0.3*sum_cnt)&(apr_cnt==mmax_cnt),"4월",
                                                ifelse((may_cnt>=0.3*sum_cnt)&(may_cnt==mmax_cnt),"5월",
                                                       ifelse((jun_cnt>=0.3*sum_cnt)&(jun_cnt==mmax_cnt),"6월",
                                                              ifelse((jul_cnt>=0.3*sum_cnt)&(jul_cnt==mmax_cnt),"7월",
                                                                     ifelse((aug_cnt>=0.3*sum_cnt)&(aug_cnt==mmax_cnt),"8월",
                                                                            ifelse((sep_cnt>=0.3*sum_cnt)&(sep_cnt==mmax_cnt),"9월",
                                                                                   ifelse((oct_cnt>=0.3*sum_cnt)&(oct_cnt==mmax_cnt),"10월",
                                                                                          ifelse((nov_cnt>=0.3*sum_cnt)&(nov_cnt==mmax_cnt),"11월",
                                                                                                 ifelse((dec_cnt>=0.3*sum_cnt)&(dec_cnt==mmax_cnt),"12월","유형없음")))))))))))))%>%
  #월별 카운트 비율 계산 
  mutate(jan_ratio3=jan_cnt/sum_cnt*100,
         fab_ratio3=fab_cnt/sum_cnt*100,
         mar_ratio3=mar_cnt/sum_cnt*100,
         apr_ratio3=apr_cnt/sum_cnt*100,
         may_ratio3=may_cnt/sum_cnt*100,
         jun_ratio3=jun_cnt/sum_cnt*100,
         jul_ratio3=jul_cnt/sum_cnt*100,
         aug_ratio3=aug_cnt/sum_cnt*100,
         sep_ratio3=sep_cnt/sum_cnt*100,
         oct_ratio3=oct_cnt/sum_cnt*100,
         nov_ratio3=nov_cnt/sum_cnt*100,
         dec_ratio3=dec_cnt/sum_cnt*100)%>%
  select(-sum_cnt)



##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 머무른 시간
cs.v9<-cls%>%
  mutate(cons_time0=ifelse(hour(ymd_h(TIME_ID))%in%0:4,ST_TIME,0),
         cons_time5=ifelse(hour(ymd_h(TIME_ID))%in%5:7,ST_TIME,0),
         cons_time8=ifelse(hour(ymd_h(TIME_ID))==8,ST_TIME,0),
         cons_time9=ifelse(hour(ymd_h(TIME_ID))%in%9:11,ST_TIME,0),
         cons_time12=ifelse(hour(ymd_h(TIME_ID))==12,ST_TIME,0),
         cons_time13=ifelse(hour(ymd_h(TIME_ID))%in%13:17,ST_TIME,0),
         cons_time18=ifelse(hour(ymd_h(TIME_ID))%in%18:21,ST_TIME,0),
         cons_time22=ifelse(hour(ymd_h(TIME_ID))%in%22:23,ST_TIME,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_time0,cons_time5,cons_time8,cons_time9,cons_time12,cons_time13,cons_time18,cons_time22)%>%
  group_by(CUS_ID)%>%
  mutate(sum_cons_time=sum(cons_time0,cons_time5,cons_time8,cons_time9,cons_time12,cons_time13,cons_time18,cons_time22), #전체 소모시간 
         max_cons_time=max(c(cons_time0,cons_time5,cons_time8,cons_time9,cons_time12,cons_time13,cons_time18,cons_time22)))%>% #시간대 중 최대 소모시간 
  #전체 시간 중 30% 이상 사용 및 최대 소모시간일 경우 패턴 부여 
  mutate(time_pat=ifelse((cons_time0>=0.3*sum_cons_time)&(cons_time0==max_cons_time),"새벽",
                         ifelse((cons_time5>=0.3*sum_cons_time)&(cons_time5==max_cons_time),"아침",
                                ifelse((cons_time8>=0.3*sum_cons_time)&(cons_time8==max_cons_time),"출근",
                                       ifelse((cons_time9>=0.3*sum_cons_time)&(cons_time9==max_cons_time),"오전",
                                              ifelse((cons_time12>=0.3*sum_cons_time)&(cons_time12==max_cons_time),"점심",
                                                     ifelse((cons_time13>=0.3*sum_cons_time)&(cons_time13==max_cons_time),"오후",
                                                            ifelse((cons_time18>=0.3*sum_cons_time)&(cons_time18==max_cons_time),"저녁",
                                                                   ifelse((cons_time22>=0.3*sum_cons_time)&(cons_time22==max_cons_time),"밤","유형없음")))))))))%>%
  #시간대 별 소모시간 비율 
  mutate(cons_time0_ratio=cons_time0/sum_cons_time*100,
         cons_time5_ratio=cons_time5/sum_cons_time*100,
         cons_time8_ratio=cons_time8/sum_cons_time*100,
         cons_time9_ratio=cons_time9/sum_cons_time*100,
         cons_time12_ratio=cons_time12/sum_cons_time*100,
         cons_time13_ratio=cons_time13/sum_cons_time*100,
         cons_time18_ratio=cons_time18/sum_cons_time*100,
         cons_time22_ratio=cons_time22/sum_cons_time*100)%>%
  select(-sum_cons_time)



##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 횟수가 많은 시간
cs.v10<-cls%>%
  mutate(cons_num0=ifelse(hour(ymd_h(TIME_ID))%in%0:4,1,0),
         cons_num5=ifelse(hour(ymd_h(TIME_ID))%in%5:7,1,0),
         cons_num8=ifelse(hour(ymd_h(TIME_ID))==8,1,0),
         cons_num9=ifelse(hour(ymd_h(TIME_ID))%in%9:11,1,0),
         cons_num12=ifelse(hour(ymd_h(TIME_ID))==12,1,0),
         cons_num13=ifelse(hour(ymd_h(TIME_ID))%in%13:17,1,0),
         cons_num18=ifelse(hour(ymd_h(TIME_ID))%in%18:21,1,0),
         cons_num22=ifelse(hour(ymd_h(TIME_ID))%in%22:23,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_num0,cons_num5,cons_num8,cons_num9,cons_num12,cons_num13,cons_num18,cons_num22)%>%
  group_by(CUS_ID)%>%
  mutate(sum_cons_num=sum(cons_num0,cons_num5,cons_num8,cons_num9,cons_num12,cons_num13,cons_num18,cons_num22),
         max_cons_num=max(c(cons_num0,cons_num5,cons_num8,cons_num9,cons_num12,cons_num13,cons_num18,cons_num22)))%>%
  mutate(time_pat2=ifelse((cons_num0>=0.3*sum_cons_num)&(cons_num0==max_cons_num),"새벽",
                         ifelse((cons_num5>=0.3*sum_cons_num)&(cons_num5==max_cons_num),"아침",
                                ifelse((cons_num8>=0.3*sum_cons_num)&(cons_num8==max_cons_num),"출근",
                                       ifelse((cons_num9>=0.3*sum_cons_num)&(cons_num9==max_cons_num),"오전",
                                              ifelse((cons_num12>=0.3*sum_cons_num)&(cons_num12==max_cons_num),"점심",
                                                     ifelse((cons_num13>=0.3*sum_cons_num)&(cons_num13==max_cons_num),"오후",
                                                            ifelse((cons_num18>=0.3*sum_cons_num)&(cons_num18==max_cons_num),"저녁",
                                                                   ifelse((cons_num22>=0.3*sum_cons_num)&(cons_num22==max_cons_num),"밤","유형없음")))))))))%>%
  mutate(cons_num0_ratio=cons_num0/sum_cons_num*100,
         cons_num5_ratio=cons_num5/sum_cons_num*100,
         cons_num8_ratio=cons_num8/sum_cons_num*100,
         cons_num9_ratio=cons_num9/sum_cons_num*100,
         cons_num12_ratio=cons_num12/sum_cons_num*100,
         cons_num13_ratio=cons_num13/sum_cons_num*100,
         cons_num18_ratio=cons_num18/sum_cons_num*100,
         cons_num22_ratio=cons_num22/sum_cons_num*100)%>%
  select(-sum_cons_num)

##number of page requests and percentage of total number of page requests during 시간
cs.d9<-cls%>%
  mutate(cons_cnt0=ifelse(hour(ymd_h(TIME_ID))%in%0:4,SITE_CNT,0),
         cons_cnt5=ifelse(hour(ymd_h(TIME_ID))%in%5:7,SITE_CNT,0),
         cons_cnt8=ifelse(hour(ymd_h(TIME_ID))==8,SITE_CNT,0),
         cons_cnt9=ifelse(hour(ymd_h(TIME_ID))%in%9:11,SITE_CNT,0),
         cons_cnt12=ifelse(hour(ymd_h(TIME_ID))==12,SITE_CNT,0),
         cons_cnt13=ifelse(hour(ymd_h(TIME_ID))%in%13:17,SITE_CNT,0),
         cons_cnt18=ifelse(hour(ymd_h(TIME_ID))%in%18:21,SITE_CNT,0),
         cons_cnt22=ifelse(hour(ymd_h(TIME_ID))%in%22:23,SITE_CNT,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_cnt0,cons_cnt5,cons_cnt8,cons_cnt9,cons_cnt12,cons_cnt13,cons_cnt18,cons_cnt22)%>%
  group_by(CUS_ID)%>%
  mutate(sum_cons_cnt=sum(cons_cnt0,cons_cnt5,cons_cnt8,cons_cnt9,cons_cnt12,cons_cnt13,cons_cnt18,cons_cnt22), #전체 소모시간 
         max_cons_cnt=max(c(cons_cnt0,cons_cnt5,cons_cnt8,cons_cnt9,cons_cnt12,cons_cnt13,cons_cnt18,cons_cnt22)))%>% #시간대 중 최대 소모시간 
  #전체 시간 중 30% 이상 사용 및 최대 소모시간일 경우 패턴 부여 
  mutate(time_pat3=ifelse((cons_cnt0>=0.3*sum_cons_cnt)&(cons_cnt0==max_cons_cnt),"새벽",
                          ifelse((cons_cnt5>=0.3*sum_cons_cnt)&(cons_cnt5==max_cons_cnt),"아침",
                                 ifelse((cons_cnt8>=0.3*sum_cons_cnt)&(cons_cnt8==max_cons_cnt),"출근",
                                        ifelse((cons_cnt9>=0.3*sum_cons_cnt)&(cons_cnt9==max_cons_cnt),"오전",
                                               ifelse((cons_cnt12>=0.3*sum_cons_cnt)&(cons_cnt12==max_cons_cnt),"점심",
                                                      ifelse((cons_cnt13>=0.3*sum_cons_cnt)&(cons_cnt13==max_cons_cnt),"오후",
                                                             ifelse((cons_cnt18>=0.3*sum_cons_cnt)&(cons_cnt18==max_cons_cnt),"저녁",
                                                                    ifelse((cons_cnt22>=0.3*sum_cons_cnt)&(cons_cnt22==max_cons_cnt),"밤","유형없음")))))))))%>%
  #시간대 별 소모시간 비율 
  mutate(cons_cnt0_ratio=cons_cnt0/sum_cons_cnt*100,
         cons_cnt5_ratio=cons_cnt5/sum_cons_cnt*100,
         cons_cnt8_ratio=cons_cnt8/sum_cons_cnt*100,
         cons_cnt9_ratio=cons_cnt9/sum_cons_cnt*100,
         cons_cnt12_ratio=cons_cnt12/sum_cons_cnt*100,
         cons_cnt13_ratio=cons_cnt13/sum_cons_cnt*100,
         cons_cnt18_ratio=cons_cnt18/sum_cons_cnt*100,
         cons_cnt22_ratio=cons_cnt22/sum_cons_cnt*100)%>%
  select(-sum_cons_cnt)



##접속 간격
cs.v11<-cs.v2.0%>%
  group_by(CUS_ID)%>%
  #첫 사용일과 마지막 사용일 평균 간격& 한번 사용한 경우 간격은 없으므로 NA(임시) 부여 
  summarize(interval=ifelse(n()==1,NA,as.numeric((max(TIME_ID2)-min(TIME_ID2))/(n()-1))))

##클래스 다양성
cs.v12<-cls%>%
  distinct(CUS_ID,BACT_NM)%>%
  group_by(CUS_ID)%>%
  summarize(diversity=n()) #사용한 클래스 개수 

##클래스 별 횟수 및 비율
cs.v13.0<-cls%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(class_num=sum(SITE_CNT)) #클래스 별 카운트 합산 

cs.v13<-cs.v13.0%>%
  cast(CUS_ID~BACT_NM,value="class_num") #열데이터를 행 데이터로 변환 
colnames(cs.v13)<-c("CUS_ID",paste("횟수",colnames(cs.v13[-1]),sep="_")) #열 이름 변경 

cs.v14<-cs.v13.0%>%
  group_by(CUS_ID)%>%
  mutate(class_ratio=class_num/sum(class_num)*100)%>% #클래스 별 카운트 비율 계산 
  select(CUS_ID,BACT_NM,class_ratio)%>%
  cast(CUS_ID~BACT_NM,value="class_ratio") #열데이터를 행 데이터로 변환 
colnames(cs.v14)<-c("CUS_ID",paste("횟수비율",colnames(cs.v14[-1]),sep="_")) #열 이름 변경 



##클래스 별 소모 시간 및 비율
cs.v15.0<-cls%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(class_time=sum(ST_TIME)) #클래스 별 소모시간 합산 

cs.v15<-cs.v15.0%>%
  cast(CUS_ID~BACT_NM,value="class_time") #열데이터를 행 데이터로 변환
colnames(cs.v15)<-c("CUS_ID",paste("소모시간",colnames(cs.v15[-1]),sep="_")) #열 이름 변경 

cs.v16<-cs.v15.0%>%
  group_by(CUS_ID)%>%
  mutate(class_ratio2=class_time/sum(class_time)*100)%>% #클래스 별 소모시간 비율 계산 
  select(CUS_ID,BACT_NM,class_ratio2)%>%
  cast(CUS_ID~BACT_NM,value="class_ratio2") #열데이터를 행 데이터로 변환
colnames(cs.v16)<-c("CUS_ID",paste("시간비율",colnames(cs.v16[-1]),sep="_")) #열 이름 변경


##클래스별 visit 및 비율
#cs.v17.0<-cls%>%
#  group_by(CUS_ID,BACT_NM)%>%
#  summarize(class_visit=n()) #클래스 별 visit 합산 
#
#cs.v17<-cs.v17.0%>%
#  cast(CUS_ID~BACT_NM,value="class_visit") #열데이터를 행 데이터로 변환
#colnames(cs.v17)<-c("CUS_ID",paste("visit",colnames(cs.v17[-1]),sep="_")) #열 이름 변경 
#
#cs.v18<-cs.v17.0%>%
#  group_by(CUS_ID)%>%
#  mutate(class_ratio3=class_visit/sum(class_visit)*100)%>% #클래스 별 visit 비율 계산 
#  select(CUS_ID,BACT_NM,class_ratio3)%>%
#  cast(CUS_ID~BACT_NM,value="class_ratio3") #열데이터를 행 데이터로 변환
#colnames(cs.v18)<-c("CUS_ID",paste("visit비율",colnames(cs.v18[-1]),sep="_")) #열 이름 변경


#dongyoun 변수만들기
##min,max,mean,median,standard deviation of time per website visit
cs.d3<-cls%>%
  group_by(CUS_ID)%>%
  summarize(v_t_min=min(ST_TIME),v_t_max=max(ST_TIME),v_t_mean=mean(ST_TIME),v_t_median=median(ST_TIME),v_t_sd=sd(ST_TIME))

##min,max,mean,median,standard deviation of number of page requests per website visit
cs.d4<-cls%>%
  group_by(CUS_ID)%>%
  summarize(v_pr_min=min(SITE_CNT),v_pr_max=max(SITE_CNT),v_pr_mean=mean(SITE_CNT),v_pr_median=median(SITE_CNT),v_pr_sd=sd(SITE_CNT))

##coefficient of variation for website category
cs.d5.1<-cls%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(SITE_CNT=sum(SITE_CNT),visit=n())%>%
  group_by(CUS_ID)%>%
  summarize(cat_coef_visit=sd(visit)/mean(visit),cat_coef_cnt=sd(SITE_CNT)/mean(SITE_CNT))
cs.d5.2<-cls%>%
  filter(ST_TIME>0)%>%
  group_by(CUS_ID,BACT_NM)%>%
  summarize(ST_TIME=sum(ST_TIME))%>%
  group_by(CUS_ID)%>%
  summarize(cat_coef_time=sd(ST_TIME)/mean(ST_TIME))
cs.d5<-cs.d5.2%>%
  left_join(cs.d5.1)

##coefficient of variation for time
cs.d6.1<-cs.v9%>%
  group_by(CUS_ID)%>%
  summarize(time_coef_time=sd(c(cons_time0,cons_time5,cons_time8,cons_time9,cons_time12,cons_time13,cons_time18,cons_time22))/
              mean(c(cons_time0,cons_time5,cons_time8,cons_time9,cons_time12,cons_time13,cons_time18,cons_time22)))
cs.d6.2<-cs.v10%>%
  group_by(CUS_ID)%>%
  summarize(time_coef_visits=sd(c(cons_num0,cons_num5,cons_num8,cons_num9,cons_num12,cons_num13,cons_num18,cons_num22))/
              mean(c(cons_num0,cons_num5,cons_num8,cons_num9,cons_num12,cons_num13,cons_num18,cons_num22)))
cs.d6.3<-cs.d9%>%
  group_by(CUS_ID)%>%
  summarize(time_coef_cnt=sd(c(cons_cnt0,cons_cnt5,cons_cnt8,cons_cnt9,cons_cnt12,cons_cnt13,cons_cnt18,cons_cnt22))/
              mean(c(cons_cnt0,cons_cnt5,cons_cnt8,cons_cnt9,cons_cnt12,cons_cnt13,cons_cnt18,cons_cnt22)))
cs.d6<-cs.d6.1%>%
  left_join(cs.d6.2)%>%
  left_join(cs.d6.3)


##coefficient of variation for day
cs.d7.1<-cs.v5%>%
  group_by(CUS_ID)%>%
  summarize(day_coef_time=sd(c(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time))/
           mean(c(sun_time,mon_time,tues_time,wednes_time,thurs_time,fri_time,satur_time)))
cs.d7.2<-cs.v6%>%
  group_by(CUS_ID)%>%
  summarize(day_coef_visit=sd(c(sun_day,mon_day,tues_day,wednes_day,thurs_day,fri_day,satur_day))/
           mean(c(sun_day,mon_day,tues_day,wednes_day,thurs_day,fri_day,satur_day)))
cs.d7.3<-cs.d1%>%
  group_by(CUS_ID)%>%
  summarize(day_coef_cnt=sd(c(sun_cnt,mon_cnt,tues_cnt,wednes_cnt,thurs_cnt,fri_cnt,satur_cnt))/
           mean(c(sun_cnt,mon_cnt,tues_cnt,wednes_cnt,thurs_cnt,fri_cnt,satur_cnt)))
cs.d7<-cs.d7.1%>%
  left_join(cs.d7.2)%>%
  left_join(cs.d7.3)


##coefficient of variation for month
cs.d8.1<-cs.v7%>%
  group_by(CUS_ID)%>%
  summarize(mon_coef_time=sd(c(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time))/
           mean(c(jan_time,fab_time,mar_time,apr_time,may_time,jun_time,jul_time,aug_time,sep_time,oct_time,nov_time,dec_time)))
cs.d8.2<-cs.v8%>%
  group_by(CUS_ID)%>%
  summarize(mon_coef_visit=sd(c(jan_day,fab_day,mar_day,apr_day,may_day,jun_day,jul_day,aug_day,sep_day,oct_day,nov_day,dec_day))/
          mean(c(jan_day,fab_day,mar_day,apr_day,may_day,jun_day,jul_day,aug_day,sep_day,oct_day,nov_day,dec_day)))
cs.d8.3<-cs.d0%>%
  group_by(CUS_ID)%>%
  summarize(mon_coef_cnt=sd(c(jan_cnt,fab_cnt,mar_cnt,apr_cnt,may_cnt,jun_cnt,jul_cnt,aug_cnt,sep_cnt,oct_cnt,nov_cnt,dec_cnt))/
           mean(c(jan_cnt,fab_cnt,mar_cnt,apr_cnt,may_cnt,jun_cnt,jul_cnt,aug_cnt,sep_cnt,oct_cnt,nov_cnt,dec_cnt)))
cs.d8<-cs.d8.1%>%
  left_join(cs.d8.2)%>%
  left_join(cs.d8.3)

cs<-cs%>%
  left_join(cs.v1)%>%
  left_join(cs.v2)%>%
  left_join(cs.v3)%>%
  left_join(cs.v4)%>%
  left_join(cs.d2)%>%
  left_join(cs.v5)%>%
  left_join(cs.v6)%>%
  left_join(cs.d1)%>%
  left_join(cs.v7)%>%
  left_join(cs.v8)%>%
  left_join(cs.d0)%>%
  left_join(cs.v9)%>%
  left_join(cs.v10)%>%
  left_join(cs.d9)%>%
  left_join(cs.v11)%>%
  left_join(cs.v12)%>%
  left_join(cs.v13)%>%
  left_join(cs.v14)%>%
  left_join(cs.v15)%>%
  left_join(cs.v16)%>%
  left_join(cs.v17)%>%
  left_join(cs.v18)%>%
  left_join(cs.d3)%>%
  left_join(cs.d4)%>%
  left_join(cs.d5)%>%
  left_join(cs.d6)%>%
  left_join(cs.d7)%>%
  left_join(cs.d8)

write.csv(cs,"dw_cs_others.csv",row.names=FALSE)

#변수 생성 with Search keyword
##검색량

##관심사별 비율

##관심사별 횟수

##관심사별 다양성
