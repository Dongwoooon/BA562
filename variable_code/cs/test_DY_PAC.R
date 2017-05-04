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


setwd("D:/test/")
cls<-read.delim("test_clickstreams.tab",stringsAsFactors = T)

#z 뽑는 function
getz<-function(x) return((x-mean(x,na.rm=T))/sd(x,na.rm=T))


#데이터 전처리 과정


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
  mutate(wk_ratio1=wk_time/sum_time*100,
         we_ratio1=we_time/sum_time*100)%>%
  mutate(wk_z=getz(wk_ratio1),
         we_z=getz(we_ratio1))%>%
  mutate(wk_pat1=ifelse(wk_z>=1,"주중형",
                       ifelse(we_z>=1,"주말형","유형없음")))%>%
  select(-sum_time)



##(day base: 소모시간을 무시하고 방문한 날에 1값 부여) 주중/주말형
cs.v4<-cs.v2.0%>%
  mutate(wk_day=ifelse(wday(TIME_ID2)%in%2:6,1,0),we_day=ifelse(wday(TIME_ID2)%in%c(1,7),1,0))%>% 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_day,we_day)%>%
  mutate(sum_day=wk_day+we_day)%>%
  mutate(wk_ratio2=wk_day/sum_day*100,
         we_ratio2=we_day/sum_day*100)%>%
  mutate(wk_z2=getz(wk_ratio2),
         we_z2=getz(we_ratio2))%>%
  mutate(wk_pat2=ifelse(wk_z2>=1,"주중형",
                       ifelse(we_z2>=1,"주말형","유형없음")))%>%  
  select(-sum_day)

##number of page requests and percentage of total number of page requests during 주중/주말
cs.d2<-cls%>%
  mutate(wk_cnt=ifelse(wday(ymd_h(TIME_ID))%in%2:6,SITE_CNT,0),we_cnt=ifelse(wday(ymd_h(TIME_ID))%in%c(1,7),SITE_CNT,0))%>% #주중/주말 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),wk_cnt,we_cnt)%>% #주중/ 주말 카운트 합산
  mutate(sum_cnt=wk_cnt+we_cnt)%>%
  mutate(wk_ratio3=wk_cnt/sum_cnt*100,
         we_ratio3=we_cnt/sum_cnt*100)%>%
  mutate(wk_z3=getz(wk_ratio3),
         we_z3=getz(we_ratio3))%>%
  mutate(wk_pat3=ifelse(wk_z3>=1,"주중형",
                       ifelse(we_z3>=1,"주말형","유형없음")))%>%  
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
  mutate(sum_time=sun_time+mon_time+tues_time+wednes_time+thurs_time+fri_time+satur_time)%>% #전체 소모시간
  #요일 별 소모시간 비율 계산
  mutate(mon_ratio=mon_time/sum_time*100,
         tues_ratio=tues_time/sum_time*100,
         wednes_ratio=wednes_time/sum_time*100,
         thurs_ratio=thurs_time/sum_time*100,
         fri_ratio=fri_time/sum_time*100,
         satur_ratio=satur_time/sum_time*100,
         sun_ratio=sun_time/sum_time*100)%>%
  mutate(mon_z=getz(mon_ratio),
         tues_z=getz(tues_ratio),
         wednes_z=getz(wednes_ratio),
         thurs_z=getz(thurs_ratio),
         fri_z=getz(fri_ratio),
         satur_z=getz(satur_ratio),
         sun_z=getz(sun_ratio))%>%
  mutate(day_pat1=ifelse(((mon_z>=1)+(tues_z>=1)+(wednes_z>=1)+(thurs_z>=1)+(fri_z>=1)+(satur_z>=1)+(sun_z>=1))==1,
                    ifelse(mon_z>=1,"월요일",ifelse(tues_z>=1,"화요일",ifelse(wednes_z>=1,"수요일",ifelse(thurs_z>=1,"목요일",
                      ifelse(fri_z>=1,"금요일",ifelse(satur_z>=1,"토요일",ifelse(sun_z>=1,"일요일","Error"))))))),"유형없음"))%>%
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
  mutate(sum_day=sun_day+mon_day+tues_day+wednes_day+thurs_day+fri_day+satur_day)%>%
  mutate(mon_ratio2=mon_day/sum_day*100,
         tues_ratio2=tues_day/sum_day*100,
         wednes_ratio2=wednes_day/sum_day*100,
         thurs_ratio2=thurs_day/sum_day*100,
         fri_ratio2=fri_day/sum_day*100,
         satur_ratio2=satur_day/sum_day*100,
         sun_ratio2=sun_day/sum_day*100)%>%
mutate(mon_z2=getz(mon_ratio2),
       tues_z2=getz(tues_ratio2),
       wednes_z2=getz(wednes_ratio2),
       thurs_z2=getz(thurs_ratio2),
       fri_z2=getz(fri_ratio2),
       satur_z2=getz(satur_ratio2),
       sun_z2=getz(sun_ratio2))%>%
  mutate(day_pat2=ifelse(((mon_z2>=1)+(tues_z2>=1)+(wednes_z2>=1)+(thurs_z2>=1)+(fri_z2>=1)+(satur_z2>=1)+(sun_z2>=1))==1,
                   ifelse(mon_z2>=1,"월요일",ifelse(tues_z2>=1,"화요일",ifelse(wednes_z2>=1,"수요일",ifelse(thurs_z2>=1,"목요일",
                      ifelse(fri_z2>=1,"금요일",ifelse(satur_z2>=1,"토요일",ifelse(sun_z2>=1,"일요일","Error"))))))),"유형없음"))%>%
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
  mutate(sum_cnt=sun_cnt+mon_cnt+tues_cnt+wednes_cnt+thurs_cnt+fri_cnt+satur_cnt)%>% #전체 카운트
  #요일 별 카운트 비율 계산
  mutate(mon_ratio3=mon_cnt/sum_cnt*100,
         tues_ratio3=tues_cnt/sum_cnt*100,
         wednes_ratio3=wednes_cnt/sum_cnt*100,
         thurs_ratio3=thurs_cnt/sum_cnt*100,
         fri_ratio3=fri_cnt/sum_cnt*100,
         satur_ratio3=satur_cnt/sum_cnt*100,
         sun_ratio3=sun_cnt/sum_cnt*100)%>%
mutate(mon_z3=getz(mon_ratio3),
       tues_z3=getz(tues_ratio3),
       wednes_z3=getz(wednes_ratio3),
       thurs_z3=getz(thurs_ratio3),
       fri_z3=getz(fri_ratio3),
       satur_z3=getz(satur_ratio3),
       sun_z3=getz(sun_ratio3))%>%
  mutate(day_pat3=ifelse(((mon_z3>=1)+(tues_z3>=1)+(wednes_z3>=1)+(thurs_z3>=1)+(fri_z3>=1)+(satur_z3>=1)+(sun_z3>=1))==1,
                  ifelse(mon_z3>=1,"월요일",ifelse(tues_z3>=1,"화요일",ifelse(wednes_z3>=1,"수요일",ifelse(thurs_z3>=1,"목요일",
                    ifelse(fri_z3>=1,"금요일",ifelse(satur_z3>=1,"토요일",ifelse(sun_z3>=1,"일요일","Error"))))))),"유형없음"))%>%
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
  mutate(sum_time=jan_time+fab_time+mar_time+apr_time+may_time+jun_time+jul_time+aug_time+sep_time+oct_time+nov_time+dec_time)%>%
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
  mutate(jan_z=getz(jan_ratio),
         fab_z=getz(fab_ratio),
         mar_z=getz(mar_ratio),
         apr_z=getz(apr_ratio),
         may_z=getz(may_ratio),
         jun_z=getz(jun_ratio),
         jul_z=getz(jul_ratio),
         aug_z=getz(aug_ratio),
         sep_z=getz(sep_ratio),
         oct_z=getz(oct_ratio),
         nov_z=getz(nov_ratio),
         dec_z=getz(dec_ratio))%>%
  mutate(month_pat1=ifelse(((jan_z>=1)+(fab_z>=1)+(mar_z>=1)+(apr_z>=1)+(may_z>=1)+(jun_z>=1)+(jul_z>=1)+(aug_z>=1)+
                             (sep_z>=1)+(oct_z>=1)+(nov_z>=1)+(dec_z>=1))==1,
                             ifelse(jan_z>=1,"1월",ifelse(fab_z>=1,"2월",ifelse(mar_z>=1,"3월",ifelse(apr_z>=1,"4월",ifelse(may_z>=1,"5월",
                               ifelse(jun_z>=1,"6월",ifelse(jul_z>=1,"7월",ifelse(aug_z>=1,"8월",ifelse(sep_z>=1,"9월",
                                  ifelse(oct_z>=1,"10월",ifelse(nov_z>=1,"11월",ifelse(dec_z>=1,"12월","Error")))))))))))),"유형없음"))%>%
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
  mutate(sum_day=jan_day+fab_day+mar_day+apr_day+may_day+jun_day+jul_day+aug_day+sep_day+oct_day+nov_day+dec_day)%>%
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
  mutate(jan_z2=getz(jan_ratio2),
         fab_z2=getz(fab_ratio2),
         mar_z2=getz(mar_ratio2),
         apr_z2=getz(apr_ratio2),
         may_z2=getz(may_ratio2),
         jun_z2=getz(jun_ratio2),
         jul_z2=getz(jul_ratio2),
         aug_z2=getz(aug_ratio2),
         sep_z2=getz(sep_ratio2),
         oct_z2=getz(oct_ratio2),
         nov_z2=getz(nov_ratio2),
         dec_z2=getz(dec_ratio2))%>%
  mutate(month_pat2=ifelse(((jan_z2>=1)+(fab_z2>=1)+(mar_z2>=1)+(apr_z2>=1)+(may_z2>=1)+(jun_z2>=1)+(jul_z2>=1)+(aug_z2>=1)+(sep_z2>=1)+(oct_z2>=1)+(nov_z2>=1)+(dec_z2>=1))==1,
                      ifelse(jan_z2>=1,"1월",ifelse(fab_z2>=1,"2월",ifelse(mar_z2>=1,"3월",ifelse(apr_z2>=1,"4월",ifelse(may_z2>=1,"5월",
                        ifelse(jun_z2>=1,"6월",ifelse(jul_z2>=1,"7월",ifelse(aug_z2>=1,"8월",ifelse(sep_z2>=1,"9월",
                          ifelse(oct_z2>=1,"10월",ifelse(nov_z2>=1,"11월",ifelse(dec_z2>=1,"12월","Error")))))))))))),"유형없음"))%>%
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
  mutate(sum_cnt=jan_cnt+fab_cnt+mar_cnt+apr_cnt+may_cnt+jun_cnt+jul_cnt+aug_cnt+sep_cnt+oct_cnt+nov_cnt+dec_cnt)%>% #전체 카운트
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
  mutate(jan_z3=getz(jan_ratio3),
         fab_z3=getz(fab_ratio3),
         mar_z3=getz(mar_ratio3),
         apr_z3=getz(apr_ratio3),
         may_z3=getz(may_ratio3),
         jun_z3=getz(jun_ratio3),
         jul_z3=getz(jul_ratio3),
         aug_z3=getz(aug_ratio3),
         sep_z3=getz(sep_ratio3),
         oct_z3=getz(oct_ratio3),
         nov_z3=getz(nov_ratio3),
         dec_z3=getz(dec_ratio3))%>%
  mutate(month_pat3=ifelse(((jan_z3>=1)+(fab_z3>=1)+(mar_z3>=1)+(apr_z3>=1)+(may_z3>=1)+(jun_z3>=1)+(jul_z3>=1)+(aug_z3>=1)+(sep_z3>=1)+(oct_z3>=1)+(nov_z3>=1)+(dec_z3>=1))==1,
                           ifelse(jan_z3>=1,"1월",ifelse(fab_z3>=1,"2월",ifelse(mar_z3>=1,"3월",ifelse(apr_z3>=1,"4월",ifelse(may_z3>=1,"5월",
                              ifelse(jun_z3>=1,"6월",ifelse(jul_z3>=1,"7월",ifelse(aug_z3>=1,"8월",ifelse(sep_z3>=1,"9월",
                                 ifelse(oct_z3>=1,"10월",ifelse(nov_z3>=1,"11월",ifelse(dec_z3>=1,"12월","Error")))))))))))),"유형없음"))%>%
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
  mutate(sum_cons_time=cons_time0+cons_time5+cons_time8+cons_time9+cons_time12+cons_time13+cons_time18+cons_time22)%>% #전체 소모시간 
  #시간대 별 소모시간 비율 
  mutate(cons_time0_ratio=cons_time0/sum_cons_time*100,
         cons_time5_ratio=cons_time5/sum_cons_time*100,
         cons_time8_ratio=cons_time8/sum_cons_time*100,
         cons_time9_ratio=cons_time9/sum_cons_time*100,
         cons_time12_ratio=cons_time12/sum_cons_time*100,
         cons_time13_ratio=cons_time13/sum_cons_time*100,
         cons_time18_ratio=cons_time18/sum_cons_time*100,
         cons_time22_ratio=cons_time22/sum_cons_time*100)%>%
  mutate(time0_z=getz(cons_time0_ratio),
         time5_z=getz(cons_time5_ratio),
         time8_z=getz(cons_time8_ratio),
         time9_z=getz(cons_time9_ratio),
         time12_z=getz(cons_time12_ratio),
         time13_z=getz(cons_time13_ratio),
         time18_z=getz(cons_time18_ratio),
         time22_z=getz(cons_time22_ratio))%>%
  mutate(time_pat1=ifelse(((time0_z>=1)+(time5_z>=1)+(time8_z>=1)+(time9_z>=1)+(time12_z>=1)+(time13_z>=1)+(time18_z>=1)+(time22_z>=1))==1,
            ifelse(time0_z>=1,"새벽",ifelse(time5_z>=1,"아침",ifelse(time8_z>=1,"출근",ifelse(time9_z>=1,"오전",ifelse(time12_z>=1,"점심",
                ifelse(time13_z>=1,"오후",ifelse(time18_z>=1,"저녁",ifelse(time22_z>=1,"밤","Error")))))))),"유형없음"))%>%
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
  mutate(sum_cons_num=cons_num0+cons_num5+cons_num8+cons_num9+cons_num12+cons_num13+cons_num18+cons_num22)%>%
  mutate(cons_num0_ratio=cons_num0/sum_cons_num*100,
         cons_num5_ratio=cons_num5/sum_cons_num*100,
         cons_num8_ratio=cons_num8/sum_cons_num*100,
         cons_num9_ratio=cons_num9/sum_cons_num*100,
         cons_num12_ratio=cons_num12/sum_cons_num*100,
         cons_num13_ratio=cons_num13/sum_cons_num*100,
         cons_num18_ratio=cons_num18/sum_cons_num*100,
         cons_num22_ratio=cons_num22/sum_cons_num*100)%>%
  mutate(time0_z2=getz(cons_num0_ratio),
         time5_z2=getz(cons_num5_ratio),
         time8_z2=getz(cons_num8_ratio),
         time9_z2=getz(cons_num9_ratio),
         time12_z2=getz(cons_num12_ratio),
         time13_z2=getz(cons_num13_ratio),
         time18_z2=getz(cons_num18_ratio),
         time22_z2=getz(cons_num22_ratio))%>%
  mutate(time_pat2=ifelse(((time0_z2>=1)+(time5_z2>=1)+(time8_z2>=1)+(time9_z2>=1)+(time12_z2>=1)+(time13_z2>=1)+(time18_z2>=1)+(time22_z2>=1))==1,
                     ifelse(time0_z2>=1,"새벽",ifelse(time5_z2>=1,"아침",ifelse(time8_z2>=1,"출근",ifelse(time9_z2>=1,"오전",ifelse(time12_z2>=1,"점심",
                       ifelse(time13_z2>=1,"오후",ifelse(time18_z2>=1,"저녁",ifelse(time22_z2>=1,"밤","Error")))))))),"유형없음"))%>%
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
  mutate(sum_cons_cnt=cons_cnt0+cons_cnt5+cons_cnt8+cons_cnt9+cons_cnt12+cons_cnt13+cons_cnt18+cons_cnt22)%>% #전체 소모시간
  #시간대 별 소모시간 비율 
  mutate(cons_cnt0_ratio=cons_cnt0/sum_cons_cnt*100,
         cons_cnt5_ratio=cons_cnt5/sum_cons_cnt*100,
         cons_cnt8_ratio=cons_cnt8/sum_cons_cnt*100,
         cons_cnt9_ratio=cons_cnt9/sum_cons_cnt*100,
         cons_cnt12_ratio=cons_cnt12/sum_cons_cnt*100,
         cons_cnt13_ratio=cons_cnt13/sum_cons_cnt*100,
         cons_cnt18_ratio=cons_cnt18/sum_cons_cnt*100,
         cons_cnt22_ratio=cons_cnt22/sum_cons_cnt*100)%>%
  mutate(time0_z3=getz(cons_cnt0_ratio),
         time5_z3=getz(cons_cnt5_ratio),
         time8_z3=getz(cons_cnt8_ratio),
         time9_z3=getz(cons_cnt9_ratio),
         time12_z3=getz(cons_cnt12_ratio),
         time13_z3=getz(cons_cnt13_ratio),
         time18_z3=getz(cons_cnt18_ratio),
         time22_z3=getz(cons_cnt22_ratio))%>%
  mutate(time_pat3=ifelse(((time0_z3>=1)+(time5_z3>=1)+(time8_z3>=1)+(time9_z3>=1)+(time12_z3>=1)+(time13_z3>=1)+(time18_z3>=1)+(time22_z3>=1))==1,
                     ifelse(time0_z3>=1,"새벽",ifelse(time5_z3>=1,"아침",ifelse(time8_z3>=1,"출근",ifelse(time9_z3>=1,"오전",ifelse(time12_z3>=1,"점심",
                       ifelse(time13_z3>=1,"오후",ifelse(time18_z3>=1,"저녁",ifelse(time22_z3>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_cnt)
####주중/주말 데이터 split
cls_week<-cls%>%
  mutate(week=ifelse(wday(ymd_h(TIME_ID))%in%2:6,"주중","주말")) #주중/주말 구분

cls_wk<-cls_week%>% 
  filter(week=="주중")%>%
  select(-week)

cls_we<-cls_week%>%
  filter(week=="주말")%>%
  select(-week)


#########주중 시간대
##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 머무른 시간
cs.v17<-cls_wk%>%
  mutate(cons_time0_wk=ifelse(hour(ymd_h(TIME_ID))%in%0:4,ST_TIME,0),
         cons_time5_wk=ifelse(hour(ymd_h(TIME_ID))%in%5:7,ST_TIME,0),
         cons_time8_wk=ifelse(hour(ymd_h(TIME_ID))==8,ST_TIME,0),
         cons_time9_wk=ifelse(hour(ymd_h(TIME_ID))%in%9:11,ST_TIME,0),
         cons_time12_wk=ifelse(hour(ymd_h(TIME_ID))==12,ST_TIME,0),
         cons_time13_wk=ifelse(hour(ymd_h(TIME_ID))%in%13:17,ST_TIME,0),
         cons_time18_wk=ifelse(hour(ymd_h(TIME_ID))%in%18:21,ST_TIME,0),
         cons_time22_wk=ifelse(hour(ymd_h(TIME_ID))%in%22:23,ST_TIME,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_time0_wk,cons_time5_wk,cons_time8_wk,cons_time9_wk,cons_time12_wk,cons_time13_wk,cons_time18_wk,cons_time22_wk)%>%
  mutate(sum_cons_time_wk=cons_time0_wk+cons_time5_wk+cons_time8_wk+cons_time9_wk+cons_time12_wk+cons_time13_wk+cons_time18_wk+cons_time22_wk)%>% #전체 소모시간 
  #시간대 별 소모시간 비율 
  mutate(cons_time0_ratio_wk=cons_time0_wk/sum_cons_time_wk*100,
         cons_time5_ratio_wk=cons_time5_wk/sum_cons_time_wk*100,
         cons_time8_ratio_wk=cons_time8_wk/sum_cons_time_wk*100,
         cons_time9_ratio_wk=cons_time9_wk/sum_cons_time_wk*100,
         cons_time12_ratio_wk=cons_time12_wk/sum_cons_time_wk*100,
         cons_time13_ratio_wk=cons_time13_wk/sum_cons_time_wk*100,
         cons_time18_ratio_wk=cons_time18_wk/sum_cons_time_wk*100,
         cons_time22_ratio_wk=cons_time22_wk/sum_cons_time_wk*100)%>%
  mutate(time0_z_wk=getz(cons_time0_ratio_wk),
         time5_z_wk=getz(cons_time5_ratio_wk),
         time8_z_wk=getz(cons_time8_ratio_wk),
         time9_z_wk=getz(cons_time9_ratio_wk),
         time12_z_wk=getz(cons_time12_ratio_wk),
         time13_z_wk=getz(cons_time13_ratio_wk),
         time18_z_wk=getz(cons_time18_ratio_wk),
         time22_z_wk=getz(cons_time22_ratio_wk))%>%
  mutate(time_pat_wk=ifelse(((time0_z_wk>=1)+(time5_z_wk>=1)+(time8_z_wk>=1)+(time9_z_wk>=1)+(time12_z_wk>=1)+(time13_z_wk>=1)+(time18_z_wk>=1)+(time22_z_wk>=1))==1,
                       ifelse(time0_z_wk>=1,"새벽",ifelse(time5_z_wk>=1,"아침",ifelse(time8_z_wk>=1,"출근",ifelse(time9_z_wk>=1,"오전",ifelse(time12_z_wk>=1,"점심",
                        ifelse(time13_z_wk>=1,"오후",ifelse(time18_z_wk>=1,"저녁",ifelse(time22_z_wk>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_time_wk)



##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 횟수가 많은 시간
cs.v18<-cls_wk%>%
  mutate(cons_num0_wk=ifelse(hour(ymd_h(TIME_ID))%in%0:4,1,0),
         cons_num5_wk=ifelse(hour(ymd_h(TIME_ID))%in%5:7,1,0),
         cons_num8_wk=ifelse(hour(ymd_h(TIME_ID))==8,1,0),
         cons_num9_wk=ifelse(hour(ymd_h(TIME_ID))%in%9:11,1,0),
         cons_num12_wk=ifelse(hour(ymd_h(TIME_ID))==12,1,0),
         cons_num13_wk=ifelse(hour(ymd_h(TIME_ID))%in%13:17,1,0),
         cons_num18_wk=ifelse(hour(ymd_h(TIME_ID))%in%18:21,1,0),
         cons_num22_wk=ifelse(hour(ymd_h(TIME_ID))%in%22:23,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_num0_wk,cons_num5_wk,cons_num8_wk,cons_num9_wk,cons_num12_wk,cons_num13_wk,cons_num18_wk,cons_num22_wk)%>%
  mutate(sum_cons_num_wk=cons_num0_wk+cons_num5_wk+cons_num8_wk+cons_num9_wk+cons_num12_wk+cons_num13_wk+cons_num18_wk+cons_num22_wk)%>%
  mutate(cons_num0_ratio_wk=cons_num0_wk/sum_cons_num_wk*100,
         cons_num5_ratio_wk=cons_num5_wk/sum_cons_num_wk*100,
         cons_num8_ratio_wk=cons_num8_wk/sum_cons_num_wk*100,
         cons_num9_ratio_wk=cons_num9_wk/sum_cons_num_wk*100,
         cons_num12_ratio_wk=cons_num12_wk/sum_cons_num_wk*100,
         cons_num13_ratio_wk=cons_num13_wk/sum_cons_num_wk*100,
         cons_num18_ratio_wk=cons_num18_wk/sum_cons_num_wk*100,
         cons_num22_ratio_wk=cons_num22_wk/sum_cons_num_wk*100)%>%
  mutate(time0_z2_wk=getz(cons_num0_ratio_wk),
         time5_z2_wk=getz(cons_num5_ratio_wk),
         time8_z2_wk=getz(cons_num8_ratio_wk),
         time9_z2_wk=getz(cons_num9_ratio_wk),
         time12_z2_wk=getz(cons_num12_ratio_wk),
         time13_z2_wk=getz(cons_num13_ratio_wk),
         time18_z2_wk=getz(cons_num18_ratio_wk),
         time22_z2_wk=getz(cons_num22_ratio_wk))%>%
  mutate(time_pat2_wk=ifelse(((time0_z2_wk>=1)+(time5_z2_wk>=1)+(time8_z2_wk>=1)+(time9_z2_wk>=1)+(time12_z2_wk>=1)+(time13_z2_wk>=1)+(time18_z2_wk>=1)+(time22_z2_wk>=1))==1,
                        ifelse(time0_z2_wk>=1,"새벽",ifelse(time5_z2_wk>=1,"아침",ifelse(time8_z2_wk>=1,"출근",ifelse(time9_z2_wk>=1,"오전",ifelse(time12_z2_wk>=1,"점심",
                          ifelse(time13_z2_wk>=1,"오후",ifelse(time18_z2_wk>=1,"저녁",ifelse(time22_z2_wk>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_num_wk)

##number of page requests and percentage of total number of page requests during 시간
cs.v19<-cls_wk%>%
  mutate(cons_cnt0_wk=ifelse(hour(ymd_h(TIME_ID))%in%0:4,SITE_CNT,0),
         cons_cnt5_wk=ifelse(hour(ymd_h(TIME_ID))%in%5:7,SITE_CNT,0),
         cons_cnt8_wk=ifelse(hour(ymd_h(TIME_ID))==8,SITE_CNT,0),
         cons_cnt9_wk=ifelse(hour(ymd_h(TIME_ID))%in%9:11,SITE_CNT,0),
         cons_cnt12_wk=ifelse(hour(ymd_h(TIME_ID))==12,SITE_CNT,0),
         cons_cnt13_wk=ifelse(hour(ymd_h(TIME_ID))%in%13:17,SITE_CNT,0),
         cons_cnt18_wk=ifelse(hour(ymd_h(TIME_ID))%in%18:21,SITE_CNT,0),
         cons_cnt22_wk=ifelse(hour(ymd_h(TIME_ID))%in%22:23,SITE_CNT,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_cnt0_wk,cons_cnt5_wk,cons_cnt8_wk,cons_cnt9_wk,cons_cnt12_wk,cons_cnt13_wk,cons_cnt18_wk,cons_cnt22_wk)%>%
  mutate(sum_cons_cnt_wk=cons_cnt0_wk+cons_cnt5_wk+cons_cnt8_wk+cons_cnt9_wk+cons_cnt12_wk+cons_cnt13_wk+cons_cnt18_wk+cons_cnt22_wk)%>% #전체 소모시간
  #시간대 별 소모시간 비율 
  mutate(cons_cnt0_ratio_wk=cons_cnt0_wk/sum_cons_cnt_wk*100,
         cons_cnt5_ratio_wk=cons_cnt5_wk/sum_cons_cnt_wk*100,
         cons_cnt8_ratio_wk=cons_cnt8_wk/sum_cons_cnt_wk*100,
         cons_cnt9_ratio_wk=cons_cnt9_wk/sum_cons_cnt_wk*100,
         cons_cnt12_ratio_wk=cons_cnt12_wk/sum_cons_cnt_wk*100,
         cons_cnt13_ratio_wk=cons_cnt13_wk/sum_cons_cnt_wk*100,
         cons_cnt18_ratio_wk=cons_cnt18_wk/sum_cons_cnt_wk*100,
         cons_cnt22_ratio_wk=cons_cnt22_wk/sum_cons_cnt_wk*100)%>%
  mutate(time0_z3_wk=getz(cons_cnt0_ratio_wk),
         time5_z3_wk=getz(cons_cnt5_ratio_wk),
         time8_z3_wk=getz(cons_cnt8_ratio_wk),
         time9_z3_wk=getz(cons_cnt9_ratio_wk),
         time12_z3_wk=getz(cons_cnt12_ratio_wk),
         time13_z3_wk=getz(cons_cnt13_ratio_wk),
         time18_z3_wk=getz(cons_cnt18_ratio_wk),
         time22_z3_wk=getz(cons_cnt22_ratio_wk))%>%
  mutate(time_pat3_wk=ifelse(((time0_z3_wk>=1)+(time5_z3_wk>=1)+(time8_z3_wk>=1)+(time9_z3_wk>=1)+(time12_z3_wk>=1)+(time13_z3_wk>=1)+(time18_z3_wk>=1)+(time22_z3_wk>=1))==1,
                       ifelse(time0_z3_wk>=1,"새벽",ifelse(time5_z3_wk>=1,"아침",ifelse(time8_z3_wk>=1,"출근",ifelse(time9_z3_wk>=1,"오전",ifelse(time12_z3_wk>=1,"점심",
                         ifelse(time13_z3_wk>=1,"오후",ifelse(time18_z3_wk>=1,"저녁",ifelse(time22_z3_wk>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_cnt_wk)


#########주말 시간대
##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 머무른 시간
cs.v20<-cls_we%>%
  mutate(cons_time0_we=ifelse(hour(ymd_h(TIME_ID))%in%0:4,ST_TIME,0),
         cons_time5_we=ifelse(hour(ymd_h(TIME_ID))%in%5:7,ST_TIME,0),
         cons_time8_we=ifelse(hour(ymd_h(TIME_ID))==8,ST_TIME,0),
         cons_time9_we=ifelse(hour(ymd_h(TIME_ID))%in%9:11,ST_TIME,0),
         cons_time12_we=ifelse(hour(ymd_h(TIME_ID))==12,ST_TIME,0),
         cons_time13_we=ifelse(hour(ymd_h(TIME_ID))%in%13:17,ST_TIME,0),
         cons_time18_we=ifelse(hour(ymd_h(TIME_ID))%in%18:21,ST_TIME,0),
         cons_time22_we=ifelse(hour(ymd_h(TIME_ID))%in%22:23,ST_TIME,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_time0_we,cons_time5_we,cons_time8_we,cons_time9_we,cons_time12_we,cons_time13_we,cons_time18_we,cons_time22_we)%>%
  mutate(sum_cons_time_we=cons_time0_we+cons_time5_we+cons_time8_we+cons_time9_we+cons_time12_we+cons_time13_we+cons_time18_we+cons_time22_we)%>% #전체 소모시간 
  #시간대 별 소모시간 비율 
  mutate(cons_time0_ratio_we=cons_time0_we/sum_cons_time_we*100,
         cons_time5_ratio_we=cons_time5_we/sum_cons_time_we*100,
         cons_time8_ratio_we=cons_time8_we/sum_cons_time_we*100,
         cons_time9_ratio_we=cons_time9_we/sum_cons_time_we*100,
         cons_time12_ratio_we=cons_time12_we/sum_cons_time_we*100,
         cons_time13_ratio_we=cons_time13_we/sum_cons_time_we*100,
         cons_time18_ratio_we=cons_time18_we/sum_cons_time_we*100,
         cons_time22_ratio_we=cons_time22_we/sum_cons_time_we*100)%>%
  mutate(time0_z_we=getz(cons_time0_ratio_we),
         time5_z_we=getz(cons_time5_ratio_we),
         time8_z_we=getz(cons_time8_ratio_we),
         time9_z_we=getz(cons_time9_ratio_we),
         time12_z_we=getz(cons_time12_ratio_we),
         time13_z_we=getz(cons_time13_ratio_we),
         time18_z_we=getz(cons_time18_ratio_we),
         time22_z_we=getz(cons_time22_ratio_we))%>%
  mutate(time_pat_we=ifelse(((time0_z_we>=1)+(time5_z_we>=1)+(time8_z_we>=1)+(time9_z_we>=1)+(time12_z_we>=1)+(time13_z_we>=1)+(time18_z_we>=1)+(time22_z_we>=1))==1,
                      ifelse(time0_z_we>=1,"새벽",ifelse(time5_z_we>=1,"아침",ifelse(time8_z_we>=1,"출근",ifelse(time9_z_we>=1,"오전",ifelse(time12_z_we>=1,"점심",
                        ifelse(time13_z_we>=1,"오후",ifelse(time18_z_we>=1,"저녁",ifelse(time22_z_we>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_time_we)



##선호 시간대(00~04새벽,05~07아침,08출근,09~11오전,12점심,13~17오후,18~21저녁,22~23밤): 24시간 중 가장 오래 횟수가 많은 시간
cs.v21<-cls_we%>%
  mutate(cons_num0_we=ifelse(hour(ymd_h(TIME_ID))%in%0:4,1,0),
         cons_num5_we=ifelse(hour(ymd_h(TIME_ID))%in%5:7,1,0),
         cons_num8_we=ifelse(hour(ymd_h(TIME_ID))==8,1,0),
         cons_num9_we=ifelse(hour(ymd_h(TIME_ID))%in%9:11,1,0),
         cons_num12_we=ifelse(hour(ymd_h(TIME_ID))==12,1,0),
         cons_num13_we=ifelse(hour(ymd_h(TIME_ID))%in%13:17,1,0),
         cons_num18_we=ifelse(hour(ymd_h(TIME_ID))%in%18:21,1,0),
         cons_num22_we=ifelse(hour(ymd_h(TIME_ID))%in%22:23,1,0))%>%
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_num0_we,cons_num5_we,cons_num8_we,cons_num9_we,cons_num12_we,cons_num13_we,cons_num18_we,cons_num22_we)%>%
  mutate(sum_cons_num_we=cons_num0_we+cons_num5_we+cons_num8_we+cons_num9_we+cons_num12_we+cons_num13_we+cons_num18_we+cons_num22_we)%>%
  mutate(cons_num0_ratio_we=cons_num0_we/sum_cons_num_we*100,
         cons_num5_ratio_we=cons_num5_we/sum_cons_num_we*100,
         cons_num8_ratio_we=cons_num8_we/sum_cons_num_we*100,
         cons_num9_ratio_we=cons_num9_we/sum_cons_num_we*100,
         cons_num12_ratio_we=cons_num12_we/sum_cons_num_we*100,
         cons_num13_ratio_we=cons_num13_we/sum_cons_num_we*100,
         cons_num18_ratio_we=cons_num18_we/sum_cons_num_we*100,
         cons_num22_ratio_we=cons_num22_we/sum_cons_num_we*100)%>%
  mutate(time0_z2_we=getz(cons_num0_ratio_we),
         time5_z2_we=getz(cons_num5_ratio_we),
         time8_z2_we=getz(cons_num8_ratio_we),
         time9_z2_we=getz(cons_num9_ratio_we),
         time12_z2_we=getz(cons_num12_ratio_we),
         time13_z2_we=getz(cons_num13_ratio_we),
         time18_z2_we=getz(cons_num18_ratio_we),
         time22_z2_we=getz(cons_num22_ratio_we))%>%
  mutate(time_pat2_we=ifelse(((time0_z2_we>=1)+(time5_z2_we>=1)+(time8_z2_we>=1)+(time9_z2_we>=1)+(time12_z2_we>=1)+(time13_z2_we>=1)+(time18_z2_we>=1)+(time22_z2_we>=1))==1,
                        ifelse(time0_z2_we>=1,"새벽",ifelse(time5_z2_we>=1,"아침",ifelse(time8_z2_we>=1,"출근",ifelse(time9_z2_we>=1,"오전",ifelse(time12_z2_we>=1,"점심",
                          ifelse(time13_z2_we>=1,"오후",ifelse(time18_z2_we>=1,"저녁",ifelse(time22_z2_we>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_num_we)

##number of page requests and percentage of total number of page requests during 시간
cs.v22<-cls_we%>%
  mutate(cons_cnt0_we=ifelse(hour(ymd_h(TIME_ID))%in%0:4,SITE_CNT,0),
         cons_cnt5_we=ifelse(hour(ymd_h(TIME_ID))%in%5:7,SITE_CNT,0),
         cons_cnt8_we=ifelse(hour(ymd_h(TIME_ID))==8,SITE_CNT,0),
         cons_cnt9_we=ifelse(hour(ymd_h(TIME_ID))%in%9:11,SITE_CNT,0),
         cons_cnt12_we=ifelse(hour(ymd_h(TIME_ID))==12,SITE_CNT,0),
         cons_cnt13_we=ifelse(hour(ymd_h(TIME_ID))%in%13:17,SITE_CNT,0),
         cons_cnt18_we=ifelse(hour(ymd_h(TIME_ID))%in%18:21,SITE_CNT,0),
         cons_cnt22_we=ifelse(hour(ymd_h(TIME_ID))%in%22:23,SITE_CNT,0))%>% #위의 기준으로 시간대 구분 
  group_by(CUS_ID)%>%
  summarize_each(funs(sum),cons_cnt0_we,cons_cnt5_we,cons_cnt8_we,cons_cnt9_we,cons_cnt12_we,cons_cnt13_we,cons_cnt18_we,cons_cnt22_we)%>%
  mutate(sum_cons_cnt_we=cons_cnt0_we+cons_cnt5_we+cons_cnt8_we+cons_cnt9_we+cons_cnt12_we+cons_cnt13_we+cons_cnt18_we+cons_cnt22_we)%>% #전체 소모시간
  #시간대 별 소모시간 비율 
  mutate(cons_cnt0_ratio_we=cons_cnt0_we/sum_cons_cnt_we*100,
         cons_cnt5_ratio_we=cons_cnt5_we/sum_cons_cnt_we*100,
         cons_cnt8_ratio_we=cons_cnt8_we/sum_cons_cnt_we*100,
         cons_cnt9_ratio_we=cons_cnt9_we/sum_cons_cnt_we*100,
         cons_cnt12_ratio_we=cons_cnt12_we/sum_cons_cnt_we*100,
         cons_cnt13_ratio_we=cons_cnt13_we/sum_cons_cnt_we*100,
         cons_cnt18_ratio_we=cons_cnt18_we/sum_cons_cnt_we*100,
         cons_cnt22_ratio_we=cons_cnt22_we/sum_cons_cnt_we*100)%>%
  mutate(time0_z3_we=getz(cons_cnt0_ratio_we),
         time5_z3_we=getz(cons_cnt5_ratio_we),
         time8_z3_we=getz(cons_cnt8_ratio_we),
         time9_z3_we=getz(cons_cnt9_ratio_we),
         time12_z3_we=getz(cons_cnt12_ratio_we),
         time13_z3_we=getz(cons_cnt13_ratio_we),
         time18_z3_we=getz(cons_cnt18_ratio_we),
         time22_z3_we=getz(cons_cnt22_ratio_we))%>%
  mutate(time_pat3_we=ifelse(((time0_z3_we>=1)+(time5_z3_we>=1)+(time8_z3_we>=1)+(time9_z3_we>=1)+(time12_z3_we>=1)+(time13_z3_we>=1)+(time18_z3_we>=1)+(time22_z3_we>=1))==1,
                        ifelse(time0_z3_we>=1,"새벽",ifelse(time5_z3_we>=1,"아침",ifelse(time8_z3_we>=1,"출근",ifelse(time9_z3_we>=1,"오전",ifelse(time12_z3_we>=1,"점심",
                          ifelse(time13_z3_we>=1,"오후",ifelse(time18_z3_we>=1,"저녁",ifelse(time22_z3_we>=1,"밤","Error")))))))),"유형없음"))%>%
  select(-sum_cons_cnt_we)



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

cs<-cs.v1%>%
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
  left_join(cs.v17)%>%
  left_join(cs.v18)%>%
  left_join(cs.v19)%>%
  left_join(cs.v20)%>%
  left_join(cs.v21)%>%
  left_join(cs.v22)%>%
  left_join(cs.v11)%>%
  left_join(cs.v12)%>%
  left_join(cs.v13)%>%
  left_join(cs.v14)%>%
  left_join(cs.v15)%>%
  left_join(cs.v16)%>%
  left_join(cs.d3)%>%
  left_join(cs.d4)%>%
  left_join(cs.d5)%>%
  left_join(cs.d6)%>%
  left_join(cs.d7)%>%
  left_join(cs.d8)

write.csv(cs,"test_cs_others.csv",row.names=FALSE)
