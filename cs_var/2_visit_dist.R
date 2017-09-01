# library(dplyr)
# library(lubridate)
# library(reshape)
# library(ggplot2)

#setwd('J:/data/BA562/final/2_Data')
cls<-read.delim("test_clickstreams.tab",stringsAsFactors = T)

### visit 관련 변수 (d_v,n_v,p_v)
# n_v_website (n_v_website 만든 후, 그 결과 바탕으로 dummy (d_v_website) 생성)
cs.v1.0 <- cls %>%
  group_by(CUS_ID,BACT_NM) %>%
  summarize(freq=n())

cs.v1 <- cs.v1.0 %>%
  cast(CUS_ID~BACT_NM)

name=names(cs.v1)
for(i in 2:length(name)){
  name[i] = paste('n_v_',i-1)
}
names(cs.v1)=name

# d_v_website
CUS_ID <- cs.v1$CUS_ID
n_v_web <- as.matrix(cs.v1 %>% subset(select=-c(CUS_ID)))
get_dum <- function(x){
  unlist(lapply(x, function(i){ifelse(is.na(i)==T,0,1)}))
}
cs.v2 <- data.frame(t(apply(n_v_web, 1, get_dum)))
cs.v2 <- cbind(CUS_ID, cs.v2)
name=names(cs.v2)
for(i in 2:length(name)){
  name[i] = paste('d_v_',i-1)
}
names(cs.v2)=name

# p_v_website
cs.v3 <- cs.v1.0 %>% group_by(CUS_ID) %>%
  mutate(p_v=round(freq/sum(freq)*100,3)) %>% select(CUS_ID,BACT_NM,p_v) %>%
  cast(CUS_ID~BACT_NM,value="p_v")

name=names(cs.v3)
for(i in 2:length(name)){
  name[i] = paste('p_v_',i-1)
}
names(cs.v3)=name

visit_vars <- cs.v2 %>%
  left_join(cs.v1) %>%
  left_join(cs.v3)

write.csv(visit_vars,'test_cs_visit_vars.csv',row.names=FALSE)

### 시간대별 distribution 파악
#cus profile 불러오기
pf <-read.csv('train_profiles.csv',stringsAsFactors = T)

#시간대별 정보
ncls <- cls %>% subset(select=c(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)) %>% mutate(hour=hour(ymd_h(TIME_ID)))
CNT_dist <- ncls %>% group_by(CUS_ID) %>% cast(CUS_ID~hour,sum,value="SITE_CNT")
spending_dist <- ncls %>% group_by(CUS_ID) %>% cast(CUS_ID~hour,sum,value="ST_TIME")
profile_CNT <- pf %>% left_join(CNT_dist)
name=names(profile_CNT)
for(i in 6:length(name)){
  name[i] = paste('h_',name[i],sep="")
}
names(profile_CNT)=name

profile_TIME <- pf %>% left_join(spending_dist)
name=names(profile_TIME)
for(i in 6:length(name)){
  name[i] = paste('h_',name[i],sep="")
}
names(profile_TIME)=name
### ID=1230인 친구 TIME에 na 섞여있어서 처리필요
profile_TIME[is.na(profile_TIME)] <- 0

#남녀 dist
gender_dist_CNT <- profile_CNT %>% 
  group_by(GENDER) %>% summarize_each(funs(sum),select=-c(CUS_ID,AGE,RESIDENCE,GROUP))

gender_dist_CNT <- data.frame(t(gender_dist_CNT))
names(gender_dist_CNT) <- c('남자','여자')
gender_dist_CNT <- gender_dist_CNT[-1,]

gender_dist_TIME <- profile_TIME %>% 
  group_by(GENDER) %>% summarize_each(funs(sum),h_0:h_23)
gender_dist_TIME <- data.frame(t(gender_dist_TIME))
names(gender_dist_TIME) <- c('남자','여자')
gender_dist_TIME <- gender_dist_TIME[-1,]

n_gender = profile_TIME %>% group_by(GENDER) %>% summarize(freq=n())

#연령 dist
age_dist_CNT <- profile_CNT %>% 
  group_by(GROUP) %>% summarize_each(funs(sum),select=-c(CUS_ID,AGE,RESIDENCE,GENDER))
age_dist_CNT <- data.frame(t(age_dist_CNT))
names(age_dist_CNT) <- c('F20-','F30','F40+','M20-','M30','M40+')
age_dist_CNT <- age_dist_CNT[-1,]

age_dist_TIME <- profile_TIME %>% 
  group_by(GROUP) %>% summarize_each(funs(sum),select=-c(CUS_ID,AGE,RESIDENCE,GENDER))
age_dist_TIME <- data.frame(t(age_dist_TIME))
names(age_dist_TIME) <- c('F20-','F30','F40+','M20-','M30','M40+')
age_dist_TIME <- age_dist_TIME[-1,]

n_age = profile_TIME %>% group_by(GROUP) %>% summarize(freq=n())

'
count 기준, 남녀: 9 ~ 17 여자 많음, 18 ~ 8 남자 조금 많음 
time 기준, 남녀: 위랑 동일, 12에서 비슷해짐
count 기준, 나이: 0 ~ 4 20>30>40, 5시에 수렴, 6 ~ 7 40>30>20, 9 ~ 17 30>20>40, 18 ~ 20 30=20>40
time 기준, 나이: 0 ~ 4 20>30>40, 5 ~ 7 40>30>20, 12 30>40>20, 9~11/13~18 30>20=40, 19~21 40>30>20, 22~23 30>40>20

새벽: 0~4, 아침: 5~7, 출근: 8, 오전일과: 9~11, 점심: 12, 오후일과: 13~17, 저녁: 18~21, 밤:22~23  
'