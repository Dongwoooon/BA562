library(KoNLP)
useNIADic()
library(tm)
library(dplyr)

setwd('J:/data/BA562/final/2_Data')
sample1<-read.csv("train_keyword_cleaned.csv",stringsAsFactors = F)
sample2<-read.csv("test_keyword_cleaned.csv",stringsAsFactors = F)
sample<-rbind(sample1,sample2)

sample$kor = gsub("[^가-힣]"," ",sample$QRY_clean)

cut<-unlist(strsplit(sample$kor,' '))
cut<-cut[!cut=='']
freq<-as.data.frame(table(cut))
freq$cut<-sapply(freq$cut,as.character)
dict<-freq[freq$Freq>=140 & nchar(freq$cut)>1,]
dict<-dict[order(-dict[2]),]

write.csv(dict,'keyword_dict.csv',row.names = F)
