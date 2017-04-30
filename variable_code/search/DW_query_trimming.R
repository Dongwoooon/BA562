library(KoNLP)
useNIADic()

setwd('J:/data/BA562/final/2_Data')
kwd<-read.delim("train_searchkeywords.tab",stringsAsFactors = F)

#sample
ksam<-kwd[sample(nrow(kwd),200),]

trimming <- function(x){
  if(grepl('acq',x)==T){
    acq<-unlist(strsplit(x,'&'))
    return(acq[1])
  }
  else if(grepl('query',x)==T){
    qry<-unlist(strsplit(x,'query='))
    return(qry[length(qry)])
  }
  else{
    return(x)
  }
}

ksam$QRY_clean <- mapply(trimming,ksam$QRY_STR)
ksam$QRY_clean <- gsub('\\+', ' ', ksam$QRY_clean)
write.csv(ksam,'keywordsample.csv',row.names = F)

#진짜
kwd$QRY_clean <- mapply(trimming,kwd$QRY_STR)
kwd$QRY_clean <- gsub('\\+', ' ', kwd$QRY_clean)
write.csv(kwd,'train_keyword_cleaned.csv',row.names = F)
