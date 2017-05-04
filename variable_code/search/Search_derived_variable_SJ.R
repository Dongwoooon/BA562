# PAC derived variable
# Search Keyword
# Date: 2017.04.30
# http://blog.naver.com/hss2864/220980568640
# ------------------------------------------------------------------
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("reshape")
install.packages("KoNLP")      ##rjava ¼³Ä¡ÇØ¾ßÇÔ
install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")
install.packages("data.table")

library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)
library(KoNLP)
library(data.table)       ### ÀÌ°Ô ÈÎ¾À ºü¸§ dpylrº¸´Ù 20¹è // Å« µ¥ÀÌÅÍ ´Ù·ê¶§´Â ÀÌ°Ô ´õ ÁÁÀ½
library(wordVectors)

rm(list=ls())

setwd("D:\\ºñÁî´Ï½º ¸ğµ¨¸µ\\Predictive Analytics Challenge\\Challenge_170430")



#### test, train keyword cleaned by DW

train <- read.csv("train_keyword_cleaned.csv",stringsAsFactors=F)
test <- read.csv("test_keyword_cleaned.csv",stringsAsFactors=F)


#### Subtracting other characters
train$kor = gsub(pattern="[^°¡-ÆR]",replacement=" ",train$QRY_clean)
test$kor = gsub(pattern="[^°¡-ÆR]",replacement=" ",test$QRY_clean)

#  ^[[:digit:]]$ : digits ///  [^A-z] : english


#### Dictionary constructing / #### µ¿¿îÀÌ°¡ dictionary ¸¸µé±â

dic <- read.csv("sample_dict.csv")

dic <- dic %>%
  select(-count)

names(dic)<-c("QRY")



#### train, test data keyword spliting
cutx <- function(x) {
  cut<-unlist(strsplit(x,' '))
  cut <- cut[!cut=='']
}
train$cut<-sapply(train$kor,cutx)
test$cut<-sapply(test$kor,cutx)

## ÀÌ»ó ¾ÆÀÌµğ Á¦°Å
train <- train %>%
  select(CUS_ID, QRY_CNT, cut) %>%
  arrange(CUS_ID) %>%
  filter(is.na(QRY_CNT)==FALSE)

test <- test %>%
  select(CUS_ID, QRY_CNT, cut) %>%
  arrange(CUS_ID) %>%
  filter(is.na(QRY_CNT)==FALSE)

## ¿Ü±¹¾î vector Âî²¨±â Á¦°Å
no0<-function(x){
  if(identical(unlist(x),character(0))==T){
    x=1
  }
  else{
    x=0
  }
}
train$cut_dum<-mapply(no0,train$cut)
test$cut_dum<-mapply(no0,test$cut)

train <- train %>%
  filter(cut_dum==0) %>%
  select(-cut_dum)

test <- test %>%
  filter(cut_dum==0) %>%
  select(-cut_dum)


#### train, test data --> structure ¹Ù²Ù´Â ÇÔ¼ö (1row 1keyword)
g <- function(x){
  CUS_ID=x[1]
  cut = rep(unlist(x[3]), x[2])
  dt = as.matrix(data.frame(CUS_ID=CUS_ID, QRY=cut))
  return(dt)
}

train.mat <- as.matrix(train,nrow=nrow(train),ncol=ncol(train))
test.mat <- as.matrix(test,nrow=nrow(test),ncol=ncol(test))

train.key <- apply(train.mat,1,g)
test.key <- apply(test.mat,1,g)

train.str <- NULL
test.str <- NULL

## 10450 sec
system.time(
for (i in 1:nrow(train)){
  train.str <- rbind(train.str,train.key[[i]])
}
## 27485 sec
)
system.time(
for (i in 1:nrow(test)){
  test.str <- rbind(test.str,test.key[[i]])
}
)


train.str <- as.data.frame(train.str,row.names = F)
test.str <- as.data.frame(test.str, row.names = F)

## write.csv(train.str,"train_keyword_seperated.csv",row.names = F)
## write.csv(test.str,"test_keyword_seperated.csv",row.names = F)

## save(train.str,file="train_str.Rdata")
## save(test.str,file="test_str.Rdata")




#### str, dic data join
load("train_str.Rdata")     # train.str
load("test_str.Rdata")      # test.str

tr.train <- merge(train.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   ## °Ë»ö¾î Áß dictionary¿¡ ÀÖ´Â ´Ü¾î¿¡ ´ëÇÑ °üÃøÄ¡¸¸ ³²±ä °Í
tr.test <- merge(test.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   
tr.train$CUS_ID <- as.numeric(tr.train$CUS_ID)
tr.test$CUS_ID <- as.numeric(tr.test$CUS_ID)



#### Convert data.frame to data.table for fast computing
#### cs data
cs <- read.csv("cs_merge.csv",stringsAsFactors=F) %>%
  select(CUS_ID, GENDER, AGE, GROUP)

cs.dt <- data.table(cs, key="CUS_ID")       ### key¸¦ ÁöÁ¤ÇØÁÜ / key¸¦ ÅëÇÑ ¿¬»êÀ» ºü¸£°Ô °¡´É 
tr.dt <- data.table(tr, key="CUS_ID")
md.dt <- merge(cs.dt, tr.dt)

md.dt$QRY <- as.character(md.dt$QRY)
md.dt$GROUP <- as.character(md.dt$GROUP)
######### search keyword¸¦ Àß grouping ÇØÁÖ¾î¼­ ºĞ¼®ÇØ¾ßÇÔ. ³ë°¡´Ù·Î 
# Make sites sentences
f <- function(x) {
  grp <- md.dt[CUS_ID==x, GROUP][1]               ###cus_id=x ÀÎ ¾ÖÀÇ gender
  act <- unique(md.dt[CUS_ID==x, QRY])       ###1³âÄ¡ÀÇ ´ëºĞ·ù ¼öÁØÀÇ °Ë»ö¾î¸¦ »Ì¾Æ¶ó 
  as.vector((sapply(1:20, function(x) c(grp, sample(act, length(act))))))    ### act¸¦ 20¹ø »½Æ¢±â¸¦ ÇÑ ÈÄ¿¡ ¸Ç ¾Õ¿¡ ¼ºº° Á¤º¸¸¦ ºÙÀÌ´Â °úÁ¤ . µ¥ÀÌÅÍ°¡ ÀÛÀ¸´Ï±î 20¹ø »½Æ¢±âÇÏ´Â°Å¾ß (random order) / ÃÑ 4000sentence
}
### ÀÌ°Ô ¿ø·¡´Â ¼ø¼­°¡ °í·ÁµÇ´Â°ÅÀÓ. µû¶ó¼­ µ¥ÀÌÅÍ°¡ Å©´Ù¸é samplingÇÏÁö ¸»°í ¼ø¼­ °í·ÁÇØ¼­ ÇÏ¸é °Ë»ö ¼ø¼­µµ °í·ÁµÅ¼­ ¹İ¿µ

items <- unlist(sapply(unique(md.dt[,CUS_ID]), f))     ###unique(md.dt[,cus_id]) cus_id ¿­¸¸ »ÌÀ¸¶ó´Â ¼Ò¸®ÀÓ // °í°´¸¶´Ù Á¢¼Ó »çÀÌÆ® ¼ö ´Ù¸£´Ï±î unlistÇØ¼­ vector·Î ¸¸µé¾úÀ½  // ÀÌ°Å dpylrÇÏ¸é 20¹è ÀÌ»ó °É¸² 
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)    ### ¾ê´Â º»·¡ Å« µ¥ÀÌÅÍ·Î ¾²±â ¶§¹®¿¡ ±âº» °¡Á¤ÀÌ ÇÏµåµğ½ºÅ©¿¡ ÀúÀåµÈ ÆÄÀÏ ºÒ·¯¿Í¼­ »ç¿ëÇÒ°ÅÀÓ // eol: end of lineÀ» ±¸ºĞÇÏ´Â °Í ¾øÀÌ Âß ÀÌ¾îÁö°Ô 

# Train site2vec model
model = train_word2vec("items.txt","vec.bin",vectors=100,threads=4,window=10,iter=5,negative_samples=0, force = T)
##########################################   Â÷¿ø ¼ö     / ³» ÄÄÇ»ÅÍÀÇ core¼ö / window´Â Àü ÈÄ ¸î°³ ÇÒÁö / negative sampleÀº default·Î ÇÏ±â (5ÀÓ), µ¥ÀÌÅÍ ÀûÀ» °æ¿ì 5 ÀÌ»óÀ¸·Î µ¹·Á¾ßÇÔ)

# Explore the model
for (v in unique(md.dt[,GROUP])) print(closest_to(model, v, n=10))    #######³²ÀÚ¿¡ °¡±î¿î º¤ÅÍ, ¿©ÀÚ¿¡ °¡±î¿î º¤ÅÍ ³ª¿È 10°³¾¿ 
model[[unique(md.dt[,GROUP]), average=F]] %>% plot(method="pca")     ####³²ÀÚ, ¿©ÀÚ À§Ä¡¸¦ ±×·¡ÇÁ¿¡ Ç¥½Ã 

######### µ¹·Áº¸¸é ³²ÀÚµéÀÌ ÀÚÁÖ Ã£´Â »çÀÌÆ®µé, ¿©ÀÚµéÀÌ ÀÚÁÖ Ã£´Â »çÀÌÆ®µéÀÌ ºñ½ÁÇÑ ¹æÇâÀ¸·Î ÇØ¼­ º¤ÅÍ ³ª¿È. 
a<-unique(md.dt$GROUP)
cosineSimilarity(model[[unique(md.dt[CUS_ID==14, QRY]), average=T]], model[[a, average=F]])  
cosineSimilarity(model[[unique(md.dt[CUS_ID==16, QRY]), average=T]], model[[a, average=F]])  

### ÀÌ°É·Î ÇØ´ç id Áı¾î³ÖÀ¸¸é ³²ÀÚ º¤ÅÍÂÊ¿¡ °¡±î¿îÁö ¿©ÀÚ º¤ÅÍÂÊ¿¡ °¡±î¿îÁö ³ª¿È. / ´Ù¸¸ ¿Ïº®ÇÏ°Ô ¿¹ÃøÀº ¸øÇÏ´Ï Æ©´×À»ÇÏ°í Âü°í¸¦ ÇØ¼­ ÇÕÃÄ¾ßÁö 
### ³²ÀÚÀÏ È®·ü, ¿©ÀÚÀÏ È®·ü ³ª¿È 


#### Challenge ÇÒ ¶§ Âü°í 
unique(md.dt[cus_id==24, category3])   #### ÀÌ°Ô 24¹øÀÌ ÃÑ °Ë»öÇÑ ´Ü¾î ´Ù ÀÖ´Â°Å. ÀÌ°Å¸¦ ´Ù ¾²Áö ¸»°í ÀÌ°Å¸¦ µé¿©´Ùº¸°í Àû´çÈ÷ Ãß·Á¼­ °¡°øÇØ¼­ »ç¿ëÇÏ¸é ´õ Àß ³ª¿À°ÚÁö 







