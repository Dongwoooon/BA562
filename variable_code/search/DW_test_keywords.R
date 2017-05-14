library(dplyr)
library(data.table)
library(wordVectors)

setwd('J:/data/BA562/final/2_Data')
dic = read.csv('keyword_dict.csv',stringsAsFactors=F)
load("test_str.Rdata")      # test.str

tr.test <- merge(test.str,dic,by="QRY",all.x=FALSE, all.y=TRUE)   
tr.test$CUS_ID <- as.numeric(as.character(tr.test$CUS_ID))

tr.dt <- data.table(tr.test, key="CUS_ID")

######### search keyword를 잘 grouping 해주어서 분석해야함. 노가다로 
# Make sentences

f <- function(x) {
  act <- unique(tr.dt[CUS_ID==x, QRY])       ### keyword를 뽑아라 
  c(x,as.vector(act))
}

### 이게 원래는 순서가 고려되는거임. 따라서 데이터가 크다면 sampling하지 말고 순서 고려해서 하면 검색 순서도 고려돼서 반영
items_grp <- sapply(unique(tr.dt[,CUS_ID]), f)
kwd <- as.vector(sapply(items_grp, paste, collapse = " "))
kwds <- as.matrix(sapply(kwd,rbind))
write.csv(kwds,'keyword_test.csv',row.names = F)
