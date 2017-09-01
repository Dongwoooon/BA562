#########################################################################
# Creating ensembles from submission files: Geometric averaging
# For more details, refer to https://mlwave.com/kaggle-ensembling-guide/
#########################################################################

#한 폴더에 CUS_ID와 각 그룹 별 예측 확률 만든 prediction.csv 파일들( 총 7 col)을 모아놓고 그 해당 폴더주소로 setwd() 설정하면 끝 

### Merge your submission files
nf <- 0
for (f in list.files(pattern="*.csv")) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F30+", "M20-", "M30", "M30+")
write.csv(pred, "submission_ensemble.csv", row.names = F)