#
# Code sample: multi-class logarithmic loss (logloss)
# Create a directory named "Submission" within the current working directory before running this code
#

test.public <- read.csv("test_public.csv", stringsAsFactors = F)
actual <- as.matrix(test.public[,-1])

logLoss <- function(actual, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted,1-eps),eps)
  return(-1*sum(actual * log(predicted)) / nrow(actual))
}

eval <- data.frame(model=NULL,logloss=NULL)
path <- paste(getwd(), "/Submission", sep="")
for (f in list.files(path=path, pattern="*.csv")) {
  pred <- read.csv(paste(path, "/", f, sep=""))
  pred <- subset(pred, CUS_ID %in% test.public$CUS_ID)
  pred <- as.matrix(pred[,-1])
  pred <- pred / rowSums(pred) # required to sum to one
  acc <- logLoss(actual, pred)
  names(acc) <- NULL
  eval <- rbind(eval, data.frame(model=gsub(".csv","",f),logloss=acc))
}

eval <- eval[order(eval$logloss),]
eval$rank <- order(eval$logloss)
print(eval)
write.csv(eval, "eval.csv", row.names = F)