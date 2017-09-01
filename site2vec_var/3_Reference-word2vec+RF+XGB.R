#########################################################################
# PA Challenge: 
# This predictive model uses only word2vec without feature engineering
#########################################################################

##### Required packages

# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")

###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
setwd('J:/data/BA562/final/2_Data')

# train data
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)

###### Make Corpus (sites sentences)

f <- function(x, min) {
# Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
# Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
# Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Build site2vec model
set.seed(12345)
model = train_word2vec("items.txt","vec.bin",vectors=300,threads=4,window=5,cbow=1,negative_samples=10,iter=5,force = T)

" 
(word2vec performance) The main choices to make are:
- architecture: skip-gram (slower, better for infrequent words) vs CBOW (fast)
- the training algorithm: hierarchical softmax (better for infrequent words) vs negative sampling (better for frequent words, better with low dimensional vectors)
- sub-sampling of frequent words: can improve both accuracy and speed for large data sets (useful values are in range 1e-3 to 1e-5)
- dimensionality of the word vectors: usually more is better, but not always
- context (window) size: for skip-gram usually around 10, for CBOW around 5
"

##### Explore the model
model <- read.binary.vectors("vec.bin") # reload the pre-trained word2vec model 
for (v in unique(cs.dt[,GROUP])) print(closest_to(model, as.character(v), n=10))
model[[as.character(unique(md.dt[,GROUP])), average=F]] %>% plot(method="pca")
items.1 <- c(unique(md.dt[,GROUP]), unique(md.dt[CUS_ID==1, ACT_NM]))
model[[items.1[1:10], average=F]] %>% plot(method="pca")
cosineSimilarity(model[[unique(md.dt[CUS_ID==1, ACT_NM]), average=T]], model[[as.character(unique(cs.dt[,GROUP])), average=F]])
cosineSimilarity(model[[unique(md.dt[CUS_ID==100, ACT_NM]), average=T]], model[[as.character(unique(cs.dt[,GROUP])), average=F]])


##### Prediction using site2vec + classifaction methods

### Make features (mean vector)
# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, ACT_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
write.csv(train, "cs_site2vec300_train.csv", row.names = F)

# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))
write.csv(test, "cs_site2vec300_test.csv", row.names = F)

### RF Modeling using caret
control <- trainControl(method="repeatedcv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)
tunegrid <- expand.grid(mtry=c(40,60,80,100))
set.seed(123)
rf_model <- caret::train(GROUP ~ .,
                         data = subset(train, select=-CUS_ID),
                         method = "rf",
                         preProcess = NULL,
                         ntree = 200,
                         tuneGrid = tunegrid,
                         metric = "logLoss",
                         trControl = control)
print(rf_model$finalModel)
print(rf_model$results)  # logloss=1.362869 (SD=0.010160948) when ntree=200, mtry=80
plot(rf_model, main="random Forest model")

# Tuned RF Prediction
predicted <- predict(rf_model, test, type="prob")
write.csv(cbind(CUS_ID=test.CUS_ID,predicted), "submission_rf.csv", row.names = F)

### XGBoost Modeling using caret
control <- trainControl(method="repeatedcv", number=5, repeats=1, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)
xgb_model <- caret::train(GROUP ~ .,
                          data = subset(train, select=-CUS_ID),
                          method = "xgbTree",
                          preProcess = NULL,
                          metric = "logLoss",
                          trControl = control)
print(xgb_model$finalModel)
print(xgb_model$results)  # logloss=1.332989 (SD=0.01720859)
plot(xgb_model, main="XGBoost model")

# Tuned XGBoost Prediction
predicted <- predict(xgb_model, test, type="prob")
write.csv(cbind(CUS_ID=test.CUS_ID,predicted), "submission_xgb.csv", row.names = F)