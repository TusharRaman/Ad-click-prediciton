

##########################################
# Loading the libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(xgboost)
library(Matrix)
library(randomForest)
#library(MLmetrics)
train <- read.csv("content_train.tsv", sep = '\t', header = T)
# checking the structure of the dataset
str(train)
# Cleaning the data
summary(train)
# Starting with content_1
train_content1 <- train[!is.na(train$content_1), -(3:10)]
summary(train_content1)
# Analysisnf the depencent variable
table(train_content1$content_1)
# 0     1 
# 45520   502 
# there are alot of out liers in the data, hence the data is very imbalance
# removing the outliers using the box plot technique

# express.total.spend
boxplot(train_content1$express.total.spend)
outlier <- boxplot.stats(train_content1$express.total.spend)$out
min(train_content1$express.total.spend)
train_content1$express.total.spend[train_content1$express.total.spend >= min(outlier)] <- min(outlier)

boxplot(train_content1$express.total.spend)
# checking the distribution
hist(train_content1$express.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$express.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$express.total.spend.log <- log(train_content1$express.total.spend)
hist(train_content1$express.total.spend.log)

# metro.total.spend
boxplot(train_content1$metro.total.spend)
outlier <- boxplot.stats(train_content1$metro.total.spend)$out
min(train_content1$metro.total.spend)
train_content1$metro.total.spend[train_content1$metro.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$metro.total.spend)
# checking the distribution
hist(train_content1$metro.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$metro.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$metro.total.spend.log <- log(train_content1$metro.total.spend)
hist(train_content1$metro.total.spend.log)

# superstore.total.spend
boxplot(train_content1$superstore.total.spend)
outlier <- boxplot.stats(train_content1$superstore.total.spend)$out
min(train_content1$superstore.total.spend)
train_content1$superstore.total.spend[train_content1$superstore.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$superstore.total.spend)
# checking the distribution
hist(train_content1$superstore.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$superstore.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$superstore.total.spend.log <- log(train_content1$superstore.total.spend)
hist(train_content1$superstore.total.spend.log)

# extra.total.spend
boxplot(train_content1$extra.total.spend)
outlier <- boxplot.stats(train_content1$extra.total.spend)$out
min(train_content1$extra.total.spend)
train_content1$extra.total.spend[train_content1$extra.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$extra.total.spend)
# checking the distribution
hist(train_content1$extra.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$extra.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$extra.total.spend.log <- log(train_content1$extra.total.spend)
hist(train_content1$extra.total.spend.log)

# fandf.total.spend
boxplot(train_content1$fandf.total.spend)
outlier <- boxplot.stats(train_content1$fandf.total.spend)$out
min(train_content1$fandf.total.spend)
train_content1$fandf.total.spend[train_content1$fandf.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$fandf.total.spend)
# checking the distribution
hist(train_content1$fandf.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$fandf.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$fandf.total.spend.log <- log(train_content1$fandf.total.spend)
hist(train_content1$fandf.total.spend.log)

# petrol.total.spend
boxplot(train_content1$petrol.total.spend)
outlier <- boxplot.stats(train_content1$petrol.total.spend)$out
min(train_content1$petrol.total.spend)
train_content1$petrol.total.spend[train_content1$petrol.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$petrol.total.spend)
# checking the distribution
hist(train_content1$petrol.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$petrol.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$petrol.total.spend.log <- log(train_content1$petrol.total.spend)
hist(train_content1$petrol.total.spend.log)

# direct.total.spend
boxplot(train_content1$direct.total.spend)
outlier <- boxplot.stats(train_content1$direct.total.spend)$out
min(train_content1$direct.total.spend)
train_content1$direct.total.spend[train_content1$direct.total.spend >= min(outlier)] <- min(outlier)
boxplot(train_content1$direct.total.spend)
# checking the distribution
hist(train_content1$direct.total.spend)
# Clearly the data is positively skewed, chiking the distribution using log
hist(log(train_content1$direct.total.spend))
# using log the distribution became near normal, adding a variable with log values
train_content1$direct.total.spend.log <- log(train_content1$direct.total.spend)
hist(train_content1$direct.total.spend.log)

# so far we have removed all the outliers from the spend varibles
# now spliting the dataset into train and validation
set.seed(88)
split <- sample.split(train_content1$content_1, SplitRatio = 0.7)
Train <- subset(train_content1, split == T)
Test <- subset(train_content1, split == F)

# Since the dataset is highly imbalanced we cannot use the rpart with default parameters. We will be doing cost-sensitive learning We cannot use classification error as a performance metric here. We will be using penalty error as our performance metric
# Penalty Error = sum(Confusion Matrix * Penalty Matrix)/(No. of Observations) since count od 0s are 91 times 1s, the penalty factor of 1 should be 91 times the 0

PenaltyMatrix <- matrix(data = c(0,1,91,0),nrow = 2,ncol = 2,byrow = T)
train_content1$county<- as.character(train_content1$county)

# Applying Gradient Boosting using xgboost
# We will be using scale_pos_weight argument to balance the data.
# Along with that we will be passing a custom evaluation metric in the feval argument that calculates the penalty error and minimizes it

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend
                                     -metro.total.spend-superstore.total.spend-extra.total.spend
                                     -fandf.total.spend-petrol.total.spend-direct.total.spend,
                                     data = Train)
sparse_matrix@Dim
# 32215    20

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend
                                          -metro.total.spend-superstore.total.spend-extra.total.spend
                                          -fandf.total.spend-petrol.total.spend-direct.total.spend, data = Test)
sparse_matrix_test@Dim
# 13807    20
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = Test$content_1)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "penalty_error", value = err))
}

watchlist <- list(train = dtrain, test = dtest)

params=list(eta=0.01,max_depth=8,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  9187 4469
# 1    82   69

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.8641269

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01520494

# changing eta = 0.001
params=list(eta=0.001,max_depth=8,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# eta = 0.001
# FALSE TRUE
# 0  8634 5022
# 1    83   68

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.9107699 eta = 0.001

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01335953

# changing the max_depth=6
params=list(eta=0.001,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# eta = 0.001, max_depth=6
# FALSE TRUE
# 0  6905 6751
# 1    55   96

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.8514522 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01402074


