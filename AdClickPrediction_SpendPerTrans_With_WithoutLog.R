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
# there are alot of outliers in the data, hence the data is very imbalance hence checking the distribution

# express.total.spend
# checking the distribution
hist(train_content1$express.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$express.total.spend.unit <- train_content1$express.total.spend/train_content1$express.no.transactions
train_content1$express.total.spend.unit <- ifelse(is.na(train_content1$express.total.spend.unit), 0, train_content1$express.total.spend.unit)
hist(train_content1$express.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$express.total.spend.unit.log <- log(train_content1$express.total.spend.unit)
hist(train_content1$express.total.spend.unit.log)
# -Infinity in the log due to log0
train_content1$express.total.spend.unit.log <- ifelse(train_content1$express.total.spend.unit.log == -Inf, 
                                                      0, train_content1$express.total.spend.unit.log)

# metro.total.spend
# checking the distribution
hist(train_content1$metro.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$metro.total.spend.unit <- train_content1$metro.total.spend/train_content1$metro.no.transactions
train_content1$metro.total.spend.unit <- ifelse(is.na(train_content1$metro.total.spend.unit), 0, train_content1$metro.total.spend.unit)
hist(train_content1$metro.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$metro.total.spend.unit.log <- log(train_content1$metro.total.spend.unit)
hist(train_content1$metro.total.spend.unit.log)
train_content1$metro.total.spend.unit.log <- ifelse(train_content1$metro.total.spend.unit.log == -Inf, 
                                                      0, train_content1$metro.total.spend.unit.log)

# superstore.total.spend
# checking the distribution
hist(train_content1$superstore.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$superstore.total.spend.unit <- train_content1$superstore.total.spend/train_content1$superstore.no.transactions
train_content1$superstore.total.spend.unit <- ifelse(is.na(train_content1$superstore.total.spend.unit), 0, train_content1$superstore.total.spend.unit)
hist(train_content1$superstore.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$superstore.total.spend.unit.log <- log(train_content1$superstore.total.spend.unit)
hist(train_content1$superstore.total.spend.unit.log)
train_content1$superstore.total.spend.unit.log <- ifelse(train_content1$superstore.total.spend.unit.log == -Inf, 
                                                    0, train_content1$superstore.total.spend.unit.log)
# extra.total.spend
# checking the distribution
hist(train_content1$extra.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$extra.total.spend.unit <- train_content1$extra.total.spend/train_content1$extra.no.transactions
train_content1$extra.total.spend.unit <- ifelse(is.na(train_content1$extra.total.spend.unit), 0, train_content1$extra.total.spend.unit)
hist(train_content1$extra.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$extra.total.spend.unit.log <- log(train_content1$extra.total.spend.unit)
hist(train_content1$extra.total.spend.unit.log)
train_content1$extra.total.spend.unit.log <- ifelse(train_content1$extra.total.spend.unit.log == -Inf, 
                                                         0, train_content1$extra.total.spend.unit.log)

# fandf.total.spend
# checking the distribution
hist(train_content1$fandf.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$fandf.total.spend.unit <- train_content1$fandf.total.spend/train_content1$fandf.no.transactions
train_content1$fandf.total.spend.unit <- ifelse(is.na(train_content1$fandf.total.spend.unit), 0, train_content1$fandf.total.spend.unit)
hist(train_content1$fandf.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$fandf.total.spend.unit.log <- log(train_content1$fandf.total.spend.unit)
hist(train_content1$fandf.total.spend.unit.log)
train_content1$fandf.total.spend.unit.log <- ifelse(train_content1$fandf.total.spend.unit.log == -Inf, 
                                                    0, train_content1$fandf.total.spend.unit.log)

# petrol.total.spend
# checking the distribution
hist(train_content1$petrol.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$petrol.total.spend.unit <- train_content1$petrol.total.spend/train_content1$petrol.no.transactions
train_content1$petrol.total.spend.unit <- ifelse(is.na(train_content1$petrol.total.spend.unit), 0, train_content1$petrol.total.spend.unit)
hist(train_content1$petrol.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$petrol.total.spend.unit.log <- log(train_content1$petrol.total.spend.unit)
hist(train_content1$petrol.total.spend.unit.log)
train_content1$petrol.total.spend.unit.log <- ifelse(train_content1$petrol.total.spend.unit.log == -Inf, 
                                                    0, train_content1$petrol.total.spend.unit.log)

# direct.total.spend
# checking the distribution
hist(train_content1$direct.total.spend)
# Clearly the data is positively skewed, adding one column with value as spend per transaction
train_content1$direct.total.spend.unit <- train_content1$direct.total.spend/train_content1$direct.no.transactions
train_content1$direct.total.spend.unit <- ifelse(is.na(train_content1$direct.total.spend.unit), 0, train_content1$direct.total.spend.unit)
hist(train_content1$direct.total.spend.unit)
# Still the distribution is skewed, hence adding a log column to make it normal
train_content1$direct.total.spend.unit.log <- log(train_content1$direct.total.spend.unit)
hist(train_content1$direct.total.spend.unit.log)
train_content1$direct.total.spend.unit.log <- ifelse(train_content1$direct.total.spend.unit.log == -Inf, 
                                                     0, train_content1$direct.total.spend.unit.log)


train_content1$county<- as.character(train_content1$county)

# so far we have removed all the outliers from the spend varibles
# now spliting the dataset into train and validation
set.seed(88)
split <- sample.split(train_content1$content_1, SplitRatio = 0.7)
Train <- subset(train_content1, split == T)
Test <- subset(train_content1, split == F)

# Since the dataset is highly imbalanced we cannot use the rpart with default parameters. We will be doing cost-sensitive learning We cannot use classification error as a performance metric here. We will be using penalty error as our performance metric
# Penalty Error = sum(Confusion Matrix * Penalty Matrix)/(No. of Observations) since count od 0s are 91 times 1s, the penalty factor of 1 should be 91 times the 0

PenaltyMatrix <- matrix(data = c(0,1,91,0),nrow = 2,ncol = 2,byrow = T)


# Applying Gradient Boosting using xgboost with the unit variable added only
# We will be using scale_pos_weight argument to balance the data.
# Along with that we will be passing a custom evaluation metric in the feval argument that calculates the penalty error and minimizes it

###################Using spend per transaction variables################

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit.log
                                     -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                     -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log,
                                     data = Train)
sparse_matrix@Dim
# 32215    27

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit.log
                                          -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                          -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log
                                          , data = Test)
sparse_matrix_test@Dim
# 13807    27
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
#  eta = 0.01
# FALSE TRUE
# 0  8552 5104
# 1    72   79
PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.8442095 eta = 0.01

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01524214

# changing eta = 0.001
params=list(eta=0.001,max_depth=8,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001
# FALSE TRUE
# 0  8958 4698
# 1    77   74

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.8477584 eta = 0.001

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01550712

# changing the max_depth=6
params=list(eta=0.001,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001, max_depth=6
# FALSE TRUE
# 0  8312 5344
# 1    63   88

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.8022742 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01620029

############### Using  penalty matrix with greater penalty
PenaltyMatrix <- matrix(data = c(0,1,136,0),nrow = 2,ncol = 2,byrow = T)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "penalty_error", value = err))
}

# changing the max_depth=4
params=list(eta=0.001,max_depth=4,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 1000,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001, max_depth=4
# FALSE TRUE
# 0  7460 6196
# 1    55   96

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.9905121 eta = 0.001, max_depth=4

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01525747

# changing the max_depth=2
params=list(eta=0.001,max_depth=2,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 1000,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001, max_depth=2
#    FALSE TRUE
# 0  6929 6727
# 1    49  102

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.9698704 eta = 0.001, max_depth=2

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.0149363

# changing the max_depth=1
params=list(eta=0.001,max_depth=1,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 1000,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001, max_depth=1
# FALSE TRUE
# 0  6929 6727
# 1    49  102

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.9698704 eta = 0.001, max_depth=1

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.0149363
#########################Using the county as factor and including in the model
train_content1$county <- as.factor(train_content1$county)
set.seed(88)
split <- sample.split(train_content1$content_1, SplitRatio = 0.7)
Train <- subset(train_content1, split == T)
Test <- subset(train_content1, split == F)

PenaltyMatrix <- matrix(c(0, 1, 91, 0), nrow = 2, ncol = 2, byrow = T)

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-express.total.spend.unit.log
                                     -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                     -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log,
                                     data = Train)
sparse_matrix@Dim
# 32215    116

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-express.total.spend.unit.log
                                          -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                          -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log
                                          , data = Test)
sparse_matrix_test@Dim
# 13807    116
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

params=list(eta=0.001,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 1000,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE eta = 0.001, max_depth=6
# FALSE TRUE
# 0  8312 5344
# 1    64   87

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.8088651 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01601915
# Adding county did not increase the precision

############### Using log of spend per transaction#################

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit
                                     -metro.total.spend.unit-superstore.total.spend.unit-extra.total.spend.unit
                                     -fandf.total.spend.unit-petrol.total.spend.unit-direct.total.spend.unit,
                                     data = Train)
sparse_matrix@Dim
# 32215    27

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit
                                          -metro.total.spend.unit-superstore.total.spend.unit-extra.total.spend.unit
                                          -fandf.total.spend.unit-petrol.total.spend.unit-direct.total.spend.unit, 
                                          data = Test)
sparse_matrix_test@Dim
# 13807    27
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
# 0  7527 6129
# 1    65   86
PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.8723111 eta = 0.01

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01383749

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
# 0  7967 5689
# 1    70   81

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
# 0.8733976 eta = 0.001

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01403813

# changing the max_depth=6
params=list(eta=0.001,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 1000,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# eta = 0.001, max_depth=6
# FALSE TRUE
# 0  7816 5840
# 1    59   92

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.8118346 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.0155091

###########################taking all the values 
sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county, data = Train)
sparse_matrix@Dim
# 32215    34

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county, data = Test)
sparse_matrix_test@Dim
# 13807    34
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
params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# eta = 0.001, max_depth=6
# FALSE TRUE
# 0  8312 5344
# 1    63   88

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.8022742 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01620029

########################
PrecisionMatrix <- matrix(data = c(1, 0, 0, 91), ncol = 2, nrow = 2, byrow = T)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PrecisionMatrix)/nrow(dtrain)
  return(list(metric = "precision", value = err))
}
watchlist <- list(train = dtrain, test = dtest)
params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=T,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8477 5179
# 1    63   88

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7903237 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.0167078

################################# converting the affluency value into numeric
levels(train_content1$affluency) <- as.integer(ifelse(train_content1$affluency =="Very High", 5, 
                                                      ifelse(train_content1$affluency =="High", 4, 
                                                             ifelse(train_content1$affluency =="Mid", 3, 
                                                                    ifelse(train_content1$affluency =="Low", 2, 1)))))

set.seed(88)
split <- sample.split(train_content1$content_1, SplitRatio = 0.7)
Train <- subset(train_content1, split == T)
Test <- subset(train_content1, split == F)

PrecisionMatrix <- matrix(data = c(1, 0, 0, 91), ncol = 2, nrow = 2, byrow = T)

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-express.total.spend.unit.log
                                     -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                     -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log,
                                     data = Train)
sparse_matrix@Dim
# 32215    116

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-express.total.spend.unit.log
                                          -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                          -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log
                                          , data = Test)
sparse_matrix_test@Dim
# 13807    116
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = Test$content_1)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PrecisionMatrix)/nrow(dtrain)
  return(list(metric = "precision", value = err))
}
watchlist <- list(train = dtrain, test = dtest)
params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=T,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8310 5346
# 1    64   87

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01601325

################changing precision matrix
PrecisionMatrix <- matrix(data = c(1, 1, 1, 1), ncol = 2, nrow = 2, byrow = T)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum((B*PrecisionMatrix)[2,2])/sum(B*PrecisionMatrix[,2])
  return(list(metric = "precision", value = err))
}

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit.log
                                     -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                     -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log,
                                     data = Train)
sparse_matrix@Dim
# 32215    27

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county-express.total.spend.unit.log
                                          -metro.total.spend.unit.log-superstore.total.spend.unit.log-extra.total.spend.unit.log
                                          -fandf.total.spend.unit.log-petrol.total.spend.unit.log-direct.total.spend.unit.log
                                          , data = Test)
sparse_matrix_test@Dim
# 13807    27
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = Test$content_1)
watchlist <- list(train = dtrain, test = dtest)
params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=T,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8072 5584
# 1    60   91

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7998841 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01603524

########################using the log spedn per unit
sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-county, data = Train)
sparse_matrix@Dim
# 32215    34

dtrain <- xgb.DMatrix(data = sparse_matrix, label = Train$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-county, data = Test)
sparse_matrix_test@Dim
# 13807    34
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = Test$content_1)
watchlist <- list(train = dtrain, test = dtest)
params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=T,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8072 5584
# 1    60   91

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7998841 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01603524
###############
PrecisionMatrix <- matrix(data = c(1, 0, 0, 182), ncol = 2, nrow = 2, byrow = T)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PrecisionMatrix)/nrow(dtrain)
  return(list(metric = "precision", value = err))
}

params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=T,early_stopping_rounds = 100,
                       scale_pos_weight=91,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(Test$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8072 5584
# 1    60   91

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7998841 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01603524

############################ Spliting the male and female data
train_content1_M <- train_content1[train_content1$gender == 'Male', ]
table(train_content1_M$content_1)
# 0     1 
# 23046   146

train_content1_M$county <- as.factor(train_content1_M$county)
set.seed(88)
split <- sample.split(train_content1_M$content_1, SplitRatio = 0.7)
TrainM <- subset(train_content1_M, split == T)
TestM <- subset(train_content1_M, split == F)

PenaltyMatrix <- matrix(data = c(0,1,158,0), nrow=2, ncol = 2, byrow = T)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "precision", value = err))
}

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-gender, data = TrainM)
sparse_matrix@Dim
dtrain <- xgb.DMatrix(data = sparse_matrix, label = TrainM$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-gender, data = TestM)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = TestM$content_1)

watchlist <-  list(train = dtrain, test = dtest)

params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=158,feval=evalerror)
imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(TestM$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8072 5584
# 1    60   91

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7998841 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01603524

####################

train_content1_F <- train_content1[train_content1$gender == 'Female', ]
table(train_content1_F$content_1)
# 0     1 
# 22474   356

train_content1_F$county <- as.factor(train_content1_F$county)
set.seed(88)
split <- sample.split(train_content1_M$content_1, SplitRatio = 0.7)
TrainF <- subset(train_content1_M, split == T)
TestF <- subset(train_content1_M, split == F)

PenaltyMatrix <- matrix(data = c(0,1,64,0), nrow=2, ncol = 2, byrow = T)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  B<-matrix(data = rep(0,4),nrow = 2,ncol = 2)
  A<-as.matrix(table(labels,preds>=0.5))
  B[1:nrow(A),1:ncol(A)]<-A
  err<-sum(B*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "precision", value = err))
}

sparse_matrix <- sparse.model.matrix(content_1~.-1-customer.id-gender, data = TrainF)
sparse_matrix@Dim
dtrain <- xgb.DMatrix(data = sparse_matrix, label = TrainF$content_1)

sparse_matrix_test <- sparse.model.matrix(content_1~.-1-customer.id-gender, data = TestF)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = TestF$content_1)

watchlist <-  list(train = dtrain, test = dtest)

params=list(eta=0.01,max_depth=6,objective="binary:logistic")

model_xgb <- xgb.train(params = params,data = dtrain,verbose = 0,watchlist = watchlist,
                       nrounds=10000,nthread=8,maximize=F,early_stopping_rounds = 100,
                       scale_pos_weight=64,feval=evalerror)

imp <-xgb.importance(feature_names = colnames(sparse_matrix),model = model_xgb)

xgb.plot.importance(imp)

pred_xgb<-predict(model_xgb,newdata = dtest)
conf_xgb <- as.matrix(table(TestF$content_1,pred_xgb>=0.5))
# FALSE TRUE
# 0  8072 5584
# 1    60   91

PenaltyError_xgb <- sum(conf_xgb*PenaltyMatrix)/nrow(Test)
#  0.7998841 eta = 0.001, max_depth=6

# Precision
conf_xgb[2,2]/sum(conf_xgb[,2]) # 0.01603524


