rm(list=ls())

library(readr)
train <- read_csv("train.csv")
test  <- read_csv("test.csv")

train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

feature.names <- names(train)[3:ncol(train)-1]

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

tra <- train[,feature.names]
test <- test[,feature.names]

RMSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds-elab)^2))
  return(list(metric = "RMSE", value = err))
}

library(xgboost)
set.seed(21)

h<-sample(nrow(train),50000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Purchase+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Purchase+1)[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                eta                 = 0.1, # 0.06, #0.01,
                max_depth           = 10, 
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 600, 
                    verbose             = 1,
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMSE
)

pred1 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

h<-sample(nrow(train),50000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Purchase+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Purchase+1)[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                #booster = "gblinear",
                eta                 = 0.1, 
                max_depth           = 8, 
                subsample           = 0.7, 
                colsample_bytree    = 0.7 
                
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 700, 
                    verbose             = 1,
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMSE
)
pred2 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

h<-sample(nrow(train),50000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Purchase+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Purchase+1)[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                eta                 = 0.1, # 0.06, #0.01,
                max_depth           = 9, 
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7 # 0.7
                
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 800, 
                    verbose             = 1,
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMSE
)

pred3 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

preds<-(pred1+pred2 + pred3)/3
test  <- read_csv("test.csv")
submission <- data.frame(User_ID=test$User_ID, Product_ID = test$Product_ID, Purchase=preds)
submission_adjust <- submission$Purchase
write_csv(submission, "XgbModif.csv")
zip(zipfile = "Xgb_submission.zip", files = c("XgbModif.csv"))
