## Preprocessing steps in processing_basic.R

library(readr)
X_train <- read_csv("train.csv")
X_test <- read_csv("test.csv")

source("processing_basic.R")
source("eval_metric.R")
convert_binary_columns()
remove(remove_duplicate_columns)
remove(remove_duplicate_rows)
remove(convert_binary_columns)
remove(remove_redundant_columns)

X_train[is.na(X_train)]   <- 0
X_test[is.na(X_test)]   <- 0

set.seed(21)

library(caTools)
split_labels <- caTools::sample.split(X_train$Purchase, 0.7)
X_train_training <- X_train[split_labels, ]
X_train_cv <- X_train[!split_labels, ]

# write.csv(X_train_training, "train_training.csv")
# write.csv(X_train_cv, "train_crossvalidation.csv")

# X_train_training <- read.csv("train_training.csv")
# X_train_cv <- read.csv("train_crossvalidation.csv")

# base_mod <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status + Product_Category_1, data = X_train_training)

# pred_lm <- predict.lm(base_mod, newdata = X_train_cv)
# eval_metric(X_train_cv$Purchase, pred_lm)

# pred_vals <- predict(base_mod, X_test)
# 
# write.csv(data.frame(User_ID = X_test$User_ID, Product_ID = X_test$Product_ID, Purchase = pred_vals), "base_gbm_mod.csv")
# zip(zipfile = "base_lm_submission.zip", files = c("base_gbm_mod.csv"))


## Prediction with imputation ####################################################
# library(mice)
# 
# train_for_impute <- X_train[9:11]
# imputed_train <- complete(mice(train_for_impute))
# cnames_imputed <- colnames(train_for_impute)
# X_train[cnames_imputed] <- imputed_train
# 
# split_labels <- caTools::sample.split(X_train$Purchase, 0.7)
# X_train_training <- X_train[split_labels, ]
# X_train_cv <- X_train[!split_labels, ]
# 
# base_mod_imputed <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status + Product_Category_1 + Product_Category_2 + Product_Category_3, data = X_train_training)
# 
# pred_lm <- predict.lm(base_mod, newdata = X_train_cv)
# eval_metric(X_train_cv$Purchase, pred_lm)
##################################################################################


## GBM based model
library(caret)
library(gbm)
## Increaing number to 6 resulted in substantial increase in error! (Check out why!?)
fitControl <- trainControl(method = "repeatedcv", number = 8, repeats = 6)
gbmFit1 <- train(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status + Product_Category_1 + Product_Category_2 + Product_Category_3, data = X_train_training, method = "gbm", trControl = fitControl, verbose = T)

gbm_train_pred <- predict(gbmFit1, X_train_cv)
eval_metric(original_y = X_train_cv$Purchase, gbm_train_pred)
pred_vals <- predict(gbmFit1, X_test)

## Obtained RMSE = 3011.821
# eval_metric(original_y = X_train_cv$Purchase, gbm_train_pred)

write.csv(data.frame(User_ID = X_test$User_ID, Product_ID = X_test$Product_ID, Purchase = pred_vals), "allprods_gbm_mod.csv")
zip(zipfile = "allprods_gbm_submission.zip", files = c("allprods_gbm_mod.csv"))