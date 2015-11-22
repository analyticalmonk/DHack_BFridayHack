#importing libraries
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
import xgboost as xgb
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error
from sklearn import preprocessing
from sklearn import ensemble

# setting the input path and reading the data into dataframe #
data_path = "./"
train = pd.read_csv(data_path+"Train.csv")
test = pd.read_csv(data_path+"Test.csv")

## categical column name list ##
categorical_columns = ["Product_ID","Gender","Age","Occupation","City_Category","Stay_In_Current_City_Years","Marital_Status","Product_Category_1","Product_Category_2","Product_Category_3"]

## Getting the ID and DV from the data frame ##
train_y = np.array(train["Purchase"])

## Creating the IDVs from the train and test dataframe ##
train_X = train.copy()
test_X = test.copy()

## Fill up the na values with -999 ##
train_X = train_X.fillna(-999)
test_X = test_X.fillna(-999)

#encoding categorical variable
for var in categorical_columns:
    lb = preprocessing.LabelEncoder()
    full_var_data = pd.concat((train_X[var],test_X[var]),axis=0).astype('str')
    lb.fit( full_var_data )
    train_X[var] = lb.transform(train_X[var].astype('str'))
    test_X[var] = lb.transform(test_X[var].astype('str'))

## Dropping the unnecessary columns from IDVs ##
train_X = np.array( train_X.drop(['Purchase'],axis=1) )
print "Train shape is : ",train_X.shape
print "Test shape is : ",test_X.shape

print "Building XGB1"
params = {}
params["objective"] = "reg:linear"
params["eta"] = 0.1
params["min_child_weight"] = 10
params["subsample"] = 0.7
params["colsample_bytree"] = 0.7
params["scale_pos_weight"] = 0.8
params["silent"] = 1
params["max_depth"] = 8
#params["max_delta_step"]=2
params["seed"] = 0
plst = list(params.items())
xgtrain = xgb.DMatrix(train_X, label=train_y)
xgtest = xgb.DMatrix(test_X)
num_rounds = 1420
model = xgb.train(plst, xgtrain, num_rounds)
pred_test_y_xgb1 = model.predict(xgtest)

#submission
test['Purchase']=pred_test_y_xgb1
test.to_csv(data_path+'Solution.csv',columns = ['User_ID','Product_ID','Purchase'],index = False)
