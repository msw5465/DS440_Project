# Mitch Wentzel
# DS440

# XGBoost Code - Baseball Pitcher Injury Analysis

# Preliminary Model - Regression Based on Age, ERA, WAR, IP, Balls and Strikes

# Clean R Environment
rm(list = ls())

# Load in Packages
library(data.table)
library(caret)
library(Metrics)
library(xgboost)

# Read in the the data set.
data <- fread("fulldataset.csv", head = TRUE)

# Head of specific attributes.
header <- data[,c("Name", "Season", "DL_length", "Age", "ERA", "WAR", "IP", 
                  "Balls", "Strikes")]
head(header)

# Split data into training and test sets.
dt = sort(sample(nrow(data), nrow(data)*.75))
train_data <- data[dt,]
test <- data[-dt,]

# Add a validation set by splitting the test set.
valid_dt = sort(sample(nrow(test), nrow(test)*.5))
valid_set <- test[valid_dt,]
test_data <- test[-valid_dt,]

# Save train, validation and test data DL Length values.
DL.train <- train_data$DL_length
DL.valid <- valid_set$DL_length
DL.test <- test_data$DL_length

# Initialize the DL length variable to 0 in the validation and test data.
valid_set$DL_length <- 0
test_data$DL_length <- 0

# Run dummyVars on train data for DL_length response.
dummies <- dummyVars(DL_length ~ Age+ERA+WAR+IP+Balls+Strikes,
                     data = train_data)

# Run predict on train, validation and test data using the results from dummyVars.
x.train <- predict(dummies, newdata = train_data)
x.valid <- predict(dummies, newdata = valid_set)
x.test <- predict(dummies, newdata = test_data)

# Convert predict results from above to xgb.DMatrix.
# Do this for train, validation and test data sets.
dtrain <- xgb.DMatrix(x.train, label = DL.train, missing = NA)
dvalid <- xgb.DMatrix(x.valid, missing = NA)
dtest <- xgb.DMatrix(x.test, missing = NA)

# Initialize an information table for testing trees.
# This will tell us the parameters used from a specific tree as well as error.
hyper_perm_tune <- NULL

# Cross Validation Process:

# Specify parameters to be used in cross validation model below.
param <- list(  objective           = "reg:squarederror",
                gamma               = 0.02,   
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.25,   
                max_depth           = 4,      
                subsample           = 1.0,
                colsample_bytree    = 1.0,    
                tree_method         = 'hist'  
)

# Run a cross validation model on the train data.
# Use the parameters stated above as well as 10 folds in cv.
XGBfit <- xgb.cv(params                  = param,
                 nfold                   = 10,
                 nrounds                 = 10000,
                 missing                 = NA,
                 data                    = dtrain,
                 print_every_n           = 1,
                 early_stopping_rounds   = 25)

# Find the best tree and save the parameters as a row of data.
best_tree_n <- unclass(XGBfit)$best_iteration
new_row <- data.table(t(param))
new_row$best_tree_n <- best_tree_n

# Calculate the test error from the best_tree_n we found above.
valid_error <- unclass(XGBfit)$evaluation_log[best_tree_n,]$test_rmse_mean
new_row$valid_error <- valid_error

# Add the parameters and test error from the tree into the tree-info table.
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

# Fit the model using all data:

# Set the watchlist parameter to be used in the full model below.
watchlist <- list(train = dtrain)

# Fit a model using all of the train data and the best_tree_n.
XGBfit <- xgb.train(params          = param,
                    nrounds         = best_tree_n,
                    missing         = NA,
                    data            = dtrain,
                    watchlist       = watchlist,
                    print_every_n   = 1)

# Run predict on the validation data using the full data model.
pred <- predict(XGBfit, newdata = dvalid)

# Replace the DL length 0 values with the predictions for DL length.
valid_set$DL_length <- pred
Valid_wPred <- valid_set

# Clean Resulting data table for easy read access.
setnames(Valid_wPred, "DL_length", "Pred_DL_length")
Valid_wPred <- Valid_wPred[,c("Name", "Season", "Pred_DL_length")]
Valid_wPred$Season <- (Valid_wPred$Season + 1)

# Save the prediction file to the project.
fwrite(Valid_wPred,"Validation_Predictions.csv")

# Calculate rmse of validation set predictions from actual values.
rmse(Valid_wPred$Pred_DL_length, DL.valid)

# Run predict on the test data using the full data model.
pred <- predict(XGBfit, newdata = dtest)

# Replace the DL length 0 values with the predictions for DL length.
test_data$DL_length <- pred
test_wPred <- test_data

# Clean Resulting data table for easy read access.
setnames(test_wPred, "DL_length", "Pred_DL_length")
test_wPred <- test_wPred[,c("Name", "Season", "Pred_DL_length")]
test_wPred$Season <- (test_wPred$Season + 1)

# Save the prediction file to the project.
fwrite(test_wPred,"Test_Predictions.csv")

# Calculate rmse of test predictions from actual values.
rmse(test_wPred$Pred_DL_length, DL.test)

# Compute feature importance matrix.
importance_matrix = xgb.importance(colnames(dtrain), model = XGBfit)
importance_matrix
xgb.plot.importance(importance_matrix[1:6,], xlab = "Importance", 
                    ylab = "Feature")








