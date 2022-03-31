# Mitch Wentzel
# DS440

# Single Pitcher Analysis Code

# REDESIGN - XGBoost Code - Baseball Pitcher Injury Analysis

# REDESIGN Model - Regression Based on Age, IP, ERA, Pitches, FB_pct, SL_pct, 
# CT_pct, CB_pct, CH_pct, SF_pct, KN_pct, XX_pct, PO_pct

# This XGBoost model is to be trained and tested on season data from a single
# pitcher. Then it will predict the DL length for the upcoming season for the 
# specific pitcher it trained/tested on. Simply change input data for each 
# iteration, make sure input data set has the following variables all in
# numeric form: Age, IP, ERA, Pitches, FB_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
# SF_pct, KN_pct, XX_pct, PO_pct, DL_length.



#------------ Skip to line 97 to input specific pitcher name -----------------#



# Clean R Environment
rm(list = ls())

# Load in Packages
library(data.table)
library(caret)
library(Metrics)
library(xgboost)

# Read in full dataset of MLB pitching statistics by season.
pitchers <- read.csv("pitching_by_season(updated).csv", head = TRUE)

# Read in full MLB disabled list dataset.
dldata <- read.csv("MLB-DL-Data(pre-cleaning).csv", head = TRUE)

# Read in full dataset of MLB player IDs.
playerids <- read.csv("df_injury_sm.csv", head = TRUE)

# Alter the date attribute to only include the season year.
playerids$Date = substr(playerids$Date,1,nchar(playerids$Date)-4)

# Assign this new date value to the variable Season to later join on.
playerids$Season <- playerids$Date

# Complete a full join of the player IDs and pitching data, by pitcher name.
all_data <- full_join(playerids, pitchers, by = "Name")

# Remove noninjured pitchers from the dataset
noninjured <- all_data[is.na(all_data$DL_length),]

# Filter the data by the season values.
new_data <- all_data[all_data$Season.x == all_data$Season.y,]

# Add noninjured pitchers back into the dataset.
new_data <- rbind(new_data, noninjured)

# Remove any rows that have no name ID/NA for name value.
new_data <- new_data[-which(is.na(new_data$Name)),]

# Limit the dataset to include only data from 2006 and newer.
# 2006 is when the MLB implemented PitchFX into league statistics.
new_data <- new_data[new_data$Season.y >= 2006,]

# Replace all NA values with 0 to avoid problems in regression/modeling.
new_data[is.na(new_data)] <- 0

# Save values for teams.
Team <- new_data$Team.x

# Add teams back into dataset as a renamed variable.
new_data <- cbind(Team, new_data)

# Save Season.x values.
seasonvalues.x <- as.integer(new_data$Season.x[(1:2789)])

# Save Season.y values.
seasonvalues.y <- new_data$Season.y[(2790:8005)]

# Combine the season values.
season_values <- c(seasonvalues.x, seasonvalues.y)

# Remove unnecessary columns.
new_data <- new_data %>% 
  select(-c("X", "Team.x", "Team.y", "Season.x", "Season.y"))

# Change the Date to be the season values.
setnames(new_data, "Date", "Season")
new_data$Season <- season_values

# Order data by Season.
new_data <- new_data[order(new_data[,2]),]

# ---------- INPUT PITCHER NAME HERE -------------#
new_data <- new_data[new_data$Name == "Pitcher Name Here",]

# Write final cleaned data CSV (pre-null values).
write.csv(new_data,'fulldataset.csv', row.names = FALSE)

# Write final cleaned data CSV (pre-null values).
write.csv(new_data,'single_pticher_season_data.csv', row.names = FALSE)

# Read in the the data set.
data <- fread("single_pticher_season_data.csv", head = TRUE)

# Specify data into training and testing sets.
train_data <- data
test_data <- data

# Save train and test data DL Length values.
DL.train <- train_data$DL_length
DL.test <- test_data$DL_length

# Initialize the DL length variable to 0 in the test data.
test_data$DL_length <- 0

# Run dummyVars on train data for DL_length response.
dummies <- dummyVars(DL_length ~ Age+IP+ERA+Pitches+FB_pct+SL_pct+CT_pct+CB_pct+CH_pct+SF_pct+KN_pct+XX_pct+PO_pct,
                     data = train_data)

# Run predict on train and test data using the results from dummyVars.
x.train <- predict(dummies, newdata = train_data)
x.test <- predict(dummies, newdata = test_data)

# Convert predict results from above to xgb.DMatrix.
# Do this for both train and test data sets.
dtrain <- xgb.DMatrix(x.train, label = DL.train, missing = NA)
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
test_error <- unclass(XGBfit)$evaluation_log[best_tree_n,]$test_rmse_mean
new_row$test_error <- test_error

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
xgb.plot.importance(importance_matrix[1:10000000,], xlab = "Importance",
                    ylab = "Feature")




