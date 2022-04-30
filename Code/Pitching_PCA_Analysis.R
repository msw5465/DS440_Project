# Mitch Wentzel

# Pitcher DL Length Analysis - Classification Code

# REDESIGN - XGBoost Code - Baseball Pitcher Injury Analysis

# REDESIGN Model - Classification Based on same variables as preliminary model.

# Feature Manipulation - Scaled PCA

# Clean R environment.
rm(list = ls())

# Load packages into R.
library(data.table)
library(ggplot2)
library(caret)
library(ClusterR)
library(Metrics)
library(Rtsne)

# Set seed for randomization.
set.seed(440)

# Load in the data set.
data <- read.csv("fulldataset.csv", header = TRUE)

# Save the id values from the data set.
names <- data$Name
seasons <- data$Season
Age <- data$Age
ERA <- data$ERA
WAR <- data$WAR
IP <- data$IP
Strikes <- data$Strikes
Balls <- data$Balls

data <- data[,c("Age", "ERA", "WAR", "IP", "Strikes", "Balls")]

# Run PCA on the data set to find the Principle Components.
# Use scale. = TRUE.
pca <- prcomp(x = data, scale. = TRUE)

# Now unclass the information stored in PCA and make it into a data table.
# This will allow us to visualize the PCA-component values for each of the 
# variables in the data set.
pca_dt <- data.table(unclass(pca)$x)

# From the line graph we created, we can see that 4 clusters appears to be
# an optimal amount.
opt_num_clus <- 3

# Now we will run GMM on the PCA data set (first 3 features), using the optimal 
# number of clusters (which is 4).
gmm_data <- GMM(pca_dt[,1:6], opt_num_clus)

# Now we run predict on the PCA data using the results from the GMM
# line above. 
clusterInfo <- predict_GMM(pca_dt[,1:6], 
                           gmm_data$centroids, 
                           gmm_data$covariance_matrices, 
                           gmm_data$weights)

# Now we create a temporary submission file that will contain the probabilties
# on each of the 4 clusters we are predicting for.
tmp <- data.table(clusterInfo$cluster_proba)

# Now we add the id variable back into the data set from the initial data set.
tmp$Name <- names
tmp$Season <- seasons
tmp$Age <- Age
tmp$ERA <- ERA
tmp$WAR <- WAR
tmp$IP <- IP
tmp$Strikes <- Strikes
tmp$Balls <- Balls

setnames(tmp, "V1", "0-Day DL")
setnames(tmp, "V2", "15-Day DL")
setnames(tmp, "V3", "60-Day DL")

# Here just organize the columns for submission.
tmp <- tmp[,c('Name', '0-Day DL', '15-Day DL', '60-Day DL', 'Age', 'ERA', 'WAR', 'IP', 'Strikes', 'Balls')]

# Finally, we write the csv that contains the final submission.
write.csv(tmp, 'Pitching_PCA_Results.csv')





