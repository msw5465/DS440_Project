# Mitch Wentzel
# DS440

# MLB Pitching by Season, DL Length - Basic Exploratory Data Analysis

# Goal: Produce plots that identify trends in age range and average DL length.

# Clean R Environment
rm(list = ls())

# Load in Packages
library(data.table)
library(caret)
library(Metrics)
library(xgboost)
library(ggplot2)
library(ClusterR)
library(Rtsne)
library(dplyr)

# Read in the the data set.
data <- fread("fulldataset.csv", head = TRUE)

# Order by Age.
data <- data[order(data$Age),]

# Make Age numeric rather than integer.
data$Age <- as.numeric(data$Age)

# Initialize the age ranges.
data$Age_Range <- 0

# Set age ranges for analysis.

# Range 1
data$Age_Range[data$Age == 19] <- "19-22"
data$Age_Range[data$Age == 20] <- "19-22"
data$Age_Range[data$Age == 21] <- "19-22"
data$Age_Range[data$Age == 22] <- "19-22"
# Range 2
data$Age_Range[data$Age == 23] <- "23-27"
data$Age_Range[data$Age == 24] <- "23-27"
data$Age_Range[data$Age == 25] <- "23-27"
data$Age_Range[data$Age == 26] <- "23-27"
data$Age_Range[data$Age == 27] <- "23-27"
# Range 3
data$Age_Range[data$Age == 28] <- "28-33"
data$Age_Range[data$Age == 29] <- "28-33"
data$Age_Range[data$Age == 30] <- "28-33"
data$Age_Range[data$Age == 31] <- "28-33"
data$Age_Range[data$Age == 32] <- "28-33"
data$Age_Range[data$Age == 33] <- "28-33"
# Range 4
data$Age_Range[data$Age == 34] <- "34-39"
data$Age_Range[data$Age == 35] <- "34-39"
data$Age_Range[data$Age == 36] <- "34-39"
data$Age_Range[data$Age == 37] <- "34-39"
data$Age_Range[data$Age == 38] <- "34-39"
data$Age_Range[data$Age == 39] <- "34-39"
# Range 5
data$Age_Range[data$Age == 40] <- "40-47"
data$Age_Range[data$Age == 41] <- "40-47"
data$Age_Range[data$Age == 42] <- "40-47"
data$Age_Range[data$Age == 43] <- "40-47"
data$Age_Range[data$Age == 44] <- "40-47"
data$Age_Range[data$Age == 45] <- "40-47"
data$Age_Range[data$Age == 46] <- "40-47"
data$Age_Range[data$Age == 47] <- "40-47"

# Subset data set to only include specific attributes.
data <- data[,c("Name", "Season", "Age", "Age_Range", "DL_length")]

# Separate data tables by age range for individual analysis.
range1 <- data[data$Age_Range == "19-22"]
range2 <- data[data$Age_Range == "23-27"]
range3 <- data[data$Age_Range == "28-33"]
range4 <- data[data$Age_Range == "34-39"]
range5 <- data[data$Age_Range == "40-47"]

# Create histograms for injury length frequency.
hist(range1$DL_length)
hist(range2$DL_length)
hist(range3$DL_length)
hist(range4$DL_length)
hist(range5$DL_length)

# Find counts for each injury length in each age range.
# Also, find number of distinct pitchers in each range.
range1 %>%
  count(DL_length)
n_distinct(range1$Name)
range2 %>%
  count(DL_length)
n_distinct(range2$Name)
range3 %>%
  count(DL_length)
n_distinct(range3$Name)
range4 %>%
  count(DL_length)
n_distinct(range4$Name)
range5 %>%
  count(DL_length)
n_distinct(range5$Name)





