# Mitch Wentzel
# DS440

# MLB Pitching by Season, DL Length - Feature Selection Code

# Goal: Perform Feature Selection on different attribute sets that were used
# in actual predictive model scripts.

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
library(mlbench)
library(earth)
library(Boruta)

# Read in the the data set.
data <- fread("fulldataset.csv", head = TRUE)

# Set seed for randomization.
set.seed(7)

# Create two separate data frames to house the different sets of attributes
# used in the different modeling scripts.

# General Pitching Attributes
gen <- data[,c("Age", "ERA", "WAR", "IP", "Balls", "Strikes", "DL_length")]

# Pitch Type Statistics
pitch <- data[,c("FB_pct", "SL_pct", "CT_pct", "CB_pct", "CH_pct", "SF_pct",
                 "KN_pct", "XX_pct", "PO_pct", "DL_length")]

#------------- MARS Model Analysis ----------------#

# Feature Selection for General Pitching Attributes:

# Run MARS model on Gen data subset.
marsGen <- earth(DL_length ~ ., data = gen)

# Calculate the normalized GCV/number of subsets.
evGen <- evimp(marsGen)

# Plot the results.
plot(evGen)

# Feature Selection for Pitch Type Statistics:

# Run MARS model on Pitch data subset.
marsPitch <- earth(DL_length ~ ., data = pitch)

# Calculate the normalized GCV/number of subsets.
evPitch <- evimp(marsPitch)

# Plot the results.
plot(evPitch)

#------------- BORUTA Model Analysis ----------------#

# General Pitching Attributes

# Calculate Chi-Square Feature Selection with Boruta function.
boruta_output_gen <- Boruta(DL_length ~ ., data = gen, doTrace = 2)

# Visualize the results.
plot(boruta_output_gen, cex.axis = 0.7, las = 2, xlab = "Pitching Attribute", 
     main = "Variable Importance")

# Pitch Type Statistics

# Calculate Chi-Square Feature Selection with Boruta function.
boruta_output_pitch <- Boruta(DL_length ~ ., data = pitch, doTrace = 2)

# Visualize the results.
plot(boruta_output_pitch, cex.axis = 0.5, las = 2, xlab = "Pitch Type", 
     main = "Variable Importance")



