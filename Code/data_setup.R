# Mitch Wentzel
# DS440

# Data Cleaning Code - Baseball Pitcher Injury Analysis

# Clean R Environment.
rm(list = ls())

# Load in Packages.
library(data.table)
library(dplyr)

# Read in full dataset of MLB pitching statistics by season.
pitchers <- read.csv("pitching_by_season(updated).csv", head = TRUE)

# Read in full MLB disabled list dataset.
dldata <- read.csv("MLB-DL-Data(pre-cleaning).csv", head = TRUE)

# Read in full dataset of MLB player IDs.
playerids1 <- read.csv("df_injury_sm.csv", head = TRUE)
playerids2 <- read.csv("df_injury_sm(1).csv", head = TRUE)
playerids <- rbind(playerids1, playerids2)

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
seasonvalues.y <- new_data$Season.y[(2790:8233)]

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

# Write final cleaned data CSV (pre-null values).
write.csv(new_data,'fulldataset.csv', row.names = FALSE)



