# DS440 Project

## MLB Pitcher Predictive Injury Analysis

All code used for this project is in the "Code" folder.

### Code Files

MLB_DL_DataScraper.ipynb - Pre-built Web-Scraper, used to obtain MLB Disabled List data from prosportstransactions.com. Pre-Built by GitHub user robotallie

data_setup.R - Data Cleaning/Joining

XGBoost_Regression.R - XGBoost Regression code, Produces DL Length Predictions for MLB Pitchers, Includes hyperparameter tuning

REDESIGN_XGBoost.R - Redesign of the XGBoost_Regression.R code, Produces DL length Predictions as well as Feature Importance Analysis

REDESIGN_Individual_Pitchers.R - Redesign of XGBoost_Regression.R code, Produces DL length Predictions based on the statistics of individual pitchers.

Pitching_PCA_Analysis - Runs Scaled PCA and Gaussian Mixture Model (clustering) on season-by-season pitching data and output proportioned chances of pitcher's next DL length being 0, 15 or 60 days long.

Pitcher_FeatureSelection.R - Two different feature selection methods, first calculates based on normalized sum of squares, second calculates based on chi-squared.

Pitcher_DifferenceXGBoost.R - XGBoost Code to include differences from year to year in pitching data. Easily change desired attributes with instructions in top comments of code file.

Pitching_EDA.R - Basic Exploratory Data Analysis to detect pitching injury trends by age range in the MLB. 

For Further Information Regarding the Project, Please Contact mwentzel1932@gmail.com

Thank you
