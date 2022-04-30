# # ==============================================================================
# # Task 4. Develop models to predict air pollution (PM2.5 concentrations) 
# # using the meteorological data, the time/type of day, season and year. 
# # Two of these that you develop should be the standard linear model 
# # and the random forest.
# # ==============================================================================
# Using Aggregated Beijing Data

source('./jb957_v7_task1_data_prep.R')

library(tidyverse)
library(dplyr)

Beijing <- data[-c(11, 13)] # 52584

# drop all rows with PM_Beijing data is NA Records 
Beijing <- Beijing %>% drop_na(PM_Beijing) # 20508 Rows

#----------linear Regression-------------#
# https://towardsdatascience.com/understanding-linear-regression-output-in-r-7a9cbda948b3
# https://www.tutorialspoint.com/r/r_linear_regression.htm

summary(lm(
  Beijing.fit <-PM_Beijing ~ (
    HUMI +
    DEWP + 
    PRES + 
    TEMP + 
    Iws + 
    precipitation), 
  data = Beijing))


      
