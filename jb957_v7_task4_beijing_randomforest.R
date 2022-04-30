# # ==============================================================================
# # Task 4. Develop models to predict air pollution (PM2.5 concentrations) 
# # using the meteorological data, the time/type of day, season and year. 
# # Two of these that you develop should be the standard linear model 
# # and the random forest.
# # ==============================================================================
# Using Aggregated Beijing Data
#install.packages("randomForest")
source('./jb957_v7_task1_data_prep.R')

library(tidyverse)
library(dplyr)


# Drop all columns we dont need
# Beijing <- data[-c(1:9, 11:13, 16:18)] # 52584
# # drop all rows with PM_Beijing data is NA Records 
# Beijing <- Beijing %>% drop_na(Beijing_PMRange) # 32103 Rows
# Beijing <- Beijing %>% drop_na(DEWP) # 32103 Rows
# Beijing <- Beijing %>% drop_na(HUMI) # 32103 Rows
# 
# RF_DATA <- Beijing
# RF_DATA$Beijing_PMRange <- as.character(RF_DATA$Beijing_PMRange)
# RF_DATA$DEWP <- as.character(RF_DATA$DEWP)
# RF_DATA$HUMI <- as.character(RF_DATA$HUMI)
# 
# RF_DATA$Beijing_PMRange <- as.factor(RF_DATA$Beijing_PMRange)
# RF_DATA$DEWP <- as.factor(RF_DATA$DEWP)
# RF_DATA$HUMI <- as.factor(RF_DATA$HUMI)

# https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/
# set.seed(911)
# train <- sample(nrow(RF_DATA), 0.8*nrow(RF_DATA), replace = FALSE)
# TrainSet <- RF_DATA[train,]
# ValidSet <- RF_DATA[-train,]
# summary(TrainSet)
# summary(ValidSet)

Beijing <- data[-c(1:11,13,16:20)] # 52584
Beijing <- Beijing %>% drop_na(PM_Beijing) # 32103 Rows
Beijing <- Beijing %>% drop_na(DEWP) # 32103 Rows
Beijing <- Beijing %>% drop_na(HUMI) # 32103 Rows

#(2) random forest (it takes hours)
library(randomForest)
library(e1071)

start_time <- Sys.time()

  # Tune Random Forest
  model_rf <- tune.randomForest(
    PM_Beijing ~ ., 
    data = Beijing, 
    cv=1,
    ntree =c(501))#mtry=c(10:20),
  
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
  
  #Choose Best Model
  model_rf$best.model

  end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
  
  #???
  var_importance <- varImp(model_rf,scale=FALSE)
  var_importance

  end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time() 

  #Plot 
  plot(var_importance)

end_time <- Sys.time()
print(end_time - start_time)





