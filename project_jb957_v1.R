#1

#road the data
data <- read.csv(file="BeijingPM20100101_20151231.csv") 

#remove No and PM2.5 info from the other locations 
data <- data[-c(1,7,9:10)]

#Replace all NA with zero
library(dplyr)
data <- data %>% replace(is.na(.), 0)

# Create Date Field
library(tidyverse)
library(lubridate)
data <- data %>% select(year,month,day,hour) %>% mutate(date = make_datetime(year,month,day,hour,min="0", sec="0", tz="Asia/Shanghai"))
# assume week starts monday day 1
data$weekday <- wday(data$date, week_start=1)

# https://asialinkbusiness.com.au/china/business-practicalities-in-china/business-hours-in-china?doNothing=1
# working days are monday to Friday (8:00 – 17:59)
# set w_hours to 1 if weekday and between 8 and 17, else 0
# note: we are working with hours so 17 assumes 17:00 to 17:59
data <- data %>% mutate(data, w_hour = ifelse(hour %in% 8:17 & weekday < 6,1,0))

#https://www.mdpi.com/2071-1050/10/12/4519/htm
data <- data %>% mutate(data, season = ifelse(
  month %in% 3:5,"Spring",
  ifelse(month %in% 6:8,"Summer",
  ifelse(month %in% 9:11, "Autumn", "Winter"))))

