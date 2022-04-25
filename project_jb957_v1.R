#1
setwd("~/dev-local/UOW/info911/info911_A22_project")
#road the data
data <- read.csv(file="BeijingPM20100101_20151231.csv") 

#remove No and PM2.5 info from the other locations 
data <- data[-c(1,7,9:10)] # 52584

#drop all rows with PM data is NA Records (as there are too many >20k).
data<- data %>% drop_na(PM_Dongsihuan) #20508

#Replace all NA with zero
library(dplyr)
data <- data %>% replace(is.na(.), 0)

# Create Date Field
library(tidyverse)
library(lubridate)
#data <- data %>% select(year,month,day,hour) %>% mutate(date = make_datetime(year,month,day,hour,min="0", sec="0", tz="Asia/Shanghai"))
data <- data %>% mutate(date = make_datetime(year,month,day,hour,min="0", sec="0", tz="Asia/Shanghai"))

# assume week starts monday day 1
data$weekday <- wday(data$date, week_start=1)

# https://asialinkbusiness.com.au/china/business-practicalities-in-china/business-hours-in-china?doNothing=1
# working days are monday to Friday (8:00 – 17:59)
# set w_hours to 1 if weekday and between 8 and 17, else 0
# note: we are working with hours so 17 assumes 17:00 to 17:59
data <- data %>% mutate(data, w_hour = ifelse(hour %in% 8:17 & weekday < 6,1,0))

# get the min and max year
(min_year <- min(data$year))
(max_year <- max(data$year))

# make a histogram of rows per day and see if there are any periods missing
stats <- data %>% group_by(year,month) %>% tally()
# after deleting na records we can see that data is not collected until march 2013
# thus we will need to drop 2013 as the missing field in this season may skew data.
# or we can drop rows from march 2015 
plot(stats$n,type = 'h')
# based on this plot there is a significant drop in rows so I would prefer to 
# drop rows from March 2015 an keep the 2013 rows.

# get the season start and end date each year
four_seasons <- data %>% 
  select(year,month, season) %>% 
  group_by(year, season) %>% 
  summarise(
    begin = min(month),
    end = max(month)) %>%
  arrange(year)
# from the above we learn the builtin seasons are 
# 1 Mar to May  Spring
# 2 Jun to Aug  Summer
# 3 Sep to Nov  Autumn
# 4 Dec to Feb  Winter

# #So we have no need of creating our own
# #https://www.mdpi.com/2071-1050/10/12/4519/htm
# data <- data %>% mutate(data, season2 = ifelse(
#   month %in% 3:5,"Spring",
#   ifelse(month %in% 6:8,"Summer",
#          ifelse(month %in% 9:11, "Autumn", "Winter"))))
