# project_jb957_v1.R

#https://archive.ics.uci.edu/ml/datasets/Beijing+PM2.5+Data
# No: row number
# year: year of data in this row
# month: month of data in this row
# day: day of data in this row
# hour: hour of data in this row
# pm2.5: PM2.5 concentration (ug/m^3)
# DEWP: Dew Point
# HUMI: Humidity (%)
# PRES: Pressure (hPa)
# TEMP: Temperature (Celcius)
# cbwd: Combined wind direction 
#    Northwest(NW), Northeast(NE), Southeast(SE), Southwest (SW), static wind (CV)
# Iws: Cumulated wind speed (m/s)
# precipitation: hourly precipitation (mm)
# Iprec: Cumulated precipitation (mm)

setwd("~/dev-local/UOW/info911/info911_A22_project")
#road the data
data <- read.csv(file="BeijingPM20100101_20151231.csv") 

#remove No and PM2.5 info from the other locations 
library(tidyverse)
library(dplyr)
library(lubridate)

data <- data[-c(1,7,9:10)] # 52584


#drop all rows with PM data is NA Records (as there are too many >20k).
data <- data %>% drop_na(PM_Dongsihuan) #20508

#Replace all NA with zero
data <- data %>% replace(is.na(.), 0)

# Create Date Field and sort
data <- data %>% 
  mutate(datetime = make_datetime(year,month,day,hour,min="0", sec="0", tz="Asia/Shanghai")) %>%
  arrange(datetime)

# assume week starts monday day 1
data$weekday <- wday(data$date, week_start=1)

# https://asialinkbusiness.com.au/china/business-practicalities-in-china/business-hours-in-china?doNothing=1
# working days are monday to Friday (8:00 – 17:59)
# set w_hours to 1 if weekday and between 8 and 17, else 0
# note: we are working with hours so 17 assumes 17:00 to 17:59
data <- data %>% mutate(data, w_hour = ifelse(hour %in% 8:17 & weekday < 6,1,0))

#exploratory analysis
summary(data)

#historgrams of Meteorological 
library(ggplot2)
ggplot(data=data, aes(PM_Dongsihuan)) + geom_histogram(binwidth = 2)
ggplot(data=data, aes(DEWP)) + geom_histogram(binwidth = 2)
ggplot(data=data, aes(HUMI)) + geom_histogram(binwidth = 2)
ggplot(data=data, aes(PRES)) + geom_histogram(binwidth = 2) + xlim(980,1050)
ggplot(data=data, aes(TEMP)) + geom_histogram(binwidth = 2)

#there are some values that were NA and converted to 0
#perhaps these should be deleted
ggplot(data=data, aes(cbwd)) + geom_histogram(stat = "count")
#what disctinct values are available
data %>% group_by(cbwd) %>% tally()

# Iws: Cumulated wind speed (m/s)
# winds over 100m/sec equate to 360km/per winds 
ggplot(data=data, aes(Iws)) + geom_histogram(binwidth = 2)

# precipitation: hourly precipitation (mm)
plot(data$precipitation)

#shown in in aggregrate for each month
rain <- data %>% group_by(month) %>% tally(precipitation)
ggplot(data=rain, aes(x=month, y=n)) + geom_point() + scale_x_continuous(
  breaks = seq(1, 12, 1), limits=c(1, 12))
# much higher rain full in summer (wet season), than winter (Dry season)
# I predict that high rain will correlate with low PM2.5 due to a 'cleansing' effect.

# Iprec: Cumulated precipitation (mm)
# reflects a similar story to precipitation which is understandable.
ggplot(data=data, aes(Iprec)) + geom_histogram(binwidth = 2)

#move factor columns forward so its easier to work with column numbers later
data <- data %>% relocate(cbwd, .after = season)
data <- data %>% relocate(datetime, .after = cbwd)
data <- data %>% relocate(weekday, .after = datetime)
data <- data %>% relocate(w_hour, .after = weekday)

# are there any correlations ?
library("Hmisc")
data.rcorr <- rcorr(as.matrix(data[10:17]))
data.rcorr
data.coeff = data.rcorr$r
data.p = data.rcorr$P
install.packages("corrplot")
library(corrplot)
corrplot(data.coeff)
data.coeff
# Temp and DewP 0.79552810 strong   positive
# Humi and DewP 0.65298    moderate positive
# PM   and Humi 0.3929006  weak     positive








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



### relationships between meteorological attributes

# correlation between attributes 

#correlation with pm2.5



