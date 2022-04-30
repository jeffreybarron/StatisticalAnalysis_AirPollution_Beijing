# project_jb957_v2.R
rm(list = ls())
source('./user_functions.R')
# ==============================================================================
# Background Information
# ==============================================================================
# https://archive.ics.uci.edu/ml/datasets/Beijing+PM2.5+Data
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
## Reference: http://dx.doi.org/10.1098/rspa.2015.0257
## takes into account wind velocity in a particular direction. It reﬂects the
## fact that it is sustained wind from a ﬁxed direction that reduces or increases
## the pollution. CWP is the cumulated wind speed from the start of the wind
## direction to the time of interest. When the wind direction changes, the
## CWP under a new direction starts to cumulate again. For calm wind, we
## use 0.445 m s −1 as the unit of cumulation.

# precipitation: hourly precipitation (mm)
# Iprec: Cumulated precipitation (mm) #NOTE: Max(Iprec)p\day not equal sum(precipitation) on same day, therefrom drop this. 

# ==============================================================================
# Hypothesis
# ==============================================================================
# at first I thought we were going to be doing a timeseries analysis, but then I 
# realised by catagorising datetimes into working hours(0-1) professor is 
# simplifying this for us.. in other words he wants us to consider 
# working(0-1), season(1-4), PM2.5 and meteorological attributes (attributes)
# so we could do PCA on attributes (likely wind and rain will be highest) and 
# compare these by season(1-4) and working(0-1)
# thats my conceptual view of the assessment anyway

# ==============================================================================
# Data Input and cleaning 
# ==============================================================================
setwd("~/dev-local/UOW/info911/info911_A22_project")
data <- read.csv(file="BeijingPM20100101_20151231.csv") # 52584 Rows
#Remove Row number and Iprec, as they are not needed.
data <- data[-c(1)] # 52584

# ==============================================================================
# Task 1. Creation of required fields for analysis 
# ==============================================================================
# Aggregate date columns into new datetime column and sort by datetime
# Note: Asia/Beijing is not a valid time code, Shanghai is the closest I think.
library(dplyr)
library(lubridate)
data <- data %>% 
  mutate(datetime = make_datetime(year,month,day,hour,min="0", sec="0", tz="Asia/Shanghai")) %>%
  arrange(datetime)

# assume week starts monday day 1, create weekday field (1:Monday,..,7:Sunday)
data$weekday <- wday(data$date, week_start=1)

# create w_hours and set to 1 if weekday and between 8 and 17, else 0
# Reference:- https://asialinkbusiness.com.au/china/business-practicalities-in-china/business-hours-in-china?doNothing=1
# Therefore, working days are monday to Friday (8:00 – 17:59) and we are working
# with hours so 17 assumes 17:00 to 17:59
data <- data %>% mutate(data, w_hour = ifelse(hour %in% 8:17 & weekday < 6,1,0))


# We considered that Beijing is a large city and there may be statistically 
# significant differences in PM between location, however, we also note that 
# there is only one location for all meteorological attributes. Therefore if we
# were to analyse PM Locations seperately we would be left with confounders 
# from the unknown variations in weather attributes per location.
# Therefore with a single source of weather data, we decided to consider 
# an mean aggregate of all PM2.5 locations for our modelling analysis.
# In addition exploratory analysis revealed ~30k cells with missing data 
# for Dongsihuan (~60% of data), and given the potential usefullness of all the
# data in aggregrate the team concluded that aggregating PM2.5 was the most 
# reliable to use most all of the available data.
#
# Create aggregate PM Column
data <- mutate(
  data, PM_Beijing = rowMeans(
    select(data,c(PM_Dongsi,PM_Dongsihuan,PM_Nongzhanguan,PM_US.Post)), 
    na.rm = TRUE)
)
# Now remove Dongsi, Nongzhanguan and US.Post as we dont need them
data <- data[-c(6,8:9)] # 52584

# Categorise PM2.5 into Low(0, <= 35), Medium(1, >35 & <= 150), High(2, >150) 
# per (Liang et al., 2015)
# we are doing this for only Dongsihuan and the aggregated PM_Beijing
data <- data %>% mutate(data, Dongsihuan_PMRange = ifelse(
  PM_Dongsihuan %in% 0:35, 1,
  ifelse(PM_Dongsihuan %in% 36:149, 2, 
         ifelse(PM_Dongsihuan %in% 150:10000, 3, NA))))

data <- data %>% mutate(data, Beijing_PMRange = ifelse(
  PM_Beijing %in% 0:35, 1,
  ifelse(PM_Beijing %in% 36:149, 2, 
         ifelse(PM_Beijing %in% 150:10000, 3, NA))))

#move new columns and cbwd, so its easier to work with column numbers later
data <- data %>% relocate(datetime, .after = hour)
data <- data %>% relocate(weekday, .after = datetime)
data <- data %>% relocate(w_hour, .after = weekday)
data <- data %>% relocate(cbwd, .after = season)
data <- data %>% relocate(Beijing_PMRange, .after = cbwd)
data <- data %>% relocate(Dongsihuan_PMRange, .after = Beijing_PMRange)
data <- data %>% relocate(PM_Beijing, .after = Dongsihuan_PMRange)
# PM Ranges are 10 through 13, PM Values are 14 through 17
# Meteorological attributes columns 18 through 24

#Replace all NA with zero in Meteorological attributes columns 18 through 24
data <- data %>% replace(is.na(c(14:20)), 0)

# for consistency we update the new PM_Beijing NaN rows to NA
data$PM_Beijing[is.nan(data$PM_Beijing)] <- NA

