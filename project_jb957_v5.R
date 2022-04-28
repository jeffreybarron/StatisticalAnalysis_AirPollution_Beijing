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



# ==============================================================================
# Task 2. Explore the relationships between air pollution at Dongsihuan, 
# the meteorological variables, the time/type of day, season and year.
# ==============================================================================
# Analyse PM_Dongsihuan ONLY, so drop Other weather stations
Dongsihuan <- data[-c(10, 12)] # 52584

# drop all rows with PM_Dongsihuan data is NA Records 
# (as there are too many >20k)
library(tidyverse)
Dongsihuan <- Dongsihuan %>% drop_na(PM_Dongsihuan) # 20508 Rows

#exploratory analysis
summary(Dongsihuan)

# make a histogram of to see if there are any months missing data
Dongsihuan_stats <- Dongsihuan %>% group_by(year,month) %>% tally()
Dongsihuan_stats <- as.data.frame(Dongsihuan_stats)

Dongsihuan_stats$name <- paste(Dongsihuan_stats[,1], Dongsihuan_stats[,2], sep="-")

# names(Dongsihuan_stats) <- c("year","month","n","name")
barplot(Dongsihuan_stats$n,
        main="Datum Frequency per Month",
        names.arg = Dongsihuan_stats$name,
        xlab="Month Data collected")

# create correlation matrix of PM2 and attributes ?
library("Hmisc")
Dongsihuan.rcorr <- rcorr(as.matrix(Dongsihuan[11:18]))
Dongsihuan.coeff = Dongsihuan.rcorr$r
Dongsihuan.coeff
# PM_Dongsihuan       DEWP        HUMI        PRES        TEMP         Iws precipitation       Iprec
# PM_Dongsihuan    1.00000000  0.1022474  0.39441570  0.03927175 -0.18045059 -0.23447534   -0.02690737 -0.04563127
# DEWP             0.10224735  1.0000000  0.65452225 -0.77121963  0.79558709 -0.29078475    0.08539870  0.10514430
# HUMI             0.39441570  0.6545223  1.00000000 -0.27944403  0.09349573 -0.30939129    0.10454593  0.14727088
# PRES             0.03927175 -0.7712196 -0.27944403  1.00000000 -0.82059588  0.16616809   -0.06406584 -0.08563460
# TEMP            -0.18045059  0.7955871  0.09349573 -0.82059588  1.00000000 -0.13666415    0.04342851  0.04445596
# Iws             -0.23447534 -0.2907848 -0.30939129  0.16616809 -0.13666415  1.00000000   -0.01329787 -0.01793997
# precipitation   -0.02690737  0.0853987  0.10454593 -0.06406584  0.04342851 -0.01329787    1.00000000  0.52928085
# Iprec           -0.04563127  0.1051443  0.14727088 -0.08563460  0.04445596 -0.01793997    0.52928085  1.00000000

#install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
corrplot(Dongsihuan.coeff, method="ellipse")
corrplot(Dongsihuan.coeff, method="number")
# Positive Correlates of interest
#--------------------------------
# Temp and DewP 0.7955    strong   
# Humi and DewP 0.6529    moderate 
# PM   and Humi 0.3929    weak     
# Negative Correlates of interest
#--------------------------------
# Pres and Temp -0.8206   strong
# Pres and DewP -0.7712   strong
# Iws and HUMI  -0.3094   weak
# Dewp and Iws  -0.2908   weak
# Notable Correlates with PM
#--------------------------------
# PM and Humi   0.3944    weak Pos
# PM and Iws    -0.2345   weak Neg
# PM and TEMP   -0.1805   weak neg

# Lets see a colourised pairs scattergram 
# 1:yellow(low), 2:orange(high), 3:red(veryHigh), 
cols <- c(12:18)
pairs(
  Dongsihuan[,cols],
  pch = 20,
  col = c("yellow", "orange", "red")[Dongsihuan$Dongsihuan_PMRange],
  upper.panel = panel.cor, 
  lower.panel = panel.lm
)
Dongsihuan %>% group_by(Dongsihuan_PMRange) %>% count()


# # ==============================================================================
# # Task 3. Present relevant visualisations of the data, which help to illustrate 
# # the relationships, trends and diﬀerences found in the previous items.
# # ==============================================================================
# Dongsihuan only

#historgrams of Meteorological
library(ggplot2)
par(mfrow=c(2,3))   
ggplot(data=Dongsihuan, aes(PM_Dongsihuan)) + geom_histogram(binwidth = 2) + labs(
  x = "PM2.5 Value",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "PM2.5 Readings 2013-2015 for Dongsihuan, China")
ggplot(data=Dongsihuan, aes(DEWP)) + geom_histogram(binwidth = 2) + labs(
  x = "Temperature C",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "Dew Point readings for Beijing, China 2013-2015")
ggplot(data=Dongsihuan, aes(HUMI)) + geom_histogram(binwidth = 2) + labs(
  x = "Humidity (%)",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "Humidity readings for Beijing, China 2013-2015")
ggplot(data=Dongsihuan, aes(PRES)) + geom_histogram(binwidth = 2) + xlim(980,1050) + labs(
  x = "Pressure (hPa)",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "Barometric Pressure readings for Beijing, China 2013-2015")
ggplot(data=Dongsihuan, aes(TEMP)) + geom_histogram(binwidth = 2) + labs(
  x = "Temperature Centigrade (C)",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "Temperature readings for Beijing, China 2013-2015")
#note two maximum for temp
ggplot(data=na.omit(Dongsihuan), aes(cbwd)) + geom_histogram(stat = "count") + labs(
  x = "Northwest(NW), Northeast(NE), Southeast(SE), Southwest (SW), calm variable (cv)",
  y = "Frequency",
  title = "Frequency Histogram",
  subtitle = "Combined wind direction (cbwd) readings for Beijing, China 2013-2015")
#what distinct values are available
(Dongsihuan %>% group_by(cbwd) %>% tally())
# cbwd      n
# 1 cv     4637 (Static\No Wind)
# 2 NE     2379
# 3 NW     5991
# 4 SE     7498
# 5 NA        3
# so where is the SW wind data? 

# # Iws: Cumulated wind speed (m/s)
(
  Iws_ByDayDirection <- Dongsihuan %>% 
    group_by(year, month, day, cbwd) %>% 
    summarise(
      max = max(Iws),
      median = median(Iws),
      mean = mean(Iws)
    )
)
Iws_ByMonth <- Iws_ByDayDirection %>% 
  group_by(year, month) %>%
  summarise(
    sum = sum(max),
    .groups = "keep"
    )
Iws_ByMonth$name <- paste(Iws_ByMonth$year, Iws_ByMonth$month, sep="-")
barplot(Iws_ByMonth$sum,
        main="Cumulated wind speed readings for Beijing, China 2013-2015",
        names.arg = Iws_ByMonth$name,
        xlab="Cumulated wind speed (m/s)")


# # precipitation: hourly precipitation (mm)
precipitation_ByMonth <- Dongsihuan %>% 
  group_by(year, month, day) %>% 
  summarise(
    max = max(precipitation),
    median = median(precipitation),
    mean = mean(precipitation)
  )
precipitation_ByMonth$name <- paste(precipitation_ByMonth$year, precipitation_ByMonth$month, sep="-")
barplot(precipitation_ByMonth$max,
        main="Precipitation readings for Beijing, China 2013-2015",
        names.arg = precipitation_ByMonth$name,
        ylab="Precipitation (mm)")
# Note 3 period of higher precipitation
# much higher rain full in summer (wet season), than winter (Dry season)
# Therefore I PREDICT that high-rain Summers will correlate with low PM2.5 due 
# to a cleansing' effect.

# Iprec: Cumulated precipitation (mm)
# Lets plot PM in by group and see if our prediction is correct?
(
 Iprec_ByDay <- Dongsihuan %>% 
   group_by(year, month, day) %>% 
   summarise(
     max = max(Iprec),
     median = median(Iprec),
     mean = mean(Iprec)
   )
)
Iprec_ByDay$name <- paste(Iprec_ByDay$year, Iprec_ByDay$month, sep="-")
barplot(Iprec_ByDay$max,
        main="Cumulated precipitation for Beijing, China 2013-2015",
        names.arg = Iprec_ByDay$name,
        ylab="Cumulated precipitation (mm)")
# reflects a similar story to precipitation which is understandable.

#Precipitation and Iprec are Higly correlated, therefore we can drop 
# Iprec from future analysis
data <- data[-c(20)] # 52584
Dongsihuan <- Dongsihuan[-c(18)] # 52584

rm(Dongsihuan.coeff,Dongsihuan_stats,Dongsihuan_rain,Dongsihuan.rcorr,
   Iprec_ByDay,Iws_ByDayDirection,IWS_ByDayDirection,Iws_ByMonth,
   Precipitation_ByDay,Precipitation_ByMonth,precipitation_ByMonth)


# Lets plot PM in by group and see if our prediction is correct?
 (
   PM_ByMonth <- Dongsihuan %>% 
   group_by(month) %>% 
   summarise(
     mean = mean(PM_Dongsihuan), 
     sd=sd(PM_Dongsihuan), 
     n = n())
 )
 ggplot(data=PM_ByMonth, aes(x=month, y=mean)) + geom_area() + scale_x_continuous(
   breaks = seq(1, 12, 1), limits=c(1, 12)) + labs(
     x = "Month of the Year",
     y = "Average PM2.5 Reading",
     title = "Area Plot",
     subtitle = "PM2.5 for Dongsihuan, China 2013-2015")
 # as predicted PM averages are low in summer when precipitation is high

 #-------- bar plots for categorical columns ---------#
 par(mfrow=c(2,3)) 
 #year
 (
   PM_ByYear <- Dongsihuan %>% 
     group_by(year) %>% 
     summarise(
       mean = mean(PM_Dongsihuan), 
       sd=sd(PM_Dongsihuan), 
       n = n())
 )
  barplot(PM_ByYear$mean,
         main="Average PM2.5 per Year, 2013-2015",
         names.arg = PM_ByYear$year,
         las=2,
         xlab="Years")
 

 #month
  (
    PM_ByMonth <- Dongsihuan %>% 
      group_by(year,month) %>% 
      summarise(
        mean = mean(PM_Dongsihuan), 
        sd=sd(PM_Dongsihuan), 
        n = n())
  )
  PM_ByMonth$name <- paste(PM_ByMonth$year, PM_ByMonth$month, sep="-")
  barplot(PM_ByMonth$mean,
          main="Average PM2.5 per Month, 2013-2015",
          names.arg = PM_ByMonth$name,
          las=2)

 #days
  (
    PM_ByDay <- Dongsihuan %>% 
      group_by(year,month,day) %>% 
      summarise(
        mean = mean(PM_Dongsihuan), 
        sd=sd(PM_Dongsihuan), 
        n = n())
  )
  PM_ByDay$name <- paste(PM_ByDay$year, PM_ByDay$month,PM_ByDay$day, sep="-")
  barplot(PM_ByDay$mean,
          main="Average PM2.5 per Day, 2013-2015",
          names.arg = PM_ByDay$name,
          las=2)  

 #seasons
  (
    PM_BySeason <- Dongsihuan %>% 
      group_by(season) %>% 
      summarise(
        mean = mean(PM_Dongsihuan), 
        sd=sd(PM_Dongsihuan), 
        n = n())
  )
  barplot(PM_BySeason$mean,
          main="Average PM2.5 per Season, 2013-2015",
          names.arg = PM_BySeason$season,
          xlab="Season")  

 #weekdays
  (
    PM_ByWeekday <- Dongsihuan %>% 
      group_by(weekday) %>% 
      summarise(
        mean = mean(PM_Dongsihuan), 
        sd=sd(PM_Dongsihuan), 
        n = n())
  )
  barplot(PM_ByWeekday$mean,
          main="Average PM2.5 per Day of the week, 2013-2015",
          names.arg = PM_ByWeekday$weekday,
          xlab="Day of the week")  
 
 #working hours
  (
    PM_ByWorkingHrs <- Dongsihuan %>% 
      group_by(w_hour) %>% 
      summarise(
        mean = mean(PM_Dongsihuan), 
        sd=sd(PM_Dongsihuan), 
        n = n())
  )
  barplot(PM_ByWorkingHrs$mean,
          main="Average PM2.5 Working Hours vs Not Working, 2013-2015",
          names.arg = PM_ByWorkingHrs$w_hour,
          xlab="0 = Not Working, 1 = Working Hours")  
  
par(mfrow=c(2,3)) 


#---------scatter plots for continuous attributes--------#
plot(Dongsihuan$DEWP,Dongsihuan$PM_Dongsihuan,main="DEWP vs PM2.5")
plot(Dongsihuan$HUMI,Dongsihuan$PM_Dongsihuan,main="HUMI vs PM2.5")
plot(Dongsihuan$PRES,Dongsihuan$PM_Dongsihuan,main="PRES vs PM2.5")
plot(Dongsihuan$TEMP,Dongsihuan$PM_Dongsihuan,main="TEMP vs PM2.5")
plot(Dongsihuan$Iws,Dongsihuan$PM_Dongsihuan,main="Iws vs PM2.5")
plot(Dongsihuan$precipitation,Dongsihuan$PM_Dongsihuan,main="Precipitation vs PM2.5")

rm(PM_ByDay,PM_ByMonth,PM_BySeason,PM_ByWeekday,PM_ByWorkingHrs,PM_ByYear)
rm(Dongsihuan,cols)
# # ==============================================================================
# # Task 4. Develop models to predict air pollution (PM2.5 concentrations) 
# # using the meteorological data, the time/type of day, season and year. 
# # Two of these that you develop should be the standard linear model 
# # and the random forest.
# # ==============================================================================
# Using Aggregated Beijing Data
Beijing <- data[-c(11, 13)] # 52584

#----------correlation-------------#
Beijing$year <- as.character(Beijing$year)
Beijing$month <- as.character(Beijing$month)
Beijing$day <- as.character(Beijing$day)
Beijing$hour <- as.character(Beijing$hour)
Beijing$season <- as.character(Beijing$season)
dummy <- dummyVars(" ~ .", data = Beijing)
Beijing <- data.frame(predict(dummy, newdata = Beijing))
remove(dummy)

library(corrplot)
result <- cor(data_removed)
corrplot(result, type = "upper", 
         tl.col = "black", tl.srt = 80)

#Average PM2.5 per season


#Average PM2.5 Day of Week
 
 
#Average PM2.5 working hours 


  