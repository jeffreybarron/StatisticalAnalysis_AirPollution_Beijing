# project_jb957_v2.R
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
# precipitation: hourly precipitation (mm)
# Iprec: Cumulated precipitation (mm)

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

library(dplyr)
library(lubridate)

setwd("~/dev-local/UOW/info911/info911_A22_project")
data <- read.csv(file="BeijingPM20100101_20151231.csv") # 52584 Rows

#Remove Row number as it is not needed.
data <- data[-1] # 52584

# ==============================================================================
# Task 1. Creation of required fields for analysis 
# ==============================================================================
# Aggregate date columns into new datetime column and sort by datetime
# Note: Asia/Beijing is not a valid time code, Shanghai is the closest I think.
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

# Categorise PM2.5 into Low(0, <= 35), Medium(1, >35 & <= 150), High(2, >150) 
# per (Liang et al., 2015)
data <- data %>% mutate(data, Dongsi_PMRange = ifelse(
  PM_Dongsi %in% 0:35, 1,
  ifelse(PM_Dongsi %in% 36:149, 2, 
         ifelse(PM_Dongsi %in% 150:10000, 3, NA))))

data <- data %>% mutate(data, Dongsihuan_PMRange = ifelse(
  PM_Dongsihuan %in% 0:35, 1,
  ifelse(PM_Dongsihuan %in% 36:149, 2, 
         ifelse(PM_Dongsihuan %in% 150:10000, 3, NA))))

data <- data %>% mutate(data, Nongzhanguan_PMRange = ifelse(
  PM_Nongzhanguan %in% 0:35, 1,
  ifelse(PM_Nongzhanguan %in% 36:149, 2, 
         ifelse(PM_Nongzhanguan %in% 150:10000, 3, NA))))

data <- data %>% mutate(data, USPost_PMRange = ifelse(
  PM_US.Post %in% 0:35, 1,
  ifelse(PM_US.Post %in% 36:149, 2, 
         ifelse(PM_US.Post %in% 150:10000, 3, NA))))

#move new columns and cbwd, so its easier to work with column numbers later
data <- data %>% relocate(datetime, .after = hour)
data <- data %>% relocate(weekday, .after = datetime)
data <- data %>% relocate(w_hour, .after = weekday)
data <- data %>% relocate(cbwd, .after = season)
data <- data %>% relocate(Dongsi_PMRange, .after = cbwd)
data <- data %>% relocate(Dongsihuan_PMRange, .after = Dongsi_PMRange)
data <- data %>% relocate(Nongzhanguan_PMRange, .after = Dongsihuan_PMRange)
data <- data %>% relocate(USPost_PMRange, .after = Nongzhanguan_PMRange)
# PM Ranges are 10 through 13
# PM Values are 14 through 17)
# Meteorological attributes columns 18 through 24

#Replace all NA with zero in Meteorological attributes columns 18 through 24
data <- data %>% replace(is.na(c(18:24)), 0)

# ==============================================================================
# Task 2. Explore the relationships between air pollution at Dongsihuan, 
# the meteorological variables, the time/type of day, season and year.
# ==============================================================================
# Analyse PM_Dongishuan ONLY, so drop Other weather stations
dongishuan <- data[-c(10, 12:13,14,16:17)] # 52584

# drop all rows with PM_Dongsihuan data is NA Records 
# (as there are too many >20k)
library(tidyverse)
dongishuan <- dongishuan %>% drop_na(PM_Dongsihuan) # 20508 Rows

#exploratory analysis
summary(dongishuan)

# create correlation matrix of PM2 and attributes ?
library("Hmisc")
dongishuan.rcorr <- rcorr(as.matrix(dongishuan[11:18]))
dongishuan.coeff = dongishuan.rcorr$r
dongishuan.coeff
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
corrplot(dongishuan.coeff)
# Positive Correlates of interest
# Temp and DewP 0.7955    strong   
# Humi and DewP 0.6529    moderate 
# PM   and Humi 0.3929    weak     

# Precip and lPrec 0.5293 moderate, but uninteresting because 
# cumulative precipitation is expected to correlate with precipitation.

# Negative Correlates of interest
# Pres and Temp -0.8206   strong
# Pres and DewP -0.7712   strong
# Iws and HUMI  -0.3094   weak
# Dewp and Iws  -0.2908   weak

# Notable Correlates with PM
# PM and Humi   0.3944    weak Pos
# PM and Iws    -0.2345   weak Neg
# PM and TEMP   -0.1805   weak neg

(dongishuan %>% group_by(Dongsihuan_PMRange) %>% count())

# Lets see a colourised pairs scattergram matrix
# 1:green, 2:yellow, 3:red
cols <- c(12:18)
layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
pairs(
  dongishuan[,cols],
  pch = 20,
  col = c("green", "yellow", "red")[dongishuan$Dongsihuan_PMRange],
)
par(mar=c(0, 0, 0, 0))
legend('center','groups',
       c(1,2,3),
       pch = c(20,20,20),
       col = c('green','yellow','red'),
       ncol=3,bty ="n")

install.packages("GGally")
library(GGally)
ggpairs(dongishuan, 
        columns = 1:4, 
        aes(color = Dongsihuan_PMRange, alpha = 0.5),
        lower = list(continuous = "smooth")) 




hist(dongishuan$Dongsihuan_PMRange,
     xlim=c(0,2),
     breaks = c(0,1,2))


# pairs(
#   dongishuan[,cols],
#   col = c("green", "orange", "red")[dongishuan$Dongsihuan_PMRange],
#   main = "ScatterPlot Matrix, selected attributes",
#   upper.panel = panel.cor, lower.panel = panel.smooth,
#   pch=20,
# )


# # ==============================================================================
# # Task 3. Present relevant visualisations of the data, which help to illustrate 
# # the relationships, trends and diﬀerences found in the previous items.
# # ==============================================================================
# # Dongishuan only
# 
# #historgrams of Meteorological 
# library(ggplot2)
# ggplot(data=dongishuan, aes(PM_Dongsihuan)) + geom_histogram(binwidth = 2)
# ggplot(data=dongishuan, aes(DEWP)) + geom_histogram(binwidth = 2)
# ggplot(data=dongishuan, aes(HUMI)) + geom_histogram(binwidth = 2)
# ggplot(data=dongishuan, aes(PRES)) + geom_histogram(binwidth = 2) + xlim(980,1050)
# ggplot(data=dongishuan, aes(TEMP)) + geom_histogram(binwidth = 2)
# 
# #there are some values that were NA and converted to 0
# #perhaps these should be deleted
# ggplot(data=dongishuan, aes(cbwd)) + geom_histogram(stat = "count")
# #what disctinct values are available
# dongishuan %>% group_by(cbwd) %>% tally()
# 
# # Iws: Cumulated wind speed (m/s)
# # winds over 100m/sec equate to 360km/per winds 
# ggplot(data=dongishuan, aes(Iws)) + geom_histogram(binwidth = 2)
# 
# # precipitation: hourly precipitation (mm)
# plot(dongishuan$precipitation)
# 
# #shown in in aggregrate for each month
# dongishuan_rain <- dongishuan %>% group_by(month) %>% tally(precipitation)
# ggplot(data=dongishuan_rain, aes(x=month, y=n)) + geom_point() + scale_x_continuous(
#   breaks = seq(1, 12, 1), limits=c(1, 12))
# # much higher rain full in summer (wet season), than winter (Dry season)
# # I predict that high rain will correlate with low PM2.5 due to a 'cleansing' effect.
# 
# # Iprec: Cumulated precipitation (mm)
# # reflects a similar story to precipitation which is understandable.
# ggplot(data=dongishuan, aes(Iprec)) + geom_histogram(binwidth = 2)
# 
# 
# 
# # make a histogram of rows per day and see if there are any periods missing
# dongishuan_stats <- dongishuan %>% group_by(year,month) %>% tally()
# # after deleting na records we can see that data is not collected until march 2013
# # thus we will need to drop 2013 as the missing field in this season may skew data.
# # or we can drop rows from march 2015 
# plot(dongishuan_stats$n,type = 'h')
# # based on this plot there is a significant drop in rows so I would prefer to 
# # drop rows from March 2015 an keep the 2013 rows.





# ==============================================================================
# EXTRA CODE


# # get the season start and end date each year
# dongishuan_four_seasons <- dongishuan %>% 
#   select(year,month, season) %>% 
#   group_by(year, season) %>% 
#   summarise(
#     begin = min(month),
#     end = max(month)) %>%
#   arrange(year)
# # from the above we learn the builtin seasons are 
# # 1 Mar to May  Spring
# # 2 Jun to Aug  Summer
# # 3 Sep to Nov  Autumn
# # 4 Dec to Feb  Winter

### relationships between meteorological attributes

# correlation between attributes 

#correlation with pm2.5





