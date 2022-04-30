source('./jb957_v7_task1_data_prep.R')

library(dplyr)
library(lubridate)

savePNG <- TRUE

# ==============================================================================
# Task 2. Explore the relationships between air pollution at Beijing, 
# the meteorological variables, the time/type of day, season and year.
# ==============================================================================
# Analyse PM_Beijing ONLY, so drop Other weather stations

Beijing <- data[-c(11, 13)] # 52584

# drop all rows with PM_Beijing data is NA Records 
# (as there are too many >20k)
library(tidyverse)
Beijing <- Beijing %>% drop_na(PM_Beijing) # 20508 Rows

#exploratory analysis
print(summary(Beijing))

# make a histogram of to see if there are any months missing data
Beijing_stats <- Beijing %>% group_by(year,month) %>% tally()
Beijing_stats <- as.data.frame(Beijing_stats)
Beijing_stats$name <- paste(Beijing_stats[,1], Beijing_stats[,2], sep="-")

if (savePNG){png(filename = "figures/Beijing_dataFreq_pMonth.png",
                 width = 480, height = 480, units = "px", pointsize = 12)} 

barplot(Beijing_stats$n,
        main="Datum Frequency per Month\nBeijing 2013-2015",
        names.arg = Beijing_stats$name,
        xlab="Month Data collected")

if (savePNG){ dev.off() } 



# create correlation matrix of PM2 and attributes ?
library("Hmisc")
Beijing.rcorr <- rcorr(as.matrix(Beijing[11:18]))
Beijing.coeff = Beijing.rcorr$r
print(Beijing.coeff)

#install.packages("corrplot")
library(corrplot)
if (savePNG){png(filename = "figures/Beijing_correlation_matrix.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
corrplot(Beijing.coeff, method="ellipse")
corrplot(Beijing.coeff, method="number")

if (savePNG){ dev.off() } 

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

#---------scatter plots for continuous attributes--------#
# Lets see a colourised pairs scattergram 
# 1:yellow(low), 2:orange(high), 3:red(veryHigh), 
if (savePNG){png(filename = "figures/Beijing_pairs_scattergram.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

cols <- c(12:18)
pairs(
  Beijing[,cols],
  pch = 20,
  col = c("yellow", "orange", "red")[Beijing$Beijing_PMRange],
  upper.panel = panel.cor, 
  lower.panel = panel.lm
)

if (savePNG){ dev.off() } 

print(Beijing %>% group_by(Beijing_PMRange) %>% count())

if (savePNG){png(filename = "figures/Beijing_attributeVPM_scattergrams.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(2,3)) 
plot(Beijing$DEWP,Beijing$PM_Beijing,main="DEWP vs PM2.5\nBeijing 2013-2015")
plot(Beijing$HUMI,Beijing$PM_Beijing,main="HUMI vs PM2.5\nBeijing 2013-2015")
plot(Beijing$PRES,Beijing$PM_Beijing,main="PRES vs PM2.5\nBeijing 2013-2015")
plot(Beijing$TEMP,Beijing$PM_Beijing,main="TEMP vs PM2.5\nBeijing 2013-2015")
plot(Beijing$Iws,Beijing$PM_Beijing,main="Iws vs PM2.5\nBeijing 2013-2015")
plot(Beijing$precipitation,Beijing$PM_Beijing,main="Precipitation vs PM2.5\nBeijing 2013-2015")

if (savePNG){ dev.off() } 

# # ==============================================================================
# # Task 3. Present relevant visualisations of the data, which help to illustrate 
# # the relationships, trends and diﬀerences found in the previous items.
# # ==============================================================================
# Beijing only

#historgrams of Meteorological
library(ggplot2)
source('./user_functions.R')
p1 <- ggplot(data=Beijing, aes(PM_Beijing)) + geom_histogram(binwidth = 2) + labs(
  x = "PM2.5 Value",
  y = "Frequency",
  subtitle = "PM2.5\nBeijing 2013-2015")
p2 <- ggplot(data=Beijing, aes(DEWP)) + geom_histogram(binwidth = 2) + labs(
  x = "Temperature C",
  y = "Frequency",
  subtitle = "Dew Point\nBeijing 2013-2015")
p3 <- ggplot(data=Beijing, aes(HUMI)) + geom_histogram(binwidth = 2) + labs(
  x = "Humidity (%)",
  y = "Frequency",
  subtitle = "Humidity\nBeijing 2013-2015")
p4 <- ggplot(data=Beijing, aes(PRES)) + geom_histogram(binwidth = 2) + xlim(980,1050) + labs(
  x = "Pressure (hPa)",
  y = "Frequency",
  subtitle = "Barometric Pressure\nBeijing 2013-2015")
p5 <- ggplot(data=Beijing, aes(TEMP)) + geom_histogram(binwidth = 2) + labs(
  x = "Temperature Centigrade (C)",
  y = "Frequency",
  subtitle = "Temperature\nBeijing 2013-2015")
#note two maximum for temp
p6 <- ggplot(data=na.omit(Beijing), aes(cbwd)) + geom_histogram(stat = "count") + labs(
  x = "calm variable(cv), Northeast(NE), Northwest(NW), Southeast(SE)",
  y = "Frequency",
  subtitle = "Combined wind direction\nBeijing 2013-2015")

# now use multiplot to put them all on one page (requires user_functions.R)
if (savePNG){png(filename = "figures/Beijing_weather_readings.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

multiplot(p1, p2, p3, p4,p5,p6, cols=2)

if (savePNG){ dev.off() } 

rm(p1,p2,p3,p4,p5,p6)



#what distinct values are available
print(Beijing %>% group_by(cbwd) %>% tally())
print("so where is the SW wind data?")

# More Plots
if (savePNG){png(filename = "figures/Beijing_grouped_plots.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(3,1))    # set the plotting area into a 1*2 array
# # Iws: Cumulated wind speed (m/s)
(
  Iws_ByDayDirection <- Beijing %>% 
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
        main="Cumulated wind speed\nBeijing 2013-2015",
        names.arg = Iws_ByMonth$name,
        xlab="Cumulated wind speed (m/s)")


# # precipitation: hourly precipitation (mm)
precipitation_ByMonth <- Beijing %>% 
  group_by(year, month, day) %>% 
  summarise(
    max = max(precipitation),
    median = median(precipitation),
    mean = mean(precipitation)
  )
precipitation_ByMonth$name <- paste(precipitation_ByMonth$year, precipitation_ByMonth$month, sep="-")
barplot(precipitation_ByMonth$max,
        main="Precipitation\nBeijing 2013-2015",
        names.arg = precipitation_ByMonth$name,
        ylab="Precipitation (mm)")
# Note 3 period of higher precipitation
# much higher rain full in summer (wet season), than winter (Dry season)
# Therefore I PREDICT that high-rain Summers will correlate with low PM2.5 due 
# to a cleansing' effect.

# Iprec: Cumulated precipitation (mm)
# Lets plot PM in by group and see if our prediction is correct?
(
  Iprec_ByDay <- Beijing %>% 
    group_by(year, month, day) %>% 
    summarise(
      max = max(Iprec),
      median = median(Iprec),
      mean = mean(Iprec)
    )
)
Iprec_ByDay$name <- paste(Iprec_ByDay$year, Iprec_ByDay$month, sep="-")
barplot(Iprec_ByDay$max,
        main="Cumulated precipitation\nBeijing 2013-2015",
        names.arg = Iprec_ByDay$name,
        ylab="Cumulated precipitation (mm)")
# *** NOTE *** Iprec reflects a similar story to precipitation 

if (savePNG){ dev.off() } 


#Precipitation and Iprec are Higly correlated, therefore we can drop 
# Iprec from future analysis
Beijing <- Beijing[-c(18)] # 52584
rm(Beijing.coeff,Beijing_stats,Beijing.rcorr,
   Iprec_ByDay,Iws_ByDayDirection,Iws_ByMonth, precipitation_ByMonth)


#-------- bar plots for categorical columns ---------#
if (savePNG){png(filename = "figures/Beijing_catagorical_plots.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(1,3)) 
ymax <- 160
#year
(PM_ByYear <- Beijing %>% group_by(year) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
barplot(PM_ByYear$mean,
        main="Average PM2.5 p/year\nBeijing 2013-2015",
        names.arg = PM_ByYear$year,
        las=2,
        ylim = c(0,ymax),
        xlab="Years")
#seasons
(PM_BySeason <- Beijing %>% group_by(season) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
barplot(PM_BySeason$mean,
        main="Average PM2.5 p/season\nBeijing 2013-2015",
        names.arg = PM_BySeason$season,
        ylim = c(0,ymax),
        xlab="Season")   
#Month only, lets see how it looks when aggregated by month
(PM_ByMonth <- Beijing %>% group_by(month) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
barplot(PM_ByMonth$mean,
        main="Average PM2.5 p/month\nBeijing 2013-2015",
        names.arg = PM_ByMonth$month,
        ylim = c(0,ymax),
        xlab="Grouped by Month of the Year")

if (savePNG){ dev.off() } 
rm(PM_ByYear,PM_BySeason,PM_ByMonth)

if (savePNG){png(filename = "figures/Beijing_pm2.5_bygroup.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(2,1)) 
ymax <- 160
# In these graphs we limit the height to 150 PM2.5 to indicate that, many days
# exceed Very High category or PM2.5 Danger
#Year-month
(PM_ByMonth <- Beijing %>% group_by(year,month) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
PM_ByMonth$name <- paste(PM_ByMonth$year, PM_ByMonth$month, sep="-")
barplot(PM_ByMonth$mean,
        main="Average PM2.5 p/year and month\nBeijing 2013-2015",
        names.arg = PM_ByMonth$name,
        ylim = c(0,ymax),
        xpd = FALSE,
        las=2)
#year-month-day
(PM_ByDay <- Beijing %>% group_by(year,month,day) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
PM_ByDay$name <- paste(PM_ByDay$year, PM_ByDay$month,PM_ByDay$day, sep="-")
barplot(PM_ByDay$mean,
        main="Average PM2.5 p/year, month and day\nBeijing 2013-2015",
        names.arg = PM_ByDay$name,
        ylim = c(0,ymax),
        xpd = FALSE,
        las=2)  

if (savePNG){ dev.off() } 
rm(PM_ByMonth,PM_ByDay)

if (savePNG){png(filename = "figures/Beijing_pmByPeriodType.png",
                 width = 480, height = 480, units = "px", pointsize = 12)}

par(mfrow=c(1,2)) 
ymax <- 160
#weekdays
(PM_ByWeekday <- Beijing %>% group_by(weekday) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
barplot(PM_ByWeekday$mean,
        main="Avg PM2.5 p/weekday\nBeijing 2013-2015",
        names.arg = PM_ByWeekday$weekday,
        ylim = c(0,ymax),
        xlab="Day of the week")  
#working hours
(PM_ByWorkingHrs <- Beijing %>% group_by(w_hour) %>% summarise(
  mean = mean(PM_Beijing), 
  sd=sd(PM_Beijing), 
  n = n()))
barplot(PM_ByWorkingHrs$mean,
        main="Avg PM2.5 working vs not\nBeijing 2013-2015",
        names.arg = PM_ByWorkingHrs$w_hour,
        ylim = c(0,ymax),
        xlab="0 = Not Working, 1 = Working Hours")  
rm(PM_ByWeekday,PM_ByWorkingHrs,ymax,cols) 

if (savePNG){ dev.off() }


