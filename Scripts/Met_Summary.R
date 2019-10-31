library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)

#setting paths
Plot.title = "HH115"
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_clean/", Plot.title, sep="")

setwd(path.out)

plot.month <- one_plot.month
plot.2017 <- read.csv(paste(Plot.title, "-2017", ".csv", sep=""))
plot.2018 <- read.csv(paste(Plot.title, "-2018", ".csv", sep=""))
plot.2019 <- read.csv(paste(Plot.title, "-2019",".csv", sep=""))
plot.all <- read.csv(paste(Plot.title, ".csv", sep=""))

#Creats a short table of Date and Temp to be checked for first and last frost
frost.check <- subset(plot.2019, select = c(2,7))
frost.check <- subset(frost.check, frost.check$Air_Temp <=0.5 & frost.check$Air_Temp >= -10)
View(frost.check)

str(plot.all)
plot.all$Date_Time <- as.POSIXct(plot.all$Date_Time)
summary(plot.all)

#Making a list of NA values so we know missing dates
Dates.missing <- plot.all[is.na(plot.all$Plot_Name),]

#Creating year column so years can be compared
plot.all <- plot.all %>% mutate(Year = as.character(year(Date_Time)))
plot.all <- plot.all %>% mutate(Date = format(Date_Time, format="%m/%d %I:%M:%S %p"))
plot.all$Date <- as.Date(plot.all$Date, format = "%m/%d %I:%M:%S %p")

# Changing data to a "long" format that ggplot likes for yearly summary
plot.stack <- stack(plot.year[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("Date_Time")] <- plot.year[,c("Date_Time")]
summary(plot.stack)

#Changing data to a "long" format that ggplot likes for total summary
plot.allstack <- stack(plot.all[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(plot.allstack) <- c("values", "var")
plot.allstack[,c("Year", "Date")] <- plot.all[,c("Year", "Date")]
summary(plot.allstack)

#Plot to compare years
ggplot(plot.allstack, aes(x = Date, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_smooth(aes(color=Year)) +
  theme_bw()

#Initial plot to just view the data as is over one year
ggplot(plot.stack, aes(x = Date_Time, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_line() +
  theme_bw()
