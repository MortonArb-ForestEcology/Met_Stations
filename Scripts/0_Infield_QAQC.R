#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: To do an infield QAQC check after an intial data pull
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
# Outputs: Figures to evaluate that sensors are working properly
# Notes: You need to enter the plot number of the plot you are at
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#Setting File paths
path.met <- "C:/Users/lfitzpatrick/Documents/Meter/"

#This should hopefully become a loop but I don't want to structure it that way until we have the data and can see for sure
#--------------------------------#
#Only pulling what we need

PLOT <- "B127"

Today <- Sys.Date()

Today <- "2021-06-07"

start_plot <- read.csv(file.path(path.met, paste0(PLOT, "_", Today, ".csv")))
colnames(start_plot)
####Organizing the column names off Meter
colnames(start_plot) <- c("Time_ON"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "Relative_Humidity", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"kPa VPD", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")

start_plot$Plot_Name <- PLOT


start_plot$Date_Check <- as.POSIXct(strptime(start_plot$Time_ON, format="%m/%d/%Y %H"))
start_plot <- transform(start_plot, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing Meter labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
one_plot <- subset(start_plot, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#Consolidating the plot and fixing redundacies in Time
#Addressing daylight saving times issue (Time6 + Time5)
#Getting rid of extra time5 and time6 columns in front

#Getting rid of redundant dates of data collection#
one_plot <- one_plot[!duplicated(one_plot[c('Date_Check')]),]
one_plot <- one_plot[!is.na(one_plot[c('Date_Check')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.Date(one_plot$Date_Check), na.rm = T)
Date.last <- max(as.Date(one_plot$Date_Check) + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
one_plot['Date_Time'] <- lapply(one_plot['Date_Time'], as.POSIXct) 
one_plot.loop <- full_join(time_fill, one_plot)
one_plot.loop$Date_Check = NULL

#Arranging the columns so they are standard across plots
one_plot.loop <- one_plot.loop[c("Plot_Name", "Date_Time", "Soil_Moisture", "Relative_Humidity",
                                 "PAR", "Soil_Temp", "Air_Temp")]

#Making sure columns are of the right datatype
#You may get warning sof NA's but that is removing the rows of Meter that function as row names
one_plot.loop$Relative_Humidity <- as.numeric(one_plot.loop$Relative_Humidity)
one_plot.loop$Soil_Moisture <- as.numeric(one_plot.loop$Soil_Moisture)
one_plot.loop$Soil_Temp <- as.numeric(one_plot.loop$Soil_Temp)
one_plot.loop$Air_Temp <- as.numeric(one_plot.loop$Air_Temp)
one_plot.loop$PAR <- as.numeric(one_plot.loop$PAR)


#Marking NA values as NA
one_plot.loop[!is.na(one_plot.loop$Soil_Temp) & (one_plot.loop$Soil_Temp< -888 | one_plot.loop$Soil_Temp>999), "Soil_Temp"] <- NA
one_plot.loop[!is.na(one_plot.loop$Soil_Moisture) & (one_plot.loop$Soil_Moisture< -0.5| one_plot.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
one_plot.loop[!is.na(one_plot.loop$PAR) & (one_plot.loop$PAR< -888 | one_plot.loop$PAR>999), "PAR"] <- NA
one_plot.loop[!is.na(one_plot.loop$Air_Temp) & (one_plot.loop$Air_Temp< -888 | one_plot.loop$Air_Temp>999), "Air_Temp"] <- NA
one_plot.loop[!is.na(one_plot.loop$Relative_Humidity) & (one_plot.loop$Relative_Humidity< -888 | one_plot.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Removing date added to end of year files
one_plot.loop <- one_plot.loop[-nrow(one_plot.loop),]

#Removing duplicates from multiple measures or Daylight Savings
rows <- nrow(one_plot.loop)
for (i in 2:rows){
  Date.double <- one_plot.loop[i, "Date_Time"]
  Date.lag <- one_plot.loop[i - 1, "Date_Time"]
  if (Date.double == Date.lag){
    one_plot.loop %>% transform(Soil_Moisture = (Soil_Moisture + lag(Soil_Moisture))/2,
                                Relative_Humidity = (Relative_Humidity + lag(Relative_Humidity))/2,
                                PAR = (PAR + lag(PAR))/2,
                                Soil_Temp = (Soil_Temp + lag(Soil_Temp))/2,
                                Air_Temp = (Air_Temp + lag(Air_Temp))/2)
    #one_plot.loop <- one_plot.loop[-c(i),]
  }
}

one_plot.loop <- one_plot.loop[!is.na(one_plot.loop[c('Plot_Name')]),]

#-----------------------------------------------------------------#
Columns <- c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR")
plot.na <- data.frame()

set <- subset(one_plot.loop, select = c(Columns))
df.na <- set %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) 

df.na$Date_Time <- one_plot.loop[df.na$id, "Date_Time"]

df.na$Plot_Name <- PLOT


ggplot(df.na, aes(x = key, y = Date_Time, fill = isna)) +
  geom_tile(alpha=0.8) +
  ggtitle("NA's values") +
  scale_fill_discrete(name = "",
                      labels = c("Present", "Missing"))+
  coord_flip()

raw.stack <- stack(one_plot.loop[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(raw.stack) <- c("values", "var")
raw.stack[,c("Plot_Name", "Date_Time")] <- one_plot.loop[,c("Plot_Name", "Date_Time")]

#Raw Data plot
ggplot(raw.stack, aes(x = Date_Time, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_line(aes(color=Plot_Name)) +
  theme_bw()+
  ggtitle("Raw Met Station Data")
