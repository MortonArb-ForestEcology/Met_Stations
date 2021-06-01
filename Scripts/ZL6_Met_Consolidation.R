#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: This is a temporary script that will be used once all of the data loggers and sensors are updated
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
# Outputs: B127.csv
#          U134.csv
#          N115.csv
#          HH115.csv
# Notes: This script should be broken into two seperate scripts for the two seperate types of data loggers as we go forward
#        10/29/2020 is when the Meter data loggers were installed
#         
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting File paths
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
setwd(path.met)
path.out <- paste(path.met, "Data_Clean", sep="")

#This should hopefully become a loop but I don't want to structure it that way until we have the data and can see for sure
#--------------------------------#
#Only pulling what we need

#Finding the last date we have data for
old.B127 <- read.csv(file.path(path.out, "B127/B127.csv"))

end.B127 <- max(old.B127$Date_Time, na.rm = T)

end.B127 <- sub(" .*", "", end.B127)


#Finding the files we need to update
dir.B127 <- dir(file.path(path.met, "Meter_B127"), ".csv")

split.B127 <- strsplit(dir.B127, "_")

split.B127 <- lapply(split.B127, function (x) x[2])

date.B127 <- unlist(lapply(split.B127, function (x) sub(".csv", "", x)))

date.B127 <- as.Date(date.B127)

#pull.B127 <- date.B127[date.B127 >= end.B127]
pull.B127 <- date.B127

B127 <- data.frame()
for(i in 1:length(pull.B127)){
  date <- pull.B127[i]
  file <- read.csv(paste0(path.met, "Meter_B127/B127_", date, ".csv"))
  B127 <- rbind(B127, file)
}

colnames(B127)
####Organizing the column names off Meter
colnames(B127) <- c("Time_ON"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "Relative_Humidity", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"kPa VPD", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")
B127.mod <- B127

Plot.title <- "B127"
B127.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(B127.mod)

B127.mod$Date_Check <- as.POSIXct(strptime(B127.mod$Time_ON, format="%m/%d/%Y %H"))
B127.mod <- transform(B127.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing Meter labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
B127.mod <- subset(B127.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#-----------------------------------------------------------------#
U134 <-read_bulk(directory = "Meter_U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))
colnames(U134)
####Organizing the column names off Meter
colnames(U134) <- c("Time1"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"Relative_Humidity", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature", "File_Name", "Time2", "Time3", "Time4")

U134.mod <- U134 %>% mutate(Time3 = ifelse(is.na(Time3), Time4, Time3),
                            Time2 = ifelse(is.na(Time2), Time3, Time2),
                            Time_ON = ifelse(is.na(Time1), Time2, Time1))

Plot.title <- "U134"
U134.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(U134.mod)

U134.mod$Date_Check <- as.POSIXct(strptime(U134.mod$Time_ON, format="%m/%d/%Y %H"))
U134.mod <- transform(U134.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing Meter labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
U134.mod <- subset(U134.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#-----------------------------------------------------------------#
N115 <-read_bulk(directory = "Meter_N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))
colnames(N115)
####Organizing the column names off Meter
colnames(N115) <- c("Time1"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"Relative_Humidity", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature", "File_Name", "Time2", "Time3", "Time4")

N115.mod <- N115 %>% mutate(Time3 = ifelse(is.na(Time3), Time4, Time3),
                            Time2 = ifelse(is.na(Time2), Time3, Time2),
                            Time_ON = ifelse(is.na(Time1), Time2, Time1))

Plot.title <- "N115"
N115.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(N115.mod)

N115.mod$Date_Check <- as.POSIXct(strptime(N115.mod$Time_ON, format="%m/%d/%Y %H"))
N115.mod <- transform(N115.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing Meter labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
N115.mod <- subset(N115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#-----------------------------------------------------------------#
HH115 <-read_bulk(directory = "Meter_HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))
colnames(HH115)
####Organizing the column names off Meter
colnames(HH115) <- c("Time1"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"Relative_Humidity", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature", "File_Name", "Time2", "Time3", "Time4")

HH115.mod <- HH115 %>% mutate(Time3 = ifelse(is.na(Time3), Time4, Time3),
                            Time2 = ifelse(is.na(Time2), Time3, Time2),
                            Time_ON = ifelse(is.na(Time1), Time2, Time1))

Plot.title <- "HH115"
HH115.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(HH115.mod)

HH115.mod$Date_Check <- as.POSIXct(strptime(HH115.mod$Time_ON, format="%m/%d/%Y %H"))
HH115.mod <- transform(HH115.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing Meter labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
HH115.mod <- subset(HH115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))
#------------------------------------------------------------------#

comb_plot <- rbind(B127.mod, N115.mod, HH115.mod, U134.mod)

for(PLOT in unique(comb_plot$Plot_Name)){
  one_plot <- comb_plot[comb_plot$Plot_Name == PLOT,]
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
  
  one_plot.loop$Date_Time <- as.character(one_plot.loop$Date_Time)
  
  old.plot <- read.csv(file.path(path.out, PLOT, paste0(PLOT,".csv")))
  
  first <- min(one_plot.loop$Date_Time)
  
  old.plot <- old.plot[old.plot$Date_Time < first, ]
  
  final.plot <- rbind(old.plot, one_plot.loop)
  
  #Setting the path out to be in the corresponding folder
  path.out <- paste(path.met, "Data_Clean/", PLOT, sep="")
  filename <- paste(PLOT, ".csv", sep = "")
  write.csv(final.plot, file.path(path.out,  file = filename), row.names = FALSE)
  
}
