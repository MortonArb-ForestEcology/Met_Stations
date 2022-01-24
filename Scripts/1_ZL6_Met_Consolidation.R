#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: This is the primary script for the initial processing of Met station data from ZL6 loggers
#         To convert the raw data we receive from data loggers into consistent units and formats across plots and data loggers
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
# Outputs: B127.csv
#          U134.csv
#          N115.csv
#          HH115.csv
# Notes: 
#         
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting File paths
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
setwd(path.met)
path.out <- paste(path.met, "Data_Clean/Clean_data", sep="")

#This should hopefully become a loop but I don't want to structure it that way until we have the data and can see for sure
#--------------------------------#
#Only pulling what we need

#Finding the last date we have data for
old.B127 <- read.csv(file.path(path.out, "B127/B127.csv"))

old.B127$Date_Time <- as.POSIXct(strptime(old.B127$Date_Time, format="%Y-%m-%d %H"))

end.B127 <- max(old.B127$Date_Time, na.rm = T)

end.B127 <- sub(" .*", "", end.B127)

#Finding the files we need to update
dir.B127 <- dir(file.path(path.met, "Meter_B127"), ".csv")

split.B127 <- strsplit(dir.B127, "_")

split.B127 <- lapply(split.B127, function (x) x[2])

date.B127 <- unlist(lapply(split.B127, function (x) sub(".csv", "", x)))

date.B127 <- as.Date(date.B127)

pull.B127 <- date.B127[date.B127 >= end.B127]
#pull.B127 <- date.B127

B127 <- data.frame()
for(i in 1:length(pull.B127)){
  date <- pull.B127[i]
  file <- read.csv(paste0(path.met, "Meter_B127/B127_", date, ".csv"))
  B127 <- rbind(B127, file)
}

colnames(B127)
####Organizing the column names off Meter
colnames(B127) <- c("Time_ON"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"0 Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "0 X-axis Level",
                    "0 Y-axis Level", "mm/h Max Precip Rate", "0C RH Sensor Temp",	"kPa VPD", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "0C Logger Temperature")

B127.mod <- B127[3:nrow(B127),]

#Calculating the saturated vapour pressure to calculate relative humidty
B127.mod$Sat_vap_press <- .611*exp((17.502*as.numeric(B127.mod$Air_Temp))/(240.97+as.numeric(B127.mod$Air_Temp)))

B127.mod$Relative_Humidity <- (as.numeric(B127.mod$`kPa Vapor Pressure`)/B127.mod$Sat_vap_press)*100

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
#Finding the last date we have data for
old.U134 <- read.csv(file.path(path.out, "U134/U134.csv"))

old.U134$Date_Time <- as.POSIXct(strptime(old.U134$Date_Time, format="%Y-%m-%d %H"))

end.U134 <- max(old.U134$Date_Time, na.rm = T)

end.U134 <- sub(" .*", "", end.U134)

end.U134 <- "2021-07-03"

#Finding the files we need to update
dir.U134 <- dir(file.path(path.met, "Meter_U134"), ".csv")

split.U134 <- strsplit(dir.U134, "_")

split.U134 <- lapply(split.U134, function (x) x[2])

date.U134 <- unlist(lapply(split.U134, function (x) sub(".csv", "", x)))

date.U134 <- as.Date(date.U134)

pull.U134 <- date.U134[date.U134 >= end.U134]
#pull.U134 <- date.U134

U134 <- data.frame()
for(i in 1:length(pull.U134)){
  date <- pull.U134[i]
  file <- read.csv(paste0(path.met, "Meter_U134/U134_", date, ".csv"))
  U134 <- rbind(U134, file)
}


colnames(U134)
####Organizing the column names off Meter

colnames(U134) <- c("Time_ON"	, "Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"0 Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "0 X-axis Level",
                    "0 Y-axis Level", "mm/h Max Precip Rate", "0C RH Sensor Temp",	"kPa VPD", "AIR_2", "VAPOR_2", "Sensor Output",
                    "Atmos_2", "VPD_3" ,"% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "0C Logger Temperature")
U134.mod <- U134[3:nrow(U134),]

#Calculating the saturated vapour pressure to calculate relative humidty
U134.mod$Sat_vap_press <- .611*exp((17.502*as.numeric(U134.mod$Air_Temp))/(240.97+as.numeric(U134.mod$Air_Temp)))

U134.mod$Relative_Humidity <- (as.numeric(U134.mod$`kPa Vapor Pressure`)/U134.mod$Sat_vap_press)*100

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
#Finding the last date we have data for
old.N115 <- read.csv(file.path(path.out, "N115/N115.csv"))

old.N115$Date_Time <- as.POSIXct(strptime(old.N115$Date_Time, format="%Y-%m-%d %H"))

end.N115 <- max(old.N115$Date_Time, na.rm = T)

end.N115 <- sub(" .*", "", end.N115)

end.N115 <- "2021-07-03"


#Finding the files we need to update
dir.N115 <- dir(file.path(path.met, "Meter_N115"), ".csv")

split.N115 <- strsplit(dir.N115, "_")

split.N115 <- lapply(split.N115, function (x) x[2])

date.N115 <- unlist(lapply(split.N115, function (x) sub(".csv", "", x)))

date.N115 <- as.Date(date.N115)

pull.N115 <- date.N115[date.N115 >= end.N115]
#pull.N115 <- date.N115

N115 <- data.frame()
for(i in 1:length(pull.N115)){
  date <- pull.N115[i]
  file <- read.csv(paste0(path.met, "Meter_N115/N115_", date, ".csv"))
  N115 <- rbind(N115, file)
}

colnames(N115)
####Organizing the column names off Meter
colnames(N115) <- c("Time_ON"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"0 Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "0 X-axis Level",
                    "0 Y-axis Level", "mm/h Max Precip Rate", "0C RH Sensor Temp",	"kPa VPD", 
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "0C Logger Temperature")

N115.mod <- N115[3:nrow(N115),]

#Calculating the saturated vapour pressure to calculate relative humidty
N115.mod$Sat_vap_press <- .611*exp((17.502*as.numeric(N115.mod$Air_Temp))/(240.97+as.numeric(N115.mod$Air_Temp)))

N115.mod$Relative_Humidity <- (as.numeric(N115.mod$`kPa Vapor Pressure`)/N115.mod$Sat_vap_press)*100 

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
old.HH115 <- read.csv(file.path(path.out, "HH115/HH115.csv"))

old.HH115$Date_Time <- as.POSIXct(strptime(old.HH115$Date_Time, format="%Y-%m-%d %H"))

end.HH115 <- max(old.HH115$Date_Time, na.rm = T)

end.HH115 <- sub(" .*", "", end.HH115)

end.HH115 <- "2021-07-03"


#Finding the files we need to update
dir.HH115 <- dir(file.path(path.met, "Meter_HH115"), ".csv")

split.HH115 <- strsplit(dir.HH115, "_")

split.HH115 <- lapply(split.HH115, function (x) x[2])

date.HH115 <- unlist(lapply(split.HH115, function (x) sub(".csv", "", x)))

date.HH115 <- as.Date(date.HH115)

pull.HH115 <- date.HH115[date.HH115 >= end.HH115]
#pull.HH115 <- date.HH115

HH115 <- data.frame()
for(i in 1:length(pull.HH115)){
  date <- pull.HH115[i]
  file <- read.csv(paste0(path.met, "Meter_HH115/HH115_", date, ".csv"))
  HH115 <- rbind(HH115, file)
}

colnames(HH115)
####Organizing the column names off Meter
colnames(HH115) <- c("Time_ON"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"0 Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"kPa Vapor Pressure", "kPa Atmospheric Pressure", "0 X-axis Level",
                    "0 Y-axis Level", "mm/h Max Precip Rate", "0C RH Sensor Temp",	"kPa VPD", 
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "0C Logger Temperature")

HH115.mod <- HH115[3:nrow(HH115),]

#Calculating the saturated vapour pressure to calculate relative humidty
HH115.mod$Sat_vap_press <- .611*exp((17.502*as.numeric(HH115.mod$Air_Temp))/(240.97+as.numeric(HH115.mod$Air_Temp)))

HH115.mod$Relative_Humidity <- (as.numeric(HH115.mod$`kPa Vapor Pressure`)/HH115.mod$Sat_vap_press)*100

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

comb_plot <- rbind(N115.mod, HH115.mod, U134.mod)

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
  one_plot.loop$Relative_Humidity <- as.numeric(as.character(one_plot.loop$Relative_Humidity))
  one_plot.loop$Soil_Moisture <- as.numeric(as.character(one_plot.loop$Soil_Moisture))
  one_plot.loop$Soil_Temp <- as.numeric(as.character(one_plot.loop$Soil_Temp))
  one_plot.loop$Air_Temp <- as.numeric(as.character(one_plot.loop$Air_Temp))
  one_plot.loop$PAR <- as.numeric(as.character(one_plot.loop$PAR))
  
  
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
  
  old.plot <- old.plot[as.Date(old.plot$Date_Time) < first, ]
  
  old.plot$Date <- as.Date(old.plot$Date)
  one_plot.loop$Date <- as.Date(one_plot.loop$Date_Time)
  one_plot.loop$SIGFLAG <- NA
  
  final.plot <- rbind(old.plot, one_plot.loop)
  
  #Setting the path out to be in the corresponding folder
  path.fin <- paste(path.met, "Data_Clean/Harmonized_data/", PLOT, sep="")
  filename <- paste(PLOT, ".csv", sep = "")
  write.csv(final.plot, file.path(path.fin,  file = filename), row.names = FALSE)
  
}
