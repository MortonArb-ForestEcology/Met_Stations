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
library(dplyr)
library(lubridate)
library(tidyr)


#Setting File paths
# path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.in <- paste(path.met, "Data_processed/Clean_data", sep="")


# Setting up a function for renaming columns
renameCols <- function(x){
  vecCols <- x
  vecCols[grep("Solar Radiation", vecCols, useBytes = T)] <- "PAR"
  vecCols[grep("mm Precipitation", vecCols, useBytes = T)] <- "mm Precipitation"
  vecCols[grep("Lightning Activity", vecCols, useBytes = T)] <- "Lightning Activity"
  vecCols[grep("Lightning Distance", vecCols, useBytes = T)] <- "km Lightning Distance"
  vecCols[grep("Wind Direction", vecCols, useBytes = T)] <- "deg Wind Direction"
  vecCols[grep("Wind Speed", vecCols, useBytes = T)] <- "m/s Wind Speed"
  vecCols[grep("Gust Speed", vecCols, useBytes = T)] <- "m/s Gust Speed"
  vecCols[grep("Air Temp", vecCols, useBytes = T)] <- "Air_Temp"
  vecCols[grep("Air_Temp", vecCols, useBytes = T)] <- "Air_Temp" # Putting these in just in case there's some extraneous wonk
  vecCols[grep("Vapor Press", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Relative Humidity", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Relative_Humidity", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Atmospheric Press", vecCols, useBytes = T)] <- "kPa Atmospheric Pressure"
  vecCols[grep("X-axis", vecCols, useBytes = T)] <- "deg X-axis Level"
  vecCols[grep("Y-axis", vecCols, useBytes = T)] <- "deg Y-axis Level"
  vecCols[grep("Max Precip", vecCols, useBytes = T)] <- "mm/h Max Precip Rate"
  vecCols[grep("RH Sensor Temp", vecCols, useBytes = T)] <- "degC RH Sensor Temp"
  vecCols[grep("Water Content", vecCols, useBytes = T)] <- "Soil_Moisture"
  vecCols[grep("Soil_Moisture", vecCols, useBytes = T)] <- "Soil_Moisture"
  vecCols[grep("Soil Temp", vecCols, useBytes = T)] <- "Soil_Temp"
  vecCols[grep("Soil_Temp", vecCols, useBytes = T)] <- "Soil_Temp"
  vecCols[grep("Battery Percent", vecCols, useBytes = T)] <- "Battery Percent"
  vecCols[grep("Battery Voltage", vecCols, useBytes = T)] <- "mV Battery Voltage"
  vecCols[grep("Reference Press", vecCols, useBytes = T)] <- "kPa Reference Pressure"
  vecCols[grep("Logger Temp", vecCols, useBytes = T)] <- "degC Logger Temperature"
  
  return(vecCols)
}

colOrder <- c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"deg Wind Direction",
              "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "deg X-axis Level",
              "deg Y-axis Level", "mm/h Max Precip Rate", "degC RH Sensor Temp", 
              "Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "degC Logger Temperature", "Plot_Name")

sensorList <- c("ATMOS 41", "TEROS 11", "Battery", "Barometer")


#-------------------------------------------------#
# B127
#-------------------------------------------------#
#Reading in our old file with complete data
dir.old.B127 <- as.data.frame(dir(file.path(path.in, "B127"), ".csv"))
colnames(dir.old.B127) <- "file"

path.B127 <-  dir.old.B127[stringr::str_detect(dir.old.B127$file, 'up_to'),]

old.B127 <- read.csv(file.path(path.in, "B127", path.B127))

old.B127$Date_Time <- as.POSIXct(strptime(old.B127$Date_Time, format="%Y-%m-%d %H"))

#Finding the last date we have data for
end.B127 <- max(old.B127$Date_Time, na.rm = T)

end.B127 <- sub(" .*", "", end.B127)

#Finding the new files
dir.B127 <- dir(file.path(path.met, "Data_raw/Meter_B127"), ".csv")

split.B127 <- strsplit(dir.B127, "_")

split.B127 <- lapply(split.B127, function (x) x[2])

#Pulling out a list of new dates to pull specific files
date.B127 <- unlist(lapply(split.B127, function (x) sub(".csv", "", x)))

date.B127 <- as.Date(date.B127)

pull.B127 <- date.B127[date.B127 > end.B127]
pull.B127

#Loop for pulling files in case there is more than 1
B127 <- data.frame()
for(i in 1:length(pull.B127)){
  date <- pull.B127[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_B127/B127_", date, ".csv"))
  
  #Getting rid of columns without the sensors we want
  file <- file[,c(grep("z6", names(file)), which(file[1,] %in% sensorList))]
  # head(file)
  
  #Renaming to harmonize with old data
  colnames(file) <- renameCols(file[file[grep("z6", names(file))] == "Timestamp",])
  file <- file[-(grep("Records", file$Timestamp)),]
  file <- file[-(grep("Timestamp", file$Timestamp)),]
  
  B127 <- rbind(B127, file)
}


head(B127)
tail(B127)

B127.mod <- B127

B127.mod$Plot_Name <- "B127" ## This is what gives Brendon errors

#Getting rid of redundant dates of data collection#
B127.mod <- B127.mod[!duplicated(B127.mod[c('Timestamp')]),]

#rearranging column order to match other plots
B127.mod <- B127.mod[, colOrder]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
# ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
#                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
tsB127 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
tsB127 <- as.POSIXct(tsB127,'%m/%d/%y %I:%M:%S %p')
time_fillB127 <- data.frame(Timestamp=tsB127)
B127.mod$Timestamp <- as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H")
B127.mod.loop <- full_join(time_fillB127, B127.mod)

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
filename <- paste("B127.csv", sep = "")
write.csv(B127.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)

#-------------------------------------------------#
# N115
#-------------------------------------------------#
#Reading in our old file with complete data
dir.old.N115 <- as.data.frame(dir(file.path(path.in, "N115"), ".csv"))
colnames(dir.old.N115) <- "file"

path.N115 <-  dir.old.N115[stringr::str_detect(dir.old.N115$file, 'up_to'),]

old.N115 <- read.csv(file.path(path.in, "N115", path.N115))

old.N115$Date_Time <- as.POSIXct(strptime(old.N115$Date_Time, format="%Y-%m-%d %H"))

end.N115 <- max(old.N115$Date_Time, na.rm = T)

end.N115 <- sub(" .*", "", end.N115)

#Finding the files we need to update
dir.N115 <- dir(file.path(path.met, "Data_raw/Meter_N115"), ".csv")

split.N115 <- strsplit(dir.N115, "_")

split.N115 <- lapply(split.N115, function (x) x[2])

#Pulling out a list of new dates to pull specific files
date.N115 <- unlist(lapply(split.N115, function (x) sub(".csv", "", x)))

date.N115 <- as.Date(date.N115)

pull.N115 <- date.N115[date.N115 > end.N115]
pull.N115

N115 <- data.frame()
for(i in 1:length(pull.N115)){
  date <- pull.N115[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_N115/N115_", date, ".csv"))

  #Getting rid of columns without the sensors we want
  file <- file[,c(grep("z6", names(file)), which(file[1,] %in% sensorList))]
  # head(file)
  
  #Renaming to harmonize with old data
  colnames(file) <- renameCols(file[file[grep("z6", names(file))] == "Timestamp",])
  file <- file[-(grep("Records", file$Timestamp)),]
  file <- file[-(grep("Timestamp", file$Timestamp)),]

  N115 <- rbind(N115, file)
}

head(N115)
tail(N115)


N115.mod <- N115

N115.mod$Plot_Name <- "N115"

#rearranging column order to match other plots
N115.mod <- N115.mod[, colOrder]
names(N115.mod)

#Getting rid of redundant dates of data collection#
N115.mod <- N115.mod[!duplicated(N115.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
# ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
#                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
tsN115 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
tsN115 <- as.POSIXct(tsN115,'%m/%d/%y %I:%M:%S %p')
time_fillN115 <- data.frame(Timestamp=tsN115)
N115.mod$Timestamp <- as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H")
N115.mod.loop <- full_join(time_fillN115, N115.mod)

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
filename <- paste("N115.csv", sep = "")
write.csv(N115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)


#-------------------------------------------------#
# HH115
#-------------------------------------------------#
#Reading in our old file with complete data
dir.old.HH115 <- as.data.frame(dir(file.path(path.in, "HH115"), ".csv"))
colnames(dir.old.HH115) <- "file"

path.HH115 <-  dir.old.HH115[stringr::str_detect(dir.old.HH115$file, 'up_to'),]

old.HH115 <- read.csv(file.path(path.in, "HH115", path.HH115))

old.HH115$Date_Time <- as.POSIXct(strptime(old.HH115$Date_Time, format="%Y-%m-%d %H"))

end.HH115 <- max(old.HH115$Date_Time, na.rm = T)

end.HH115 <- sub(" .*", "", end.HH115)

#Finding the files we need to update
dir.HH115 <- dir(file.path(path.met, "Data_raw/Meter_HH115"), ".csv")

split.HH115 <- strsplit(dir.HH115, "_")

split.HH115 <- lapply(split.HH115, function (x) x[2])

#Pulling out a list of new dates to pull specific files
date.HH115 <- unlist(lapply(split.HH115, function (x) sub(".csv", "", x)))

date.HH115 <- as.Date(date.HH115)

pull.HH115 <- date.HH115[date.HH115 > end.HH115]
pull.HH115 # Note: if there are NAs, it means there's a typo in the file name!

HH115 <- data.frame()
for(i in 1:length(pull.HH115)){
  date <- pull.HH115[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_HH115/HH115_", date, ".csv"))

  #Getting rid of columns without the sensors we want
  file <- file[,c(grep("z6", names(file)), which(file[1,] %in% sensorList))]
  # head(file)
  
  #Renaming to harmonize with old data
  colnames(file) <- renameCols(file[file[grep("z6", names(file))] == "Timestamp",])
  file <- file[-(grep("Records", file$Timestamp)),]
  file <- file[-(grep("Timestamp", file$Timestamp)),]
  
  HH115 <- rbind(HH115, file)
}

head(HH115)
tail(HH115)

HH115.mod <- HH115

HH115.mod$Plot_Name <- "HH115"

#rearranging column order to match other plots
HH115.mod <- HH115.mod[, colOrder]
names(HH115.mod)


#Getting rid of redundant dates of data collection#
HH115.mod <- HH115.mod[!duplicated(HH115.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
# ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
#                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
tsH115 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
tsH115 <- as.POSIXct(tsH115,'%m/%d/%y %I:%M:%S %p')
time_fillH115 <- data.frame(Timestamp=tsH115)
HH115.mod$Timestamp <- as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H")
HH115.mod.loop <- full_join(time_fillH115, HH115.mod)
# head(HH115.mod.loop)

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
filename <- paste("HH115.csv", sep = "")
write.csv(HH115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)


#-------------------------------------------------#
# U134
#-------------------------------------------------#
#Reading in our old file with complete data
dir.old.U134 <- as.data.frame(dir(file.path(path.in, "U134"), ".csv"))
colnames(dir.old.U134) <- "file"

path.U134 <-  dir.old.U134[stringr::str_detect(dir.old.U134$file, 'up_to'),]

old.U134 <- read.csv(file.path(path.in, "U134", path.U134))

old.U134$Date_Time <- as.POSIXct(strptime(old.U134$Date_Time, format="%Y-%m-%d %H"))

end.U134 <- max(old.U134$Date_Time, na.rm = T)

end.U134 <- sub(" .*", "", end.U134)

#Finding the files we need to update
dir.U134 <- dir(file.path(path.met, "Data_raw/Meter_U134"), ".csv")

split.U134 <- strsplit(dir.U134, "_")

split.U134 <- lapply(split.U134, function (x) x[2])

#Pulling out a list of new dates to pull specific files
date.U134 <- unlist(lapply(split.U134, function (x) sub(".csv", "", x)))

date.U134 <- as.Date(date.U134)

pull.U134 <- date.U134[date.U134 > end.U134]
pull.U134

U134 <- data.frame()
for(i in 1:length(pull.U134)){
  date <- pull.U134[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_U134/U134_", date, ".csv"))
  
  #Getting rid of columns without the sensors we want
  file <- file[,c(grep("z6", names(file)), which(file[1,] %in% sensorList))]
  # head(file)
  
  #Renaming to harmonize with old data
  colnames(file) <- renameCols(file[file[grep("z6", names(file))] == "Timestamp",])
  file <- file[-(grep("Records", file$Timestamp)),]
  file <- file[-(grep("Timestamp", file$Timestamp)),]
  
  U134 <- rbind(U134, file)
}

head(U134)
tail(U134)

U134.mod <- U134

U134.mod$Plot_Name <- "U134"

#rearranging column order to match other plots
U134.mod <- U134.mod[, colOrder]
names(U134.mod)


#Getting rid of redundant dates of data collection#
U134.mod <- U134.mod[!duplicated(U134.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
# ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
#                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
tsU134 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
tsU134 <- as.POSIXct(tsU134,'%m/%d/%y %I:%M:%S %p')
time_fillU134 <- data.frame(Timestamp=tsU134)
U134.mod$Timestamp <- as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H")
U134.mod.loop <- full_join(time_fillU134, U134.mod)

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
filename <- paste("U134.csv", sep = "")
write.csv(U134.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
