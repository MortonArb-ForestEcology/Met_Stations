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
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots"
path.clean <- file.path(path.met, "Data_processed/Clean_data")
path.raw <- file.path(path.met, "Data_raw")


# Setting up a function for renaming columns
source("0_MetHelperFunctions.R")

# colOrder <- c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"deg Wind Direction",
#               "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "deg X-axis Level",
#               "deg Y-axis Level", "mm/h Max Precip Rate", "degC RH Sensor Temp", 
#               "Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "degC Logger Temperature", "Plot_Name")

cols.final <- c("Plot_Name", "Timestamp", "Date", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPa.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")

# For when things go weird, we need to . instead of the slashes that were in some old files
colOrderGood <- gsub(" ", ".", colOrder)
colOrderGood <- gsub("-", ".", colOrderGood)
colOrderGood <- gsub("/", ".", colOrderGood)

sensorList <- c("ATMOS 41", "TEROS 11", "Battery", "Barometer")


#-------------------------------------------------#
# B127
#-------------------------------------------------#
#Reading in our old file with complete data
dir.old.B127 <- as.data.frame(dir(file.path(path.clean, "B127"), ".csv"))
colnames(dir.old.B127) <- "file"

# Finding a working file for the current year
path.B127 <-  dir.old.B127[stringr::str_detect(dir.old.B127$file, 'up_to'),]

# If no current file, pull the oldest one
if(length(path.B127)==0) path.B127 <- dir.old.B127[nrow(dir.old.B127),]
path.B127

old.B127 <- read.csv(file.path(path.clean, "B127", path.B127), na.strings=c("#N/A", "NA", ""))
summary(old.B127)
head(old.B127)
tail(old.B127)

# 2022 has a mix of year/date foramts!
# old.B127$Date_Time <- as.POSIXct(strptime(old.B127$Timestamp, format="%Y-%m-%d %H")) # This is causing
old.B127$Timestamp <- as.POSIXct(old.B127$Timestamp, tz="Etc/GMT+6") # This is causing
# summary(old.B127$Timestamp)
# head(old.B127)
# tail(old.B127)
# dim(old.B127)

# head(old.B127[is.na(old.B127$Timestamp),])
# old.B127[9000:9088,c("Timestamp", "Date_Time", "Date")]

summary(old.B127)
# tail(old.B127[is.na(old.B127$Date_Time),])

#Finding the last date we have data for
end.B127 <- max(old.B127$Timestamp, na.rm = T)

# end.B127 <- sub(" .*", "", end.B127)

#Finding the new files
dir.B127 <- dir(file.path(path.raw, "Meter_B127"), ".csv")

# #Pulling out a list of new dates to pull specific files
split.B127 <- strsplit(dir.B127, "_")
split.B127 <- lapply(split.B127, function (x) x[2])
date.B127 <- unlist(lapply(split.B127, function (x) sub(".csv", "", x)))
date.B127 <- as.Date(date.B127)

pull.B127 <- dir.B127[which(date.B127 > end.B127)]
pull.B127

#Loop for pulling files in case there is more than 1
B127new <- combineMetFiles(plotID="B127", pathPlot=file.path(path.raw, "Meter_B127"), filesPlot=pull.B127)
summary(B127new)

# Do some data cleaning -- skipping this step for now so we're better capturing what's going on
# B127Clean <- cleanMet(metData=B127new)

# Get rid of any old data
B127new <- B127new[B127new$Timestamp>end.B127,]
summary(B127new)

# Now combining new files with old files
if(lubridate::mday(end.B127)=="31" & lubridate::month(end.B127)==12 & lubridate::hour(end.B127)==23){
  B127all <- B127new
} else {
  B127all <- rbind(old.B127, B127new)
}
summary(B127all)

# Insert any missing timesteps
stampMin <- min(B127all$Timestamp, na.rm = T)
stampMax <- max(B127all$Timestamp, na.rm = T)
tsCheck <- data.frame(Timestamp=seq.POSIXt(stampMin, stampMax, by="hour", tz="Etc/GMT+6"))
tsCheck$Date <- as.Date(trunc(tsCheck$Timestamp, 'days'))
summary(tsCheck)

# Merge in missing dates/times
B127all <- merge(B127all, tsCheck, all=T)
B127all <- B127all[order(B127all$Timestamp),cols.final] # Putting everythign is a "good" order
summary(B127all)

# Checking to see if we need to finish off last year's file
yrsFile <- unique(lubridate::year(B127all$Timestamp))

# Start by writing all the data for the current/most recent year
rowsYrMax <- which(lubridate::year(B127all$Timestamp)==max(yrsFile))

PLOT="B127"
filename <- paste(PLOT,"_", max(yrsFile), "_up_to_" , max(B127all$Date), ".csv", sep = "")
write.csv(B127all[rowsYrMax,], file.path(path.clean, PLOT,  file = filename), row.names = FALSE)


# If we have >1 year, we need to create the final final for last year
if(rowsYrMax>1) stop("Get Christy to figure out what to do about past years!")


# head(B127)
# tail(B127)
# # B127[is.na(B127$Timestamp2),]
# 
# B127.mod <- B127
# 
# B127.mod$Plot_Name <- "B127" ## This is what gives Brendon errors
# 
# #Getting rid of redundant dates of data collection#
# B127.mod <- B127.mod[!duplicated(B127.mod[c('Timestamp')]),]
# 
# #rearranging column order to match other plots
# B127.mod <- B127.mod[, colOrderGood]
# summary(B127.mod)
# 
# #Adding in missing times so missing data can be seen
# #Defining the first and last date
# Date.first <- min(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
# Date.last <- max(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
# #Creating a sequence in between the dates and filling in gaps
# # ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
# #                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
# tsB127 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
# tsB127 <- as.POSIXct(tsB127,'%m/%d/%y %I:%M:%S %p')
# time_fillB127 <- data.frame(Timestamp=tsB127)
# B127.mod$Timestamp <- as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H")
# B127.mod.loop <- full_join(time_fillB127, B127.mod)
# tail(B127.mod.loop)
# # summary(B127.mod.loop)
# 
# #Setting the path out to be in the corresponding folder
# path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
# filename <- paste("B127.csv", sep = "")
# 
# write.csv(B127.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
# 
# #-------------------------------------------------#
# # N115
# #-------------------------------------------------#
# #Reading in our old file with complete data
# dir.old.N115 <- as.data.frame(dir(file.path(path.clean, "N115"), ".csv"))
# colnames(dir.old.N115) <- "file"
# 
# path.N115 <-  dir.old.N115[stringr::str_detect(dir.old.N115$file, 'up_to'),]
# 
# if(length(path.N115)==0) path.N115 <- dir.old.N115[nrow(dir.old.N115),]
# 
# old.N115 <- read.csv(file.path(path.clean, "N115", path.N115), na.strings=c("#N/A", "NA", ""))
# 
# old.N115$Date_Time <- as.POSIXct(strptime(old.N115$Timestamp, format="%Y-%m-%d %H"))
# 
# end.N115 <- max(old.N115$Date_Time, na.rm = T)
# 
# end.N115 <- sub(" .*", "", end.N115)
# 
# #Finding the files we need to update
# dir.N115 <- dir(file.path(path.met, "Data_raw/Meter_N115"), ".csv")
# 
# split.N115 <- strsplit(dir.N115, "_")
# 
# split.N115 <- lapply(split.N115, function (x) x[2])
# 
# #Pulling out a list of new dates to pull specific files
# date.N115 <- unlist(lapply(split.N115, function (x) sub(".csv", "", x)))
# 
# date.N115 <- as.Date(date.N115)
# 
# pull.N115 <- date.N115[date.N115 > end.N115]
# pull.N115
# 
# N115 <- data.frame()
# for(i in 1:length(pull.N115)){
#   date <- pull.N115[i]
#   fNow <- read.csv(paste0(path.met, "Data_raw/Meter_N115/N115_", date, ".csv"), na.strings=c("#N/A", "NA", ""))
# 
#   #Getting rid of columns without the sensors we want
#   fNow <- fNow[,c(grep("z6", names(fNow)), which(fNow[1,] %in% sensorList))]
#   # head(fNow)
#   
#   #Renaming to harmonize with old data
#   colnames(fNow) <- renameCols(fNow[fNow[grep("z6", names(fNow))] == "Timestamp",])
#   fNow <- fNow[-(grep("Records", fNow$Timestamp)),]
#   fNow <- fNow[-(grep("Timestamp", fNow$Timestamp)),]
# 
#   # For some reason a column was dropped at some point!  No bueno! need to add that column in
#   if(nrow(N115)>0 & !all(names(N115) %in% names(fNow))){
#     fNow[,names(N115)[!names(N115) %in% names(fNow)]] <- NA
#   }
#   
#   if(nrow(N115)>0){
#     N115 <- rbind(N115, fNow[,names(N115)])
#   } else {
#     N115 <- fNow
#   }
# }
# 
# head(N115)
# tail(N115)
# 
# 
# N115.mod <- N115
# 
# N115.mod$Plot_Name <- "N115"
# 
# #rearranging column order to match other plots
# N115.mod <- N115.mod[, colOrderGood]
# names(N115.mod)
# 
# #Getting rid of redundant dates of data collection#
# N115.mod <- N115.mod[!duplicated(N115.mod[c('Timestamp')]),]
# 
# #Adding in missing times so missing data can be seen
# #Defining the first and last date
# Date.first <- min(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
# Date.last <- max(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
# #Creating a sequence in between the dates and filling in gaps
# # ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
# #                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
# tsN115 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
# tsN115 <- as.POSIXct(tsN115,'%m/%d/%y %I:%M:%S %p')
# time_fillN115 <- data.frame(Timestamp=tsN115)
# N115.mod$Timestamp <- as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H")
# N115.mod.loop <- full_join(time_fillN115, N115.mod)
# head(N115.mod.loop)
# tail(N115.mod.loop)
# 
# #Setting the path out to be in the corresponding folder
# path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
# filename <- paste("N115.csv", sep = "")
# write.csv(N115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
# 
# 
# #-------------------------------------------------#
# # HH115
# #-------------------------------------------------#
# #Reading in our old file with complete data
# dir.old.HH115 <- as.data.frame(dir(file.path(path.clean, "HH115"), ".csv"))
# colnames(dir.old.HH115) <- "file"
# 
# path.HH115 <-  dir.old.HH115[stringr::str_detect(dir.old.HH115$file, 'up_to'),]
# 
# if(length(path.HH115)==0) path.HH115 <- dir.old.HH115[nrow(dir.old.HH115),]
# 
# old.HH115 <- read.csv(file.path(path.clean, "HH115", path.HH115), na.strings=c("#N/A", "NA", ""))
# 
# old.HH115$Date_Time <- as.POSIXct(strptime(old.HH115$Timestamp, format="%Y-%m-%d %H"))
# 
# end.HH115 <- max(old.HH115$Date_Time, na.rm = T)
# 
# end.HH115 <- sub(" .*", "", end.HH115)
# 
# #Finding the files we need to update
# dir.HH115 <- dir(file.path(path.met, "Data_raw/Meter_HH115"), ".csv")
# 
# split.HH115 <- strsplit(dir.HH115, "_")
# 
# split.HH115 <- lapply(split.HH115, function (x) x[2])
# 
# #Pulling out a list of new dates to pull specific files
# date.HH115 <- unlist(lapply(split.HH115, function (x) sub(".csv", "", x)))
# 
# date.HH115 <- as.Date(date.HH115)
# 
# pull.HH115 <- date.HH115[date.HH115 > end.HH115]
# pull.HH115 # Note: if there are NAs, it means there's a typo in the file name!
# 
# HH115 <- data.frame()
# for(i in 1:length(pull.HH115)){
#   date <- pull.HH115[i]
#   fNow <- read.csv(paste0(path.met, "Data_raw/Meter_HH115/HH115_", date, ".csv"), na.strings=c("#N/A", "NA", ""))
# 
#   #Getting rid of columns without the sensors we want
#   fNow <- fNow[,c(grep("z6", names(fNow)), which(fNow[1,] %in% sensorList))]
#   # head(fNow)
#   
#   #Renaming to harmonize with old data
#   colnames(fNow) <- renameCols(fNow[fNow[grep("z6", names(fNow))] == "Timestamp",])
#   fNow <- fNow[-(grep("Records", fNow$Timestamp)),]
#   fNow <- fNow[-(grep("Timestamp", fNow$Timestamp)),]
#   
#   # For some reason a column was dropped at some point!  No bueno! need to add that column in
#   if(nrow(HH115)>0 & !all(names(HH115) %in% names(fNow))){
#     fNow[,names(HH115)[!names(HH115) %in% names(fNow)]] <- NA
#   }
#   
#   if(nrow(HH115)>0){
#     HH115 <- rbind(HH115, fNow[,names(HH115)])
#   } else {
#     HH115 <- fNow
#   }
# }
# 
# head(HH115)
# tail(HH115)
# 
# HH115.mod <- HH115
# 
# HH115.mod$Plot_Name <- "HH115"
# 
# #rearranging column order to match other plots
# HH115.mod <- HH115.mod[, colOrderGood]
# names(HH115.mod)
# 
# 
# #Getting rid of redundant dates of data collection#
# HH115.mod <- HH115.mod[!duplicated(HH115.mod[c('Timestamp')]),]
# 
# #Adding in missing times so missing data can be seen
# #Defining the first and last date
# Date.first <- min(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
# Date.last <- max(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
# #Creating a sequence in between the dates and filling in gaps
# # ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
# #                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
# tsH115 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
# tsH115 <- as.POSIXct(tsH115,'%m/%d/%y %I:%M:%S %p')
# time_fillH115 <- data.frame(Timestamp=tsH115)
# HH115.mod$Timestamp <- as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H")
# HH115.mod.loop <- full_join(time_fillH115, HH115.mod)
# head(HH115.mod.loop)
# tail(HH115.mod.loop)
# 
# 
# #Setting the path out to be in the corresponding folder
# path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
# filename <- paste("HH115.csv", sep = "")
# write.csv(HH115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
# 
# 
# #-------------------------------------------------#
# # U134
# #-------------------------------------------------#
# #Reading in our old file with complete data
# dir.old.U134 <- as.data.frame(dir(file.path(path.clean, "U134"), ".csv"))
# colnames(dir.old.U134) <- "file"
# 
# path.U134 <-  dir.old.U134[stringr::str_detect(dir.old.U134$file, 'up_to'),]
# 
# if(length(path.U134)==0) path.U134 <- dir.old.U134[nrow(dir.old.U134),]
# 
# old.U134 <- read.csv(file.path(path.clean, "U134", path.U134), na.strings=c("#N/A", "NA", ""))
# head(old.U134)
# tail(old.U134)
# 
# old.U134$Date_Time <- as.POSIXct(strptime(old.U134$Timestamp, format="%Y-%m-%d %H"))
# 
# end.U134 <- max(old.U134$Date_Time, na.rm = T)
# 
# end.U134 <- sub(" .*", "", end.U134)
# 
# #Finding the files we need to update
# dir.U134 <- dir(file.path(path.met, "Data_raw/Meter_U134"), ".csv")
# 
# split.U134 <- strsplit(dir.U134, "_")
# 
# split.U134 <- lapply(split.U134, function (x) x[2])
# 
# #Pulling out a list of new dates to pull specific files
# date.U134 <- unlist(lapply(split.U134, function (x) sub(".csv", "", x)))
# 
# date.U134 <- as.Date(date.U134)
# 
# pull.U134 <- date.U134[date.U134 > end.U134]
# pull.U134
# 
# U134 <- data.frame()
# for(i in 1:length(pull.U134)){
#   date <- pull.U134[i]
#   fNow <- read.csv(paste0(path.met, "Data_raw/Meter_U134/U134_", date, ".csv"), na.strings=c("#N/A", "NA", ""))
#   
#   #Getting rid of columns without the sensors we want
#   fNow <- fNow[,c(grep("z6", names(fNow)), which(fNow[1,] %in% sensorList))]
#   # head(fNow)
#   
#   #Renaming to harmonize with old data
#   colnames(fNow) <- renameCols(fNow[fNow[grep("z6", names(fNow))] == "Timestamp",])
#   fNow <- fNow[-(grep("Records", fNow$Timestamp)),]
#   fNow <- fNow[-(grep("Timestamp", fNow$Timestamp)),]
#   
#   # For some reason a column was dropped at some point!  No bueno! need to add that column in
#   if(nrow(U134)>0 & !all(names(U134) %in% names(fNow))){
#     fNow[,names(U134)[!names(U134) %in% names(fNow)]] <- NA
#   }
#   
#   if(nrow(U134)>0){
#     U134 <- rbind(U134, fNow[,names(U134)])
#   } else {
#     U134 <- fNow
#   }
# }
# 
# head(U134)
# tail(U134)
# 
# U134.mod <- U134
# 
# U134.mod$Plot_Name <- "U134"
# 
# #rearranging column order to match other plots
# U134.mod <- U134.mod[, colOrderGood]
# names(U134.mod)
# head(U134.mod)
# 
# #Getting rid of redundant dates of data collection#
# U134.mod <- U134.mod[!duplicated(U134.mod[c('Timestamp')]),]
# 
# #Adding in missing times so missing data can be seen
# #Defining the first and last date
# Date.first <- min(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
# Date.last <- max(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
# #Creating a sequence in between the dates and filling in gaps
# # ts1 <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
# #                  as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
# tsU134 <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
# tsU134 <- as.POSIXct(tsU134,'%m/%d/%y %I:%M:%S %p')
# time_fillU134 <- data.frame(Timestamp=tsU134)
# U134.mod$Timestamp <- as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H")
# U134.mod.loop <- full_join(time_fillU134, U134.mod)
# head(U134.mod.loop)
# tail(U134.mod.loop)
# 
# #Setting the path out to be in the corresponding folder
# path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
# filename <- paste("U134.csv", sep = "")
# write.csv(U134.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
