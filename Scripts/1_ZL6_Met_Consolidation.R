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
path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.in <- paste(path.met, "Data_processed/Clean_data", sep="")

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

#Loop for pulling files in case there is more than 1
B127 <- data.frame()
for(i in 1:length(pull.B127)){
  date <- pull.B127[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_B127/B127_", date, ".csv"))
  #This step checks if there are two file and if there are then it removes the redudant column and sensor names
  if(i >1){
    file <- file[file$Port.1 != "ATMOS 41",]
    file <- file[file$z6.10460 != "Timestamp",]
  }
  B127 <- rbind(B127, file)
}

#Setting the column names to match what is the column names row of data
colnames(B127) <- B127[B127$z6.10460 == "Timestamp",]
#Removing old column and sensor labels
B127 <- B127[B127$` W/m² Solar Radiation` != "ATMOS 41",]
B127 <- B127[B127$Timestamp != "Timestamp",]

#Renaming to harmonize with old data
colnames(B127) <- c("Timestamp"	, "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", "Soil_Moisture", "Soil_Temp",
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")


B127.mod <- B127

B127.mod$Plot_Name <- "B127"

#Getting rid of redundant dates of data collection#
B127.mod <- B127.mod[!duplicated(B127.mod[c('Timestamp')]),]

#rearrangin column order to match other plots
B127.mod <- B127.mod[,c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                        "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "° X-axis Level",
                        "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", 
                        "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature", "Plot_Name")]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Timestamp=ts)
B127.mod$Timestamp <- as.POSIXct(B127.mod$Timestamp, format="%m/%d/%Y %H")
B127.mod.loop <- full_join(time_fill, B127.mod)

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

N115 <- data.frame()
for(i in 1:length(pull.N115)){
  date <- pull.N115[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_N115/N115_", date, ".csv"))
  if(i >1){
    #This step checks if there are two file and if there are then it removes the redudant column and sensor names
    file <- file[file$Port.1 != "ATMOS 41",]
    file <- file[file$z6.10464 != "Timestamp",]
  }
  N115 <- rbind(N115, file)
}

#Making the column names proper
colnames(N115) <- N115[N115$z6.10464 == "Timestamp",]
N115 <- N115[N115$` W/m² Solar Radiation` != "ATMOS 41",]
N115 <- N115[N115$Timestamp != "Timestamp",]


colnames(N115) <- c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", 
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")


N115.mod <- N115

N115.mod$Plot_Name <- "N115"

#Getting rid of redundant dates of data collection#
N115.mod <- N115.mod[!duplicated(N115.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Timestamp=ts)
N115.mod$Timestamp <- as.POSIXct(N115.mod$Timestamp, format="%m/%d/%Y %H")
N115.mod.loop <- full_join(time_fill, N115.mod)

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

HH115 <- data.frame()
for(i in 1:length(pull.HH115)){
  date <- pull.HH115[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_HH115/HH115_", date, ".csv"))
  if(i >1){
    #This step checks if there are two file and if there are then it removes the redudant column and sensor names
    file <- file[file$Port.1 != "ATMOS 41",]
    file <- file[file$z6.10461 != "Timestamp",]
  }
  HH115 <- rbind(HH115, file)
}

#Making the column names proper
colnames(HH115) <- HH115[HH115$z6.10461 == "Timestamp",]
HH115 <- HH115[HH115$` W/m² Solar Radiation` != "ATMOS 41",]
HH115 <- HH115[HH115$Timestamp != "Timestamp",]


colnames(HH115) <- c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                     "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "° X-axis Level",
                     "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", 
                     "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")


HH115.mod <- HH115

HH115.mod$Plot_Name <- "HH115"

#Getting rid of redundant dates of data collection#
HH115.mod <- HH115.mod[!duplicated(HH115.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Timestamp=ts)
HH115.mod$Timestamp <- as.POSIXct(HH115.mod$Timestamp, format="%m/%d/%Y %H")
HH115.mod.loop <- full_join(time_fill, HH115.mod)

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

U134 <- data.frame()
for(i in 1:length(pull.U134)){
  date <- pull.U134[i]
  file <- read.csv(paste0(path.met, "Data_raw/Meter_U134/U134_", date, ".csv"))
  if(i >1){
    #This step checks if there are two file and if there are then it removes the redudant column and sensor names
    file <- file[file$Port.1 != "ATMOS 41",]
    file <- file[file$z6.10465 != "Timestamp",]
  }
  U134 <- rbind(U134, file)
}

#Weird behavior with U134 thinkin we have a third sensor
U134 <- subset(U134, select = -c(Port.3, Port.3.1, Port.3.2, Port.4))

#Making the column names proper
colnames(U134) <- U134[U134$z6.10465 == "Timestamp",]
U134 <- U134[U134$` W/m² Solar Radiation` != "ATMOS 41",]
U134 <- U134[U134$Timestamp != "Timestamp",]


colnames(U134) <- c("Timestamp"	,"Soil_Moisture", "Soil_Temp", "PAR", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                    "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp",	"Relative_Humidity", "kPa Atmospheric Pressure", "° X-axis Level",
                    "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", 
                    "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")


U134.mod <- U134

U134.mod$Plot_Name <- "U134"

#Getting rid of redundant dates of data collection#
U134.mod <- U134.mod[!duplicated(U134.mod[c('Timestamp')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H"), na.rm = T)
Date.last <- max(as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H") + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Timestamp=ts)
U134.mod$Timestamp <- as.POSIXct(U134.mod$Timestamp, format="%m/%d/%Y %H")
U134.mod.loop <- full_join(time_fill, U134.mod)

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "/Data_processed/Harmonized_data/", sep="")
filename <- paste("U134.csv", sep = "")
write.csv(U134.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
