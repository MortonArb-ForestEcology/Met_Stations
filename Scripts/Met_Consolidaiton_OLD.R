#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: Old script from when we had to transition betweeen two different types of data loggers. Shouldn't be needed anymore
#         To convert the raw data we receive from data loggers into consistent units and formats across plots and data loggers
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
# Outputs: B127.csv
#          U134.csv
#          N115.csv
#          HH115.csv
# Notes: This script should be broken into two seperate scripts for the two seperate types of data loggers as we go forward
#        10/29/2020 is when the Meter data loggers were installed
#-----------------------------------------------------------------------------------------------------------------------------------#

library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting File paths
path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/Data_raw/"
setwd(path.met)

#--------------------------------#
#Consolidating B127 data
B127.HB <-read.csv("../Data_processed/OLD_Onset/B127.csv") # Combine all data
#B127.HB$File_Name <- "Onset"

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
B127.MET <- read.csv(paste0(path.met, "Data_Backup/B127_backup.csv"))

#Removing the sensor label column
B127.MET <- B127.MET[B127.MET$Port.1 != "ATMOS 41",]

#Making the column names proper
colnames(B127.MET) <- B127.MET[B127.MET$z6.10460 == "Timestamp",]

#Removing the old row of column names
B127.MET <- B127.MET[B127.MET$Timestamp != "Timestamp",]

B127.MET$Plot_Name <- "B127"


B127.MET$` °C Air Temperature` <- as.numeric(as.character(B127.MET$` °C Air Temperature`))

B127.MET$` RH Relative Humidity` <- as.numeric(as.character(B127.MET$` RH Relative Humidity`))*100  

B127.MET$` W/m² Solar Radiation` <- as.numeric(as.character(B127.MET$` W/m² Solar Radiation`)) 

B127.MET$` m³/m³ Water Content` <- as.numeric(as.character(B127.MET$` m³/m³ Water Content`))

B127.MET$` °C Soil Temperature` <- as.numeric(as.character(B127.MET$` °C Soil Temperature`))
  
B127.MET$Date_Time <- as.POSIXct(strptime(B127.MET$Timestamp, format="%m/%d/%Y %H"))
B127.HB$Date_Time <- as.POSIXct(B127.HB$Date_Time)


B127.mod <- full_join(B127.HB, B127.MET)

#Checking columns to delete are correct for next lines
colnames(B127.mod)

#Making sure we use the new meter data when we run out of hoboware
B127.mod$Soil_Moisture <- ifelse(is.na(B127.mod$` m³/m³ Water Content`), B127.mod$Soil_Moisture, B127.mod$` m³/m³ Water Content`)
B127.mod$Soil_Temp <- ifelse(is.na(B127.mod$` °C Soil Temperature`), B127.mod$Soil_Temp, B127.mod$` °C Soil Temperature`)
B127.mod$Relative_Humidity <- ifelse(is.na(B127.mod$` RH Relative Humidity`), B127.mod$Relative_Humidity, B127.mod$` RH Relative Humidity`)
B127.mod$PAR <- ifelse(is.na(B127.mod$` W/m² Solar Radiation`), B127.mod$PAR, B127.mod$` W/m² Solar Radiation`)
B127.mod$Air_Temp <- ifelse(is.na(B127.mod$` °C Air Temperature`), B127.mod$Air_Temp, B127.mod$` °C Air Temperature`)

#Getting rid of redundant dates of data collection#
B127.mod <- B127.mod[!duplicated(B127.mod[c('Date_Time')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.Date(B127.mod$Date_Time), na.rm = T)
Date.last <- max(as.Date(B127.mod$Date_Time) + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
B127.mod['Date_Time'] <- lapply(B127.mod['Date_Time'], as.POSIXct) 
B127.mod.loop <- full_join(time_fill, B127.mod)
B127.mod.loop$Date_Check = NULL

#Making sure columns are of the right datatype
#You may get warning sof NA's but that is removing the rows of Meter that function as row names
B127.mod.loop$Soil_Moisture <- as.numeric(B127.mod.loop$Soil_Moisture)
B127.mod.loop$Soil_Temp <- as.numeric(B127.mod.loop$Soil_Temp)
B127.mod.loop$Air_Temp <- as.numeric(B127.mod.loop$Air_Temp)
B127.mod.loop$Relative_Humidity <- as.numeric(B127.mod.loop$Relative_Humidity)


#Marking NA values as NA
B127.mod.loop[!is.na(B127.mod.loop$Soil_Temp) & (B127.mod.loop$Soil_Temp< -888 | B127.mod.loop$Soil_Temp>999), "Soil_Temp"] <- NA
B127.mod.loop[!is.na(B127.mod.loop$Soil_Moisture) & (B127.mod.loop$Soil_Moisture< -0.5| B127.mod.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
B127.mod.loop[!is.na(B127.mod.loop$PAR) & (B127.mod.loop$PAR< -888 | B127.mod.loop$PAR>999), "PAR"] <- NA
B127.mod.loop[!is.na(B127.mod.loop$Air_Temp) & (B127.mod.loop$Air_Temp< -888 | B127.mod.loop$Air_Temp>999), "Air_Temp"] <- NA
B127.mod.loop[!is.na(B127.mod.loop$Relative_Humidity) & (B127.mod.loop$Relative_Humidity< -888 | B127.mod.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Removing date added to end of year files
B127.mod.loop <- B127.mod.loop[-nrow(B127.mod.loop),]

#Removing duplicates from multiple measures or Daylight Savings
rows <- nrow(B127.mod.loop)
for (i in 2:rows){
  Date.double <- B127.mod.loop[i, "Date_Time"]
  Date.lag <- B127.mod.loop[i - 1, "Date_Time"]
  if (Date.double == Date.lag){
    B127.mod.loop %>% transform(Soil_Moisture = (Soil_Moisture + lag(Soil_Moisture))/2,
                                Relative_Humidity = (Relative_Humidity + lag(Relative_Humidity))/2,
                                PAR = (PAR + lag(PAR))/2,
                                Soil_Temp = (Soil_Temp + lag(Soil_Temp))/2,
                                Air_Temp = (Air_Temp + lag(Air_Temp))/2)
    #B127.mod.loop <- B127.mod.loop[-c(i),]
  }
}

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "../Data_processed/Harmonized_data/", sep="")
filename <- paste("B127.csv", sep = "")
write.csv(B127.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)

#--------------------------------#
#Consolidating N115 data
N115.HB <-read.csv("../Data_processed/OLD_Onset/N115.csv") # Combine all data
#N115.HB$File_Name <- "Onset"

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
N115.MET <- read.csv(paste0(path.met, "Data_Backup/N115_backup.csv"))

#Removing the sensor label column
N115.MET <- N115.MET[N115.MET$Port.2 != "ATMOS 41",]

#Making the column names proper
colnames(N115.MET) <- N115.MET[N115.MET$z6.10464 == "Timestamp",]

#Removing the old row of column names
N115.MET <- N115.MET[N115.MET$Timestamp != "Timestamp",]

N115.MET$Plot_Name <- "N115"


N115.MET$` °C Air Temperature` <- as.numeric(as.character(N115.MET$` °C Air Temperature`))

N115.MET$` RH Relative Humidity` <- as.numeric(as.character(N115.MET$` RH Relative Humidity`))*100 

N115.MET$` W/m² Solar Radiation` <- as.numeric(as.character(N115.MET$` W/m² Solar Radiation`)) 

N115.MET$` m³/m³ Water Content` <- as.numeric(as.character(N115.MET$` m³/m³ Water Content`))

N115.MET$` °C Soil Temperature` <- as.numeric(as.character(N115.MET$` °C Soil Temperature`))

N115.MET$Date_Time <- as.POSIXct(strptime(N115.MET$Timestamp, format="%m/%d/%Y %H"))
N115.HB$Date_Time <-  as.POSIXct(N115.HB$Date_Time)


N115.mod <- full_join(N115.HB, N115.MET)

#Checking columns to delete are correct for next lines
colnames(N115.mod)

#Making sure we use the new meter data when we run out of hoboware
N115.mod$Soil_Moisture <- ifelse(is.na(N115.mod$` m³/m³ Water Content`), N115.mod$Soil_Moisture, N115.mod$` m³/m³ Water Content`)
N115.mod$Soil_Temp <- ifelse(is.na(N115.mod$` °C Soil Temperature`), N115.mod$Soil_Temp, N115.mod$` °C Soil Temperature`)
N115.mod$Relative_Humidity <- ifelse(is.na(N115.mod$` RH Relative Humidity`), N115.mod$Relative_Humidity, N115.mod$` RH Relative Humidity`)
N115.mod$PAR <- ifelse(is.na(N115.mod$` W/m² Solar Radiation`), N115.mod$PAR, N115.mod$` W/m² Solar Radiation`)
N115.mod$Air_Temp <- ifelse(is.na(N115.mod$` °C Air Temperature`), N115.mod$Air_Temp, N115.mod$` °C Air Temperature`)

#Getting rid of redundant dates of data collection#
N115.mod <- N115.mod[!duplicated(N115.mod[c('Date_Time')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.Date(N115.mod$Date_Time), na.rm = T)
Date.last <- max(as.Date(N115.mod$Date_Time) + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
N115.mod['Date_Time'] <- lapply(N115.mod['Date_Time'], as.POSIXct) 
N115.mod.loop <- full_join(time_fill, N115.mod)
N115.mod.loop$Date_Check = NULL

#Making sure columns are of the right datatype
#You may get warning sof NA's but that is removing the rows of Meter that function as row names
N115.mod.loop$Soil_Moisture <- as.numeric(N115.mod.loop$Soil_Moisture)
N115.mod.loop$Soil_Temp <- as.numeric(N115.mod.loop$Soil_Temp)
N115.mod.loop$Air_Temp <- as.numeric(N115.mod.loop$Air_Temp)
N115.mod.loop$Relative_Humidity <- as.numeric(N115.mod.loop$Relative_Humidity)


#Marking NA values as NA
N115.mod.loop[!is.na(N115.mod.loop$Soil_Temp) & (N115.mod.loop$Soil_Temp< -888 | N115.mod.loop$Soil_Temp>999), "Soil_Temp"] <- NA
N115.mod.loop[!is.na(N115.mod.loop$Soil_Moisture) & (N115.mod.loop$Soil_Moisture< -0.5| N115.mod.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
N115.mod.loop[!is.na(N115.mod.loop$PAR) & (N115.mod.loop$PAR< -888 | N115.mod.loop$PAR>999), "PAR"] <- NA
N115.mod.loop[!is.na(N115.mod.loop$Air_Temp) & (N115.mod.loop$Air_Temp< -888 | N115.mod.loop$Air_Temp>999), "Air_Temp"] <- NA
N115.mod.loop[!is.na(N115.mod.loop$Relative_Humidity) & (N115.mod.loop$Relative_Humidity< -888 | N115.mod.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Removing date added to end of year files
N115.mod.loop <- N115.mod.loop[-nrow(N115.mod.loop),]

#Removing duplicates from multiple measures or Daylight Savings
rows <- nrow(N115.mod.loop)
for (i in 2:rows){
  Date.double <- N115.mod.loop[i, "Date_Time"]
  Date.lag <- N115.mod.loop[i - 1, "Date_Time"]
  if (Date.double == Date.lag){
    N115.mod.loop %>% transform(Soil_Moisture = (Soil_Moisture + lag(Soil_Moisture))/2,
                                Relative_Humidity = (Relative_Humidity + lag(Relative_Humidity))/2,
                                PAR = (PAR + lag(PAR))/2,
                                Soil_Temp = (Soil_Temp + lag(Soil_Temp))/2,
                                Air_Temp = (Air_Temp + lag(Air_Temp))/2)
    #N115.mod.loop <- N115.mod.loop[-c(i),]
  }
}

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "../Data_processed/Harmonized_data/", sep="")
filename <- paste("N115.csv", sep = "")
write.csv(N115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
#--------------------------------#
#Consolidating HH115 data
HH115.HB <-read.csv("../Data_processed/OLD_Onset/HH115.csv") # Combine all data
#HH115.HB$File_Name <- "Onset"

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
HH115.MET <- read.csv(paste0(path.met, "Data_Backup/HH115_backup.csv"))

#Removing the sensor label column
HH115.MET <- HH115.MET[HH115.MET$Port.2 != "ATMOS 41",]

#Making the column names proper
colnames(HH115.MET) <- HH115.MET[HH115.MET$z6.10461 == "Timestamp",]

#Removing the old row of column names
HH115.MET <- HH115.MET[HH115.MET$Timestamp != "Timestamp",]

HH115.MET$Plot_Name <- "HH115"


HH115.MET$` °C Air Temperature` <- as.numeric(as.character(HH115.MET$` °C Air Temperature`))

HH115.MET$` RH Relative Humidity` <- as.numeric(as.character(HH115.MET$` RH Relative Humidity`))*100  

HH115.MET$` W/m² Solar Radiation` <- as.numeric(as.character(HH115.MET$` W/m² Solar Radiation`)) 

HH115.MET$` m³/m³ Water Content` <- as.numeric(as.character(HH115.MET$` m³/m³ Water Content`))

HH115.MET$` °C Soil Temperature` <- as.numeric(as.character(HH115.MET$` °C Soil Temperature`))

HH115.MET$Date_Time <- as.POSIXct(strptime(HH115.MET$Timestamp, format="%m/%d/%Y %H"))
HH115.HB$Date_Time <-  as.POSIXct(HH115.HB$Date_Time)


HH115.mod <- full_join(HH115.HB, HH115.MET)

#Checking columns to delete are correct for next lines
colnames(HH115.mod)

#Making sure we use the new meter data when we run out of hoboware
HH115.mod$Soil_Moisture <- ifelse(is.na(HH115.mod$` m³/m³ Water Content`), HH115.mod$Soil_Moisture, HH115.mod$` m³/m³ Water Content`)
HH115.mod$Soil_Temp <- ifelse(is.na(HH115.mod$` °C Soil Temperature`), HH115.mod$Soil_Temp, HH115.mod$` °C Soil Temperature`)
HH115.mod$Relative_Humidity <- ifelse(is.na(HH115.mod$` RH Relative Humidity`), HH115.mod$Relative_Humidity, HH115.mod$` RH Relative Humidity`)
HH115.mod$PAR <- ifelse(is.na(HH115.mod$` W/m² Solar Radiation`), HH115.mod$PAR, HH115.mod$` W/m² Solar Radiation`)
HH115.mod$Air_Temp <- ifelse(is.na(HH115.mod$` °C Air Temperature`), HH115.mod$Air_Temp, HH115.mod$` °C Air Temperature`)

#Getting rid of redundant dates of data collection#
HH115.mod <- HH115.mod[!duplicated(HH115.mod[c('Date_Time')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.Date(HH115.mod$Date_Time), na.rm = T)
Date.last <- max(as.Date(HH115.mod$Date_Time) + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
HH115.mod['Date_Time'] <- lapply(HH115.mod['Date_Time'], as.POSIXct) 
HH115.mod.loop <- full_join(time_fill, HH115.mod)
HH115.mod.loop$Date_Check = NULL

#Making sure columns are of the right datatype
#You may get warning sof NA's but that is removing the rows of Meter that function as row names
HH115.mod.loop$Soil_Moisture <- as.numeric(HH115.mod.loop$Soil_Moisture)
HH115.mod.loop$Soil_Temp <- as.numeric(HH115.mod.loop$Soil_Temp)
HH115.mod.loop$Air_Temp <- as.numeric(HH115.mod.loop$Air_Temp)
HH115.mod.loop$Relative_Humidity <- as.numeric(HH115.mod.loop$Relative_Humidity)


#Marking NA values as NA
HH115.mod.loop[!is.na(HH115.mod.loop$Soil_Temp) & (HH115.mod.loop$Soil_Temp< -888 | HH115.mod.loop$Soil_Temp>999), "Soil_Temp"] <- NA
HH115.mod.loop[!is.na(HH115.mod.loop$Soil_Moisture) & (HH115.mod.loop$Soil_Moisture< -0.5| HH115.mod.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
HH115.mod.loop[!is.na(HH115.mod.loop$PAR) & (HH115.mod.loop$PAR< -888 | HH115.mod.loop$PAR>999), "PAR"] <- NA
HH115.mod.loop[!is.na(HH115.mod.loop$Air_Temp) & (HH115.mod.loop$Air_Temp< -888 | HH115.mod.loop$Air_Temp>999), "Air_Temp"] <- NA
HH115.mod.loop[!is.na(HH115.mod.loop$Relative_Humidity) & (HH115.mod.loop$Relative_Humidity< -888 | HH115.mod.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Removing date added to end of year files
HH115.mod.loop <- HH115.mod.loop[-nrow(HH115.mod.loop),]

#Removing duplicates from multiple measures or Daylight Savings
rows <- nrow(HH115.mod.loop)
for (i in 2:rows){
  Date.double <- HH115.mod.loop[i, "Date_Time"]
  Date.lag <- HH115.mod.loop[i - 1, "Date_Time"]
  if (Date.double == Date.lag){
    HH115.mod.loop %>% transform(Soil_Moisture = (Soil_Moisture + lag(Soil_Moisture))/2,
                                Relative_Humidity = (Relative_Humidity + lag(Relative_Humidity))/2,
                                PAR = (PAR + lag(PAR))/2,
                                Soil_Temp = (Soil_Temp + lag(Soil_Temp))/2,
                                Air_Temp = (Air_Temp + lag(Air_Temp))/2)
    #HH115.mod.loop <- HH115.mod.loop[-c(i),]
  }
}

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "../Data_processed/Harmonized_data/", sep="")
filename <- paste("HH115.csv", sep = "")
write.csv(HH115.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)
#--------------------------------#
#Consolidating U134 data
U134.HB <-read.csv("../Data_processed/OLD_Onset/U134.csv") # Combine all data
#U134.HB$File_Name <- "Onset"

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
U134.MET <- read.csv(paste0(path.met, "Data_Backup/U134_backup.csv"))

#Weird behavior with U134 thinkin we have a third sensor
U134.MET <- subset(U134.MET, select = -c(Port.3, Port.3.1, Port.3.2))

#Removing the sensor label column
U134.MET <- U134.MET[U134.MET$Port.2 != "ATMOS 41",]

#Making the column names proper
colnames(U134.MET) <- U134.MET[U134.MET$z6.10465 == "Timestamp",]

#Removing the old row of column names
U134.MET <- U134.MET[U134.MET$Timestamp != "Timestamp",]

U134.MET$Plot_Name <- "U134"


U134.MET$` °C Air Temperature` <- as.numeric(as.character(U134.MET$` °C Air Temperature`))

U134.MET$` RH Relative Humidity` <- as.numeric(as.character(U134.MET$` RH Relative Humidity`))*100  

U134.MET$` W/m² Solar Radiation` <- as.numeric(as.character(U134.MET$` W/m² Solar Radiation`)) 

U134.MET$` m³/m³ Water Content` <- as.numeric(as.character(U134.MET$` m³/m³ Water Content`))

U134.MET$` °C Soil Temperature` <- as.numeric(as.character(U134.MET$` °C Soil Temperature`))

U134.MET$Date_Time <- as.POSIXct(strptime(U134.MET$Timestamp, format="%m/%d/%Y %H"))
U134.HB$Date_Time <-  as.POSIXct(U134.HB$Date_Time)


U134.mod <- full_join(U134.HB, U134.MET)

#Checking columns to delete are correct for next lines
colnames(U134.mod)

#Making sure we use the new meter data when we run out of hoboware
U134.mod$Soil_Moisture <- ifelse(is.na(U134.mod$` m³/m³ Water Content`), U134.mod$Soil_Moisture, U134.mod$` m³/m³ Water Content`)
U134.mod$Soil_Temp <- ifelse(is.na(U134.mod$` °C Soil Temperature`), U134.mod$Soil_Temp, U134.mod$` °C Soil Temperature`)
U134.mod$Relative_Humidity <- ifelse(is.na(U134.mod$` RH Relative Humidity`), U134.mod$Relative_Humidity, U134.mod$` RH Relative Humidity`)
U134.mod$PAR <- ifelse(is.na(U134.mod$` W/m² Solar Radiation`), U134.mod$PAR, U134.mod$` W/m² Solar Radiation`)
U134.mod$Air_Temp <- ifelse(is.na(U134.mod$` °C Air Temperature`), U134.mod$Air_Temp, U134.mod$` °C Air Temperature`)

#Getting rid of redundant dates of data collection#
U134.mod <- U134.mod[!duplicated(U134.mod[c('Date_Time')]),]

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- min(as.Date(U134.mod$Date_Time), na.rm = T)
Date.last <- max(as.Date(U134.mod$Date_Time) + 1, na.rm = T)
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXct(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
U134.mod['Date_Time'] <- lapply(U134.mod['Date_Time'], as.POSIXct) 
U134.mod.loop <- full_join(time_fill, U134.mod)
U134.mod.loop$Date_Check = NULL

#Making sure columns are of the right datatype
#You may get warning sof NA's but that is removing the rows of Meter that function as row names
U134.mod.loop$Soil_Moisture <- as.numeric(U134.mod.loop$Soil_Moisture)
U134.mod.loop$Soil_Temp <- as.numeric(U134.mod.loop$Soil_Temp)
U134.mod.loop$Air_Temp <- as.numeric(U134.mod.loop$Air_Temp)
U134.mod.loop$Relative_Humidity <- as.numeric(U134.mod.loop$Relative_Humidity)


#Marking NA values as NA
U134.mod.loop[!is.na(U134.mod.loop$Soil_Temp) & (U134.mod.loop$Soil_Temp< -888 | U134.mod.loop$Soil_Temp>999), "Soil_Temp"] <- NA
U134.mod.loop[!is.na(U134.mod.loop$Soil_Moisture) & (U134.mod.loop$Soil_Moisture< -0.5| U134.mod.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
U134.mod.loop[!is.na(U134.mod.loop$PAR) & (U134.mod.loop$PAR< -888 | U134.mod.loop$PAR>999), "PAR"] <- NA
U134.mod.loop[!is.na(U134.mod.loop$Air_Temp) & (U134.mod.loop$Air_Temp< -888 | U134.mod.loop$Air_Temp>999), "Air_Temp"] <- NA
U134.mod.loop[!is.na(U134.mod.loop$Relative_Humidity) & (U134.mod.loop$Relative_Humidity< -888 | U134.mod.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Removing date added to end of year files
U134.mod.loop <- U134.mod.loop[-nrow(U134.mod.loop),]

#Removing duplicates from multiple measures or Daylight Savings
rows <- nrow(U134.mod.loop)
for (i in 2:rows){
  Date.double <- U134.mod.loop[i, "Date_Time"]
  Date.lag <- U134.mod.loop[i - 1, "Date_Time"]
  if (Date.double == Date.lag){
    U134.mod.loop %>% transform(Soil_Moisture = (Soil_Moisture + lag(Soil_Moisture))/2,
                                Relative_Humidity = (Relative_Humidity + lag(Relative_Humidity))/2,
                                PAR = (PAR + lag(PAR))/2,
                                Soil_Temp = (Soil_Temp + lag(Soil_Temp))/2,
                                Air_Temp = (Air_Temp + lag(Air_Temp))/2)
    #U134.mod.loop <- U134.mod.loop[-c(i),]
  }
}

#Setting the path out to be in the corresponding folder
path.out <- paste(path.met, "../Data_processed/Harmonized_data/", sep="")
filename <- paste("U134.csv", sep = "")
write.csv(U134.mod.loop, file.path(path.out,  file = filename), row.names = FALSE)

