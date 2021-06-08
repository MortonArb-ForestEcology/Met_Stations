#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: To convert the raw data we receive from data loggers into consistent units and formats across plots and data loggers
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
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
setwd(path.met)

#--------------------------------#

#Consolidating B127 data
B127.HB <-read_bulk(directory = "B127", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
#B127.MET <-read_bulk(directory = "Meter_B127", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
dir.B127 <- dir(file.path(path.met, "Meter_B127"), ".csv")

split.B127 <- strsplit(dir.B127, "_")

split.B127 <- lapply(split.B127, function (x) x[2])

date.B127 <- unlist(lapply(split.B127, function (x) sub(".csv", "", x)))

date.B127 <- as.Date(date.B127)

pull.B127 <- date.B127

B127.MET <- data.frame()
for(i in 1:length(pull.B127)){
  date <- pull.B127[i]
  file <- read.csv(paste0(path.met, "Meter_B127/B127_", date, ".csv"))
  B127.MET <- rbind(B127.MET, file)
}

colnames(B127.MET)
####Fixing redundant column names produced by updating HOBOware program which adds data logger and SN's in headers####
#Renaming columns produced by old and new Hoboware:
#Variable ON stands for Meter and represents the data we get from Meter.
colnames(B127.MET) <- c("Time_MET"	, "PAR_MET", "mm Precipitation", "Lightning Activity", "km Lightning Distance",	"° Wind Direction",
                        "m/s Wind Speed", "m/s Gust Speed",	"Air_Temp_MET",	"kPa Vapor Pressure", "Relative_Humidity_MET", "° X-axis Level",
                        "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp",	"kPa VPD", "Soil_Moisture_MET", "Soil_Temp_MET",
                        "% Battery Percent", "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature")

B127.MET$File_Name <- "Meter"

B127.MET <- subset(B127.MET, select = c("Time_MET",  "Air_Temp_MET", "Relative_Humidity_MET", "PAR_MET",
                                        "Soil_Moisture_MET", "Soil_Temp_MET", "File_Name"))
                    
                    #Hoboware sensors and data columns
                    #Variable A-C are used for the same variable but different loggers
colnames(B127.HB) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","Air_Temp_B", "Relative_Humidity_B","PAR_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C")

B127 <- full_join(B127.HB, B127.MET)

#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
#Need a solution for the grwoing number of "timestamp columns" that will increase every time I make a new pull from the stations
B127.convert <- B127 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                                Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9))))

#Consolidating redundant columns:
B127.mod <- B127.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                    Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                                    Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                                    Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                                    
                                    #PAR = ifelse(is.na(PAR_MET), PAR_A, PAR_MET),
                                    #Combinding the converted (to celsius) hoboware with the Meter
                                    )
Plot.title <- "B127"
B127.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(B127.mod)

B127.mod$Time6 <- as.POSIXct(strptime(B127.mod$Time6, format="%m/%d/%y %I:%M:%S %p"))
B127.mod$Time5 <- as.POSIXct(strptime(B127.mod$Time5, format="%m/%d/%y %I:%M:%S %p"))
B127.mod$Time_MET <- as.POSIXct(strptime(B127.mod$Time_MET, format="%m/%d/%Y %H"))
B127.mod$Date_Check <- B127.mod$Time5
B127.mod[is.na(B127.mod$Date_Check) & is.na(B127.mod$Time_MET) ,  "Date_Check"] <- B127.mod[is.na(B127.mod$Date_Check) & is.na(B127.mod$Time_MET), "Time6"] + 60*60
B127.mod[is.na(B127.mod$Date_Check) ,  "Date_Check"] <- B127.mod[is.na(B127.mod$Date_Check), "Time_MET"]
B127.mod <- transform(B127.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Making sure we use the new meter data when we run out of hoboware
B127.mod$Soil_Moisture <- ifelse(is.na(B127.mod$Soil_Moisture_MET), B127.mod$Soil_Moisture, B127.mod$Soil_Moisture_MET)
B127.mod$Soil_Temp <- ifelse(is.na(B127.mod$Soil_Temp_MET), B127.mod$Soil_Temp, B127.mod$Soil_Temp_MET)
B127.mod$Relative_Humidity <- ifelse(is.na(B127.mod$Relative_Humidity_MET), B127.mod$Relative_Humidity, B127.mod$Relative_Humidity_MET)
B127.mod$PAR <- ifelse(is.na(B127.mod$PAR_MET), B127.mod$PAR, B127.mod$PAR_MET)
B127.mod$Air_Temp <- ifelse(is.na(B127.mod$Air_Temp_MET), B127.mod$Air_Temp, B127.mod$Air_Temp_MET)

#For B127 the Meter soil data automatically takes over since there isn't overlap
B127.mod <- subset(B127.mod, select = c("Date_Time", "Date_Check", "Soil_Temp",  "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR",
                                        "Plot_Name", "Soil_Moisture_MET", "Soil_Temp_MET", "File_Name"))


#-------------------------------------#
#Consolidating N115 data#
N115.HB <-read_bulk(directory = "N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
dir.N115 <- dir(file.path(path.met, "Meter_N115"), ".csv")

split.N115 <- strsplit(dir.N115, "_")

split.N115 <- lapply(split.N115, function (x) x[2])

date.N115 <- unlist(lapply(split.N115, function (x) sub(".csv", "", x)))

date.N115 <- as.Date(date.N115)

pull.N115 <- date.N115

N115.MET <- data.frame()
for(i in 1:length(pull.N115)){
  date <- pull.N115[i]
  file <- read.csv(paste0(path.met, "Meter_N115/N115_", date, ".csv"))
  N115.MET <- rbind(N115.MET, file)
}



colnames(N115.MET)
colnames(N115.MET) <- c("Time_MET", "Soil_Moisture_MET", "Soil_Temp_MET",	"% Battery Percent", "mV Battery Voltage",
                    "kPa Reference Pressure",	"°C Logger Temperature")

N115.MET$File_Name <- "Meter"


N115.MET$Time_MET <- as.POSIXct(strptime(N115.MET$Time_MET, format="%m/%d/%Y %H"))
N115.MET$Soil_Moisture_MET <- as.numeric(N115.MET$Soil_Moisture_MET)
N115.MET$Soil_Temp_MET <- as.numeric(N115.MET$Soil_Temp_MET) 
N115.mid <- aggregate(N115.MET[,c("Soil_Moisture_MET", "Soil_Temp_MET")],
                    by=list(N115.MET$Time_MET),
                    FUN=median, na.rm=T)[,c("Soil_Moisture_MET", "Soil_Temp_MET")]
time.MET <- unique(N115.MET$Time_MET)
N115.mid$Time_MET <- time.MET[-c(length(time.MET))]
N115.mid$File_Name <- "Meter"

 
colnames(N115.HB) 
colnames(N115.HB)  <-  c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C") #Change column names for N115



#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
N115.convert <- N115.HB %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 


  
N115.convert$Time6 <- as.POSIXct(strptime(N115.convert$Time6, format="%m/%d/%y %I:%M:%S %p"))
N115.convert$Time5 <- as.POSIXct(strptime(N115.convert$Time5, format="%m/%d/%y %I:%M:%S %p"))
N115.convert$Date_Check <- N115.convert$Time5
N115.convert[is.na(N115.convert$Date_Check), "Date_Check"] <- N115.convert[is.na(N115.convert$Date_Check), "Time6"] + 60*60
N115.convert <- transform(N115.convert, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

N115.convert$Soil_Moisture_MET <- N115.MET$Soil_Moisture_MET[match(N115.convert$Date_Time, N115.MET$Time_MET)]
N115.convert$Soil_Temp_MET <- N115.MET$Soil_Temp_MET[match(N115.convert$Date_Time, N115.MET$Time_MET)]

#Consolidating redundant columns:
N115.mod <- N115.convert %>% mutate(
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            
                            Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            
                            Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "N115"                            
N115.mod $ Plot_Name <- Plot.title
colnames(N115.mod)
N115.mod <- subset(N115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp",  "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR",
                                        "Plot_Name", "Soil_Moisture_MET", "Soil_Temp_MET", "File_Name"))
#Having the meter soil data overwrite the hoboware if we have meter data
N115.mod$Soil_Moisture <- ifelse(is.na(N115.mod$Soil_Moisture_MET), N115.mod$Soil_Moisture, N115.mod$Soil_Moisture_MET)
N115.mod$Soil_Temp <- ifelse(is.na(N115.mod$Soil_Temp_MET), N115.mod$Soil_Temp, N115.mod$Soil_Temp_MET)
N115.mod $ Plot_Name <- Plot.title
#--------------------------------#

#Consolidating HH115 data
HH115.HB <-read_bulk(directory = "HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
dir.HH115 <- dir(file.path(path.met, "Meter_HH115"), ".csv")

split.HH115 <- strsplit(dir.HH115, "_")

split.HH115 <- lapply(split.HH115, function (x) x[2])

date.HH115 <- unlist(lapply(split.HH115, function (x) sub(".csv", "", x)))

date.HH115 <- as.Date(date.HH115)

pull.HH115 <- date.HH115

HH115.MET <- data.frame()
for(i in 1:length(pull.HH115)){
  date <- pull.HH115[i]
  file <- read.csv(paste0(path.met, "Meter_HH115/HH115_", date, ".csv"))
  HH115.MET <- rbind(HH115.MET, file)
}


colnames(HH115.MET)
colnames(HH115.MET) <- c("Time_MET", "Soil_Moisture_MET", "Soil_Temp_MET",	"% Battery Percent", "mV Battery Voltage",
                       "kPa Reference Pressure",	"°C Logger Temperature")


HH115.MET$Time_MET <- ifelse(is.na(HH115.MET$Time_MET), HH115.MET$Time2, HH115.MET$Time_MET)
HH115.MET$Time_MET <- as.POSIXct(strptime(HH115.MET$Time_MET, format="%m/%d/%Y %H"))
HH115.MET$Soil_Moisture_MET <- as.numeric(HH115.MET$Soil_Moisture_MET)
HH115.MET$Soil_Temp_MET <- as.numeric(HH115.MET$Soil_Temp_MET) 
HH115.mid <- aggregate(HH115.MET[,c("Soil_Moisture_MET", "Soil_Temp_MET")],
                      by=list(HH115.MET$Time_MET),
                      FUN=median, na.rm=T)[,c("Soil_Moisture_MET", "Soil_Temp_MET")]
time.MET <- unique(HH115.MET$Time_MET)
HH115.mid$Time_MET <- time.MET[-c(length(time.MET))]
HH115.mid$File_Name <- "Meter"

colnames(HH115.HB) 

#Currently removing the 1 day of measurements every second that I accidentally recorded
#Will talk to Christy about how best to use
HH115.HB <- HH115.HB[HH115.HB$File != "Rollinson_HH115_2020-08-27.csv", ]

colnames(HH115.HB) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                        "Time6_A", "Time5_B", "Soil_Temp_B", "Soil_Moisture_B", "PAR_B", "Air_Temp_B", "Relative_Humidity_B", "Time6_B",
                        "PAR_C", "Soil_Temp_C",  "Air_Temp_C", "Soil_Temp_D", "Soil_Moisture_C", "PAR_D", "Air_Temp_D", "Relative_Humidity_C") #Change column names for HH115


HH115.convert <- HH115.HB %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                                Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                Relative_Humidity_B = ifelse(is.na(Relative_Humidity_B), Relative_Humidity_C, Relative_Humidity_B),
                                PAR_C = ifelse(is.na(PAR_C), PAR_D, PAR_C),
                                Soil_Moisture_B = ifelse(is.na(Soil_Moisture_B), Soil_Moisture_C, Soil_Moisture_B),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

#This will also need to be changed once celsius 
HH115.convert$PAR_B <- ifelse(is.na(HH115.convert$PAR_B), HH115.convert$PAR_C, HH115.convert$PAR_B)

HH115.convert$Time6 <- as.POSIXct(strptime(HH115.convert$Time6, format="%m/%d/%y %I:%M:%S %p"))
HH115.convert$Time5 <- as.POSIXct(strptime(HH115.convert$Time5, format="%m/%d/%y %I:%M:%S %p"))
HH115.convert$Date_Check <- HH115.convert$Time5
HH115.convert[is.na(HH115.convert$Date_Check), "Date_Check"] <- HH115.convert[is.na(HH115.convert$Date_Check), "Time6"] + 60*60
HH115.convert <- transform(HH115.convert, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

HH115.convert$Soil_Moisture_MET <- HH115.MET$Soil_Moisture_MET[match(HH115.convert$Date_Time, HH115.MET$Time_MET)]
HH115.convert$Soil_Temp_MET <- HH115.MET$Soil_Temp_MET[match(HH115.convert$Date_Time, HH115.MET$Time_MET)]

HH115.mod <- HH115.convert %>% mutate(
                              
                              Soil_Temp_C = ifelse(is.na(Soil_Temp_C), Soil_Temp_Y, Soil_Temp_C),
                              Air_Temp_C = ifelse(is.na(Air_Temp_C), Air_Temp_Y, Air_Temp_C),
                              
                              Soil_Temp = ifelse(is.na(Soil_Temp_D), Soil_Temp_C, Soil_Temp_D),
                              Air_Temp = ifelse(is.na(Air_Temp_D), Air_Temp_C, Air_Temp_D),
                              
                              Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                              Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                              PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "HH115"
HH115.mod $ Plot_Name <- Plot.title
colnames(HH115.mod)
HH115.mod <- subset(HH115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#Extra steps to have the MET soil data replace hoboware data
HH115.comb <- merge(HH115.mod, HH115.mid, by.x = "Date_Time", by.y = "Time_MET", all.x = T, all.y = T)

HH115.comb$Soil_Moisture <- ifelse(is.na(HH115.comb$Soil_Moisture), HH115.comb$Soil_Moisture_MET, HH115.comb$Soil_Moisture)
HH115.comb$Soil_Temp <- ifelse(is.na(HH115.comb$Soil_Temp), HH115.comb$Soil_Temp_MET, HH115.comb$Soil_Temp)
HH115.comb $ Plot_Name <- Plot.title
#-------------------------------------#
#Consolidating U134 data
U134.HB <-read_bulk(directory = "U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data

#Kind of clunky download but using things like readbulk adds an extra column for Time with every upload. This is better
dir.U134 <- dir(file.path(path.met, "Meter_U134"), ".csv")

split.U134 <- strsplit(dir.U134, "_")

split.U134 <- lapply(split.U134, function (x) x[2])

date.U134 <- unlist(lapply(split.U134, function (x) sub(".csv", "", x)))

date.U134 <- as.Date(date.U134)

pull.U134 <- date.U134

U134.MET <- data.frame()
for(i in 1:length(pull.U134)){
  date <- pull.U134[i]
  file <- read.csv(paste0(path.met, "Meter_U134/U134_", date, ".csv"))
  U134.MET <- rbind(U134.MET, file)
}

#U134 has some weirder column names because of when I was attempting to get other older sensors to work on the new device
colnames(U134.MET)
colnames(U134.MET) <- c("Time_MET", "Soil_Moisture_MET", "Soil_Temp_MET", "Unknown Sensor", 
                        "Air_Temp", "kPa Vapor Pressure",	"kPa Atmospheric Pressure",	 "kPa VPD",	
                        "Sensor Output",	"% Battery Percent", "mV Battery Voltage",
                        "kPa Reference Pressure",	"°C Logger Temperature")

U134.MET <- U134.MET[-c(1),]
U134.MET$Time_MET <- ifelse(is.na(U134.MET$Time_MET), U134.MET$Time2, U134.MET$Time_MET)
U134.MET$Time_MET <- as.POSIXct(strptime(U134.MET$Time_MET, format="%m/%d/%Y %H"))
U134.MET$Soil_Moisture_MET <- as.numeric(U134.MET$Soil_Moisture_MET)
U134.MET$Soil_Temp_MET <- as.numeric(U134.MET$Soil_Temp_MET) 
U134.mid <- aggregate(U134.MET[,c("Soil_Moisture_MET", "Soil_Temp_MET")],
                       by=list(U134.MET$Time_MET),
                       FUN=median, na.rm=T)[,c("Soil_Moisture_MET", "Soil_Temp_MET")]
time.MET <- unique(U134.MET$Time_MET)
U134.mid$Time_MET <- time.MET[-c(length(time.MET))]
U134.mid$File_Name <- "Meter"

colnames(U134.HB)
colnames(U134.HB) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time6_A", "Time5_B", "Soil_Temp_B", "Air_Temp_B", "Relative_Humidity_B","PAR_B", "Soil_Moisture_B","PAR_C", "Time6_B",
                    "Soil_Temp_C", "Air_Temp_C", "Soil_Temp_D", "Air_Temp_D", "PAR_D", "Soil_Moisture_C") #Change column names for U134


U134.convert <- U134.HB %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                   Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                                Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_C = ifelse(is.na(PAR_C), PAR_D, PAR_C),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

U134.convert$Time6 <- as.POSIXct(strptime(U134.convert$Time6, format="%m/%d/%y %I:%M:%S %p"))
U134.convert$Time5 <- as.POSIXct(strptime(U134.convert$Time5, format="%m/%d/%y %I:%M:%S %p"))
U134.convert$Date_Check <- U134.convert$Time5
U134.convert[is.na(U134.convert$Date_Check), "Date_Check"] <- U134.convert[is.na(U134.convert$Date_Check), "Time6"] + 60*60
U134.convert <- transform(U134.convert, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

U134.convert$Soil_Moisture_MET <- U134.MET$Soil_Moisture_MET[match(U134.convert$Date_Time, U134.MET$Time_MET)]
U134.convert$Soil_Temp_MET <- U134.MET$Soil_Temp_MET[match(U134.convert$Date_Time, U134.MET$Time_MET)]


U134.mod <- U134.convert %>% mutate(Soil_Moisture_B = ifelse(is.na(Soil_Moisture_B), Soil_Moisture_C, Soil_Moisture_B),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                            Soil_Temp_C = ifelse(is.na(Soil_Temp_C), Soil_Temp_Y, Soil_Temp_C),
                            Air_Temp_C = ifelse(is.na(Air_Temp_C), Air_Temp_Y, Air_Temp_C),
                            Soil_Temp = ifelse(is.na(Soil_Temp_D), Soil_Temp_C, Soil_Temp_D),
                            Air_Temp = ifelse(is.na(Air_Temp_D), Air_Temp_C, Air_Temp_D))
Plot.title <- "U134"
U134.mod $ Plot_Name <- Plot.title
colnames(U134.mod)
U134.mod <- subset(U134.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#This is removing oddly dated (Follow a different date format) values that are repeats of previous measurements
#U134.mod <- U134.mod[!is.na(U134.mod$Date_Time),]

#Extra steps to have the MET soil data replace hoboware data
#This is not done for N115 or B127 as they don't have overlap issue
U134.comb <- merge(U134.mod, U134.mid, by.x = "Date_Time", by.y = "Time_MET", all.x = T, all.y = T)

U134.comb$Soil_Moisture <- ifelse(is.na(U134.comb$Soil_Moisture), U134.comb$Soil_Moisture_MET, U134.comb$Soil_Moisture)
U134.comb$Soil_Temp <- ifelse(is.na(U134.comb$Soil_Temp), U134.comb$Soil_Temp_MET, U134.comb$Soil_Temp)
U134.comb$Plot_Name <- Plot.title
#--------------------------------#
#After running one of the above plots you run these lines.
#I wil fix this to be a loop in some way down the line

comb_plot <- rbind(B127.mod, N115.mod, HH115.comb, U134.comb)

for(PLOT in unique(comb_plot$Plot_Name)){
  one_plot <- comb_plot[comb_plot$Plot_Name == PLOT,]
  #Consolidating the plot and fixing redundacies in Time
  #Addressing daylight saving times issue (Time6 + Time5)
  #Getting rid of extra time5 and time6 columns in front

  #Getting rid of redundant dates of data collection#
  one_plot <- one_plot[!duplicated(one_plot[c('Date_Time')]),]
  
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
  one_plot.loop <- one_plot.loop[c("Plot_Name", "Date_Time", "Soil_Moisture", "Soil_Moisture_MET", "Relative_Humidity",
                                   "PAR", "Soil_Temp", "Soil_Temp_MET", "Air_Temp")]
  
  #Making sure columns are of the right datatype
  #You may get warning sof NA's but that is removing the rows of Meter that function as row names
  one_plot.loop$Soil_Moisture <- as.numeric(one_plot.loop$Soil_Moisture)
  one_plot.loop$Soil_Temp <- as.numeric(one_plot.loop$Soil_Temp)
  one_plot.loop$Air_Temp <- as.numeric(one_plot.loop$Air_Temp)
  one_plot.loop$Relative_Humidity <- as.numeric(one_plot.loop$Relative_Humidity)
  
  
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
  
  #Setting the path out to be in the corresponding folder
  path.out <- paste(path.met, "Data_Clean/", PLOT, sep="")
  filename <- paste(PLOT, ".csv", sep = "")
  write.csv(one_plot.loop, file.path(path.out,  file = filename), row.names = FALSE)

}
