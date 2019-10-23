#Seperate out files from compiled list by their month#

library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting File paths
path.personal <- "C:/Users/lfitzpatrick"
path.data <- "/GitHub/Clones/Met_Stations/Data_raw_inputs/Single_Plots"
path.met <- paste(path.personal, path.data, sep="")
path.out <- paste(path.personal, "/GitHub/Clones/Met_Stations/Data_clean", sep="")
setwd(path.met)

#--------------------------------#

#Consolidating B127 data
B127 <-read_bulk(directory = "Rollinson_B127", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
colnames(B127)
####Fixing redundant column names produced by updating HOBOware program which adds data logger and SN's in headers####
#Renaming columns produced by old and new Hoboware:
colnames(B127) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","Air_Temp_B", "Relative_Humidity_B","PAR_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C")#Change column names for B127
#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
B127.convert <- B127 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_X = ifelse((Soil_Temp_X > -800), Soil_Temp_X ,(Soil_Temp_X * -1)),
                                Air_Temp_X = ifelse((Air_Temp_X > -800), Air_Temp_X, (Air_Temp_X * -1)),
                                Soil_Temp_Y = ifelse(Soil_Temp_X > 800, Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse(Air_Temp_X > 800, Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

#Consolidating redundant columns:
B127.mod <- B127.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                    Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                                    Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                                    PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                                    Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                                    Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y))
Plot.title <- "B127"
B127.mod $ PlotName <- Plot.title
#Checking columns to delete are correct for next lines
colnames(B127.mod)
#Deleting columns before "Time6"
one_plot <- subset(B127.mod, select = c(16,23:29))

#-------------------------------------#
#Consolidating N115 data#
N115 <-read_bulk(directory = "N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
colnames(N115)
colnames(N115) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C") #Change column names for N115
#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
N115.convert <- N115 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_X = ifelse((Soil_Temp_X > -800), Soil_Temp_X ,(Soil_Temp_X * -1)),
                                Air_Temp_X = ifelse((Air_Temp_X > -800), Air_Temp_X, (Air_Temp_X * -1)),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

#Consolidating redundant columns:
N115.mod <- N115.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                            Soil_Temp = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                            Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_Y), Soil_Moisture_C, Soil_Moisture_Y),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "N115"                            
N115.mod $ PlotName <- Plot.title
colnames(N115.mod)
one_plot <- subset(N115.mod, select = c(16,23:29))

#--------------------------------#

#Consolidating HH115 data
HH115 <-read_bulk(directory = "HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
colnames(HH115)
colnames(HH115) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                     "Time6_A", "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B","Time6_B", "PAR_C") #Change column names for HH115
HH115.convert <- HH115 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B))
#This conversion is currently unneeded as it has no celsius values yet
                                #Soil_Temp_X = ifelse((Soil_Temp_X > -800), Soil_Temp_X ,(Soil_Temp_X * -1)),
                                #Air_Temp_X = ifelse((Air_Temp_X > -800), Air_Temp_X, (Air_Temp_X * -1)),
                                #Soil_Temp_Y = ifelse((Soil_Temp_X > 800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                #Air_Temp_Y = ifelse((Air_Temp_X > 800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 
#This will also need to be changed once celsius 
HH115.mod <- HH115.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                              Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                              Soil_Temp = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                              Air_Temp = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                              Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                              Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                              PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "HH115"
HH115.mod $ PlotName <- Plot.title
colnames(HH115.mod)
one_plot <- subset(HH115.mod, select = -c(1,1:17))
#-------------------------------------#


#Consolidating U134 data
U134 <-read_bulk(directory = "U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
colnames(U134)
colnames(U134) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time6_A", "Time5_B", "Soil_Temp_B", "Air_Temp_B", "Relative_Humidity_B","PAR_B", "Soil_Moisture_B","PAR_C", "Time6_B",
                    "Soil_Temp_C", "Air_Temp_C") #Change column names for U134
U134.convert <- U134 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_X = ifelse((Soil_Temp_X > -800), Soil_Temp_X ,(Soil_Temp_X * -1)),
                                Air_Temp_X = ifelse((Air_Temp_X > -800), Air_Temp_X, (Air_Temp_X * -1)),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

U134.mod <- U134.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                            Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                            Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                            Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "U134"
U134.mod $ PlotName <- Plot.title
colnames(U134.mod)
one_plot <- subset(U134.mod, select = c(24:31))

#--------------------------------#

#Consolidating the plot and fixing redundacies in Time
#Addressing daylight saving times issue (Time6 + Time5)
obs_date <- strsplit(getwd(), split = "/")
obs_date <- obs_date[[1]][length(obs_date[[1]])]
one_plot$Time6 <- strptime(one_plot$Time6, format="%m/%d/%y %I:%M:%S %p")
one_plot$Time5 <- strptime(one_plot$Time5, format="%m/%d/%y %I:%M:%S %p")
one_plot$Date_Time <- one_plot$Time5
one_plot[is.na(one_plot$Date_Time),"Date_Time"] <- one_plot[is.na(one_plot$Date_Time),"Time6"] + 60*60
summary(one_plot)
#Getting rid of extra time5 and time6 columns in front
one_plot = select(one_plot, -1, -2)
colnames(one_plot)

#Getting rid of redundant dates of data collection#
one_plot <- one_plot[!duplicated(one_plot[c('Date_Time')]),]

#------------------------------------#
# Seperating by chosen year and month values

Date.year <- "2017"
Date.month <- "07"
Date.min <- paste(Date.year,"-", Date.month, "-01 01:30:55", sep="")
Date.max <- paste(Date.year,"-", Date.month, "-31 23:18:55", sep="")
one_plot.mon <- subset(one_plot, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))

#Adding in missing times so missing data can be seen
ts <- seq.POSIXt(as.POSIXct(Date.min, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.max, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXlt(Date.min), as.POSIXlt(Date.max), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
one_plot.mon['Date_Time'] <- lapply(one_plot.mon['Date_Time'], as.POSIXct) 
one_plot.final <- full_join(time_fill, one_plot.mon)

#Writing .csv of monthly data
filename <- paste(Plot.title,"-", Date.year, "-", Date.month, ".csv", sep = "")
write.csv(one_plot.final, file.path(path.out,  file = filename)) #Write CSV to current directory

