#Seperate out files from compiled list by their month#

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
B127.ON <-read_bulk(directory = "Onset_B127", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))
colnames(B127.ON)
####Fixing redundant column names produced by updating HOBOware program which adds data logger and SN's in headers####
#Renaming columns produced by old and new Hoboware:
#Variable ON stands for onset and represents the data we get from onset.
colnames(B127.ON) <- c("Time1", "W/m² Solar Radiation", "mm Precipitation", "Lightning Activity",  "km Lightning Distance", 
                    "Wind Direction",	"m/s Wind Speed", "m/s Gust Speed", "Air_Temp_ON", "kPa Vapor Pressure",	"kPa Atmospheric Pressure",	
                    "X-axis Level",  "Y-axis Level", "mm/h Max Precip Rate",	"°C RH Sensor Temp",	 "kPa VPD",	"Soil_Moisture_ON", 
                    "Soil_Temp_ON",	"% Battery Percent", "mV Battery Voltage","kPa Reference Pressure",	"°C Logger Temperature",
  
                    "File_Name", "Time2", "Time3")
                    
                    #Hoboware sensors and data columns
                    #Variable A-C are used for the same variable but different loggers
colnames(B127.HB) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","Air_Temp_B", "Relative_Humidity_B","PAR_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C")

B127 <- full_join(B127.ON, B127.HB)

#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
#Need a solution for the grwoing number of "timestamp columns" that will increase every time I make a new pull from the stations
B127.convert <- B127 %>% mutate(Time2 = ifelse(is.na(Time2), Time3, Time2),
                                Time_ON = ifelse(is.na(Time1), Time2, Time1),
                                Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                                Soil_Moisture_A = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9))))

#Consolidating redundant columns:
B127.mod <- B127.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                    Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                                    Soil_Temp_Z = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                                    Air_Temp_Z = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                                    
                                    #PAR = ifelse(is.na(PAR_ON), PAR_A, PAR_ON),
                                    #Combinding the converted (to celsius) hoboware with the onset
                                    Soil_Temp = ifelse(is.na(Soil_Temp_ON), Soil_Temp_Z, Soil_Temp_ON),
                                    Soil_Moisture = ifelse(is.na(Soil_Moisture_ON), Soil_Moisture_A, Soil_Moisture_ON),
                                    Air_Temp = ifelse(is.na(Air_Temp_ON), Air_Temp_Z, Air_Temp_ON))
Plot.title <- "B127"
B127.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(B127.mod)

B127.mod$Time6 <- as.POSIXct(strptime(B127.mod$Time6, format="%m/%d/%y %I:%M:%S %p"))
B127.mod$Time5 <- as.POSIXct(strptime(B127.mod$Time5, format="%m/%d/%y %I:%M:%S %p"))
B127.mod$Time_ON <- as.POSIXct(strptime(B127.mod$Time_ON, format="%m/%d/%Y %H"))
B127.mod$Date_Check <- B127.mod$Time5
B127.mod[is.na(B127.mod$Date_Check) & is.na(B127.mod$Time_ON) ,  "Date_Check"] <- B127.mod[is.na(B127.mod$Date_Check) & is.na(B127.mod$Time_ON), "Time6"] + 60*60
B127.mod <- transform(B127.mod, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Removing onset labels that are the first row
#Want this to be hardcoded but couldn't find a way that didn't break other parts
B127.mod <- subset(B127.mod, select = c("Date_Time", "Date_Check","Soil_Temp", "Air_Temp", "Soil_Moisture", "Relative_Humidity", "PAR", "Plot_Name"))

#-------------------------------------#
#Consolidating N115 data#
N115.HB <-read_bulk(directory = "N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
N115.ON <-read_bulk(directory = "Onset_N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))

colnames(N115.ON)
colnames(N115.ON) <- c("Time_ON", "Soil_Moisture_ON", "Soil_Temp_ON",	"% Battery Percent", "mV Battery Voltage",
                    "kPa Reference Pressure",	"°C Logger Temperature", "File_Name", "Time2")

N115.ON <- N115.ON[-c(1),]
N115.ON$Time_ON <- ifelse(is.na(N115.ON$Time_ON), N115.ON$Time2, N115.ON$Time_ON)
N115.ON$Time_ON <- as.POSIXct(strptime(N115.ON$Time_ON, format="%m/%d/%Y %H"))
N115.ON$Soil_Moisture_ON <- as.numeric(N115.ON$Soil_Moisture_ON)
N115.ON$Soil_Temp_ON <- as.numeric(N115.ON$Soil_Temp_ON) 
N115.mid <- aggregate(N115.ON[,c("Soil_Moisture_ON", "Soil_Temp_ON")],
                    by=list(N115.ON$Time_ON),
                    FUN=median, na.rm=T)[,c("Soil_Moisture_ON", "Soil_Temp_ON")]
time.on <- unique(N115.ON$Time_ON)
N115.mid$Time_ON <- time.on[-c(length(time.on))]
N115.mid$File_Name <- "Onset"

 
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

N115.convert$Soil_Moisture_ON <- N115.ON$Soil_Moisture_ON[match(N115.convert$Date_Time, N115.ON$Time_ON)]
N115.convert$Soil_Temp_ON <- N115.ON$Soil_Temp_ON[match(N115.convert$Date_Time, N115.ON$Time_ON)]

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
N115.mod <- subset(N115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Soil_Temp_ON", "Air_Temp", "Soil_Moisture", "Soil_Moisture_ON", "Relative_Humidity", "PAR", "Plot_Name"))
#--------------------------------#

#Consolidating HH115 data
HH115.HB <-read_bulk(directory = "HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
HH115.ON <-read_bulk(directory = "Onset_HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))

colnames(HH115.ON)
colnames(HH115.ON) <- c("Time_ON", "Soil_Moisture_ON", "Soil_Temp_ON",	"% Battery Percent", "mV Battery Voltage",
                       "kPa Reference Pressure",	"°C Logger Temperature", "File_Name", "Time2")

HH115.ON <- HH115.ON[-c(1),]
HH115.ON$Time_ON <- ifelse(is.na(HH115.ON$Time_ON), HH115.ON$Time2, HH115.ON$Time_ON)
HH115.ON$Time_ON <- as.POSIXct(strptime(HH115.ON$Time_ON, format="%m/%d/%Y %H"))
HH115.ON$Soil_Moisture_ON <- as.numeric(HH115.ON$Soil_Moisture_ON)
HH115.ON$Soil_Temp_ON <- as.numeric(HH115.ON$Soil_Temp_ON) 
HH115.mid <- aggregate(HH115.ON[,c("Soil_Moisture_ON", "Soil_Temp_ON")],
                      by=list(HH115.ON$Time_ON),
                      FUN=median, na.rm=T)[,c("Soil_Moisture_ON", "Soil_Temp_ON")]
time.on <- unique(HH115.ON$Time_ON)
HH115.mid$Time_ON <- time.on[-c(length(time.on))]
HH115.mid$File_Name <- "Onset"

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

HH115.convert$Soil_Moisture_ON <- HH115.ON$Soil_Moisture_ON[match(HH115.convert$Date_Time, HH115.ON$Time_ON)]
HH115.convert$Soil_Temp_ON <- HH115.ON$Soil_Temp_ON[match(HH115.convert$Date_Time, HH115.ON$Time_ON)]

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
HH115.mod <- subset(HH115.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Soil_Temp_ON", "Air_Temp", "Soil_Moisture", "Soil_Moisture_ON", "Relative_Humidity", "PAR", "Plot_Name"))
#-------------------------------------#
#Consolidating U134 data
U134.HB <-read_bulk(directory = "U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
U134.ON <-read_bulk(directory = "Onset_U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9"))

colnames(U134.ON)
colnames(U134.ON) <- c("Time_ON", "Soil_Moisture_ON", "Soil_Temp_ON",	"% Battery Percent", "mV Battery Voltage",
                        "kPa Reference Pressure",	"°C Logger Temperature", "File_Name", "Time2")

U134.ON <- U134.ON[-c(1),]
U134.ON$Time_ON <- ifelse(is.na(U134.ON$Time_ON), U134.ON$Time2, U134.ON$Time_ON)
U134.ON$Time_ON <- as.POSIXct(strptime(U134.ON$Time_ON, format="%m/%d/%Y %H"))
U134.ON$Soil_Moisture_ON <- as.numeric(U134.ON$Soil_Moisture_ON)
U134.ON$Soil_Temp_ON <- as.numeric(U134.ON$Soil_Temp_ON) 
U134.mid <- aggregate(U134.ON[,c("Soil_Moisture_ON", "Soil_Temp_ON")],
                       by=list(U134.ON$Time_ON),
                       FUN=median, na.rm=T)[,c("Soil_Moisture_ON", "Soil_Temp_ON")]
time.on <- unique(U134.ON$Time_ON)
U134.mid$Time_ON <- time.on[-c(length(time.on))]
U134.mid$File_Name <- "Onset"

colnames(U134.HB)
colnames(U134.HB) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time6_A", "Time5_B", "Soil_Temp_B", "Air_Temp_B", "Relative_Humidity_B","PAR_B", "Soil_Moisture_B","PAR_C", "Time6_B",
                    "Soil_Temp_C", "Air_Temp_C") #Change column names for U134


U134.convert <- U134.HB %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                                   Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                                Soil_Temp_B = ifelse(is.na(Soil_Temp_B), Soil_Temp_C, Soil_Temp_B),
                                Air_Temp_B = ifelse(is.na(Air_Temp_B), Air_Temp_C, Air_Temp_B),
                                Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp = ifelse((Soil_Temp_X > 800 | Soil_Temp_X < -800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp = ifelse((Air_Temp_X > 800 | Soil_Temp_X < -800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 

U134.convert$Time6 <- as.POSIXct(strptime(U134.convert$Time6, format="%m/%d/%y %I:%M:%S %p"))
U134.convert$Time5 <- as.POSIXct(strptime(U134.convert$Time5, format="%m/%d/%y %I:%M:%S %p"))
U134.convert$Date_Check <- U134.convert$Time5
U134.convert[is.na(U134.convert$Date_Check), "Date_Check"] <- U134.convert[is.na(U134.convert$Date_Check), "Time6"] + 60*60
U134.convert <- transform(U134.convert, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

U134.convert$Soil_Moisture_ON <- U134.ON$Soil_Moisture_ON[match(U134.convert$Date_Time, U134.ON$Time_ON)]
U134.convert$Soil_Temp_ON <- U134.ON$Soil_Temp_ON[match(U134.convert$Date_Time, U134.ON$Time_ON)]


U134.mod <- U134.convert %>% mutate(
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "U134"
U134.mod $ Plot_Name <- Plot.title
colnames(U134.mod)
U134.mod <- subset(U134.mod, select = c("Date_Time", "Date_Check", "Soil_Temp", "Soil_Temp_ON", "Air_Temp", "Soil_Moisture", "Soil_Moisture_ON", "Relative_Humidity", "PAR", "Plot_Name"))

#--------------------------------#
#After running one of the above plots you run these lines.
#I wil fix this to be a loop in some way down the line

comb_plot <- rbind(B127.mod, N115.mod, HH115.mod, U134.mod)

for(PLOT in unique(comb_plot$Plot_Name)){
  one_plot <- comb_plot[comb_plot$Plot_Name == PLOT,]
  #Consolidating the plot and fixing redundacies in Time
  #Addressing daylight saving times issue (Time6 + Time5)
  #Getting rid of extra time5 and time6 columns in front

  #Getting rid of redundant dates of data collection#
  one_plot <- one_plot[!duplicated(one_plot[c('Date_Check')]),]
  
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
  if(PLOT != "B127"){
  one_plot.loop <- one_plot.loop[c("Plot_Name", "Date_Time", "Soil_Moisture", "Soil_Moisture_ON", "Relative_Humidity",
                                   "PAR", "Soil_Temp", "Soil_Temp_ON", "Air_Temp")]
  } else{
    one_plot.loop <- one_plot.loop[c("Plot_Name", "Date_Time", "Soil_Moisture", "Relative_Humidity",
                                     "PAR", "Soil_Temp", "Air_Temp")]
  }
  
  #Making sure columns are of the right datatype
  #You may get warning sof NA's but that is removing the rows of onset that function as row names
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
  
  #Setting the path out to be in the corresponding folder
  path.out <- paste(path.met, "Data_Clean/", PLOT, sep="")
  filename <- paste(PLOT, ".csv", sep = "")
  write.csv(one_plot.loop, file.path(path.out,  file = filename), row.names = FALSE)

}
