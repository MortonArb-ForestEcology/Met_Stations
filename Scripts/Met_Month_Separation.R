#Seperate out files from compiled list by their month#

library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting File paths
path.personal <- "C:/Users/lfitzpatrick"
path.data <- "/GitHub/Met_Stations/Data_raw_inputs/Single_Plots"
path.met <- paste(path.personal, path.data, sep="")
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
B127.mod $ Plot_Name <- Plot.title
#Checking columns to delete are correct for next lines
colnames(B127.mod)
#Deleting columns before "Time6"
one_plot <- subset(B127.mod, select = c(16,23:29))

#-------------------------------------#
#Consolidating N115 data#
N115 <-read_bulk(directory = "Rollinson_N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
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
                            Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                            Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "N115"                            
N115.mod $ Plot_Name <- Plot.title
colnames(N115.mod)
one_plot <- subset(N115.mod, select = c(16,23:29))

#--------------------------------#

#Consolidating HH115 data
HH115 <-read_bulk(directory = "Rollinson_HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
colnames(HH115)
colnames(HH115) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                     "Time6_A", "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B","Time6_B", "PAR_C", "Soil_Temp_C", "Air_Temp_C") #Change column names for HH115
HH115.convert <- HH115 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_X = ifelse((Soil_Temp_X > -800), Soil_Temp_X ,(Soil_Temp_X * -1)),
                                Air_Temp_X = ifelse((Air_Temp_X > -800), Air_Temp_X, (Air_Temp_X * -1)),
                                Soil_Temp_Y = ifelse((Soil_Temp_X > 800), Soil_Temp_X, ((Soil_Temp_X-32)*(5/9))), 
                                Air_Temp_Y = ifelse((Air_Temp_X > 800), Air_Temp_X, ((Air_Temp_X-32)*(5/9)))) 
#This will also need to be changed once celsius 
HH115.mod <- HH115.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                              Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                              Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                              Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y),
                              Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                              Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                              PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A))
Plot.title <- "HH115"
HH115.mod $ Plot_Name <- Plot.title
colnames(HH115.mod)
one_plot <- subset(HH115.mod, select = c(24:31))
#-------------------------------------#


#Consolidating U134 data
U134 <-read_bulk(directory = "Rollinson_U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
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
U134.mod $ Plot_Name <- Plot.title
colnames(U134.mod)
one_plot <- subset(U134.mod, select = c(24:31))

#--------------------------------#

#Consolidating the plot and fixing redundacies in Time
#Addressing daylight saving times issue (Time6 + Time5)
obs_date <- strsplit(getwd(), split = "/")
obs_date <- obs_date[[1]][length(obs_date[[1]])]
one_plot$Time6 <- strptime(one_plot$Time6, format="%m/%d/%y %I:%M:%S %p")
one_plot$Time5 <- strptime(one_plot$Time5, format="%m/%d/%y %I:%M:%S %p")
one_plot$Date_Check <- one_plot$Time5
one_plot[is.na(one_plot$Date_Check),"Date_Check"] <- one_plot[is.na(one_plot$Date_Check),"Time6"] + 60*60
summary(one_plot)
#Getting rid of extra time5 and time6 columns in front
one_plot = select(one_plot, -1, -2)
colnames(one_plot)

#Getting rid of redundant dates of data collection#
one_plot <- one_plot[!duplicated(one_plot[c('Date_Check')]),]

#------------------------------------#

#Rounding the times to be on the hour so filling missing dates doesnt require hardcoding
one_plot <- transform(one_plot, Date_Time = round.POSIXt(Date_Check, units = c("hours")))

#Adding in missing times so missing data can be seen
#Defining the first and last date
Date.first <- one_plot[1,8]
Date.last <- one_plot[nrow(one_plot), 8]
#Creating a sequence in between the dates and filling in gaps
ts <- seq.POSIXt(as.POSIXct(Date.first, '%m/%d/%y %I:%M:%S %p'), 
                 as.POSIXct(Date.last, '%m/%d/%y %I:%M:%S %p'), by="hour")
ts <- seq.POSIXt(as.POSIXlt(Date.first), as.POSIXlt(Date.last), by="hour")
ts <- as.POSIXct(ts,'%m/%d/%y %I:%M:%S %p')
time_fill <- data.frame(Date_Time=ts)
one_plot['Date_Time'] <- lapply(one_plot['Date_Time'], as.POSIXct) 
one_plot.loop <- full_join(time_fill, one_plot)
one_plot.loop$Date_Check = NULL

#Arranging the columns so they are standard across plots
one_plot.loop <- one_plot.loop[c("Plot_Name", "Date_Time", "Soil_Moisture", "Relative_Humidity",
                                 "PAR", "Soil_Temp", "Air_Temp")]
#Marking NA values as NA
one_plot.loop[!is.na(one_plot.loop$Soil_Temp) & (one_plot.loop$Soil_Temp< -888 | one_plot.loop$Soil_Temp>999), "Soil_Temp"] <- NA
one_plot.loop[!is.na(one_plot.loop$Soil_Moisture) & (one_plot.loop$Soil_Moisture< -0.5| one_plot.loop$Soil_Moisture>1), "Soil_Moisture"] <- NA
one_plot.loop[!is.na(one_plot.loop$PAR) & (one_plot.loop$PAR< -888 | one_plot.loop$PAR>999), "PAR"] <- NA
one_plot.loop[!is.na(one_plot.loop$Air_Temp) & (one_plot.loop$Air_Temp< -888 | one_plot.loop$Air_Temp>999), "Air_Temp"] <- NA
one_plot.loop[!is.na(one_plot.loop$Relative_Humidity) & (one_plot.loop$Relative_Humidity< -888 | one_plot.loop$Relative_Humidity>999), "Relative_Humidity"] <- NA

#Setting the path out to be in the corresponding folder
path.out <- paste(path.personal, "/GitHub/Met_Stations/Data_clean/Rollinson_", Plot.title, sep="")

# Seperating by chosen year and month values
month.check = 0
rows=1
for (i in rows:nrow(one_plot.loop)){
  Date.month <- one_plot.loop[i, "Date_Time"]
  month.extract <- month(Date.month)
  if (!is.na(month.extract)){
    if (month.extract != month.check){
      if(month.extract == 1 | month.extract == 3 | month.extract == 5 | 
         month.extract == 7 | month.extract == 8 | month.extract == 10 | month.extract == 12){
        last.day = "31" 
      } else if(month.extract == 4 | month.extract == 6 | month.extract == 9 | month.extract == 11){
        last.day = "30"
      } else if(month.extract == 2) {last.day = "28"}
      month.file <- ifelse(nchar(month.extract) == 2, month.extract, paste("0", month.extract, sep=""))
      Date.min <- paste(year(Date.month), "-", month.extract, "-01 00:00:00", sep="")
      Date.max <- paste(year(Date.month), "-", month.extract, "-", last.day, " 23:59:59", sep="")
      one_plot.final <- subset(one_plot.loop, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))
      filename <- paste(Plot.title,"-", year(Date.month), "-", month.file, ".csv", sep = "")
      write.csv(one_plot.final, file.path(path.out,  file = filename), row.names = FALSE)
      rows = rows + nrow(one_plot.final)
      if(month.check == 12) month.check = 1 else(month.check = (month.check + 1))
    }
  }
} 