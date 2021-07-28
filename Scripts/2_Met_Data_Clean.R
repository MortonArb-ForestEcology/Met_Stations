#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: This is the primary script for the cleaning of Met station data previously harmonized
#         To convert the raw data we harmonized into clean data
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/Harmonized_data"
# Outputs: Clean data B127.csv
#          U134.csv
#          N115.csv
#          HH115.csv
# Notes: Cleaning in this script is defined as removing impossible values (negative soil moisture) and 
#        removing measurements more than 4 standard deviations from the 2 week mean surrounding the date of measurement
#         
#-----------------------------------------------------------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Setting file paths
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_clean/Harmonized_data", sep="")
setwd(path.out)


#Currently using the Month_Separation script because combining the seperate files results in 147 columns
#Will probably seperate the current month script into a QAQC script and the seperation script

# Seperating by chosen year and month values

plot.B127 <- read.csv("B127/B127.csv")
plot.N115 <- read.csv("N115/N115.csv")
plot.HH115 <- read.csv("HH115/HH115.csv")
plot.U134 <- read.csv("U134/U134.csv")

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb$Date_Time <- as.POSIXct(comb$Date_Time)

#Removing impossible soil_moissture readings
comb$Soil_Moisture <- ifelse(comb$Soil_Moisture <= 0, NA, comb$Soil_Moisture)

#Removing NA rows 
comb <- comb[!is.na(comb$Plot_Name),]

Columns <- c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR")
metrics <- c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR")
df.clean <- data.frame()
for(PLOT in unique(comb$Plot_Name)){
  temp <- comb[comb$Plot_Name == PLOT, ]
  temp$Date <- as.Date(temp$Date_Time)
  for(DOY in unique(format(temp$Date))){
    DOY <- as.Date(DOY)
    DATES <- seq.Date(from = (DOY - 7), to = (DOY + 7), by = 1)
    for(VAL in metrics){
      SM.mean <- mean(temp[temp$Date %in% DATES, VAL], na.rm = T)
      SM.sd <- sd(temp[temp$Date %in% DATES, VAL], na.rm = T)
      temp[temp$Date == DOY, "SIGFLAG"] <- ifelse(temp[temp$Date == DOY, VAL]<SM.mean-4*SM.sd | temp[temp$Date == DOY, VAL]>SM.mean+4*SM.sd, T, F)
    }
  }
  df.clean <- rbind(df.clean, temp)
}
df.clean <- df.clean[which(df.clean$SIGFLAG == F),]
df.clean$Date_Time <- as.POSIXct(df.clean$Date_Time)

for(PLOT in unique(df.clean$Plot_Name)){
  temp <- df.clean[df.clean$Plot_Name == PLOT,]
  path.fin <- paste(path.met, "Data_Clean/Clean_data/", PLOT, sep="")
  filename <- paste(PLOT, ".csv", sep = "")
  write.csv(temp, file.path(path.fin,  file = filename), row.names = FALSE)
}



