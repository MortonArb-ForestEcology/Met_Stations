#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: This is the primary script for the cleaning of Met station data previously harmonized
#         To convert the raw data we harmonized into clean data. We save the flagged data back as raw flagged data
# Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/Harmonized_data"
# Outputs: Clean data B127.csv, U134.csv, N115.csv, HH115.csv
#          Flagged harmonized data B127.csv, U134.csv, N115.csv, HH115.csv 
# Notes: Cleaning in this script is defined as removing impossible values (negative soil moisture) and 
#        removing measurements more than 4 standard deviations from the 2 week mean surrounding the date of measurement
#         
#-----------------------------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Setting file paths
path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_processed/Harmonized_data", sep="")
path.clean <- paste(path.met, "Data_processed/Clean_data", sep="")
#setwd(path.out)

# Seperating by chosen year and month values
plot.B127 <- read.csv(file.path(path.out,"B127.csv"))
plot.N115 <- read.csv(file.path(path.out,"N115.csv"))
plot.HH115 <- read.csv(file.path(path.out,"HH115.csv"))
plot.U134 <- read.csv(file.path(path.out,"U134.csv"))

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb$Date_Time <- as.POSIXct(comb$Timestamp)

#Removing impossible soil_moissture readings
comb$Soil_Moisture <- ifelse(comb$Soil_Moisture <= 0, NA, comb$Soil_Moisture)

#Removing impossible air_temp readings
comb$Air_Temp <- ifelse(comb$Air_Temp >= 150, NA, comb$Air_Temp)

comb$Relative_Humidity <-  as.numeric(comb$Relative_Humidity)

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
    #Checking if a value deviates illogically from other values centered around it
    for(VAL in metrics){
      SM.mean <- mean(temp[temp$Date %in% DATES, VAL], na.rm = T)
      SM.sd <- sd(temp[temp$Date %in% DATES, VAL], na.rm = T)
      temp[temp$Date == DOY, paste0("SIGFLAG_", VAL)] <- ifelse(temp[temp$Date == DOY, VAL]<SM.mean-4*SM.sd | temp[temp$Date == DOY, VAL]>SM.mean+4*SM.sd, T, F)
    }
  }
  df.clean <- rbind(df.clean, temp)
}

#Removing our flagged values to create a clean dataframe.
df.clean$Soil_Moisture <- ifelse(df.clean$SIGFLAG_Soil_Moisture == T, NA, df.clean$Soil_Moisture )
df.clean$Soil_Temp <- ifelse(df.clean$SIGFLAG_Soil_Temp== T, NA, df.clean$Soil_Temp )
df.clean$Air_Temp <- ifelse(df.clean$SIGFLAG_Air_Temp== T, NA, df.clean$Air_Temp )
df.clean$Relative_Humidity <- ifelse(df.clean$SIGFLAG_Relative_Humidity== T, NA, df.clean$Relative_Humidity )
df.clean$PAR <- ifelse(df.clean$SIGFLAG_PAR== T, NA, df.clean$PAR)

# Seperating by chosen year
for(PLOT in unique(comb$Plot_Name)){
  year <- year(Sys.Date())
  
  dir.plot <- dir(file.path(path.clean, PLOT), ".csv")
  
  #Way of finding the oldest and most recent file to be deleted.
  old.path <- dir.plot[match(1, stringr::str_detect(dir.plot, "up_to_"))]
  old.plot <- read.csv(file.path(path.clean, PLOT, old.path))
  old.yearb <- read.csv(file.path(path.clean, PLOT,  file = paste(PLOT,"_" ,year-1, ".csv", sep="")))
  
  #Deleting the old version
  if (file.exists(file.path(path.clean, PLOT, old.path))) {
    unlink(file.path(path.clean, PLOT, old.path))
    cat("The file is deleted")
  }
  
  new.plot <- df.clean[df.clean$Plot_Name == PLOT,]
  new.plot$Date_Time <- as.POSIXct(new.plot$Timestamp)
  new.plot$year <- lubridate::year(new.plot$Date_Time)

  
  old.comb <- rbind(old.yearb, old.plot) 
  old.comb$Date_Time <- as.POSIXct(old.comb$Date_Time)
  colnames(old.comb) <- c("Date_Time" ,"Plot_Name" ,"Soil_Moisture", "Relative_Humidity", "PAR" , "Soil_Temp" , "Air_Temp", "Timestamp",                 
                          "mm.Precipitation" ,"Lightning.Activity","km.Lightning.Distance","X..Wind.Direction", "m.s.Wind.Speed" , "m.s.Gust.Speed",
                          "kPa.Atmospheric.Pressure", "X..X.axis.Level", "X..Y.axis.Level", "mm.h.Max.Precip.Rate", "X.C.RH.Sensor.Temp", "X..Battery.Percent", "mV.Battery.Voltage", 
                          "kPa.Reference.Pressure", "X.C.Logger.Temperature", "Date", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity" ,"SIGFLAG_PAR", "year")
  
  both.comb <- rbind(old.comb, new.plot)
  
  #Creating the previous year's complete file
  one_plot.old <- both.comb[both.comb$year <= year-1 & both.comb$year > year-2,]
  write.csv(one_plot.old, file.path(path.clean, PLOT,  file = paste(PLOT,"_" ,year-1, ".csv", sep="")), row.names = FALSE)
  
  one_plot.year <- both.comb[both.comb$year <= year & both.comb$year > year-1, ]
  Date.min <- min(one_plot.year$Date_Time)
  Date.max <- max(one_plot.year$Date_Time)
  
  #Creating a file name that includes the ending of current measurements
  filename <- paste(PLOT,"_", lubridate::year(as.Date(Date.max)), "_up_to_" , as.Date(Date.max), ".csv", sep = "")
  write.csv(one_plot.year, file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
  
}


