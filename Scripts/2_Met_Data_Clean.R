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

# From script 1 -->  Setting up a function for renaming columns this will be necessary for harmonizing files
renameCols <- function(x){
  vecCols <- x
  vecCols[grep("Solar Radiation", vecCols, useBytes = T)] <- "PAR"
  vecCols[grep("mm Precipitation", vecCols, useBytes = T)] <- "mm Precipitation"
  vecCols[grep("mm.Precipitation", vecCols, useBytes = T)] <- "mm.Precipitation"
  vecCols[grep("Lightning Activity", vecCols, useBytes = T)] <- "Lightning Activity"
  vecCols[grep("Lightning.Activity", vecCols, useBytes = T)] <- "Lightning.Activity"
  vecCols[grep("Lightning Distance", vecCols, useBytes = T)] <- "km Lightning Distance"
  vecCols[grep("Lightning.Distance", vecCols, useBytes = T)] <- "km.Lightning.Distance"
  vecCols[grep("Wind Direction", vecCols, useBytes = T)] <- "deg Wind Direction"
  vecCols[grep("Wind.Direction", vecCols, useBytes = T)] <- "deg.Wind.Direction"
  vecCols[grep("Wind Speed", vecCols, useBytes = T)] <- "m/s Wind Speed"
  vecCols[grep("Wind.Speed", vecCols, useBytes = T)] <- "m.s.Wind.Speed"
  vecCols[grep("Gust Speed", vecCols, useBytes = T)] <- "m/s Gust Speed"
  vecCols[grep("Gust.Speed", vecCols, useBytes = T)] <- "m.s.Gust.Speed"
  vecCols[grep("Air Temp", vecCols, useBytes = T)] <- "Air_Temp"
  vecCols[grep("Air_Temp", vecCols, useBytes = T)] <- "Air_Temp" # Putting these in just in case there's some extraneous wonk
  vecCols[grep("Vapor Press", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Relative Humidity", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Relative_Humidity", vecCols, useBytes = T)] <- "Relative_Humidity"
  vecCols[grep("Atmospheric Press", vecCols, useBytes = T)] <- "kPa Atmospheric Pressure"
  vecCols[grep("Atmospheric.Press", vecCols, useBytes = T)] <- "kPa.Atmospheric.Pressure"
  vecCols[grep("VPD", vecCols, useBytes = T)] <- "kPa VPD"
  vecCols[grep("VPD", vecCols, useBytes = T)] <- "kPa.VPD"
  vecCols[grep("X-axis", vecCols, useBytes = T)] <- "deg X-axis Level"
  vecCols[grep("X.axis", vecCols, useBytes = T)] <- "deg.X.axis.Level"
  vecCols[grep("Y-axis", vecCols, useBytes = T)] <- "deg Y-axis Level"
  vecCols[grep("Y.axis", vecCols, useBytes = T)] <- "deg.Y.axis.Level"
  vecCols[grep("Max Precip", vecCols, useBytes = T)] <- "mm/h Max Precip Rate"
  vecCols[grep("Max.Precip", vecCols, useBytes = T)] <- "mm.h.Max.Precip.Rate"
  vecCols[grep("RH Sensor Temp", vecCols, useBytes = T)] <- "degC RH Sensor Temp"
  vecCols[grep("RH.Sensor.Temp", vecCols, useBytes = T)] <- "degC.RH.Sensor.Temp"
  vecCols[grep("Water Content", vecCols, useBytes = T)] <- "Soil_Moisture"
  vecCols[grep("Soil_Moisture", vecCols, useBytes = T)] <- "Soil_Moisture"
  vecCols[grep("Soil Temp", vecCols, useBytes = T)] <- "Soil_Temp"
  vecCols[grep("Soil_Temp", vecCols, useBytes = T)] <- "Soil_Temp"
  vecCols[grep("Battery Percent", vecCols, useBytes = T)] <- "Battery Percent"
  vecCols[grep("Battery.Percent", vecCols, useBytes = T)] <- "Battery.Percent"
  vecCols[grep("Battery Voltage", vecCols, useBytes = T)] <- "mV Battery Voltage"
  vecCols[grep("Battery.Voltage", vecCols, useBytes = T)] <- "mV.Battery.Voltage"
  vecCols[grep("Reference Press", vecCols, useBytes = T)] <- "kPa Reference Pressure"
  vecCols[grep("Reference.Press", vecCols, useBytes = T)] <- "kPa.Reference.Pressure"
  vecCols[grep("Logger Temp", vecCols, useBytes = T)] <- "degC Logger Temperature"
  vecCols[grep("Logger.Temp", vecCols, useBytes = T)] <- "degC.Logger.Temperature"
  
  return(vecCols)
}

#Setting file paths
# path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"

path.out <- paste(path.met, "Data_processed/Harmonized_data", sep="")
path.clean <- paste(path.met, "Data_processed/Clean_data", sep="")
#setwd(path.out)

# Seperating by chosen year and month values
plot.B127 <- read.csv(file.path(path.out,"B127.csv"), na.strings=c("", "NA", "#N/A"))
plot.N115 <- read.csv(file.path(path.out,"N115.csv"), na.strings=c("", "NA", "#N/A"))
plot.HH115 <- read.csv(file.path(path.out,"HH115.csv"), na.strings=c("", "NA", "#N/A"))
plot.U134 <- read.csv(file.path(path.out,"U134.csv"), na.strings=c("", "NA", "#N/A"))

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb$Plot_Name <- as.factor(comb$Plot_Name)
comb$Date_Time <- as.POSIXct(comb$Timestamp)
summary(comb)

#Removing impossible soil_moissture readings
comb$Soil_Moisture <- ifelse(comb$Soil_Moisture <= 0, NA, comb$Soil_Moisture)

#Removing impossible air_temp readings
comb$Air_Temp <- ifelse(comb$Air_Temp >= 65, NA, comb$Air_Temp)
comb$Soil_Temp <- ifelse(comb$Soil_Temp >= 65, NA, comb$Air_Temp)

# comb$Relative_Humidity <-  as.numeric(comb$Relative_Humidity)
# summary(comb[comb$Relative_Humidity>1 & !is.na(comb$Relative_Humidity)& comb$Plot_Name=="B127",])
summary(comb[comb$Relative_Humidity>1 & !is.na(comb$Relative_Humidity),])


#Removing NA rows 
comb <- comb[!is.na(comb$Plot_Name) & !is.na(comb$Date_Time),]
summary(comb)

# Adding flags for outliers
# Columns <- c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR")
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
summary(df.clean)

#Removing our flagged values to create a clean dataframe.
# df.clean$Soil_Moisture <- ifelse(df.clean$SIGFLAG_Soil_Moisture == T, NA, df.clean$Soil_Moisture )
# df.clean$Soil_Temp <- ifelse(df.clean$SIGFLAG_Soil_Temp== T, NA, df.clean$Soil_Temp )
# df.clean$Air_Temp <- ifelse(df.clean$SIGFLAG_Air_Temp== T, NA, df.clean$Air_Temp )
# df.clean$Relative_Humidity <- ifelse(df.clean$SIGFLAG_Relative_Humidity== T, NA, df.clean$Relative_Humidity )
# df.clean$PAR <- ifelse(df.clean$SIGFLAG_PAR== T, NA, df.clean$PAR)

# Seperating by chosen year
for(PLOT in unique(comb$Plot_Name)[2:4]){
  yrNow <- year(Sys.Date())
  
  dir.plot <- dir(file.path(path.clean, PLOT), ".csv")
  
  #Way of finding the oldest and most recent file to be deleted.
  old.path <- dir.plot[match(1, stringr::str_detect(dir.plot, "up_to_"))]
  
  old.yearb <- read.csv(file.path(path.clean, PLOT,  file = paste(PLOT,"_" ,yrNow-1, ".csv", sep="")), na.strings=c("", "NA", "#N/A"))
  # Fixing column names of old data
  colnames(old.yearb)[!grepl("SIGFLAG", names(old.yearb))] <- renameCols(names(old.yearb)[!grepl("SIGFLAG", names(old.yearb))])
  # summary(old.yearb)
  
  if(length(old.path[!is.na(old.path)])>0){
    old.plot <- read.csv(file.path(path.clean, PLOT, old.path))
    colnames(old.plot)[!grepl("SIGFLAG", names(old.plot))] <- renameCols(names(old.plot)[!grepl("SIGFLAG", names(old.plot))])
    #Deleting the old version
    if (file.exists(file.path(path.clean, PLOT, old.path))) {
      unlink(file.path(path.clean, PLOT, old.path))
      paste0(PLOT, ": old file is deleted")
    }
  } else {
    old.plot <- data.frame()
  }
  
  new.plot <- df.clean[df.clean$Plot_Name == PLOT,]
  new.plot$Date_Time <- as.POSIXct(new.plot$Timestamp)
  new.plot$year <- lubridate::year(new.plot$Date_Time)

  
  old.comb <- rbind(old.yearb, old.plot) 
  old.comb$Date_Time <- as.POSIXct(old.comb$Date_Time)

  # Adding blank columns for anything that doesn't match
  # names(old.comb)[!names(old.comb) %in% names(new.plot)] # Old column names that have been lost
  # names(new.plot)[!names(new.plot) %in% names(old.comb)] # New column names that have been added
   
  if(any(!names(old.comb) %in% names(new.plot))) new.plot[,names(old.comb)[!names(old.comb) %in% names(new.plot)]] <- NA
  old.comb[,names(new.plot)[!names(new.plot) %in% names(old.comb)]] <- NA
  
  both.comb <- rbind(old.comb[,names(new.plot)], new.plot)
  # If the above line bonks, make sure columns match
  
  #Creating the previous year's complete file
  one_plot.old <- both.comb[both.comb$year == yrNow-1,]
  write.csv(one_plot.old, file.path(path.clean, PLOT,  file = paste(PLOT,"_" ,yrNow-1, ".csv", sep="")), row.names = FALSE)
  
  one_plot.NOW <- both.comb[both.comb$year == yrNow, ]
  Date.min <- min(one_plot.year$Date_Time)
  Date.max <- max(one_plot.year$Date_Time)
  
  #Creating a file name that includes the ending of current measurements
  filename <- paste(PLOT,"_", lubridate::year(as.Date(Date.max)), "_up_to_" , as.Date(Date.max), ".csv", sep = "")
  write.csv(one_plot.NOW, file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
  
}


