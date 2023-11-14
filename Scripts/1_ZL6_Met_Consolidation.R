#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick, Christy Rollinson
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

cols.final <- c("Plot_Name", "Timestamp", "Date", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPa.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")

# For when things go weird, we need to . instead of the slashes that were in some old files
colOrderGood <- gsub(" ", ".", colOrder)
colOrderGood <- gsub("-", ".", colOrderGood)
colOrderGood <- gsub("/", ".", colOrderGood)

sensorList <- c("ATMOS 41", "TEROS 11", "Battery", "Barometer")


#-------------------------------------------------#
# Loop through each Plot
#-------------------------------------------------#
plotsAll <- dir(path.clean)
for(PLOT in plotsAll){
  print(PLOT)
  #Reading in our old file with complete data
  dir.old.plot <- as.data.frame(dir(file.path(path.clean, PLOT), ".csv"))
  colnames(dir.old.plot) <- "file"
  
  # Finding a working file for the current year
  path.plot <-  dir.old.plot[stringr::str_detect(dir.old.plot$file, 'up_to'),]
  
  # If no current file, pull the oldest one
  if(length(path.plot)==0) path.plot <- dir.old.plot[nrow(dir.old.plot),]
  # path.plot
  
  old.plot <- read.csv(file.path(path.clean, PLOT, path.plot), na.strings=c("#N/A", "NA", ""))
  summary(old.plot)
  # head(old.plot)
  # tail(old.plot)
   
  # everything should be in Central Standard Time!
  old.plot$Timestamp <- as.POSIXct(old.plot$Timestamp, tz="Etc/GMT+6") 
  # summary(old.plot$Timestamp)
  # head(old.plot)
  # tail(old.plot)
  # dim(old.plot)
    # summary(old.plot)

  #Finding the last date we have data for
  end.plot <- max(old.plot$Timestamp, na.rm = T)
  
  # end.plot <- sub(" .*", "", end.plot)
  
  #Finding the new files
  pathPlotNew <- file.path(path.raw, paste("Meter", PLOT, sep="_"))
  dir.plot <- dir(pathPlotNew, ".csv")
  
  # #Pulling out a list of new dates to pull specific files
  split.plot <- strsplit(dir.plot, "_")
  split.plot <- lapply(split.plot, function (x) x[2])
  date.plot <- unlist(lapply(split.plot, function (x) sub(".csv", "", x)))
  date.plot <- as.Date(date.plot)
  
  pull.plot <- dir.plot[which(date.plot > end.plot)]
  pull.plot
  
  if(length(pull.plot)==0) next
  #Loop for pulling files in case there is more than 1
  plotNew <- combineMetFiles(plotID=PLOT, pathPlot=pathPlotNew, filesPlot=pull.plot)
  summary(plotNew)
  
  # Do some data cleaning -- skipping this step for now so we're better capturing what's going on
  # B127Clean <- cleanMet(metData=plotNew)
  
  # Get rid of any old data
  plotNew <- plotNew[plotNew$Timestamp>end.plot,]
  summary(plotNew)
  
  # Now combining new files with old files
  if(lubridate::mday(end.plot)=="31" & lubridate::month(end.plot)==12 & lubridate::hour(end.plot)==23){
    plotAll <- plotNew
  } else {
    plotAll <- rbind(old.plot, plotNew)
  }
  summary(plotAll)
  
  # Insert any missing timesteps
  stampMin <- min(plotAll$Timestamp, na.rm = T)
  stampMax <- max(plotAll$Timestamp, na.rm = T)
  tsCheck <- data.frame(Plot_Name=PLOT, Timestamp=seq.POSIXt(stampMin, stampMax, by="hour", tz="Etc/GMT+6"))
  tsCheck$Date <- as.Date(trunc(tsCheck$Timestamp, 'days'))
  summary(tsCheck)
  
  # Merge in missing dates/times
  if(nrow(tsCheck)!=nrow(plotAll)){
    plotAll <- merge(plotAll, tsCheck, all=T)
    plotAll <- plotAll[order(plotAll$Timestamp),cols.final] # Putting everythign is a "good" order
    summary(plotAll)
  }
  
  # Checking to see if we need to finish off last year's file
  yrsFile <- unique(lubridate::year(plotAll$Timestamp))
  if(length(yrsFile)>1) stop("Get Christy to figure out what to do about past years!")
  # If we have >1 year, we need to create the final final for last year
  # THIS WILL NEED CHRISTY'S HELP
  
  # Now working with the current year, which is relatively easy-peasy
  # Start by writing all the data for the current/most recent year
  rowsYrMax <- which(lubridate::year(plotAll$Timestamp)==max(yrsFile))
  
  filename <- paste0(PLOT,"_", max(yrsFile), "_up_to_" , max(plotAll$Date), ".csv")
  write.csv(plotAll[rowsYrMax,], file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
  
  # If we now have >1 "up_to" file for the year, delete it
  fWorking <- dir(file.path(path.clean, PLOT), paste0(PLOT,"_", max(yrsFile), "_up_to_"))
  if(length(fWorking)>1){
    for(i in 1:(length(fWorking)-1)){
      file.remove(file.path(path.clean, PLOT, fWorking[i]))
    }
  }
}

