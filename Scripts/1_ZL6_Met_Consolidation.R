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
#path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots"
path.clean <- file.path(path.met, "Data_processed/Clean_data")
path.raw <- file.path(path.met, "Data_raw")


# Setting up a function for renaming columns
source("0_MetHelperFunctions.R")


sensorList <- c("ATMOS 41", "TEROS 11", "Battery", "Barometer")

cols.final <- c("Plot_Name", "Timestamp", "Date", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPa.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")



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
  old.plot$Date <- as.Date(old.plot$Date)
  ## Note: Dates are being weird; we appear to need to the the format step when we save; not when we read in
  # old.plot$Timestamp <- format(as.POSIXct(old.plot$Timestamp, tz="Etc/GMT+6"), format="%Y-%m-%d %H:%M:%S")
  old.plot$Timestamp <- as.POSIXct(old.plot$Timestamp, tz="Etc/GMT+6")
  summary(old.plot) # Make sure that date & time are continuous
  # head(old.plot)
  # tail(old.plot)
   
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
  # plotNew$Timestamp[1:30]
  # head(plotNew) # make sure to check for 00s at midnight
  # tail(plotNew)
  
  # ---------------
  # Do some data cleaning -- skipping this step for now so we're better capturing what's going on
  # ---------------
  # This is a real ugly way of doing it, but clearly this needs to be fixed at the moment
  plotNew$Relative_Humidity[!is.na(plotNew$Relative_Humidity) & plotNew$Relative_Humidity<5] <- plotNew$Relative_Humidity[!is.na(plotNew$Relative_Humidity) & plotNew$Relative_Humidity<5]*100
  # ---------------
  
  # Get rid of any old data
  plotNew <- plotNew[plotNew$Timestamp>end.plot,]
  summary(plotNew)
  
  
  # names(plotNew)[!names(plotNew) %in% names(old.plot)]
  # Now combining new files with old files
  if(lubridate::mday(end.plot)=="31" & lubridate::month(end.plot)==12 & lubridate::hour(end.plot)==23){
    plotAll <- plotNew
  } else {
    plotAll <- rbind(old.plot, plotNew)
  }
  summary(plotAll)
  # head(plotAll)
  
  # Insert any missing timesteps
  stampMin <- min(plotAll$Timestamp, na.rm = T)
  stampMax <- max(plotAll$Timestamp, na.rm = T)
  tsCheck <- data.frame(Plot_Name=PLOT, Timestamp=seq.POSIXt(stampMin, stampMax, by="hour", tz="Etc/GMT+6"))
  tsCheck$Date <- as.Date(trunc(tsCheck$Timestamp, 'days'))
  tsCheck$Timestamp <- as.POSIXct(tsCheck$Timestamp, tz="Etc/GMT+6")
  # summary(tsCheck)
  # head(tsCheck) # Make sure to check for 00s at midnight
  
  # Merge in missing dates/times
  if(nrow(tsCheck)!=nrow(plotAll)){
    plotAll <- merge(plotAll, tsCheck, all=T)
    plotAll <- plotAll[order(plotAll$Timestamp),cols.final] # Putting everythign is a "good" order
    summary(plotAll)
  }
  # head(plotAll)
  
  # Checking to see if we need to finish off last year's file
  yrsFile <- unique(lubridate::year(plotAll$Timestamp))
  if(length(yrsFile)==2){
    # First, lets close out last year's file and then remove that data from plotAll so we can continue as we did
    rowsYrMin <- lubridate::year(plotAll$Timestamp)==min(yrsFile)
    plotPast <- plotAll[rowsYrMin, ]
    # summary(plotPast)
    
    # For some reason, this step seems important right before the writing step, but then it reads in just fine
    plotPast$Timestamp <- format(as.POSIXct(plotPast$Timestamp, tz="Etc/GMT+6"), format="%Y-%m-%d %H:%M:%S")
    
    
    filename <- paste0(PLOT,"_", min(yrsFile), ".csv")
    write.csv(plotPast, file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
    
    # Get rid of any old working files
    fWorking <- dir(file.path(path.clean, PLOT), paste0(PLOT,"_", min(yrsFile), "_up_to_"))
    if(length(fWorking)>0){
      for(i in 1:(length(fWorking))){
        file.remove(file.path(path.clean, PLOT, fWorking[i]))
      }
    }
    
    plotAll <- plotAll[!rowsYrMin,]
  } else if(length(yrsFile)>2){
    stop("Get Christy to figure out what to do if we have many years.\nThis means it's been a loooong time since we've done met QAQC, and that's not a good thing! :-( ")
    # Note: this should be a simple modification of the above portion, but I'm concerned if we've gone >1 yr without QAQC
  }
  
  # # Now working with the current year, which is relatively easy-peasy
  # # Start by writing all the data for the current/most recent year
  # rowsYrMax <- which(lubridate::year(plotAll$Timestamp)==max(yrsFile))
  
  # summary(plotAll$Timestamp)
  # plotAll$Timestamp[1:30]
  # 
  # For some reason, this step seems important right before the writing step, but then it reads in just fine
  plotAll$Timestamp <- format(as.POSIXct(plotAll$Timestamp, tz="Etc/GMT+6"), format="%Y-%m-%d %H:%M:%S")

  filename <- paste0(PLOT,"_", max(yrsFile), "_up_to_" , max(plotAll$Date), ".csv")
  write.csv(plotAll[,], file.path(path.clean, PLOT,  file = filename), row.names = FALSE)
  
  # # Adding a check to make sure the timestamp is working
  # TEST <- read.csv(file.path(path.clean, PLOT,  file = filename))
  # TEST$Timestamp <- as.POSIXct(TEST$Timestamp, tz="Etc/GMT+6")
  # TEST$Date <- as.Date(TEST$Date)
  # summary(TEST)
  # head(TEST)
  # tail(TEST)
  
  # If we now have >1 "up_to" file for the year, delete it
  fWorking <- dir(file.path(path.clean, PLOT), paste0(PLOT,"_", max(yrsFile), "_up_to_"))
  if(length(fWorking)>1){
    for(i in 1:(length(fWorking)-1)){
      file.remove(file.path(path.clean, PLOT, fWorking[i]))
    }
  }
}

