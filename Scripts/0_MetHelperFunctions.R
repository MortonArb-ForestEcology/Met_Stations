# Funciton now used in several scripts... lets just give it one place for our sanity ----
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



# Function to append new files to each other ----
combineMetFiles <- function(plotID, pathPlot, filesPlot){
  
  cols.final <- c("Plot_Name", "Timestamp", "Date", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPa.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")
  
  # NOTE: Will be assuming NO DST per what we've seen in some recent past data
  datAll <- data.frame()
  for(FILE in filesPlot){
    # date <- pull.datAll[i]
    fNow <- read.csv(file.path(pathPlot, FILE), na.strings=c("#N/A", "NA", ""))
    
    #Getting rid of columns without the sensors we want
    fNow <- fNow[,c(grep("z6", names(fNow)), which(fNow[1,] %in% sensorList))]
    # head(fNow)
    
    
    #Renaming to harmonize with old data
    colnames(fNow) <- renameCols(fNow[fNow[grep("z6", names(fNow))] == "Timestamp",])
    fNow <- fNow[-(grep("Records", fNow$Timestamp)),]
    fNow <- fNow[-(grep("Timestamp", fNow$Timestamp)),]
    # head(fNow)
    
    
    # 1. Check the Timestamp column and use that preferentially
    timeGone <- which(is.na(fNow$Timestamp))
    if(length(timeGone)>0) fNow$Timestamp[timeGone] <- fNow$Date_Time[timeGone]
    
    # 2. Set up a place holder for fixed dates
    fixDates <- as.POSIXct(rep(NA, length=nrow(fNow)), format="%Y-%m-%d %H", tz='Etc/GMT+6') # Use a blank vector otherwise you'll add a column you eventually want to just drop
    
    # 3. First do any dates with the dash because those are (generally) in the format I want
    rowsDash <- grep("-", fNow$Timestamp)
    
    # 3.b. The problem with the dash formats I've seen in some drop midnight!  Ugh!  Whyyyyyy did this happen??
    dateDashNA <- which(is.na(strptime(fNow$Timestamp[rowsDash], format="%Y-%m-%d %H")))
    fNow$Timestamp[rowsDash[dateDashNA]]<- paste(fNow$Timestamp[rowsDash[dateDashNA]], "0:00")
    
    fixDates[rowsDash] <- as.POSIXct(strptime(fNow$Timestamp[rowsDash], format="%Y-%m-%d %H"), tz='Etc/GMT+6') 
    
    # 4. Now deal with the slash formats.  They are generally cleaner, but need to be reformatted out of the wonky way Americans write time
    rowsSlash <- grep("/", fNow$Timestamp)
    fixDates[rowsSlash] <- as.POSIXct(strptime(fNow$Timestamp[rowsSlash], format="%m/%d/%Y %H"), format="%Y-%m-%d %H", tz='Etc/GMT+6') 
    
    # 5. Now that we have everything in a formatted vector, overwrite the entire column at once; this shoudl cause the weirdness that happened when we tried to fix or work with subsets
    # fixDates[(length(fixDates)-48):length(fixDates)]
    # fNow$Timestamp[(length(fixDates)-48):length(fixDates)]
    
    fNow$Timestamp <- fixDates
    fNow$Date <- as.Date(trunc(fNow$Timestamp, 'days')) # If just do as.Date, it will round and that sucks
    fNow$Plot_Name <- plotID
    
    # For some reason a column was dropped at some point!  No bueno! need to add that column in
    if(!all(cols.final %in% names(fNow))){
      fNow[,cols.final[!cols.final %in% names(fNow)]] <- NA
    }
    fNow <- fNow[,cols.final]
    
    
    if(nrow(datAll)>0){
      datAll <- rbind(datAll, fNow)
    } else {
      datAll <- fNow
    }
  }
  colsNum <- cols.final[!cols.final %in% c("Plot_Name", "Timestamp", "Date", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")]
  datAll[,colsNum] <- apply(datAll[,colsNum], 2, FUN=as.numeric)
  summary(datAll)
  
  return(datAll)
}


cleanMet <- function(metData){
  #Removing impossible values
  if(any(metData$Soil_Moisture[!is.na(metData$Soil_Moisture)]<0)){
    badVals <- which(!is.na(metData$Soil_Moisture & metData$Soil_Moisture<0))
    metData$SIGFLAG_Soil_Moisture[badVals] <- T
    metData$Soil_Moisture[badVals] <- NA
  }
  if(any(metData$Soil_Temp[!is.na(metData$Soil_Temp)]>60)){
    badVals <- which(!is.na(metData$Soil_Temp & metData$Soil_Temp>60))
    metData$SIGFLAG_Soil_Temp[badVals] <- T
    metData$Soil_Temp[badVals] <- NA
  }
  if(any(metData$Air_Temp[!is.na(metData$Air_Temp)]>60)){
    badVals <- which(!is.na(metData$Air_Temp & metData$Air_Temp>60))
    metData$SIGFLAG_Air_Temp[badVals] <- T
    metData$Air_Temp[badVals] <- NA
  }
  # # Apparently Relative Humidity *can* exceed 1, so lets skip this one
  # if(any(metData$Relative_Humidity[!is.na(metData$Relative_Humidity)]>1)){
  #   badVals <- which(!is.na(metData$Relative_Humidity & metData$Relative_Humidity>60))
  #   metData$SIGFLAG_Relative_Humidity[badVals] <- T
  #   metData$Relative_Humidity[badVals] <- NA
  # }
  

}