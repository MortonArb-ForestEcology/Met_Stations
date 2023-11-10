library(lubridate)
# Checking old files to rectify timestamp errors
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.in <- paste(path.met, "Data_processed/Clean_data-Wonky", sep="")
path.out <- paste(path.met, "Data_processed/Clean_data", sep="")

# Comign up with all the columns we want and just listing them in an order I want them

cleanMetFiles <- function(metNow){
  cols.final <- c("Plot_Name", "Timestamp", "Date", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPa.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")
  
  # 1. Check the Timestamp column and use that preferentially
  timeGone <- which(is.na(metNow$Timestamp))
  if(length(timeGone)>0) metNow$Timestamp[timeGone] <- metNow$Date_Time[timeGone]
  
  # 2. Set up a place holder for fixed dates
  fixDates <- as.POSIXct(NA, length=nrow(metNow)) # Use a blank vector otherwise you'll add a column you eventually want to just drop
  
  # 3. First do any dates with the dash because those are (generally) in the format I want
  rowsDash <- grep("-", metNow$Timestamp)
  
  # 3.b. The problem with the dash formats I've seen in some drop midnight!  Ugh!  Whyyyyyy did this happen??
  dateDashNA <- which(is.na(strptime(metNow$Timestamp[rowsDash], format="%Y-%m-%d %H")))
  metNow$Timestamp[rowsDash[dateDashNA]]<- paste(metNow$Timestamp[rowsDash[dateDashNA]], "0:00")
  
  fixDates[rowsDash] <- as.POSIXct(strptime(metNow$Timestamp[rowsDash], format="%Y-%m-%d %H")) 
  
  # 4. Now deal with the slash formats.  They are generally cleaner, but need to be reformatted out of the wonky way Americans write time
  rowsSlash <- grep("/", metNow$Timestamp)
  fixDates[rowsSlash] <- as.POSIXct(strptime(metNow$Timestamp[rowsSlash], format="%m/%d/%Y %H"), format="%Y-%m-%d %H") 
  
  # 5. Now that we have everything in a formatted vector, overwrite the entire column at once; this shoudl cause the weirdness that happened when we tried to fix or work with subsets
  metNow$Timestamp <- fixDates
  metNow$Date <- as.Date(trunc(metNow$Timestamp, 'days')) # If just do as.Date, it will round and that sucks
  
  # 6. Fix column ordering to be consistent and make sense!
  if(any(cols.final[1:8][!cols.final[1:8] %in% names(metNow)])){
    stop("Need to fix columns")
  } 
  if(any(!cols.final %in% names(metNow))) { 
    metNow[,cols.final[!cols.final %in% names(metNow)]] <- NA
    warning("Adding Blank Columns; mostly a retro-fit") 
  }
  metNow <- metNow[,cols.final]
  # summary(metNow)
  return(metNow)
  
}


# plots()

# cleanB127 <- dir(file.path(path.in, "B127"), ".csv")
plotEW <- dir(path.in)
plotEW <- plotEW[!grepl(".csv", plotEW)]

for(PLOT in plotEW){
  fYears <- dir(file.path(path.in, PLOT), ".csv")
  for(fNow in fYears){
    print(fNow)
    metNow <- read.csv(file.path(path.in, PLOT, fNow))
    
    fClean <- cleanMetFiles(metNow=metNow)
    
    write.csv(fClean, file.path(path.out, PLOT, fNow), row.names=F)
  }
}


TEST <- read.csv(file.path(path.out, "HH115", "HH115_2017.csv"))
head(TEST)
tail(TEST)

TEST2 <- read.csv(file.path(path.out, "U134", "U134_2022.csv"))
head(TEST2)
tail(TEST2)

