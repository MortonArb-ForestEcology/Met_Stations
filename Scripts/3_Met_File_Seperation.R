#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: To further process the sensor data into yearly and monthly csv's
# Inputs: Plot csv's created by script 1_Met_Consolidation.R
#         B127.csv
#         U134.csv
#         N115.csv
#         HH115.csv
# Outputs: Yearly and Monthly csv's for each plot (B127, U134, N115, HH115)
# Notes: With updates this script can hopefully only work with new data
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting file paths
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_processed/Clean_data", sep="")

# Seperating by chosen year and month values
plot.B127 <- read.csv(file.path(path.out,"B127/B127.csv"))
plot.N115 <- read.csv(file.path(path.out,"N115/N115.csv"))
plot.HH115 <- read.csv(file.path(path.out,"HH115/HH115.csv"))
plot.U134 <- read.csv(file.path(path.out,"U134/U134.csv"))


comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb <- comb[!is.na(comb$Plot_Name),]

for(PLOT in unique(comb$Plot_Name)){
  month.check = 0
  rows=1
  one_plot.loop <- comb[comb$Plot_Name == PLOT,]
  
  #Making sure we only update and add the neccessary files
  old.files <- dir(file.path(path.met,"Data_processed/Clean_data" ,PLOT), ".csv")
  
  split <- strsplit(old.files, "_")
  
  split <- split[-1]
  
  split <- lapply(split, function (x) x[2])
  
  date <- unlist(lapply(split, function (x) sub(".csv", "", x)))
  
  #This is working right now for reasons I don't understand. Why can't it translate this date?
  #test <- as.Date(as.character(date), format = "%Y-%m")
  
  #latest <- max(date, na.rm = T)

  
  one_plot.loop$Date_Time <- as.Date(one_plot.loop$Date_Time)
  
  #one_plot.loop <- one_plot.loop[one_plot.loop$Date_Time >= latest, ]
  
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
        
        
        one_plot.month <- subset(one_plot.loop, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))
        filename <- paste(PLOT,"_", year(Date.month), "-", month.file, ".csv", sep = "")
        write.csv(one_plot.month, file.path(path.out ,PLOT ,  file = filename), row.names = FALSE)
        rows = rows + nrow(one_plot.month)
        if(month.check == 12) month.check = 1 else(month.check = (month.check + 1))
      }
    }
  } 
}


# Seperating by chosen year
for(PLOT in unique(comb$Plot_Name)){
  one_plot.loop <- comb[comb$Plot_Name == PLOT,]
  
  year <- year(Sys.Date())
  
  one_plot.loop$Date_Time <- as.POSIXct(one_plot.loop$Date_Time)
  
  one_plot.loop <- one_plot.loop[one_plot.loop$Date_Time >= year, ]
  for(YR in 2017:year){
    Date.min <- paste(YR, "-01-01 00:00:00", sep="")
    Date.max <- paste(YR, "-12-31", " 23:59:59", sep="")
    one_plot.year <- subset(one_plot.loop, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))
    filename <- paste(PLOT,"_", YR, ".csv", sep = "")
    write.csv(one_plot.year, file.path(path.out, PLOT,  file = filename), row.names = FALSE)
  }
}


