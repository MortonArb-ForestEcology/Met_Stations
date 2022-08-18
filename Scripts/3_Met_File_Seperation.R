#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: To further process the sensor data into yearly csv's
# Inputs: Plot csv's created by script 1_Met_Consolidation.R
#         B127.csv
#         U134.csv
#         N115.csv
#         HH115.csv
# Outputs: Yearly csv's for each plot (B127, U134, N115, HH115)
# Notes: With updates this script can hopefully only work with new data
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting file paths
path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_processed/Clean_data", sep="")

# Seperating by chosen year and month values
plot.B127 <- read.csv(file.path(path.out,"B127/B127.csv"))
plot.N115 <- read.csv(file.path(path.out,"N115/N115.csv"))
plot.HH115 <- read.csv(file.path(path.out,"HH115/HH115.csv"))
plot.U134 <- read.csv(file.path(path.out,"U134/U134.csv"))


comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb <- comb[!is.na(comb$Plot_Name),]

# Seperating by chosen year
for(PLOT in unique(comb$Plot_Name)){
  
  dir.plot <- dir(file.path(path.out, PLOT), ".csv")
  
  #Way of finding the oldest and most recent file to be deleted.
  old.plot <- dir.plot[match(1, stringr::str_detect(dir.plot, "up_to_"))]
  
  #Deleting the old version
  if (file.exists(file.path(path.out, PLOT, old.plot))) {
    unlink(file.path(path.out, PLOT, old.plot))
    cat("The file is deleted")
  }
  
  one_plot.loop <- comb[comb$Plot_Name == PLOT,]
  one_plot.loop$year <- lubridate::year(one_plot.loop$Date_Time)
  
  year <- year(Sys.Date())
  
  one_plot.loop$Date_Time <- as.POSIXct(one_plot.loop$Date_Time)
  
  #Creating the previous year's complete file
  one_plot.old <- one_plot.loop[one_plot.loop$year <= year-1 & one_plot.loop$year > year-2,]
  write.csv(one_plot.old, file.path(path.out, PLOT,  file = paste(PLOT,"_" ,year-1, ".csv", sep="")), row.names = FALSE)
  
  one_plot.year <- one_plot.loop[one_plot.loop$year <= year & one_plot.loop$year > year-1, ]
  Date.min <- min(one_plot.year$Date_Time)
  Date.max <- max(one_plot.year$Date_Time)
  
  #Creating a file name that includes the ending of current measurements
  filename <- paste(PLOT,"_2022_up_to_" , as.Date(Date.max), ".csv", sep = "")
  write.csv(one_plot.year, file.path(path.out, PLOT,  file = filename), row.names = FALSE)
  
}


