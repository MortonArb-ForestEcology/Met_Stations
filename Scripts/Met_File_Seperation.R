# Script for the plotting and summarizing of data by plot and by year
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)

#Setting file paths

path.personal <- "C:/Users/lfitzpatrick"
path.data <- "/GitHub/Met_Stations/Data_raw_inputs/Single_Plots"
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
setwd(path.met)
path.out <- paste(path.met, "Data_clean/", Plot.title, sep="")

#Currently using the Month_Separation script because combining the seperate files results in 147 columns
#Will probably seperate the current month script into a QAQC script and the seperation script

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
      one_plot.month <- subset(one_plot.loop, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))
      filename <- paste(Plot.title,"-", year(Date.month), "-", month.file, ".csv", sep = "")
      write.csv(one_plot.month, file.path(path.out,  file = filename), row.names = FALSE)
      rows = rows + nrow(one_plot.month)
      if(month.check == 12) month.check = 1 else(month.check = (month.check + 1))
    }
  }
} 
# Seperating by chosen year
year.check = 2016
rows=1
for (i in rows:nrow(one_plot.loop)){
  Date.year <- one_plot.loop[i, "Date_Time"]
  year.extract <- year(Date.year)
  if (!is.na(year.extract)){
    if (year.extract != year.check){
      Date.min <- paste(year.extract, "-01-01 00:00:00", sep="")
      Date.max <- paste(year.extract, "-12-31", " 23:59:59", sep="")
      one_plot.year <- subset(one_plot.loop, Date_Time >= as.POSIXlt(Date.min) & Date_Time <= as.POSIXlt(Date.max))
      filename <- paste(Plot.title,"-", year.extract, ".csv", sep = "")
      write.csv(one_plot.year, file.path(path.out,  file = filename), row.names = FALSE)
      rows = rows + nrow(one_plot.year)
      year.check = (year.check + 1)
    }
  }
}


