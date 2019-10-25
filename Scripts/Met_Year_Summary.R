# Script for the plotting and summarizing of data by plot and by year
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)
#Setting file paths

path.personal <- "C:/Users/lfitzpatrick"
path.data <- "/GitHub/Met_Stations/Data_raw_inputs/Single_Plots"
path.met <- paste(path.personal, path.data, sep="")
setwd(path.met)
path.out <- paste(path.personal, "/GitHub/Met_Stations/Data_clean/Rollinson_", Plot.title, sep="")

#Currently using the Month_Separation script because combining the seperate files results in 147 columns
#Will probably seperate the current month script into a QAQC script and the seperation script

year.check = 2016
rows=1
for (i in rows:nrow(one_plot.loop)){
  Date.year <- one_plot.loop[i, "Date_Check"]
  year.extract <- year(Date.year)
  if (!is.na(year.extract)){
    if (year.extract != year.check){
      Date.min <- paste(year.extract, "-01-01 00:00:00", sep="")
      Date.max <- paste(year.extract, "-12-31", " 23:59:59", sep="")
      one_plot.final <- subset(one_plot.loop, Date_Check >= as.POSIXlt(Date.min) & Date_Check <= as.POSIXlt(Date.max))
      filename <- paste(Plot.title,"-", year.extract, ".csv", sep = "")
      write.csv(one_plot.final, file.path(path.out,  file = filename), row.names = FALSE)
      rows = rows + nrow(one_plot.final)
      year.check = (year.check + 1)
    }
  }
}