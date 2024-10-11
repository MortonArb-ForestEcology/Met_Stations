#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick, Christy Rollinson
# Project: Met_stations
# Purpose: To visualize the trends in our sensor data
# Inputs: Plot csv's created by script 2_Met_Data_Clean.R
#         B127.csv
#         U134.csv
#         N115.csv
#         HH115.csv
# Outputs: Summary figures for QAQC of met station data
# Notes:
#-----------------------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)

#setting paths
#path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/"
path.google <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations"
path.met <- file.path(path.google, "Single_Plots")

path.out <- file.path(path.met, "Data_processed/Clean_data")
path.qaqc <- file.path(path.google, "QAQC_figs")

# Read in script to harmonize columns
source("0_MetHelperFunctions.R")


# #######################################################
# #######################################################
# Read in and check individual plots ----
# #######################################################
# #######################################################

# Seperating by chosen year and month values
B127All <- dir(file.path(path.out,"B127"))
plot.B127 <- data.frame()
for(i in 1:length(B127All)){
  fNow <- read.csv(file.path(path.out,"B127", B127All[i]), na.strings=c("", "NA", "#N/A"))
  fNow$Timestamp <- as.POSIXct(fNow$Timestamp, tz="Etc/GMT+6")
  fNow$Date <- as.Date(fNow$Date)
  fNow$Plot_Name <- as.factor(fNow$Plot_Name)
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.B127 <- rbind(plot.B127, fNow)
}
summary(plot.B127)
# plot.B127$Timestamp[1:30]


# ###########
# ***** IMPORTANT: Check last 30 days - B127 ***** ----
# ###########
summary(plot.B127[(nrow(plot.B127)-30*24):nrow(plot.B127),]) # Look for missing values in the last week of data --> weird values of big gaps shoudl spur more investigation

B127stack <- stack(plot.B127[(nrow(plot.B127)-30*24):nrow(plot.B127),c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "m.s.Wind.Speed", "kPa.Atmospheric.Pressure", "Battery.Percent", "degC.Logger.Temperature")])
B127stack[,c("Plot_Name","Timestamp", "Date")] <- plot.B127[(nrow(plot.B127)-30*24):nrow(plot.B127),c("Plot_Name","Timestamp", "Date")]
summary(B127stack)

png(filename= file.path(path.qaqc, paste0('LAST_30_DAY_B127.png')), height=8, width=9, units="in", res=220)
ggplot(data=B127stack[,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x = Timestamp, y = values)) +
  theme_bw()
dev.off()
# ###########


N115All <- dir(file.path(path.out,"N115"))
plot.N115 <- data.frame()
for(i in 1:length(N115All)){
  fNow <- read.csv(file.path(path.out,"N115", N115All[i]), na.strings=c("", "NA", "#N/A"))
  fNow$Timestamp <- as.POSIXct(fNow$Timestamp, tz="Etc/GMT+6")
  fNow$Date <- as.Date(fNow$Date)
  fNow$Plot_Name <- as.factor(fNow$Plot_Name)
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.N115 <- rbind(plot.N115, fNow)
}
summary(plot.N115)
head(plot.N115)
plot.N115[(nrow(plot.N115)-30):nrow(plot.N115),] # Look for missing values in the past 24ish hours --> should spur more investigation

# ###########
# ***** IMPORTANT: Check last 30 days - N115 ***** ----
# ###########
summary(plot.N115[(nrow(plot.N115)-30*24):nrow(plot.N115),]) # Look for missing values in the last week of data --> weird values of big gaps shoudl spur more investigation

N115stack <- stack(plot.N115[(nrow(plot.N115)-30*24):nrow(plot.N115),c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "m.s.Wind.Speed", "kPa.Atmospheric.Pressure", "Battery.Percent", "degC.Logger.Temperature")])
N115stack[,c("Plot_Name","Timestamp", "Date")] <- plot.N115[(nrow(plot.N115)-30*24):nrow(plot.N115),c("Plot_Name","Timestamp", "Date")]
summary(N115stack)

png(filename= file.path(path.qaqc, paste0('LAST_30_DAY_N115.png')), height=8, width=9, units="in", res=220)
ggplot(data=N115stack[,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x = Timestamp, y = values)) +
  theme_bw()
dev.off()
# ###########



HH115All <- dir(file.path(path.out,"HH115"))
plot.HH115 <- data.frame()
for(i in 1:length(HH115All)){
  fNow <- read.csv(file.path(path.out,"HH115", HH115All[i]), na.strings=c("", "NA", "#N/A"))
  fNow$Date <- as.Date(fNow$Date)
  fNow$Timestamp <- as.POSIXct(fNow$Timestamp, tz="Etc/GMT+6")
  fNow$Plot_Name <- as.factor(fNow$Plot_Name)
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.HH115 <- rbind(plot.HH115, fNow)
}
summary(plot.HH115)
head(plot.HH115)
plot.HH115[(nrow(plot.HH115)-30):nrow(plot.HH115),] # Look for missing values in the past 24ish hours --> should spur more investigation

# ###########
# ***** IMPORTANT: Check last 30 days - HH115 ***** ----
# ###########
summary(plot.HH115[(nrow(plot.HH115)-30*24):nrow(plot.HH115),]) # Look for missing values in the last week of data --> weird values of big gaps shoudl spur more investigation

HH115stack <- stack(plot.HH115[(nrow(plot.HH115)-30*24):nrow(plot.HH115),c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "m.s.Wind.Speed", "kPa.Atmospheric.Pressure", "Battery.Percent", "degC.Logger.Temperature")])
HH115stack[,c("Plot_Name","Timestamp", "Date")] <- plot.HH115[(nrow(plot.HH115)-30*24):nrow(plot.HH115),c("Plot_Name","Timestamp", "Date")]
summary(HH115stack)

png(filename= file.path(path.qaqc, paste0('LAST_30_DAY_HH115.png')), height=8, width=9, units="in", res=220)
ggplot(data=HH115stack[,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x = Timestamp, y = values)) +
  theme_bw()
dev.off()
# ###########


U134All <- dir(file.path(path.out,"U134"))
plot.U134 <- data.frame()
for(i in 1:length(U134All)){
  fNow <- read.csv(file.path(path.out,"U134", U134All[i]), na.strings=c("", "NA", "#N/A"))
  fNow$Timestamp <- as.POSIXct(fNow$Timestamp, tz="Etc/GMT+6")
  fNow$Date <- as.Date(fNow$Date)
  fNow$Plot_Name <- as.factor(fNow$Plot_Name)
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.U134 <- rbind(plot.U134, fNow)
}
summary(plot.U134)
head(plot.U134)
plot.U134[(nrow(plot.U134)-30):nrow(plot.U134),] # Look for missing values in the past 24ish hours --> should spur more investigation

# ###########
# ***** IMPORTANT: Check last 30 days - U134 ***** ----
# ###########
summary(plot.U134[(nrow(plot.U134)-30*24):nrow(plot.U134),]) # Look for missing values in the last week of data --> weird values of big gaps shoudl spur more investigation

U134stack <- stack(plot.U134[(nrow(plot.U134)-30*24):nrow(plot.U134),c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "m.s.Wind.Speed", "kPa.Atmospheric.Pressure", "Battery.Percent", "degC.Logger.Temperature")])
U134stack[,c("Plot_Name","Timestamp", "Date")] <- plot.U134[(nrow(plot.U134)-30*24):nrow(plot.U134),c("Plot_Name","Timestamp", "Date")]
summary(U134stack)

png(filename= file.path(path.qaqc, paste0('LAST_30_DAY_U134.png')), height=8, width=9, units="in", res=220)
ggplot(data=U134stack[,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x = Timestamp, y = values)) +
  theme_bw()
dev.off()
# ###########
# #######################################################
# #######################################################










# #######################################################
# #######################################################
# Doing some combined and all time plotting -----
# #######################################################
# #######################################################

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
# comb$Timestamp <- as.POSIXct(comb$Timestamp)
comb$yday <- lubridate::yday(comb$Date)
comb$year <- lubridate::year(comb$Date)

plot.comb <- data.frame()
for(PLOT in unique(comb$Plot_Name)){
  temp <- comb[comb$Plot_Name == PLOT,]
  if(PLOT == "B127"){
    #Adding a sensor flag. Adding one for soil sensor's and one for other sensors since they changed at different times for all but B127
    temp$Air.Sensor <- ifelse(temp$Timestamp <= "2020-10-20 12:00:00", "Onset", "Meter")
    temp$Soil.Sensor <- ifelse(temp$Timestamp <= "2020-10-20 12:00:00", "Onset", "Meter")
  } else {
    temp$Soil.Sensor <- ifelse(temp$Timestamp <= "2020-11-05 13:00:00", "Onset", "Meter")
    temp$Air.Sensor <- ifelse(temp$Timestamp <= "2021-07-02 14:00:00", "Onset", "Meter")
  }
  plot.comb <- rbind(plot.comb, temp)
}

#Organizing data into long form for easier graphing
agg.stack <- aggregate(cbind(Soil_Temp, Soil_Moisture, PAR, Air_Temp, Relative_Humidity)~Plot_Name+Date+Air.Sensor+Soil.Sensor, data = plot.comb, FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("Plot_Name", "Date", "Air.Sensor", "Soil.Sensor")] <- agg.stack[,c("Plot_Name", "Date", "Air.Sensor", "Soil.Sensor")]
plot.stack$yday <- yday(plot.stack$Date)
plot.stack$year <- year(plot.stack$Date)

#Creating a rolling average # # NOte: this was
plot.roll <- plot.stack %>%
  dplyr::arrange(desc(Date)) %>% 
  dplyr::arrange(desc(Plot_Name)) %>% 
  dplyr::group_by(Plot_Name, var) %>%
  dplyr::mutate(VAR_07 = zoo::rollmean(values, k = 7, fill = NA),
                VAR_15 = zoo::rollmean(values, k = 15, fill = NA),
                VAR_30 = zoo::rollmean(values, k = 30, fill = NA)) %>% 
  dplyr::ungroup()

plot.roll$yday <- yday(plot.roll$Date)
plot.roll$year <- year(plot.roll$Date)
summary(plot.roll)

#Looks at one variable at all plots
for(VAR in unique(plot.stack$var)){
  if(VAR %in% c("PAR", "Air_Temp", "Relative_Humidity")){
  fig <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = Date, y = values, color = Air.Sensor), data = plot.stack[plot.stack$var == VAR,], linewidth=0.5, size=0.5) +
    theme_bw()+
    ggtitle(paste0(VAR, " Yearly Time Series using daily mean upto ", max(plot.stack$Date)))
  } else{
  fig <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = Date, y = values, color = Soil.Sensor), data = plot.stack[plot.stack$var == VAR,], linewidth=0.5, size=0.5) +
    theme_bw()+
    ggtitle(paste0(VAR, " Yearly Time Series using daily mean upto ", max(plot.stack$Date)))
  }
  png(filename= file.path(path.qaqc, paste0('All_PLOTS_',VAR,'.png')), height=8, width=8, units="in", res=220)
  print(fig)
  dev.off()    
  
  yrRange <- unique(plot.stack$year)
  yrRange <- sort(yrRange, decreasing = T)
  # test <- paste(yrRange[2])
  png(filename= file.path(path.qaqc, paste0('All_PLOTS_',VAR,'_DOY.png')), height=8, width=9, units="in", res=220)
  print(
    ggplot() +
      facet_wrap(~Plot_Name, scales="free_y") +
      geom_line(aes(x = yday, y = values, color = "past", group=year), data = plot.stack[plot.stack$var == VAR & plot.stack$year<yrRange[2],], linewidth=0.2, size=0.2) +
      geom_line(aes(x = yday, y = values, color = "last year", group=year), data = plot.stack[plot.stack$var == VAR & plot.stack$year==yrRange[2],], linewidth=0.75, size=0.75) +
      geom_line(aes(x = yday, y = values, color = "this year", group=year), data = plot.stack[plot.stack$var == VAR & plot.stack$year==yrRange[1],], linewidth=1, size=1) +
      # scale_color_manual(values=c("past"="gray30", as.character(yrRange[2])="blue3", as.character(yrRange[1])="orange2")) +
      scale_color_manual(values=c("past"="gray30", "last year"="dodgerblue2", "this year"="orange2")) +
      theme_bw()+
      ggtitle(paste0(VAR, " Yearly Time Series using daily mean upto ", max(plot.stack$Date)))
  )
  dev.off()
}


#Looks at one plot with all variables by year
for(PLOT in unique(plot.roll$Plot_Name)){
  yrRange <- unique(plot.roll$year)
  yrRange <- sort(yrRange, decreasing = T)
  
  png(filename= file.path(path.qaqc, paste0('PLOT_', PLOT, '_All_VARS_by_year','.png')), height=8, width=9, units="in", res=220)
  print(
    ggplot() +
      facet_wrap(~var, scales="free_y") +
      geom_line(aes(x = yday, y = VAR_07, color = "past", group=year), data = plot.roll[plot.roll$Plot_Name == PLOT & plot.roll$year<yrRange[2],], linewidth=0.2, size=0.2) +
      geom_line(aes(x = yday, y = VAR_07, color = "last year"), data = plot.roll[plot.roll$Plot_Name == PLOT & plot.roll$year==yrRange[2],], linewidth=0.5, size=0.5) +
      geom_line(aes(x = yday, y = VAR_07, color = "this year"), data = plot.roll[plot.roll$Plot_Name == PLOT & plot.roll$year==yrRange[1],]) +
      scale_color_manual(values=c("past"="gray30", "last year"="dodgerblue2", "this year"="orange2")) +
      ggtitle(paste0(PLOT, " Yearly Time Series using daily mean and 7-day rolling average up to ", max(plot.stack$Date))) +
      ylab(paste0("30 day rolling average")) +
      theme_bw()+
      theme(legend.position=c(0.85, 0.25))
  )
  dev.off()
}

#Looks at one plot with all variables through time
for(PLOT in unique(plot.roll$Plot_Name)){
  png(filename= file.path(path.qaqc, paste0('PLOT_', PLOT, '_All_VARS_time_series','.png')), height=6, width=9, units="in", res=220)
  print(
    ggplot() +
      facet_wrap(~var, scales="free_y") +
      geom_line(aes(x = Date, y = VAR_07), data = plot.roll[plot.roll$Plot_Name == PLOT,], linewidth=0.2, size=0.2) +
      ggtitle(paste0(PLOT, " Yearly Time Series using daily mean and 7-day rolling average up to ", max(plot.roll$Date)))+
      ylab(paste0("30 day rolling average")) +
      theme_bw()
  )
  dev.off()
}


# Figurig out WTH is going on with relative humidity
checkRH <- comb[comb$year==2023,]
summary(checkRH[checkRH$Relative_Humidity<1 & !is.na(checkRH$Relative_Humidity) & checkRH$yday>120,])

ggplot(data=checkRH) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_line(aes(x = Timestamp, y = Relative_Humidity)) +
  theme_bw()
summary(checkRH)
# #######################################################
# #######################################################

# Add a new column to distinguish between B127 and H115
B127stack$Source <- "B127"
HH115stack$Source <- "HH115"
U134stack$Source <-"U134"
N115stack$Source <-"N115"
# Combine the datasets
combstack <- rbind(B127stack, HH115stack, U134stack, N115stack)

# Plot the combined data
png(filename= file.path(path.qaqc, paste0('LAST_30_DAY_COMPARISON.png')), height=8, width=9, units="in", res=220)
ggplot(data=combstack) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x = Timestamp, y = values, color = Source, alpha =0.5)) +
  theme_bw() +
  labs(title = "30 day Comparison of Plots ", color = "Source")
dev.off()
