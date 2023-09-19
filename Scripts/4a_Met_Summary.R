#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
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
# path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.google <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations"
path.met <- file.path(path.google, "Single_Plots")

path.out <- file.path(path.met, "Data_processed/Clean_data")
path.qaqc <- file.path(path.google, "QAQC_figs")

# Read in script to harmonize columns
source("0_MetHelperFunctions.R")


# Seperating by chosen year and month values
B127All <- dir(file.path(path.out,"B127"))
plot.B127 <- data.frame()
for(i in 1:length(B127All)){
  fNow <- read.csv(file.path(path.out,"B127", B127All[i]), na.strings=c("", "NA", "#N/A"))
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.B127 <- rbind(plot.B127, fNow)
}
summary(plot.B127)


N115All <- dir(file.path(path.out,"N115"))
plot.N115 <- data.frame()
for(i in 1:length(N115All)){
  fNow <- read.csv(file.path(path.out,"N115", N115All[i]), na.strings=c("", "NA", "#N/A"))
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.N115 <- rbind(plot.N115, fNow)
}
summary(plot.N115)
head(plot.N115)



HH115All <- dir(file.path(path.out,"HH115"))
plot.HH115 <- data.frame()
for(i in 1:length(HH115All)){
  fNow <- read.csv(file.path(path.out,"HH115", HH115All[i]), na.strings=c("", "NA", "#N/A"))
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.HH115 <- rbind(plot.HH115, fNow)
}
summary(plot.HH115)
head(plot.HH115)


U134All <- dir(file.path(path.out,"U134"))
plot.U134 <- data.frame()
for(i in 1:length(U134All)){
  fNow <- read.csv(file.path(path.out,"U134", U134All[i]), na.strings=c("", "NA", "#N/A"))
  names(fNow)[!grepl("SIGFLAG", names(fNow))] <- renameCols(names(fNow)[!grepl("SIGFLAG", names(fNow))])
  
  plot.U134 <- rbind(plot.U134, fNow)
}
summary(plot.U134)
head(plot.U134)



comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb$Date_Time <- as.POSIXct(comb$Date_Time)
comb$yday <- yday(comb$Date_Time)
comb$year <- year(comb$Date_Time)

plot.comb <- data.frame()
for(PLOT in unique(comb$Plot_Name)){
  temp <- comb[comb$Plot_Name == PLOT,]
  if(PLOT == "B127"){
    #Adding a sensor flag. Adding one for soil sensor's and one for other sensors since they changed at different times for all but B127
    temp$Air.Sensor <- ifelse(temp$Date_Time <= "2020-10-20 12:00:00", "Onset", "Meter")
    temp$Soil.Sensor <- ifelse(temp$Date_Time <= "2020-10-20 12:00:00", "Onset", "Meter")
  } else {
    temp$Soil.Sensor <- ifelse(temp$Date_Time <= "2020-11-05 13:00:00", "Onset", "Meter")
    temp$Air.Sensor <- ifelse(temp$Date_Time <= "2021-07-02 14:00:00", "Onset", "Meter")
  }
  plot.comb <- rbind(plot.comb, temp)
}

#Organizing data into long form for easier graphing
agg.stack <- aggregate(cbind(Soil_Temp, Soil_Moisture, PAR, Air_Temp, Relative_Humidity)~Plot_Name+Date_Time+Air.Sensor+Soil.Sensor, data = plot.comb, FUN = median, na.action = NULL)

plot.stack <- stack(agg.stack[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("Plot_Name", "Date_Time", "Air.Sensor", "Soil.Sensor")] <- agg.stack[,c("Plot_Name", "Date_Time", "Air.Sensor", "Soil.Sensor")]
plot.stack$yday <- yday(plot.stack$Date_Time)
plot.stack$year <- year(plot.stack$Date_Time)

#Creating a rolling average
plot.roll <- plot.stack %>%
  dplyr::arrange(desc(Plot_Name)) %>% 
  dplyr::group_by(Plot_Name, var) %>% 
  dplyr::mutate(VAR_07 = zoo::rollmean(values, k = 7, fill = NA),
                VAR_15 = zoo::rollmean(values, k = 15, fill = NA),
                VAR_30 = zoo::rollmean(values, k = 30, fill = NA)) %>% 
  dplyr::ungroup()

plot.roll$yday <- yday(plot.roll$Date_Time)
plot.roll$year <- year(plot.roll$Date_Time)

#Looks at one variable at all plots
for(VAR in unique(plot.stack$var)){
  png(width= 750, filename= file.path(path.qaqc, paste0('All_PLOTS_',VAR,'.png')))
  if(VAR %in% c("PAR", "Air_Temp", "Relative_Humidity")){
  fig <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = Date_Time, y = values, color = Air.Sensor), data = plot.stack[plot.stack$var == VAR,]) +
    theme_bw()+
    ggtitle(paste0(VAR, " Yearly Time Series using daily median upto ", max(plot.stack$Date_Time)))
  print(fig)
  dev.off()
  } else{
  fig <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = Date_Time, y = values, color = Soil.Sensor), data = plot.stack[plot.stack$var == VAR,]) +
    theme_bw()+
    ggtitle(paste0(VAR, " Yearly Time Series using daily median upto ", max(plot.stack$Date_Time)))
  print(fig)
  dev.off()    
  }
}


#Looks at one plot with all variables by year
for(PLOT in unique(plot.roll$Plot_Name)){
  png(width= 750, filename= file.path(path.qaqc, paste0('PLOT_', PLOT, '_All_VARS_by_year','.png')))
  fig <- ggplot() +
    facet_wrap(~var, scales="free_y") +
    geom_line(aes(x = yday, y = VAR_30, color = as.character(year)), data = plot.roll[plot.roll$Plot_Name == PLOT,]) +
    theme_bw()+
    ggtitle(paste0(PLOT, " Yearly Time Series using daily median and 30 day rolling average upto ", max(plot.stack$Date_Time)))+
    ylab(paste0("30 day rolling average"))
    print(fig)
  dev.off()
}

#Looks at one plot with all variables through time
for(PLOT in unique(plot.roll$Plot_Name)){
  png(width= 750, filename= file.path(path.qaqc, paste0('PLOT_', PLOT, '_All_VARS_time_series','.png')))
  fig <- ggplot() +
    facet_wrap(~var, scales="free_y") +
    geom_line(aes(x = Date_Time, y = VAR_30), data = plot.roll[plot.roll$Plot_Name == PLOT,]) +
    theme_bw()+
    ggtitle(paste0(PLOT, " Yearly Time Series using daily median and 30 day rolling average upto ", max(plot.roll$Date_Time)))+
    ylab(paste0("30 day rolling average"))
  print(fig)
  dev.off()
}



  facet_wrap(~Plot_Name, scales="free_y") +
  theme_bw()

##################################################################
##################################################################
## THE FOLLOWING HAS NOT BEEN CHECKED, SO RUN WITH CARE AND BE PREPARED FOR HEAVY DEBUGGING
##################################################################
##################################################################

# #---------------------------#
# #---------------------------#
# #Summary of all plots
# 
# #Creating a dataframe of all plots combined
# all_plots <- bind_rows(plot.B127, plot.N115, plot.HH115, plot.U134)
# 
# str(all_plots)
# all_plots$Date_Time <- as.POSIXct(all_plots$Date_Time)
# summary(all_plots)
# 
# #Making a list of NA values so we know missing dates
# Dates.missing <- all_plots[is.na(all_plots$Plot_Name),]
# 
# #Creating year column so years can be compared
# all_plots <- all_plots %>% mutate(Year = as.character(year(Date_Time)))
# 
# # Changing data to a "long" format that ggplot likes
# met.stack <- stack(all_plots[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
# names(met.stack) <- c("values", "var")
# met.stack[,c("Plot_Name", "Date_Time")] <- all_plots[,c("Plot_Name", "Date_Time")]
# summary(met.stack)
# 
# #Initial plot 
# path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/PAR and SOIL Summary"
# png(width= 750, filename= file.path(path.figures, paste0('All_Vars','.png')))
# ggplot(met.stack[met.stack$var == "Soil_Moisture",], aes(x = Date_Time, y = values)) +
#   geom_smooth(aes(color=Plot_Name)) +
#   theme_bw()+
#   ggtitle("Met Stations Soil Moisture")
# dev.off()
# #---------------------------#
# #Summaries of one plot across years
# 
# 
# all_plots <- all_plots[!is.na(all_plots$Plot_Name),]
# #Incomplete year so we don't include it
# all_plots <- all_plots[all_plots$Year != "2017",]
# sumtab <- list()
# for(PLOT in unique(all_plots$Plot_Name)){
#   plot.df <- all_plots[all_plots$Plot_Name == PLOT,]
#   for(YR in unique(plot.df$Year)){
#     temp <- plot.df[plot.df$Year == YR,]
#     sumtab[[paste(PLOT, YR, sep="-")]]$plot <- PLOT
#     sumtab[[paste(PLOT, YR, sep="-")]]$year <- YR
#     sumtab[[paste(PLOT, YR, sep="-")]]$soiltemp <- paste0(round(median(temp$Soil_Temp, na.rm =T), digits = 2), " (", round(min(temp$Soil_Temp, na.rm =T), digits = 2), "-", round(max(temp$Soil_Temp , na.rm =T), digits = 2), ")")
#     sumtab[[paste(PLOT, YR, sep="-")]]$soilmoist <- paste0(round(median(temp$Soil_Moisture, na.rm =T), digits = 4), " (", round(min(temp$Soil_Moisture, na.rm =T), digits = 4), "-", round(max(temp$Soil_Moisture , na.rm =T), digits = 4), ")")
#     #PAR IS MEAN NOT MEDIAN
#     sumtab[[paste(PLOT, YR, sep="-")]]$PAR <- paste0(round(mean(temp$PAR, na.rm =T), digits = 4), " (", round(min(temp$PAR, na.rm =T), digits = 4), "-", round(max(temp$PAR , na.rm =T), digits = 4), ")")
#   }
# }
# sumfin <- dplyr::bind_rows(sumtab)
# write.csv(sumfin, file.path(path.figures, "PAR and SOIL Summary.csv"), row.names = F)
# 
# 
# #Changing data to a "long" format that ggplot likes for total summary
# plot.allstack <- stack(all_plots[,c( "PAR", "Soil_Moisture", "Soil_Temp")])
# names(plot.allstack) <- c("values", "var")
# plot.allstack[,c("Year", "Date_Time", "Plot_Name")] <- all_plots[,c("Year", "Date_Time", "Plot_Name")]
# summary(plot.allstack)
# 
# plot.allstack$Yday <- lubridate::yday(plot.allstack$Date_Time)
# 
# #Plot to compare years
# png(width= 750, filename= file.path(path.figures, paste0('Continous_timeseries','.png')))
# ggplot(plot.allstack, aes(x = Date_Time, y = values)) +
#   facet_grid(cols = vars(Plot_Name), rows = vars(var), scales="free_y") +
#   geom_smooth(aes(color=Year)) +
#   theme_bw()+
#   ggtitle(Plot.title)
# dev.off()
# 
# #Looking at PAR specifically (interested in post 2020 derecho storm effect)
# #path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
# if(!dir.exists(path.figures)) dir.create(path.figures)
# png(width= 750, filename= file.path(path.figures, paste0('Yearly_PAR','.png')))
# plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name),]
# ggplot(plot.PAR, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_smooth(aes(color=Year)) +
#   theme_bw()+
#   ggtitle("PAR")
# dev.off()
# 
# 
# #Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
# #path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
# if(!dir.exists(path.figures)) dir.create(path.figures)
# png(width= 750, filename= file.path(path.figures, paste0('Yearly_Soil_Moisture','.png')))
# plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year != 2017 ,]
# ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_smooth(aes(color=Year)) +
#   theme_bw()+
#   ggtitle("Soil_Moisture")
# dev.off()
# 
# 
# #------------------------------------#
# #Summaries of one year for one plot
# 
# #Creats a short table of Date and Temp to be checked for first and last frost
# frost.check <- subset(plot.2019, select = c(2,7))
# frost.check <- subset(frost.check, frost.check$Air_Temp <=0.5 & frost.check$Air_Temp >= -10)
# #View(frost.check)
# 
# # Changing data to a "long" format that ggplot likes for yearly summary
# plot.stack <- stack(plot.all[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
# names(plot.stack) <- c("values", "var")
# plot.stack[,c("Date_Time")] <- plot.stack[,c("Date_Time")]
# summary(plot.stack)
# 
# #Plot to just view the data as is over one year
# ggplot(plot.stack, aes(x = Date_Time, y = values)) +
#   facet_wrap(~var, scales="free_y") +
#   geom_line() +
#   theme_bw()
# 
# 
# #-------------------------------------------------#
# #DERECHO STORM WEATHER EVENT SPECIFIC GRAPHS
# #-------------------------------------------------#
# 
# #Looking at PAR specifically (interested in post 2020 derecho storm effect)
# path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
# if(!dir.exists(path.figures)) dir.create(path.figures)
# png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Par','.png')))
# plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name),]
# ggplot(plot.PAR, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_smooth(aes(color=Year)) +
#   geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
#   theme_bw()+
#   ggtitle("Post-Derecho East woods PAR")
# dev.off()
# 
# 
# #Looking at PAR specifically (interested in post 2020 derecho storm effect)
# png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Par Zoom 2020','.png')))
# plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year == 2020,]
# ggplot(plot.PAR, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_point(aes(color=Year)) +
#   geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
#   theme_bw()+
#   xlim(210, 240)+
#   ylim(0, 500)+
#   ggtitle("Post-Derecho East woods PAR")
# dev.off()
# 
# 
# #Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
# path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
# if(!dir.exists(path.figures)) dir.create(path.figures)
# png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Soil_Moisture','.png')))
# plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year != 2017 ,]
# ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_smooth(aes(color=Year)) +
#   geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
#   theme_bw()+
#   ggtitle("Post-Derecho East woods Soil_Moisture")
# dev.off()
# 
# 
# #Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
# png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Soil_Moisture Zoom 2020','.png')))
# plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year == 2020,]
# ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
#   facet_wrap(~Plot_Name, scales="free_y") +
#   geom_point(aes(color=Year)) +
#   geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
#   theme_bw()+
#   xlim(210, 240)+
#   ylim(-.5, .5)+
#   ggtitle("Post-Derecho East woods Soil_Moisture")
# dev.off()
