#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Met_stations
# Purpose: To visualize the trends in our sensor data
# Inputs: Plot csv's created by script 1_Met_Consolidation.R
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
Plot.title = "B127"
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_clean/", sep="")

setwd(path.out)

plot.B127 <- read.csv("B127/B127.csv")
plot.N115 <- read.csv("N115/N115.csv")
plot.HH115 <- read.csv("HH115/HH115.csv")
plot.U134 <- read.csv("U134/U134.csv")

#---------------------------#
#Summary of all plots

#Creating a dataframe of all plots combined
all_plots <- bind_rows(plot.B127, plot.N115, plot.HH115, plot.U134)

str(all_plots)
all_plots$Date_Time <- as.POSIXct(all_plots$Date_Time)
summary(all_plots)

#Making a list of NA values so we know missing dates
Dates.missing <- all_plots[is.na(all_plots$Plot_Name),]

#Creating year column so years can be compared
all_plots <- all_plots %>% mutate(Year = as.character(year(Date_Time)))

# Changing data to a "long" format that ggplot likes
met.stack <- stack(all_plots[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(met.stack) <- c("values", "var")
met.stack[,c("Plot_Name", "Date_Time")] <- all_plots[,c("Plot_Name", "Date_Time")]
summary(met.stack)

#Initial plot 
path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/PAR and SOIL Summary"
png(width= 750, filename= file.path(path.figures, paste0('All_Vars','.png')))
ggplot(met.stack[met.stack$var == "Soil_Moisture",], aes(x = Date_Time, y = values)) +
  geom_smooth(aes(color=Plot_Name)) +
  theme_bw()+
  ggtitle("Met Stations")
dev.off()
#---------------------------#
#Summaries of one plot across years


all_plots <- all_plots[!is.na(all_plots$Plot_Name),]
#Incomplete year so we don't include it
all_plots <- all_plots[all_plots$Year != "2017",]
sumtab <- list()
for(PLOT in unique(all_plots$Plot_Name)){
  plot.df <- all_plots[all_plots$Plot_Name == PLOT,]
  for(YR in unique(plot.df$Year)){
    temp <- plot.df[plot.df$Year == YR,]
    sumtab[[paste(PLOT, YR, sep="-")]]$plot <- PLOT
    sumtab[[paste(PLOT, YR, sep="-")]]$year <- YR
    sumtab[[paste(PLOT, YR, sep="-")]]$soiltemp <- paste0(round(median(temp$Soil_Temp, na.rm =T), digits = 2), " (", round(min(temp$Soil_Temp, na.rm =T), digits = 2), "-", round(max(temp$Soil_Temp , na.rm =T), digits = 2), ")")
    sumtab[[paste(PLOT, YR, sep="-")]]$soilmoist <- paste0(round(median(temp$Soil_Moisture, na.rm =T), digits = 4), " (", round(min(temp$Soil_Moisture, na.rm =T), digits = 4), "-", round(max(temp$Soil_Moisture , na.rm =T), digits = 4), ")")
    #PAR IS MEAN NOT MEDIAN
    sumtab[[paste(PLOT, YR, sep="-")]]$PAR <- paste0(round(mean(temp$PAR, na.rm =T), digits = 4), " (", round(min(temp$PAR, na.rm =T), digits = 4), "-", round(max(temp$PAR , na.rm =T), digits = 4), ")")
  }
}
sumfin <- dplyr::bind_rows(sumtab)
write.csv(sumfin, file.path(path.figures, "PAR and SOIL Summary.csv"), row.names = F)


#Changing data to a "long" format that ggplot likes for total summary
plot.allstack <- stack(all_plots[,c( "PAR", "Soil_Moisture", "Soil_Temp")])
names(plot.allstack) <- c("values", "var")
plot.allstack[,c("Year", "Date_Time", "Plot_Name")] <- all_plots[,c("Year", "Date_Time", "Plot_Name")]
summary(plot.allstack)

plot.allstack$Yday <- lubridate::yday(plot.allstack$Date_Time)

#Plot to compare years
png(width= 750, filename= file.path(path.figures, paste0('Continous_timeseries','.png')))
ggplot(plot.allstack, aes(x = Date_Time, y = values)) +
  facet_grid(cols = vars(Plot_Name), rows = vars(var), scales="free_y") +
  geom_smooth(aes(color=Year)) +
  theme_bw()+
  ggtitle(Plot.title)
dev.off()

#Looking at PAR specifically (interested in post 2020 derecho storm effect)
#path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Yearly_PAR','.png')))
plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name),]
ggplot(plot.PAR, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_smooth(aes(color=Year)) +
  theme_bw()+
  ggtitle("PAR")
dev.off()


#Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
#path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Yearly_Soil_Moisture','.png')))
plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year != 2017 ,]
ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_smooth(aes(color=Year)) +
  theme_bw()+
  ggtitle("Soil_Moisture")
dev.off()


#------------------------------------#
#Summaries of one year for one plot

#Creats a short table of Date and Temp to be checked for first and last frost
frost.check <- subset(plot.2019, select = c(2,7))
frost.check <- subset(frost.check, frost.check$Air_Temp <=0.5 & frost.check$Air_Temp >= -10)
#View(frost.check)

# Changing data to a "long" format that ggplot likes for yearly summary
plot.stack <- stack(plot.all[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("Date_Time")] <- plot.stack[,c("Date_Time")]
summary(plot.stack)

#Plot to just view the data as is over one year
ggplot(plot.stack, aes(x = Date_Time, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_line() +
  theme_bw()


#-------------------------------------------------#
#DERECHO STORM WEATHER EVENT SPECIFIC GRAPHS
#-------------------------------------------------#

#Looking at PAR specifically (interested in post 2020 derecho storm effect)
path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Par','.png')))
plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name),]
ggplot(plot.PAR, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_smooth(aes(color=Year)) +
  geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
  theme_bw()+
  ggtitle("Post-Derecho East woods PAR")
dev.off()


#Looking at PAR specifically (interested in post 2020 derecho storm effect)
png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Par Zoom 2020','.png')))
plot.PAR <- plot.allstack[plot.allstack$var == "PAR" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year == 2020,]
ggplot(plot.PAR, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_point(aes(color=Year)) +
  geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
  theme_bw()+
  xlim(210, 240)+
  ylim(0, 500)+
  ggtitle("Post-Derecho East woods PAR")
dev.off()


#Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
path.figures <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Soil_Moisture','.png')))
plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year != 2017 ,]
ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_smooth(aes(color=Year)) +
  geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
  theme_bw()+
  ggtitle("Post-Derecho East woods Soil_Moisture")
dev.off()


#Looking at Soil_Moisture specifically (interested in post 2020 derecho storm effect)
png(width= 750, filename= file.path(path.figures, paste0('Post-Derecho Soil_Moisture Zoom 2020','.png')))
plot.Soil_Moisture <- plot.allstack[plot.allstack$var == "Soil_Moisture" & !is.na(plot.allstack$Plot_Name) & plot.allstack$Year == 2020,]
ggplot(plot.Soil_Moisture, aes(x = Yday, y = values)) +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_point(aes(color=Year)) +
  geom_vline(aes(xintercept = lubridate::yday(as.Date("2020-08-10"))))+
  theme_bw()+
  xlim(210, 240)+
  ylim(-.5, .5)+
  ggtitle("Post-Derecho East woods Soil_Moisture")
dev.off()
