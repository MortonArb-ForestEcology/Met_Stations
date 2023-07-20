library(readbulk)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)

#setting paths
path.met <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_processed/Clean_data", sep="")
path.qaqc <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36TkVHdDNJQ1dJU1E/East Woods/Rollinson_Monitoring/Data/Met Stations/figures/2023_Drought_figs/"


# Seperating by chosen year and month values
plot.B127 <- read.csv(file.path(path.out,"B127/B127.csv"))
plot.N115 <- read.csv(file.path(path.out,"N115/N115.csv"))
plot.HH115 <- read.csv(file.path(path.out,"HH115/HH115.csv"))
plot.U134 <- read.csv(file.path(path.out,"U134/U134.csv"))

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)
comb$Date_Time <- as.POSIXct(comb$Date_Time)
comb$yday <- yday(comb$Date_Time)
comb$year <- year(comb$Date_Time)

#Organizing data into long form for easier graphing
agg.stack <- aggregate(cbind(Soil_Temp, Soil_Moisture, PAR, Air_Temp, Relative_Humidity)~Plot_Name+Date_Time, data = comb, FUN = median, na.action = NULL)
colnames(agg.stack) <- c("Plot_Name", "Date_Time", "Soil Temp C","Soil Moisture m3/m3", "PAR W/m2", "Air Temp C", "Relative_Humidity")
plot.stack <- stack(agg.stack[,c("Soil Temp C", "Soil Moisture m3/m3", "PAR W/m2", "Air Temp C")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("Plot_Name", "Date_Time")] <- agg.stack[,c("Plot_Name", "Date_Time")]
plot.stack$yday <- yday(plot.stack$Date_Time)
plot.stack$year <- year(plot.stack$Date_Time)

#Creating a rolling average
plot.roll <- plot.stack %>%
  dplyr::arrange(desc(Plot_Name)) %>% 
  dplyr::group_by(Plot_Name, var) %>% 
  dplyr::mutate(VAR_03 = zoo::rollmean(values, k = 3, fill = NA, align = "right"),
                VAR_07 = zoo::rollmean(values, k = 7, fill = NA, align = "right"),
                VAR_15 = zoo::rollmean(values, k = 15, fill = NA, align = "right"),
                VAR_30 = zoo::rollmean(values, k = 30, fill = NA, align = "right")) %>% 
  dplyr::ungroup()

plot.roll$yday <- yday(plot.roll$Date_Time)
plot.roll$year <- year(plot.roll$Date_Time)


plot.stack <- plot.stack[plot.stack$values <=80, ]
plot.stack <- plot.stack[!is.na(plot.stack$var),]

colorBlindBlack7  <- c("#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

max <- max(plot.stack$Date_Time)
#Variable by year up until now with a bolded 2023
for(VAR in unique(plot.stack$var)){
  last.day <- max(plot.stack[plot.stack$year == 2023, "yday"])
  if(VAR == "Soil Moisture m3/m3"){
  png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0('Soil_Moisture_', max,'.png')))
  } else if (VAR =="PAR W/m2"){
    png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0('PAR_2023_',max,'.png')))
  } else {
    png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0(VAR, "_", max, '.png')))
  }
  old <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == VAR & plot.stack$yday <= 250 & plot.stack$year <2023 & plot.stack$year >2020,]) +
    theme_bw()+
    labs(y = VAR, x = "day of year", color = "Year")+
    scale_color_manual(values = colorBlindBlack7)+
    ggtitle(paste("Median ", VAR, " up to ", max, sep = ""))
  
  fig <- old +
    geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == VAR & plot.stack$yday <= 250 & plot.stack$year ==2023,], size = 1.2)
    #scale_color_manual(colorBlindBlack7)
  print(fig)
  dev.off()
}

#Variable by year up until now with a bolded 2023
#THis verison using the 3 day rolling average
for(VAR in unique(plot.roll$var)){
  last.day <- max(plot.roll[plot.roll$year == 2023, "yday"])
  if(VAR == "Soil Moisture m3/m3"){
    png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0('Soil_Moisture_', max,'.png')))
  } else if (VAR =="PAR W/m2"){
    png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0('PAR_2023_',max,'.png')))
  } else {
    png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0(VAR, "_", max, '.png')))
  }
  old <- ggplot() +
    facet_wrap(~Plot_Name, scales="free_y") +
    geom_line(aes(x = yday, y = VAR_03, color = as.character(year)), data = plot.roll[plot.roll$var == VAR & plot.roll$yday <= last.day & plot.roll$year <2023,]) +
    theme_bw()+
    labs(y = VAR, x = "day of year", color = "Year")+
    ggtitle(paste("Median ", VAR, " up to ", max,  sep = ""))
  
  fig <- old +
    geom_line(aes(x = yday, y = VAR_03, color = as.character(year)), data = plot.roll[plot.roll$var == VAR & plot.roll$yday <= last.day & plot.roll$year ==2023,], size = 1.2) 
  print(fig)
  dev.off()
}

#Zooming in on soil moisture from April to Jun
old <- ggplot() +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == "Soil Moisture m3/m3" & plot.stack$yday <= 160 & plot.stack$yday >= 91 & plot.stack$year <2023,]) +
  theme_bw()+
  labs(y = "Soil Moisture", x = "day of year", color = "Year")+
  ggtitle(paste("Zoom in on Median ", "Soil Moisture ", "from April 1st to June 2nd", sep = ""))

zoom <- old +
  geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == "Soil Moisture m3/m3" & plot.stack$yday <= 160 & plot.stack$yday >= 91 & plot.stack$year ==2023,], size = 1.2) 

png(width = 8, height = 4, units = 'in', res = 300, filename= file.path("../Figures", paste0('Soil_Moisture_Zoom_2023.png')))
  zoom
dev.off()

#Zooming in on PAR from April to Jun. Not interesting
old <- ggplot() +
  facet_wrap(~Plot_Name, scales="free_y") +
  geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == "PAR" & plot.stack$yday <= 153 & plot.stack$yday >= 91 & plot.stack$year <2023,]) +
  theme_bw()+
  labs(y = "PAR", x = "day of year", color = "Year")+
  ggtitle(paste("Zoom in on Median ", "PAR ", "from April 1st to June 2nd", sep = ""))

zoom <- old +
  geom_line(aes(x = yday, y = values, color = as.character(year)), data = plot.stack[plot.stack$var == "PAR" & plot.stack$yday <= 153 & plot.stack$yday >= 91 & plot.stack$year ==2023,], size = 1.2) 

png(width= 750, filename= file.path("../Figures", paste0('PAR_Zoom_2023.png')))
  zoom
dev.off()
  