#QAQC

library(ggplot2)
library(plotly)


setwd("C:/Users/BZumwalde/Desktop/ForestEcology_R/Met_Stations/Data_raw_inputs/Single_Plots")

met.all <- read.csv("Met_Stations_Single_Plots.csv")
met.all <- met.all[,2:ncol(met.all)]

met.all[!is.na(met.all$Soil_Temp) & (met.all$Soil_Temp< -888 | met.all$Soil_Temp>999), "Soil_Temp"] <- NA
met.all[!is.na(met.all$Soil_Moisture) & (met.all$Soil_Moisture< -0.5| met.all$Soil_Moisture>1), "Soil_Moisture"] <- NA
met.all[!is.na(met.all$PAR) & (met.all$PAR< -888 | met.all$PAR>999), "PAR"] <- NA
met.all[!is.na(met.all$Air_Temp) & (met.all$Air_Temp< -888 | met.all$Air_Temp>999), "Air_Temp"] <- NA
met.all[!is.na(met.all$Relative_Humidity) & (met.all$Relative_Humidity< -888 | met.all$Relative_Humidity>999), "Relative_Humidity"] <- NA


# Change date & time from factor to a continuous variable
str(met.all)
met.all$Date_Time <- as.POSIXct(met.all$Date_Time)
summary(met.all)

# Changing data to a "long" format that ggplot likes
met.stack <- stack(met.all[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(met.stack) <- c("values", "var")
met.stack[,c("PlotName", "Date_Time")] <- met.all[,c("PlotName", "Date_Time")]
summary(met.stack)

ggplot(met.stack, aes(x = Date_Time, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_line(aes(color=PlotName)) +
  theme_bw()



