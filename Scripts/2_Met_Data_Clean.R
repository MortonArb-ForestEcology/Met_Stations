#-----------------------------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#Setting file paths
path.met <- "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.out <- paste(path.met, "Data_clean/", sep="")
setwd(path.out)


#Currently using the Month_Separation script because combining the seperate files results in 147 columns
#Will probably seperate the current month script into a QAQC script and the seperation script

# Seperating by chosen year and month values

plot.B127 <- read.csv("B127/B127.csv")
plot.N115 <- read.csv("N115/N115.csv")
plot.HH115 <- read.csv("HH115/HH115.csv")
plot.U134 <- read.csv("U134/U134.csv")

comb <- rbind(plot.B127, plot.N115, plot.HH115, plot.U134)

#Removing NA rows 
comb <- comb[!is.na(comb$Plot_Name),]

#Removing impossible values
comb$Soil_Moisture <- ifelse(comb$Soil_Moisture <= 0, NA, comb$Soil_Moisture)

metrics <- c("Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR")
df.clean <- data.frame()
for(PLOT in unique(comb$Plot_Name)){
  temp <- comb[comb$Plot_Name == PLOT, ]
  temp$Date <- as.Date(temp$Date_Time)
  for(DOY in unique(format(temp$Date))){
   DOY <- as.Date(DOY)
   DATES <- seq.Date(from = (DOY - 7), to = (DOY + 7), by = 1)
   for(VAL in metrics){
     SM.mean <- mean(temp[temp$Date %in% DATES, VAL], na.rm = T)
     SM.sd <- sd(temp[temp$Date %in% DATES, VAL], na.rm = T)
     #temp[temp$Date == DOY, VAL] <- ifelse(temp[temp$Date == DOY, VAL]<SM.mean-4*SM.sd | temp[temp$Date == DOY, VAL]>SM.mean+4*SM.sd, NA, temp[temp$Date == DOY, VAL] )
     temp[temp$Date == DOY, "SIGFLAG"] <- ifelse(temp[temp$Date == DOY, VAL]<SM.mean-4*SM.sd | temp[temp$Date == DOY, VAL]>SM.mean+4*SM.sd, T, F)
     }
  }
  df.clean <- rbind(df.clean, temp)
}
df.clean <- df.clean[which(df.clean$SIGFLAG == F),]
df.clean$Date_Time <- as.POSIXct(df.clean$Date_Time)

summary(df.clean)


# Changing data to a "long" format that ggplot likes
met.stack <- stack(df.clean[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(met.stack) <- c("values", "var")
met.stack[,c("Plot_Name", "Date_Time")] <- df.clean[,c("Plot_Name", "Date_Time")]
summary(met.stack)

#Initial plot 
ggplot(met.stack, aes(x = Date_Time, y = values)) +
  facet_wrap(~var, scales="free_y") +
  geom_smooth(aes(color=Plot_Name)) +
  theme_bw()+
  ggtitle("Met Stations")

