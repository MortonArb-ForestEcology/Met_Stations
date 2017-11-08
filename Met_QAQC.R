met.all <- read.csv("~/Google Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/All_Plots_Compilation/MetStation_2017_EW_2017-10-02.csv")
met.all <- met.all[,2:ncol(met.all)]

# Remove unreasonable values
met.all[!is.na(met.all$Soil_Temp) & (met.all$Soil_Temp< -888 | met.all$Soil_Temp>999), "Soil_Temp"] <- NA
met.all[!is.na(met.all$Soil_Moisture) & (met.all$Soil_Moisture< -888 | met.all$Soil_Moisture>999), "Soil_Moisture"] <- NA
met.all[!is.na(met.all$PAR) & (met.all$PAR< -888 | met.all$PAR>999), "PAR"] <- NA
met.all[!is.na(met.all$Air_Temp) & (met.all$Air_Temp< -888 | met.all$Air_Temp>999), "Air_Temp"] <- NA

# Change date & time to a continuous variable (not factor)
met.all$Date_Time <- strptime(met.all$Date_Time, format="%m/%d/%Y %H:%M")
summary(met.all)

# Changing data to a "long" format that ggplot likes
met.stack <- stack(met.all[,c("Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity")])
names(met.stack) <- c("values", "var")
met.stack[,c("Plot", "Date_Time")] <- met.all[,c("Plot", "Date_Time")]
summary(met.stack)

library(ggplot2)
ggplot(data=met.stack[met.stack$Date_Time> strptime("2017-09-01", "%Y-%m-%d"),]) +
	facet_wrap(~var, scales="free_y") +
	geom_line(aes(x=Date_Time, y=values, color=Plot)) +
	theme_bw()