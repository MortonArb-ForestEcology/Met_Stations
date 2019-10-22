library(readbulk)
library(dplyr)

#BZ Desktop
#setwd("G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots")

#Loading in all .csv files for each plot
B127 <-read_bulk(directory = "Rollinson_B127", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
N115 <-read_bulk(directory = "Rollinson_N115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
HH115 <-read_bulk(directory = "Rollinson_HH115", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data
U134 <-read_bulk(directory = "Rollinson_U134", extension = ".csv", header = TRUE, skip=1, na.strings=c("-888.9")) # Combine all data



####Fixing redundant column names produced by updating HOBOware program which adds data logger and SN's in headers####

#--------------------------------#

#Consolidating B127 data

colnames(B127)
#Renaming columns produced by old and new Hoboware:
colnames(B127) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","Air_Temp_B", "Relative_Humidity_B","PAR_B", "PAR_C", "Time6", 
                    "Soil_Temp_C", "Air_Temp_C")#Change column names for B127
#Consolidating columns of Fahrenheit temperature and then converting it to Celcius
B127.convert <- B127 %>% mutate(Soil_Temp_X = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                                Air_Temp_X = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                                PAR_B = ifelse(is.na(PAR_B), PAR_C, PAR_B),
                                Soil_Temp_Y = ((Soil_Temp_X-32)*(5/9)), 
                                Air_Temp_Y = ((Air_Temp_X-32)*(5/9))) 

#Consolidating redundant columns:
B127.mod <- B127.convert %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                       Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                       Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                       PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                       Soil_Temp = ifelse(is.na(Soil_Temp_Y), Soil_Temp_C, Soil_Temp_Y),
                       Air_Temp = ifelse(is.na(Air_Temp_Y), Air_Temp_C, Air_Temp_Y))
#Adding in plot name:
B127.mod $ PlotName <- "B127"
#Checking columns to delete are correct for next lines
colnames(B127.mod)
#Deleting columns before "Time6"
B127.df <- subset(B127.mod, select = c(16,23:29))

#--------------------------------#

#Consolidating N115 data

colnames(N115)
colnames(N115) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B", "PAR_C", "Time6") #Change column names for N115
N115.mod <- N115 %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                            Soil_Temp = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                            Air_Temp = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                            PAR = ifelse(is.na(PAR_A), PAR_C, PAR_A))
N115.mod $ PlotName <- "N115"
colnames(N115.mod)
N115.df <- subset(N115.mod, select = -c(1,1:15))

#--------------------------------#

#Consolidating HH115 data

colnames(HH115)
colnames(HH115) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name",
                    "Time6_A", "Time5_B", "Soil_Temp_B", "Soil_Moisture_B","PAR_B", "Air_Temp_B", "Relative_Humidity_B","Time6_B", "PAR_C") #Change column names for HH115
HH115.mod <- HH115 %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                              Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                            Soil_Temp = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                            Air_Temp = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                            Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                            Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                            PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                            PAR = ifelse(is.na(PAR_A), PAR_C, PAR_A))
HH115.mod $ PlotName <- "HH115"
colnames(HH115.mod)
HH115.df <- subset(HH115.mod, select = -c(1,1:17))

#--------------------------------#

#Consolidating U134 data

colnames(U134)
colnames(U134) <- c("Row_Num", "Time5_A", "Soil_Temp_A", "Soil_Moisture_A", "PAR_A", "Air_Temp_A", "Relative_Humidity_A", "File_Name", 
                    "Time6_A", "Time5_B", "Soil_Temp_B", "Air_Temp_B", "Relative_Humidity_B","PAR_B", "Soil_Moisture_B","PAR_C", "Time6_B") #Change column names for U134
U134.mod <- U134 %>% mutate(Time5 = ifelse(is.na(Time5_A), as.character(Time5_B), as.character(Time5_A)),
                              Time6 = ifelse(is.na(Time6_A), as.character(Time6_B), as.character(Time6_A)),
                              Soil_Temp = ifelse(is.na(Soil_Temp_A), Soil_Temp_B, Soil_Temp_A),
                              Air_Temp = ifelse(is.na(Air_Temp_A), Air_Temp_B, Air_Temp_A),
                              Soil_Moisture = ifelse(is.na(Soil_Moisture_A), Soil_Moisture_B, Soil_Moisture_A),
                              Relative_Humidity = ifelse(is.na(Relative_Humidity_A), Relative_Humidity_B, Relative_Humidity_A),
                              PAR = ifelse(is.na(PAR_A), PAR_B, PAR_A),
                              PAR = ifelse(is.na(PAR_A), PAR_C, PAR_A))
U134.mod $ PlotName <- "U134"
colnames(U134.mod)
U134.df <- subset(U134.mod, select = -c(1,1:17))
colnames(U134.df)


#--------------------------------#

#Binding all plot data together
all_plots <- bind_rows(B127.df, N115.df, HH115.df, U134.df)


#Addressing daylight saving times issue (Time6 + Time5)
obs_date <- strsplit(getwd(), split = "/")
obs_date <- obs_date[[1]][length(obs_date[[1]])]
all_plots$Time6 <- strptime(all_plots$Time6, format="%m/%d/%y %I:%M:%S %p")
all_plots$Time5 <- strptime(all_plots$Time5, format="%m/%d/%y %I:%M:%S %p")
all_plots$Date_Time <- all_plots$Time5
all_plots[is.na(all_plots$Date_Time),"Date_Time"] <- all_plots[is.na(all_plots$Date_Time),"Time6"] + 60*60
summary(all_plots)
#Getting rid of extra time5 and time6 columns in front
all_plots = select(all_plots, -1, -2)
colnames(all_plots)


#Writing .csv of compiled/consolidated data
st=format(Sys.time(), "%Y-%m-%d")
filename <- paste("Met_Stations_Compiled_",st, ".csv", sep = "")
write.csv(all_plots, file = filename) #Write CSV to current directory

#-----------------------------------#

#If using a single plot instead of consolidating them all#

one_plot <- "plotname"

obs_date <- strsplit(getwd(), split = "/")
obs_date <- obs_date[[1]][length(obs_date[[1]])]
one_plot$Time6 <- strptime(one_plot$Time6, format="%m/%d/%y %I:%M:%S %p")
one_plot$Time5 <- strptime(one_plot$Time5, format="%m/%d/%y %I:%M:%S %p")
one_plot$Date_Time <- one_plot$Time5
one_plot[is.na(one_plot$Date_Time),"Date_Time"] <- one_plot[is.na(one_plot$Date_Time),"Time6"] + 60*60
summary(one_plot)
#Getting rid of extra time5 and time6 columns in front
one_plot = select(one_plot, -1, -2)
colnames(one_plot)

#Getting rid of redundant dates of data collection#
one_plot <- one_plot[!duplicated(one_plot[c('Date_Time')]),]

#Writing .csv of compiled/consolidated data
st=format(Sys.time(), "%Y-%m-%d")
filename <- paste("Met_Stations_Plotname_",st, ".csv", sep = "")
write.csv(one_plot, file = filename) #Write CSV to current directory
