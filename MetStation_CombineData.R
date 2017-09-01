

setwd("~/Forest_Ecology/Met_Station_Data/6.23.17") #Set working directory to data file location
A1 <- read.csv(dir(pattern = "^Rollinson_A1"), skip = 1, na.strings=c("-888.9")) #read in data file, change error terms to NA
B5 <- read.csv(dir(pattern = "^Rollinson_B5"), skip = 1, na.strings=c("-888.9")) #read in data file, change error terms to NA
C6 <- read.csv(dir(pattern = "^Rollinson_C6"), skip = 1, na.strings=c("-888.9")) #read in data file, change error terms to NA
D1 <- read.csv(dir(pattern = "^Rollinson_D1"), skip = 1, na.strings=c("-888.9")) #read in data file, change error terms to NA

obs_date <- strsplit(getwd(), split = "/") #get directory and split by folder name
obs_date <- obs_date[[1]][length(obs_date[[1]])] #select last folder name to get date

A1[,1] <- "A1" #Make first column the plot ID
B5[,1] <- "B5" #Make first column the plot ID
C6[,1] <- "C6" #Make first column the plot ID
D1[,1] <- "D1" #Make first column the plot ID

all_plots <- rbind.data.frame(A1, B5, C6, D1) #Combine all plots into same dataframe
all_plots <- rbind.data.frame(B5, C6, D1) #Temporary

colnames(all_plots) <- c("Plot", "Date_Time", "Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity") #Change column names

csv_name <- sprintf("Met_Stations_%s%s", obs_date, ".csv") #Create filename

write.csv(all_plots, file = csv_name) #Write CSV to current directory
