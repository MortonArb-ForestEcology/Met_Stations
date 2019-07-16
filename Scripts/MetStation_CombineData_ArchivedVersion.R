
#Combine data for one date
setwd("~/Forest_Ecology/Met_Station_Data/2017-10-02") #Set working directory to data file location
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

colnames(all_plots) <- c("Plot", "Date_Time", "Soil_Temp", "Soil_Moisture", "PAR", "Air_Temp", "Relative_Humidity") #Change column names

csv_name <- sprintf("Met_Stations_%s%s", obs_date, ".csv") #Create filename

write.csv(all_plots, file = csv_name, row.names = FALSE) #Write CSV to current directory



#Add to prexisting compilation CSV
setwd("~/Forest_Ecology/Met_Station_Data/2017-10-02") #Set working directory to most recent combination CSV location
add_plot <- read.csv(dir(pattern = "^Met_Stations")) #read in most recent combination CSV data file

setwd("..") #set working directory to parent folder where compilation CSV is located

csvs <- file.info(list.files(pattern = "*.csv")) #get info for CSV files in directory
csvs <- csvs[with(csvs, order(mtime)),] #sort info by date file was last modified
csvs_last <- rownames(csvs)[length(rownames(csvs))] #get name of most recent file

comp_csv <- read.csv(csvs_last) #read in latest compilation data file

new_comp <- rbind.data.frame(comp_csv, add_plot) # Combine the two CSVs

comp_date <- Sys.Date() #Get today's date
comp_name <- sprintf("MetStation_2017_EW_%s%s", comp_date, ".csv") #create file name

write.csv(new_comp, file = comp_name, row.names = FALSE) #write compilation csv




