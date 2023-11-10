library(lubridate)
# Checking old files to rectify timestamp errors
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.in <- paste(path.met, "Data_processed/Clean_data", sep="")

# Comign up with all the columns we want and just listing them in an order I want them
cols.final <- c("year", "Plot_Name", "Timestamp", "Date_Time", "Date", "year", "Soil_Moisture", "Soil_Temp", "Air_Temp", "Relative_Humidity", "PAR", "mm.Precipitation", "Lightning.Activity", "km.Lightning.Distance", "deg.Wind.Direction", "m.s.Wind.Speed", "m.s.Gust.Speed", "kPa.Atmospheric.Pressure", "deg.X.axis.Level", "deg.Y.axis.Level", "mm.h.Max.Precip.Rate", "degC.RH.Sensor.Temp", "Battery.Percent", "mV.Battery.Voltage", "kPA.Reference.Pressure", "degC.Logger.Temperature", "SIGFLAG_Soil_Moisture", "SIGFLAG_Soil_Temp", "SIGFLAG_Air_Temp", "SIGFLAG_Relative_Humidity", "SIGFLAG_PAR")

cleanB127 <- dir(file.path(path.in, "B127"), ".csv")



b127.2017  <- read.csv(file.path(path.in, "B127", "B127_2017.csv"))
if(all(is.na(b127.2017$Timestamp))) b127.2017$Timestamp <- b127.2017$Date_Time
# Checkign datetime format
# b127.2017$Timestamp[rowsHyph]
head(b127.2017[,c("Timestamp", "Date_Time")])
rowsHyph <- grep("-", b127.2017$Timestamp); length(rowsHyph); nrow(b127.2017)
b127.2017$TEST  <-  as.POSIXct(b127.2017$Date_Time[rowsHyph], format="%Y-%m-%d %H", tz="America/Chicago")
test <-  as.POSIXct(b127.2017$Date_Time[rowsHyph], format="%Y-%m-%d %H", tz="America/Chicago")
b127.2017$TEST2[rowsHyph] <- test
summary(test); head(test); length(test); class(test)
summary(test)
summary(b127.2017)

# b127.2017$TEST <- test
head(b127.2017[,c("Timestamp", "Date_Time")])
summary(b127.2017[,c("Timestamp", "Date_Time")])
str(b127.2017$Timestamp)

head(b127.2017)
tail(b127.2017)



b127.2018  <- read.csv(file.path(path.in, "B127", "B127_2018.csv"))
head(b127.2018)
tail(b127.2018)
b127.2018$Date_Time <-  as.POSIXct(strptime(b127.2018$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2018)

b127.2019  <- read.csv(file.path(path.in, "B127", "B127_2019.csv"))
head(b127.2019)
tail(b127.2019)
b127.2019$Date_Time <-  as.POSIXct(strptime(b127.2019$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2019)

b127.2020  <- read.csv(file.path(path.in, "B127", "B127_2020.csv"))
head(b127.2020)
tail(b127.2020)
b127.2020$Date_Time <-  as.POSIXct(strptime(b127.2020$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2020)


b127.2021  <- read.csv(file.path(path.in, "B127", "B127_2021.csv"))
head(b127.2021)
tail(b127.2021)
b127.2021$Date_Time <-  as.POSIXct(strptime(b127.2021$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2021)

# This has a mix of formats!!
b127.2022  <- read.csv(file.path(path.in, "B127", "B127_2022.csv"))
b127.2022$TEST2 <- as.POSIXct(NA)
rowsSlash <- grep("/", b127.2022$Timestamp)
b127.2022$TEST2[rowsSlash] <- as.POSIXct(strptime(b127.2022$Timestamp[rowsSlash], format="%m/%d/%Y %H"), format="%Y-%m-%d %H") 

rowsDash <- grep("-", b127.2022$Timestamp)
dateDashNA <- which(is.na(strptime(b127.2022$Timestamp[rowsDash], format="%Y-%m-%d %H")))
b127.2022$Timestamp[rowsDash[dateDashNA]]<- paste(b127.2022$Timestamp[rowsDash[dateDashNA]], "0:00")
b127.2022$TEST2[rowsDash] <- as.POSIXct(strptime(b127.2022$Timestamp[rowsDash], format="%Y-%m-%d %H")) 
summary(b127.2022)


summary(b127.2022[,c("Timestamp", "Date_Time", "TEST2")])
head(b127.2022[,c("Timestamp", "Date_Time")])
tail(b127.2022[,c("Timestamp", "Date_Time")])

# Entries that have a dash for some reason drop 0:00 as the hour.  This is a pain!
rowsDash <- grep("-", b127.2022$Timestamp)
dateDashNA <- which(is.na(strptime(b127.2022$Timestamp[rowsDash], format="%Y-%m-%d %H")))
b127.2022$Timestamp[rowsDash[dateDashNA]]<- paste(b127.2022$Timestamp[rowsDash[dateDashNA]], "0:00")
# b127.2022$Timestamp[rowsDash[dateDashNA]]
length(rowsDash); nrow(b127.2022)
dateDash <- as.POSIXct(strptime(b127.2022$Timestamp[rowsDash], format="%Y-%m-%d %H")) 
summary(dateDash)
length(dateDash)

rowsSlash <- grep("/", b127.2022$Timestamp)
length(rowsSlash); nrow(b127.2022)
dateSlash <- as.POSIXct(strptime(b127.2022$Timestamp[rowsSlash], format="%m/%d/%Y %H"), format="%Y-%m-%d %H") 
summary(dateSlash)

# Everythign seems to work if you make the container POSIXct before putting antyhing into it
dateVect <- rep(NA, length=nrow(b127.2022))
dateVect <- as.POSIXct(dateVect)
dateVect[rowsSlash] <- as.POSIXct(dateSlash, format="%Y-%m-%d %H")
dateVect[rowsDash] <- dateDash
summary(dateVect)

b127.2022$TEST <- dateVect
summary(b127.2022)

# dateOther <- as.POSIXct(strptime(b127.2022$Timestamp, format="%m/%d/%Y %H")) 
# dateOther[is.na(dateOther)] <- dateHyph
# summary(dateOther)

# b127.2022$Timestamp[rowsHyph] <-  as.POSIXct(strptime(b127.2022$Date_Time[rowsHyph], format="%Y-%m-%d %H")) 


head(b127.2022)
tail(b127.2022)
b127.2022$Date_Time <-  as.POSIXct(strptime(b127.2022$Date_Time, format="%Y-%m-%d %H")) # Right Now introducing NAs because of weird midnight formats
summary(b127.2022)




b127.2023  <- read.csv(file.path(path.in, "B127", "B127_2023.csv"))
head(b127.2023)
tail(b127.2023)
b127.2023$Date_Time <-  as.POSIXct(strptime(b127.2023$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2023)
