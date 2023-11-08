# Checking old files to rectify timestamp errors
path.met <- "~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"
path.in <- paste(path.met, "Data_processed/Clean_data", sep="")

cleanB127 <- dir(file.path(path.in, "B127"), ".csv")

b127.2017  <- read.csv(file.path(path.in, "B127", "B127_2017.csv"))
head(b127.2017)
tail(b127.2017)
b127.2017$Date_Time <-  as.POSIXct(strptime(b127.2017$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2017)

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
head(b127.2022)
tail(b127.2022)
b127.2022$Date_Time <-  as.POSIXct(strptime(b127.2022$Date_Time, format="%Y-%m-%d %H")) 
summary(b127.2022)
