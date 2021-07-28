# Met_Stations

Mainteneance records for the data sensors and loggers can be found in this folder
G:\My Drive\East Woods\Rollinson_Monitoring\Protocols\Met_Stations

The data was gathered by Hoboware data loggers until 10/29/2020.

From 10/29/2020 data is gathered by both hoboware data loggers and onset data loggers with the exception of B127 which is only onset

# Scripts

## 1_ZL6_Consolidation.R

Purpose: To convert the raw data we receive from ZL6 data loggers into consistent units and formats across plots

Inputs: Raw data from the ZL6 data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"

Outputs: Harmonized data csvs: B127.csv, U134.csv, N115.csv, HH115.csv

Notes: This script became the main script for metstations as of 07/16/2021 when all four plots had ZL6 data loggers and atmos41 sensors


## 2_Met_Data_Clean.R

Purpose: To clean the raw data we recieve from our met stations

Inputs: Harmonized plot csv's created by script 1_ZL6_Consolidation.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: Harmonized data csvs (B127.csv, U134.csv, N115.csv, HH115.csv)

Notes: Cleaning in this script is defined as removing impossible values (negative soil moisture) and removing measurements more than 4 standard deviations from the 2 week mean surrounding the date of measurement 


## 3_Met_File_Seperation.R

Purpose: To further process the sensor data into yearly and monthly csv's

Inputs: Plot csv's created by script 2_Met_Data_Clean.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: Yearly and Monthly csv's for each plot (B127, U134, N115, HH115)

Notes: This script is set to only add on the new data instead of creating new files for all


## 4a_Met_Summary.R

Purpose: To visualize the trends in our sensor data

Inputs: Plot csv's created by script 1_Met_Consolidation.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: Summary figures for QAQC of met station data

Notes:


## 4b_Full_Data_Vis.RMD

Purpose: To visualize the trends in our sensor data

Inputs: Plot csv's created by script 1_Met_Consolidation.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: A markdown PDF that shows our data in it's various phases of cleaning and final figures

Notes: This is an RMD file (meaning RMarkdown) and not an R file


# Legacy scripts for Hoboware sensors

## Hoboware_Met_Consolidation.R

Purpose: To convert the raw data we receive from Hoboware data loggers into consistent units and formats across plots 

Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"

Outputs: B127.csv, U134.csv, N115.csv, HH115.csv

Notes: This script is for conversion and unification of data from hoboware sensors only


## Met_Consolidation_OLD.R

Purpose: To convert the raw data we receive from data loggers into consistent units and formats across plots and data loggers

Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"

Outputs: B127.csv, U134.csv, N115.csv, HH115.csv

Notes: This script is for harmonizing hoboware and meter sensors


# Old Scripts

## Met_QAQC.R

 Old Script for QAQC. May be adopted into new workflow
 
## MetData_ConsolidatingRawData.R

 Old Script for data consolidation. Function is now done by 1_Met_Consolidation.R

## MetStation_CombineData_ArchivedVersion.R

 Old script. Honestly not sure what it was used for.

 
 # Units
 
 ## Hoboware

  Variable   |     Unit
------------ | -------------
Air Temperature |  Celcius C
PAR (phoysythetically active radiation | umol/m2/sec
Relative Humidity | % water vapor need for saturaiton at current temperature
Soil Moisture | Volumetric water content m3/m3
Soil Temperature | Celcius C


## Meter (ZL6/Atmos41)

  Variable   |     Unit
------------ | -------------
Air Temperature |  Celcius C
Barometric Pressue |kPA
Horizontal Wind Speed | m/s
Humidity Sensor Temperature | Celcius C
Lightin Average Distance | km
Lighting Strike | Strikes
Precipitation | mm/h
Relative Humidity | % water vapor need for saturaiton at current temperature
Soil Moisture | Volumetric water content m3/m3
Soil Temperature | Celcius C
Solar Radiation | W/m2 (Solar irradiance)
Vapor Pressure | kPa
Wind Direction | Degrees (1-360)
Wind Gust | m/s

