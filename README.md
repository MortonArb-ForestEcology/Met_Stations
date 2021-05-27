# Met_Stations

Mainteneance records for the data sensors and loggers can be found in this folder
G:\My Drive\East Woods\Rollinson_Monitoring\Protocols\Met_Stations

The data was gathered by Hoboware data loggers until 10/29/2020.

From 10/29/2020 data is gathered by both hoboware data loggers and onset data loggers with the exception of B127 which is only onset

# Scripts

## 1_Met_Consolidation.R

Purpose: To convert the raw data we receive from data loggers into consistent units and formats across plots and data loggers

Inputs: Raw data from the data loggers stored in "G:/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/"

Outputs: B127.csv, U134.csv, N115.csv, HH115.csv

Notes: This script should be broken into two seperate scripts for the two seperate types of data loggers as we go forward
       10/29/2020 is when the onset data loggers were installed

## 2_Met_File_Seperation.R

Purpose: To further process the sensor data into yearly and monthly csv's

Inputs: Plot csv's created by script 1_Met_Consolidation.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: Yearly and Monthly csv's for each plot (B127, U134, N115, HH115)

Notes: With updates this script can hopefully only work with new data

## 3_Met_Summary.R

Purpose: To visualize the trends in our sensor data

Inputs: Plot csv's created by script 1_Met_Consolidation.R (B127.csv, U134.csv, N115.csv, HH115.csv)

Outputs: Summary figures for QAQC of met station data

Notes:

# Old Scripts

## MetData_ConsolidatingRawData.R

 Old Script for data consolidation. Function is now done by 1_Met_Consolidation.R

## MetStation_CombineData_ArchivedVersion.R

 Old script. Honestly not sure what it was used for.

## Met_QAQC.R

 Old Script for QAQC. May be adopted into new workflow
