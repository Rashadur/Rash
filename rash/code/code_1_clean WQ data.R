####cLEAN WATER QUALITY DATA

##Water quality data management

## Import lake data_TP1&TP2(Lake_1) 
##I import lake_1 & lake_2 water quality data from (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage)

data1 <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/Lake_1.csv", header = T)
## Import lake data_Secchi_depth (Lake_2) 
data2 <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/Lake_2.csv", header = T)

install.packages("lubridate")
library(lubridate)
install.packages("plyr")
library(plyr)
install.packages("rio")
library("rio")
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("geosphere")
library(geosphere)

# Check is there any missing data (data1)
sum(is.na(data1))
sum(is.na(data1$Lake.Name))
sum(is.na(data1$STN))
sum(is.na(data1$Site.ID))
sum(is.na(data1$Latitude))
sum(is.na(data1$Longitude))
sum(is.na(data1$Date))
sum(is.na(data1$TP1))
sum(is.na(data1$TP2))

# Find missing data
which(is.na(data1))
which(is.na(data1$Lake.Name))
which(is.na(data1$STN))
which(is.na(data1$Site.ID))
which(is.na(data1$Latitude))
which(is.na(data1$Longitude))
which(is.na(data1$Date))
which(is.na(data1$TP1))
which(is.na(data1$TP2))



## Subset from data1("Lake.Name", "STN","Site.ID", "Date","TP1","TP2")
data11<-subset(data1, select = c("Lake.Name", "STN","Site.ID", "Latitude", "Longitude", "Date", "TP1", "TP2"))

##Convert date into year("23 aug 2002" to "2002-05-23")
data11$yr<-as.Date(data11$Date, format="%d-%b-%y")

##Convert date into only year("2002-05-23" to "2002")
install.packages("lubridate")

library(lubridate)
data11$Year<-year(data11$yr)

##Finding mean value of TP1 & TP2 for a single year
#First, find the mean value from TP1 & TP2>> (TP1+TP2)/2

data11$meanTP <- rowMeans(data11[c('TP1', 'TP2')], na.rm=TRUE)

#Find out the yearly mean value
#add all meanTP from different months of a same year and divided it by the number of months
library(plyr)
data11_yearly_mean_TP<-ddply(data11,c("Lake.Name", "STN","Site.ID","Latitude", "Longitude", "Year"),summarise,meanTP_yearly=mean(meanTP)) 

## Change the dms lat and long data into decimal for Data1
#Latitude
dms_data11_yearly_mean_TP<-transform(data11_yearly_mean_TP, d = substr(Latitude,1,2),m = substr(Latitude,3,4),s = substr(Latitude,5,6))

#Longitude
dms_data_set_1_TP<-transform(dms_data11_yearly_mean_TP, d_long = substr(Longitude,1,2),m_long = substr(Longitude,3,4),s_long = substr(Longitude,5,6))


## Export r file as .csv
install.packages("rio")
library("rio")
export(dms_data_set_1_TP, "dms_data_set_1_TP.csv")

##Final conversion DMS to decimal for data1
#after exporting the  "dms_data_set_1" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data1". Then import the data

## Import "Final_clean_data_set_1_TP" (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage\clean lat_long data\FINAL DATA DMS TO DD)

Final_clean_data_set_1_TP <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/clean lat_long data/FINAL DATA DMS TO DD/Final_clean_data_set_1_TP.csv", header = T)

## Import lake data_Secchi_depth (Lake_2) 
data2 <-read.csv("//cabinet.usask.ca/work$/rar727/My Documents/GitHub/Water-quality/data for thesis/clean data/Lake_2.csv", header = T)

# Check is there any missing data
sum(is.na(data2))
sum(is.na(data2$Lake.Name))
sum(is.na(data2$STN))
sum(is.na(data2$Site.ID))
sum(is.na(data2$Latitude))
sum(is.na(data2$Longitude))
sum(is.na(data2$Year))
sum(is.na(data2$Secchi.depth_avg))

# Find missing data
which(is.na(data2))
which(is.na(data2$Lake.Name))
which(is.na(data2$STN))
which(is.na(data2$Site.ID))
which(is.na(data2$Latitude))
which(is.na(data2$Longitude))
which(is.na(data2$Year))
which(is.na(data2$Secchi.depth_avg))

## Subset from data2("Lake.Name", "STN","Site.ID", "Year","Secchi.depth_avg")
data22<-subset(data2,select = c("Lake.Name", "STN","Site.ID","Latitude", "Longitude", "Year","Secchi.depth_avg"))

## Change the dms lat and long data into decimal for Data2
#Latitude
dms_data22<-transform(data22, d = substr(Latitude,1,2),m = substr(Latitude,3,4),s = substr(Latitude,5,6))

#Longitude
dms_data_set_2_SD<-transform(dms_data22, d_long = substr(Longitude,1,2),m_long = substr(Longitude,3,4),s_long = substr(Longitude,5,6))

## Export r file as .csv
install.packages("rio")
library("rio")
export(dms_data_set_2_SD, "dms_data_set_2_SD.csv")

##Final conversion DMS to decimal for data1
#after exporting the  "dms_data22_data2" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data2". Then import the data
## Import "Final_clean_data_set_2_SD" (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage\clean lat_long data\FINAL DATA DMS TO DD)

Final_clean_data_set_2_SD <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/clean lat_long data/FINAL DATA DMS TO DD/Final_clean_data_set_2_SD.csv", header = T)


##Final managed data from data1 (Final_clean_data1) and data2 (Final_clean_data2)

## merges data > Final_data_set_marged (Final_clean_data_set_1_TP & Final_clean_data_set_2_SD based on "Lake.Name", "STN", "Site.ID", "Latitude", "Longitude", "Year")
Final_data_set_marged <- merge(Final_clean_data_set_1_TP, Final_clean_data_set_2_SD,by=c("Lake.Name", "STN", "Site.ID", "Latitude", "Longitude", "Year"),all=TRUE)

## Omit N/A data= only select rows with complete data in all columns
Final_data_set_marged_omit <- na.omit(Final_data_set_marged)

## Data set based on different lat and long values >> reducing the total area
library(dplyr)
library(tidyverse)

a1 <- filter(Final_data_set_marged_omit, -81.000<Longitude)
Final_clean_data_marged_low_lat_long <- filter(a1, 47.000>Latitude)
Final_wq_data <-Final_clean_data_marged_low_lat_long

## To check that is there any missing data
sum(is.na(Final_clean_data_marged_low_lat_long))

##Insert Lake_ID to "Final_wq_data"

## Export r file as .csv
install.packages("rio")
library("rio")
export(Final_wq_data , "Final_wq_data .csv")

##Add "Lake_ID" to "Final_wq_data" 
## I add a new column "Lake_ID" to Final_wq_data and then import again
Final_wq_data_1<- read.csv("//cabinet.usask.ca/work$/rar727/My Documents/GitHub/Water-quality/data for thesis/clean data/clean lat_long data/FINAL DATA DMS TO DD/Final_wq_data_1.csv", header = T)

