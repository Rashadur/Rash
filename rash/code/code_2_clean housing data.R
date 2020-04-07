
## House data

## Import house data (house data1)
HD <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/house data/house data1.csv", header = T)

sum(is.na(HD))
sum(is.na(HD$Date))
sum(is.na(HD$house_price))
sum(is.na(HD$area))
sum(is.na(HD$Latitude))
sum(is.na(HD$Longitude))
sum(is.na(HD$perimeter))
sum(is.na(HD$legal_description))
sum(is.na(HD$pin))

# Find missing data
which(is.na(HD))
which(is.na(HD$Date))
which(is.na(HD$house_price))
which(is.na(HD$area))
which(is.na(HD$Latitude))
which(is.na(HD$Longitude))
which(is.na(HD$perimeter))
which(is.na(HD$legal_description))

## Omit N/A data= only select rows with complete data in all columns
clean_HD <- na.omit(HD)
sum(is.na(clean_HD))


##Convert date into year("%m/%d/%Y" to "Y")

clean_HD$Year <- format(as.Date(clean_HD$date, format="%m/%d/%Y"),"%Y")
clean_HD$Date <- format(as.Date(clean_HD$date, format="%m/%d/%Y"),"%Y/%m")

## Subset from clean_HD("Year","Latitude", "Longitude", "area", "house_price")
clean_HD_sub<-subset(clean_HD,select = c("Year", "Date","Latitude", "Longitude", "area", "house_price"))

## Export r file as .csv
install.packages("rio")
library("rio")
export(clean_HD_sub, "clean_HD_sub.csv")

