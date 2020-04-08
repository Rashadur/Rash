
####Manage house price index (HPI) data

## Import HPI data (Base year 2016)
##In HPI_data, I converted Total(total HPI) into HPI by dividing Total with 100
data_HPI <-read.csv("data to manage/HPI/HPI_data.csv", header = T)

## Convert date: "%m/%d/%Y" to "%Y/%m"
data_HPI$Date <- format(as.Date(data_HPI$Time.period, format="%m/%d/%Y"),"%Y/%m")

## Subset from data_HPI ("Date","Total")
clean_data_HPI<-subset(data_HPI,select = c("Date", "HPI"))

## Adjust house price with HPI

##Import clean house data "clean_HD_sub"
clean_HD_sub <- read.csv("final data/house data/clean_HD_sub.csv", header = T)

##Merge HPI and housing price based on "Date" (clean_data_HPI and clean_HD_sub)
marged_HPI_HD <- merge(clean_data_HPI, clean_HD_sub,by=c("Date"),all=TRUE)

## Omit N/A data= only select rows with complete data in all columns
Final_marged_HPI_HD <- na.omit(marged_HPI_HD)
sum(is.na(Final_marged_HPI_HD))

##Multiply HPI and house price to get real house price
Final_marged_HPI_HD$Real_price <- Final_marged_HPI_HD$HPI * Final_marged_HPI_HD$house_price

##Adjusted house price-> clean_HPI_HD_real_price

clean_HPI_HD_real_price <- Final_marged_HPI_HD

## Export r file as .csv
library("rio")
export(clean_HPI_HD_real_price, "clean_HPI_HD_real_price.csv")



