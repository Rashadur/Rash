
##Merge housing data and shortest distance data

##Merge House data (price, area, sold year) and shortest lake distance based on "ID", "latitude and longitude (house)" (clean_HPI_HD_real_price and Final_shortest_dist)

## Import house price data (clean_HPI_HD_real_price)
HP <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/HPI/clean_HPI_HD_real_price.csv", header = T)
HP_1 <- subset(HP, select = c("ID", "Year", "area", "Real_price"))


## Import shortest distance data (Final_shortest_dist)
HSD <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/shortest dist/Final_shortest_dist.csv", header = T)


##I merged house data (HP) amd shortest lake distance (HSD) based on "ID", "Latitude", "Longitude"
HP_HSD_merge <- merge(HP_1, HSD,by=c("ID"),all=TRUE)


#### Merge HP_HSD_merge and WQ(Final_wq_data_1) data

### Import wq data (Final_wq_data_1)

Final_wq_data_1 <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/wq/Final_wq_data_1.csv", header = T)
F_wq_data_1 <-subset(Final_wq_data_1, select = c("Lake_ID", "Year", "meanTP_yearly", "Secchi.depth_avg"))

##marge house data & shortest lake distance (HP_HSD_merge) and wq (F_wq_data_1) data ->> (Final_marged_wq_HD_SD)
Final_merge_wq_HD_SD <- merge(F_wq_data_1, HP_HSD_merge,by=c("Year", "Lake_ID"), all=TRUE)
Final <- na.omit(Final_merge_wq_HD_SD )







