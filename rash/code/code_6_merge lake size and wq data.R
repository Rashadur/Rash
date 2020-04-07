

####Merge lake size data with WQ data

## Import data
## Import wq data
wq <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/wq/Final_wq_data_1.csv", header = T)

##Import lake size data (Lake_size)
Lake_size <-read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/data to manage/lake size/Lake_size.csv", header = T)

##lake names in lower case for WQ data
h <- data.frame(Lake_Name=tolower(wq$Lake.Name))
wq_low <- cbind(wq,h)
wq_low_final<-subset(wq_low,select = c("Lake_Name", "STN","Site.ID","Latitude", "Longitude", "Year","Secchi.depth_avg", "meanTP_yearly"))

## Remove apostrophe "s" from "Lake_Name" in wq data
wq_low_final$Lake_Name <- gsub("'", '', wq_low_final$Lake_Name)

##lake names in lower case for lake size data
h1 <- data.frame(Lake_Name=tolower(Lake_size$Lake.Name))
lake_size_low <- cbind(Lake_size,h1)
lake_size_low_final<-subset(lake_size_low,select = c("Lake_Name", "Latitude", "Longitude", "Lake.Area"))

## Remove apostrophe "s" from "Lake_Name" in lake size data
lake_size_low_final$Lake_Name <- gsub("'", '', lake_size_low_final$Lake_Name)

##merge wq&lake size
Final_marged_lake_size_wq <- merge(wq_low_final, lake_size_low_final,by=c("Lake_Name"),all=TRUE)
Final_marged_ls_wq_omit<- na.omit(Final_marged_lake_size_wq)


