
####Merge lake size data with WQ data

## Import data
## Import wq data
wq <-read.csv("final data/wq/Final_wq_data_1.csv", header = T)

##Import lake size data (Lake_size)
Lake_size <-read.csv("data to manage/lake size/Lake_size.csv", header = T)

##lake names in lower case for WQ data
h <- data.frame(Lake_Name=tolower(wq$Lake.Name))
wq_low <- cbind(wq,h)
wq_low_final<-subset(wq_low,select = c("Lake_Name", "STN","Site.ID","Latitude", "Longitude", "Year","Secchi.depth_avg", "meanTP_yearly"))

## Remove apostrophe "s" from "Lake_Name" in wq data
wq_low_final$Lake_Name <- gsub("'", '', wq_low_final$Lake_Name)

## Unique lake names
wq_low_final_1<- distinct(wq_low_final, Lake_Name, .keep_all= TRUE)

##lake names in lower case for lake size data
h1 <- data.frame(Lake_Name=tolower(Lake_size$Lake.Name))
lake_size_low <- cbind(Lake_size,h1)
lake_size_low_final<-subset(lake_size_low,select = c("Lake_Name", "Latitude", "Longitude", "Lake.Area"))

## Remove apostrophe "s" from "Lake_Name" in lake size data
lake_size_low_final$Lake_Name <- gsub("'", '', lake_size_low_final$Lake_Name)

## Remove all the duplicate lake names
##subset of lake size by Lake_Name
lake_size_low_final_1<-subset(lake_size_low,select = c("Lake_Name"))
lake_size_low_final_2<-subset(lake_size_low,select = c("Lake_Name", "Lake.Area"))

## Remove all the duplicate lake names
a <- lake_size_low_final_1[!(duplicated(lake_size_low_final_1) | duplicated(lake_size_low_final_1, fromLast = TRUE)), ]

## Lake names only come single time
aa <-data.frame(a)
aa$Lake_Name <- aa$a
aa<-subset(aa,select = c("Lake_Name"))


## Merge single appeared lake names with lake size
a_single_size <- merge(aa, lake_size_low_final_2,by=c("Lake_Name"),all.aa = TRUE)
a_single_size_omit<- na.omit(a_single_size)

## Merge unique lakes (wq_low_final_1) with lake size
a_merge <- merge(wq_low_final_1, a_single_size_omit,by=c("Lake_Name"),all.wq_low_final_1 = TRUE)
a_merge_omit<- na.omit(a_merge)


## To find out the lakes which have no lake size as there exist multiple value against the same name in the lake size data set
## subset from merged single lake size and lake name
a1<- subset(a_merge_omit,select = c("Lake_Name", "Lake.Area"))
##subset from unique lake names from water quality data
a2<- subset(wq_low_final_1,select = c("Lake_Name"))
## Find lake names which have lake size data and which don't have lake size data
a3<- merge(a2, a1,by=c("Lake_Name"),all = TRUE)

## NA values for the lake size should be filled up manually 


