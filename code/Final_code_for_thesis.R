

# CODE-1

####cLEAN WATER QUALITY DATA

##Water quality data management

## Import lake data_TP1&TP2(Lake_1) 
##I import lake_1 & lake_2 water quality data from (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage)

data1 <-read.csv("data to manage/Lake_1.csv", header = T)
## Import lake data_Secchi_depth (Lake_2) 
data2 <-read.csv("data to manage/Lake_2.csv", header = T)
## Import house data (house data1) 
HD <-read.csv("data to manage/house data/house data1.csv", header = T)
## Import house price index data (HPI_data)
data_HPI <-read.csv("data to manage/HPI/HPI_data.csv", header = T)

library(rio)
library(tidyverse)
library(lubridate)
library(geosphere)
library(plyr)
library(data.table)
library(dplyr)
library(spatialrisk)
library(stargazer)

# Cleans environment
rm(list=ls(all=TRUE))

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
data11$Year<-year(data11$yr)

##Finding mean value of TP1 & TP2 for a single year
#First, find the mean value from TP1 & TP2>> (TP1+TP2)/2

data11$meanTP <- rowMeans(data11[c('TP1', 'TP2')], na.rm=TRUE)

#Find out the yearly mean value
#add all meanTP from different months of a same year and divided it by the number of months
data11_yearly_mean_TP <- ddply(data11,c("Lake.Name", "STN","Site.ID","Latitude", "Longitude", "Year"),
                               summarise, meanTP_yearly=mean(meanTP)) 

## Change the dms lat and long data into decimal for Data1
#Latitude
dms_data11_yearly_mean_TP<-transform(data11_yearly_mean_TP, d = substr(Latitude,1,2),m = substr(Latitude,3,4),s = substr(Latitude,5,6))

#Longitude
dms_data_set_1_TP<-transform(dms_data11_yearly_mean_TP, d_long = substr(Longitude,1,2),m_long = substr(Longitude,3,4),s_long = substr(Longitude,5,6))


## Export r file as .csv

export(dms_data_set_1_TP, "dms_data_set_1_TP.csv")

##Final conversion DMS to decimal for data1
#after exporting the  "dms_data_set_1" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data1". Then import the data

## Import "Final_clean_data_set_1_TP" (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage\clean lat_long data\FINAL DATA DMS TO DD)

Final_clean_data_set_1_TP <-read.csv("data to manage/clean lat_long data/FINAL DATA DMS TO DD/Final_clean_data_set_1_TP.csv", header = T)

## Import lake data_Secchi_depth (Lake_2) 

# Check is there any missing data for data2
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
export(dms_data_set_2_SD, "dms_data_set_2_SD.csv")

##Final conversion DMS to decimal for data1
#after exporting the  "dms_data22_data2" in csv format, I use -> d+m/60+s/3600 to get the latitude data and d_long+m_long/60+s_long/3600 >>
#take -(d_long+m_long/60+s_long/3600) as longitude data and saved the file as "Final_clean_data2". Then import the data
## Import "Final_clean_data_set_2_SD" (C:\Users\rar727\OneDrive - University of Saskatchewan\rash\data to manage\clean lat_long data\FINAL DATA DMS TO DD)

Final_clean_data_set_2_SD <-read.csv("data to manage/clean lat_long data/FINAL DATA DMS TO DD/Final_clean_data_set_2_SD.csv", header = T)


##Final managed data from data1 (Final_clean_data1) and data2 (Final_clean_data2)

## merges data > Final_data_set_marged (Final_clean_data_set_1_TP & Final_clean_data_set_2_SD based on "Lake.Name", "STN", "Site.ID", "Latitude", "Longitude", "Year")
Final_data_set_marged <- merge(Final_clean_data_set_1_TP, Final_clean_data_set_2_SD,by=c("Lake.Name", "STN", "Site.ID", "Latitude", "Longitude", "Year"),all=TRUE)

## Omit N/A data= only select rows with complete data in all columns
Final_data_set_marged_omit <- na.omit(Final_data_set_marged)

## data edited by specific lat and long in excel
# I only keep the data lies inside the lat long of the study area

##Add "Lake_ID" to "Final_wq_data" 
## I add a new column "Lake_ID" to edit_lat_long data

edit_lat_long <- read.csv("data to manage/TP_SD_with_specidic_lat_long/Final_data_set_marged_omit.csv", header = T)

# Unique lakes
unique_lake_new <- distinct(edit_lat_long, Lake.Name, .keep_all= TRUE)
Final_wq_data_2<- edit_lat_long


# CODE-2
#------------------
#--------------------
## House data

## Import house data (house data1)

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
export(clean_HD_sub, "clean_HD_sub.csv")


#CODE-3
#--------------
#--------------
####Manage house price index (HPI) data

## Import HPI data (Base year 2016)
##In HPI_data, I converted Total(total HPI) into HPI by dividing Total with 100

## Convert date: "%m/%d/%Y" to "%Y/%m"
data_HPI$Date <- format(as.Date(data_HPI$Time.period, format="%m/%d/%Y"),"%Y/%m")

## Subset from data_HPI ("Date","Total")
clean_data_HPI<-subset(data_HPI,select = c("Date", "HPI"))

## Adjust house price with HPI

##Import clean house data "clean_HD_sub"
clean_HD_sub <- read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/house data/clean_HD_sub.csv", header = T)

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
export(clean_HPI_HD_real_price, "clean_HPI_HD_real_price.csv")

#----------------------------------------------------------------
# CODE-4
#---------------
#---------------

####Shortest distance calculation between lake and house location

##Calculate shortest distance between house and lake station

##Import lake wq data (Final_wq_data_2) and housing data with real price (clean_HPI_HD_real_price)

v_lake <- read.csv("final data/wq/Final_wq_data_2.csv", header = T)

##I insert a new column "ID" in "clean_HPI_HD_real_price" and then import
w_house <- read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)


## create two different subset from v_lake & w_house data (one for house coordinates(w) & one for lake coordinates(v))

v <-subset(v_lake,select = c("Lake_ID","Latitude", "Longitude"))

w <-subset(w_house,select = c("ID","Latitude", "Longitude"))


#Euclidean distance 
mydist <- function(a, b, w, x, y){
  
  dt <- data.table(sqrt((w[[x]]-a)^2 + (w[[y]]-b)^2))
  
  return(data.table(Closest.V1  = which.min(dt$V1),
                    Distance    = dt[which.min(dt$V1)]))
}

Final_shortest_dist <- setDT(w)[, j = mydist(Latitude, Longitude, setDT(v), 
                                             "Latitude", "Longitude"), 
                                by = list(ID, Latitude, Longitude)]

##Change the colamn name "Closest.V1" to "Lake_ID"
colnames(Final_shortest_dist)[4] <- "Lake_ID"

##Change the colamn name "distance.V1" to "shortest_distance"
colnames(Final_shortest_dist)[5] <- "shortest_distance"

## Export r file as .csv
export(Final_shortest_dist, "Final_shortest_dist.csv")

#----------------------------------------------------------------
# CODE-5 (CODE 6 FROM THE CODE LIST)
#---------------
#---------------

####Merge lake size data with WQ data
## Import data
## Import wq data
wq <-read.csv("final data/wq/Final_wq_data_2.csv", header = T)

##Import lake size data (Lake_size)
Lake_size <-read.csv("data to manage/lake size/Lake_size_id.csv", header = T)

##lake names in lower case for WQ data
h <- data.frame(Lake_Name=tolower(wq$Lake.Name))
wq_low <- cbind(wq,h)
wq_low_final<-subset(wq_low,select = c("Lake_ID", "STN", "Site.ID", "Lake_Name","Latitude", "Longitude", "Year", "meanTP_yearly", "Secchi.depth_avg"))

## Remove apostrophe "s" from "Lake_Name" in wq data
wq_low_final$Lake_Name <- gsub("'", '', wq_low_final$Lake_Name)

## Unique lakes by name
wq_low_unique <- distinct(wq_low_final, Lake_Name, .keep_all= TRUE)
P<- wq_low_unique

##lake names in lower case for lake size data
h1 <- data.frame(Lake_Name=tolower(Lake_size$Lake.Name))
lake_size_low <- cbind(Lake_size,h1)
lake_size_low_final<-subset(lake_size_low,select = c("L_ID", "Lake_Name", "Latitude", "Longitude", "Lake.Area"))

## Remove apostrophe "s" from "Lake_Name" in lake size data
lake_size_low_final$Lake_Name <- gsub("'", '', lake_size_low_final$Lake_Name)
Q<-lake_size_low_final

## Closest lake 
P<- wq_low_unique
Q<-lake_size_low_final


#Euclidean distance 
mydist <- function(a, b, P, x, y){
  
  dt <- data.table(sqrt((P[[x]]-a)^2 + (P[[y]]-b)^2))
  
  return(data.table(Closest.V1  = which.min(dt$V1),
                    Distance    = dt[which.min(dt$V1)]))
}

PQ <- setDT(P)[, j = mydist(Latitude, Longitude, setDT(Q), 
                            "Latitude", "Longitude"), 
               by = list(Lake_ID, Latitude, Longitude)]

##Change the colamn name "Closest.V1" to "L_ID"
colnames(PQ)[4] <- "L_ID"
colnames(Q)[3] <- "L_Latitude"
colnames(Q)[4] <- "L_Longitude"
colnames(Q)[2] <- "L_Lake_Name"

## Join 
##Left_join (P and PQ)
PP <- left_join(P, PQ)
PP_Q <- left_join(PP, Q)

##Subset of PP_Q
PP_Q_sub <- subset(PP_Q,select = c("Lake_Name", "Lake.Area"))

##Left_join (wq_low_final and PP_Q_sub)
Final_wq_lake_size <- left_join(wq_low_final, PP_Q_sub)

##Export data
export(Final_wq_lake_size , "Final_wq_lake_size.csv")

# CODE-6
#------------------------------------------------------------

# Calculates average WQ within certain threshold as well as closest WQ station by year

# Function to calculate WQ variables

CalcWQVariables <- function(df_wq, # Wq data
                            df_house, # Single row of housing data
                            dist_metres) # distance thresholds for radius
{
  
  # Filter out WQ data for particular year
  df_wq <- df_wq[df_wq$Year == df_house$Year, ]
  
  # Calculate distance between house and all stations within year
  points <- points_in_circle(df_wq, df_house$Longitude, df_house$Latitude, radius = 2000000000) # metres
  
  # Return closest station
  closest <- points[which.min(points$distance_m),]
  
  # Return all stations within threshold distance
  points <- points[points$distance_m < dist_metres,]
  
  out <- df_house %>%
    mutate(
      # take average of WQ within threshold
      meanTP = mean(points$meanTP_yearly),
      meanSecchi = mean(points$Secchi.depth_avg),
      # How many stations average based on
      n_stations = nrow(points),
      # Return closest WQ station and data
      distance_m = closest$distance_m,
      closeTP = closest$meanTP_yearly,
      closeSecchi = closest$Secchi.depth_avg)
  
  return(out)
}

# Distance threshold to use (in metres)
dist_metres <- 3000

# load data
v_lake <- Final_wq_lake_size
w_house <- read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)

df_wq <- v_lake %>%
  select(Year, Latitude, Longitude, meanTP_yearly, Secchi.depth_avg) 


colnames(df_wq)[2] <- "lat"
colnames(df_wq)[3] <- "lon"
summary(df_wq)

# Split housing data into lists for use in purrr:map
df_house <- split(w_house, w_house$ID)

df_wq_link <- map_dfr(df_house, CalcWQVariables, 
                      df_wq = df_wq, 
                      dist_metres = dist_metres)

write_csv(df_wq_link, "final data/house data/wq_house_merged.csv")

##############

## Import house price data (clean_HPI_HD_real_price)
HP <-read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)

## Import shortest distance data (Final_shortest_dist)
HSD <-Final_shortest_dist

## Subset of house data and shortest distance data
HP_sub <- subset(HP, select = c("ID", "Latitude", "Longitude", "Year", "area", "Real_price"))
HSD_sub <- subset(HSD, select = c("ID", "Latitude", "Longitude", "Lake_ID", "shortest_distance"))

##jOin HP_sub and HSD_sub
##left join                                                                                                                                                                                                        
HP_HSD <- left_join(HP_sub, HSD_sub)
v_lake_1 <- subset(v_lake, select = c("Lake_ID", "Lake.Area"))
HP_HSD_1 <- left_join(HP_HSD, v_lake_1)
HP_HSD_WQ <- left_join(HP_HSD_1, df_wq_link)


Final_data <-subset(HP_HSD_WQ, select = c("Latitude", "Longitude", "Year", "area", "Real_price", "shortest_distance","Lake.Area", "closeTP", "closeSecchi"))

sum(is.na(Final_data))

#---------------------------------------
# CODE-7 (Calculate shortest distance between house and nearest city)
#--------------------------
#--------------------------
##Calculate shortest distance between house and nearest city

v_lake <- read.csv("data to manage/exp_locations.csv", header = T)

##I insert a new column "ID" in "clean_HPI_HD_real_price" and then import
w_house <- read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)

## create two different subset from v_lake & w_house data (one for house coordinates(w) & one for lake coordinates(v))

v <-subset(v_lake,select = c("c_ID","Latitude", "Longitude"))
w <-subset(w_house,select = c("ID","Latitude", "Longitude"))

#Euclidean distance 
mydist <- function(a, b, w, x, y){
  
  dt <- data.table(sqrt((w[[x]]-a)^2 + (w[[y]]-b)^2))
  
  return(data.table(Closest.V1  = which.min(dt$V1),
                    Distance    = dt[which.min(dt$V1)]))
}

Final_shortest_dist_1 <- setDT(w)[, j = mydist(Latitude, Longitude, setDT(v), 
                                               "Latitude", "Longitude"), 
                                  by = list(ID, Latitude, Longitude)]

##Change the colamn name "Closest.V1" to "c_ID"
colnames(Final_shortest_dist_1)[4] <- "c_ID"

##Change the colamn name "distance.V1" to "city_distance"
colnames(Final_shortest_dist_1)[5] <- "city_distance"
city_distance <-Final_shortest_dist_1

#merge HP_HSD_WQ and city_distance
HP_HSD_WQ_city <- left_join(HP_HSD_WQ, city_distance)
Final_data_city <-subset(HP_HSD_WQ_city, select = c("Real_price","Year", "area", "shortest_distance","Lake.Area", "distance_m", "closeTP", "closeSecchi", "city_distance", "c_ID", "Latitude", "Longitude"))

## model calculation
##Change the column name "Closest.V1" to "L_ID"
colnames(Final_data_city)[6] <- "Distance"
colnames(Final_data_city)[7] <- "Total_phosphorus"
colnames(Final_data_city)[8] <- "Secchi_depth"
colnames(Final_data_city)[3] <- "Lot_size"

df <- Final_data_city

# Take log for Real_price, distance, lot_size
df1<- subset(df,select = c(Real_price, Distance, Lot_size, Lake.Area))
l_Real_price <- log(df1[,c(1)])
l_Distance <- log(df1[,c(2)])
l_Lot_size <- log(df1[,c(3)])
l_Lake.Area <- log(df1[,c(4)])

## Take log for SD & TP
l_Secchi_depth <- log(df$Secchi_depth)
l_Total_phosphorus <- log(df$Total_phosphorus)


##combine all the log variables
df2 <- data.frame(l_Real_price)
df3 <- data.frame(l_Distance)
df4 <- data.frame(l_Lot_size)
df5 <- data.frame(l_Lake.Area)
df6 <- data.frame(l_Secchi_depth)
df7 <- data.frame(l_Total_phosphorus)
df <- cbind(df, df2, df3, df4, df5,df6,df7)

summary(df)

# remove infinite l_Lot_size data
df<- df[-c(25582, 34481),]

#create dummy variable for neighboring cities (4 dummy for 5 cities)
df$c_ID_dummy_P<- ifelse(df$c_ID==1,1,0)
df$c_ID_dummy_O<- ifelse(df$c_ID==2,1,0)
df$c_ID_dummy_H<- ifelse(df$c_ID==3,1,0)
df$c_ID_dummy_Br<- ifelse(df$c_ID==4,1,0)

#create dummy variable for different years (9 dummies for 10 years)
df$year_dummy_2005<- ifelse(df$Year==2005,1,0)
df$year_dummy_2006<- ifelse(df$Year==2006,1,0)
df$year_dummy_2007<- ifelse(df$Year==2007,1,0)
df$year_dummy_2008<- ifelse(df$Year==2008,1,0)
df$year_dummy_2009<- ifelse(df$Year==2009,1,0)
df$year_dummy_2010<- ifelse(df$Year==2010,1,0)
df$year_dummy_2011<- ifelse(df$Year==2011,1,0)
df$year_dummy_2012<- ifelse(df$Year==2012,1,0)
df$year_dummy_2013<- ifelse(df$Year==2013,1,0)

#create dummy variable for lake distance
df$lake_dist_dummy_250m<- ifelse(df$Distance <=250,1,0)
df$lake_dist_dummy_500m<- ifelse(df$Distance > 250 & df$Distance <=500,1,0)
df$lake_dist_dummy_750m<- ifelse(df$Distance > 500 & df$Distance <=750,1,0)
df$lake_dist_dummy_1000m<- ifelse(df$Distance > 750 & df$Distance <=1000,1,0)
df$lake_dist_dummy_2000m<- ifelse(df$Distance > 1000 & df$Distance <=2000,1,0)
df$lake_dist_dummy_3000m<- ifelse(df$Distance > 2000 & df$Distance <=3000,1,0)


# create dummy for TP
df$tp_dummy <- ifelse(df$Total_phosphorus >= 10 & df$Total_phosphorus <=20,1,0)

#Interacted variables without dummy
df$interact_SD_TP<- df$Total_phosphorus * df$Secchi_depth
df$interact_l_Lake.Area_TP<- df$Total_phosphorus * df$l_Lake.Area
df$interact_l_Lake.Area_SD<- df$Secchi_depth * df$l_Lake.Area

df$interact_l_SD_l_TP<- df$l_Secchi_depth * df$l_Total_phosphorus
df$interact_l_Lake.Area_l_TP<- df$l_Total_phosphorus * df$l_Lake.Area
df$interact_l_Lake.Area_l_SD<- df$l_Secchi_depth * df$l_Lake.Area

#Interacted variables with dummy
df$interact_SD_TP_dummy<- df$tp_dummy * df$Secchi_depth
df$interact_l_Lake.Area_TP_dummy<- df$tp_dummy * df$l_Lake.Area

df$interact_l_SD_TP_dummy<- df$tp_dummy * df$l_Secchi_depth
df$interact_l_Lake.Area_TP_dummy<- df$tp_dummy * df$l_Lake.Area

## Interacted variable lake distance*SD
df$Distance_SD<- df$l_Distance * df$Secchi_depth

## Interacted variable lake distance*TP
df$Distance_TP<- df$l_Distance * df$Total_phosphorus

## Interacted variable lake distance dummy*SD
df$lake_dist_dummy_250m_SD<- df$lake_dist_dummy_250m * df$Secchi_depth
df$lake_dist_dummy_500m_SD<- df$lake_dist_dummy_500m * df$Secchi_depth
df$lake_dist_dummy_750m_SD<- df$lake_dist_dummy_750m * df$Secchi_depth
df$lake_dist_dummy_1000m_SD<- df$lake_dist_dummy_1000m * df$Secchi_depth
df$lake_dist_dummy_2000m_SD<- df$lake_dist_dummy_2000m * df$Secchi_depth
df$lake_dist_dummy_3000m_SD<- df$lake_dist_dummy_3000m * df$Secchi_depth

## Interacted variable lake distance dummy*TP
df$lake_dist_dummy_250m_TP<- df$lake_dist_dummy_250m * df$Total_phosphorus
df$lake_dist_dummy_500m_TP<- df$lake_dist_dummy_500m * df$Total_phosphorus
df$lake_dist_dummy_750m_TP<- df$lake_dist_dummy_750m * df$Total_phosphorus
df$lake_dist_dummy_1000m_TP<- df$lake_dist_dummy_1000m * df$Total_phosphorus
df$lake_dist_dummy_2000m_TP<- df$lake_dist_dummy_2000m * df$Total_phosphorus
df$lake_dist_dummy_3000m_TP<- df$lake_dist_dummy_3000m * df$Total_phosphorus



## find the data set within specific lake distance
## within 250m
df_250m<-df[df$Distance<=250,]
## within 250m-500m
df_500m<-df[df$Distance > 250 & df$Distance <=500,]
## within 500m-750m
df_750m<-df[df$Distance > 500 & df$Distance <=750,]
## within 750m-1000m
df_1km<-df[df$Distance > 750 & df$Distance <=1000,]
## within 1000m-2000m
df_2km<-df[df$Distance > 1000 & df$Distance <=2000,]
## within 2000m-3000m
df_3km<-df[df$Distance > 2000 & df$Distance <=3000,]
## above 3000m
df_above_3km<-df[df$Distance > 3000,]

df$lake_dist_dummy_250m<- ifelse(df$Distance <=250,1,0)
df$lake_dist_dummy_500m<- ifelse(df$Distance > 250 & df$Distance <=500,1,0)
df$lake_dist_dummy_750m<- ifelse(df$Distance > 500 & df$Distance <=750,1,0)
df$lake_dist_dummy_1000m<- ifelse(df$Distance > 750 & df$Distance <=1000,1,0)
df$lake_dist_dummy_2000m<- ifelse(df$Distance > 1000 & df$Distance <=2000,1,0)
df$lake_dist_dummy_3000m<- ifelse(df$Distance > 2000 & df$Distance <=3000,1,0)

summary(df)
summary(df_250m)
summary(df_500m)
summary(df_750m)
summary(df_1km)
summary(df_2km)
summary(df_3km)
summary(df_above_3km)
