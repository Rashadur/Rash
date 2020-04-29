rm(list=ls(all=TRUE))

library(tidyverse)
library(dplyr)
library(plyr)

##Merge housing data and shortest distance data

##Merge House data (price, area, sold year) and shortest lake distance based on "ID", "latitude and longitude (house)" (clean_HPI_HD_real_price and Final_shortest_dist)

## Import house price data (clean_HPI_HD_real_price)
HP <-read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)

## Import shortest distance data (Final_shortest_dist)
HSD <-read.csv("final data/shortest dist/Final_shortest_dist.csv", header = T)

## Import water quatity data Final_wq_data_1
Final_wq_data_1 <- read.csv("final data/wq/Final_wq_lake_size.csv", header = T)

## Subset of house data and shortest distance data
HP_sub <- subset(HP, select = c("ID", "Latitude", "Longitude", "Year", "area", "Real_price"))
HSD_sub <- subset(HSD, select = c("ID", "Latitude", "Longitude", "Lake_ID", "shortest_distance"))

##jOin HP_sub and HSD_sub
##left join                                                                                                                                                                                                        
HP_HSD <- left_join(HP_sub, HSD_sub)

## Create new columns L_Latitude for lake latitude and L_Longitude for lake longitude
Final_wq_data_1$L_Latitude <- Final_wq_data_1$Latitude
Final_wq_data_1$L_Longitude <- Final_wq_data_1$Longitude

## Subset water quality data
F_wq_data_1 <-subset(Final_wq_data_1, select = c("Lake_ID", "L_Latitude", "L_Longitude"))
F_wq_data_2 <-subset(Final_wq_data_1, select = c("L_Latitude", "L_Longitude", "Year", "meanTP_yearly", "Secchi.depth_avg", "Lake.Area"))

## join house data (HP_HSD) and lake location (F_wq_data_1)
##left join                                                                                                                                                                                                        
HP_HSD_wq_1 <- left_join(HP_HSD, F_wq_data_1)

## join HP_HSD_wq_1 and lake location (F_wq_data_2)
##left join  
HP_HSD_wq_2 <- left_join(HP_HSD_wq_1, F_wq_data_2)
HP_HSD_wq_3 <- na.omit(HP_HSD_wq_2)


## inner_join also shows the same result
HP_HSD_wq_22 <- inner_join(HP_HSD_wq_1, F_wq_data_2)

#### To find out the missing values (Lake_ID & Year)
## anti_join
HP_HSD_wq_4 <- anti_join(HP_HSD_wq_1, F_wq_data_2)

## unique lakes
unique_lake <- distinct(Final_wq_data_1, Lake_Name, .keep_all= TRUE)
unique_lake_1 <- distinct(Final_wq_data_1, Lake_Name, STN, Site.ID, .keep_all= TRUE)

## Missing Lake_ID
H_5 <- count(HP_HSD_wq_4, vars=c("Lake_ID", "Year"))
H_6 <- count(HP_HSD_wq_4, vars=c("Lake_ID"))








