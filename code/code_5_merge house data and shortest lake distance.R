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
Final_wq_data_1 <- read.csv("final data/wq/Final_wq_data_1.csv", header = T)

## Create new columns L_Latitude for lake latitude and L_Longitude for lake longitude
Final_wq_data_1$L_Latitude <- Final_wq_data_1$Latitude
Final_wq_data_1$L_Longitude <- Final_wq_data_1$Longitude

## Subset water quality data
F_wq_data_1 <-subset(Final_wq_data_1, select = c("Lake_ID", "L_Latitude", "L_Longitude"))
F_wq_data_2 <-subset(Final_wq_data_1, select = c("L_Latitude", "L_Longitude", "Year", "meanTP_yearly", "Secchi.depth_avg"))

## search duplicate values
x1 <- count(HP, vars=c("Latitude", "Longitude"))
x2 <- count(HSD, vars=c("Latitude", "Longitude"))

sum(duplicated(HP$Latitude))
sum(duplicated(HP$Longitude))

which(duplicated(HP$Latitude))
which(duplicated(HP$Longitude))

## Remove duplicate latitude and longitude from house price & closest distance data
H <- distinct(HP, Latitude, .keep_all= TRUE)
HH <- distinct(H,Longitude, .keep_all= TRUE)

H1 <- distinct(HSD, Latitude, .keep_all= TRUE)
HH1 <- distinct(H1, Longitude, .keep_all= TRUE)

## Subset of house data and shortest distance data
HH_sub <- subset(HH, select = c("Latitude", "Longitude", "Year", "area", "Real_price"))
HH1_sub <- subset(HH1, select = c("Latitude", "Longitude", "Lake_ID", "shortest_distance"))

## Merge house data (HH_sub) and shortest distance data (HH1_sub)
HP_HSD_merge <- merge(HH_sub, HH1_sub,by=c("Latitude", "Longitude"),all=TRUE)
HP_HSD_merge<- na.omit(HP_HSD_merge)

## Merge house data (HP_HSD_merge) and lake location (F_wq_data_1)
HP_HSD_merge_1 <- merge(HP_HSD_merge, F_wq_data_1,by=c("Lake_ID"),all=TRUE)
HP_HSD_merge_1_omit<- na.omit(HP_HSD_merge_1)

## Merge HP_HSD_merge_1_omit with water quality data ()
HP_HSD_merge_2 <- merge(HP_HSD_merge_1_omit, F_wq_data_2, by=c("L_Latitude", "L_Longitude", "Year"),all=TRUE)
Final<- na.omit(HP_HSD_merge_2)








