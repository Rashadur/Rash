####Merge lake size data with WQ data

library(tidyverse)
library(dplyr)
library(data.table)
library(rio)

## Import data
## Import wq data
wq <-read.csv("final data/wq/Final_wq_data_1.csv", header = T)

##Import lake size data (Lake_size)
Lake_size <-read.csv("data to manage/lake size/Lake_ID.csv", header = T)


##lake names in lower case for WQ data
h <- data.frame(Lake_Name=tolower(wq$Lake.Name))
wq_low <- cbind(wq,h)
wq_low_final<-subset(wq_low,select = c("Lake_ID", "STN", "Site.ID", "Lake_Name","Latitude", "Longitude", "meanTP_yearly", "Secchi.depth_avg"))

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
