####Merge lake size data with WQ data

## Import data
## Import wq data
wq <-read.csv("final data/wq/Final_wq_data_1.csv", header = T)

##Import lake size data (Lake_size)
Lake_size <-read.csv("data to manage/lake size/Lake_ID.csv", header = T)


##lake names in lower case for WQ data
h <- data.frame(Lake_Name=tolower(wq$Lake.Name))
wq_low <- cbind(wq,h)
wq_low_final<-subset(wq_low,select = c("Lake_ID", "Lake_Name", "STN","Site.ID","Latitude", "Longitude"))

## Remove apostrophe "s" from "Lake_Name" in wq data
wq_low_final$Lake_Name <- gsub("'", '', wq_low_final$Lake_Name)
P<- wq_low_final

##lake names in lower case for lake size data
h1 <- data.frame(Lake_Name=tolower(Lake_size$Lake.Name))
lake_size_low <- cbind(Lake_size,h1)
lake_size_low_final<-subset(lake_size_low,select = c("L_ID", "Lake_Name", "Latitude", "Longitude", "Lake.Area"))

## Remove apostrophe "s" from "Lake_Name" in lake size data
lake_size_low_final$Lake_Name <- gsub("'", '', lake_size_low_final$Lake_Name)
Q<-lake_size_low_final

## Closest lake 
P<- wq_low_final
Q<-lake_size_low_final

library(data.table)
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
colnames(P)[2] <- "lake.name"


## Join 
library(dplyr)
PQ_join <-merge(x=PQ,y=Q,by="L_ID",all.x=TRUE)
PQ_join_wq <-merge(x=P,y=PQ_join,by= c("Lake_ID", "Latitude", "Longitude"),all.x=TRUE)
Final_lake_size_wq <- merge(x=wq,y=PQ_join_wq,by= c("Lake_ID", "Latitude", "Longitude", "STN", "Site.ID"),all.x=TRUE)
Final_lake_size_wq <-subset(Final_lake_size_wq,select = c("Lake_ID", "lake.name", "Latitude", "Longitude", "STN", "Site.ID", "Lake_Name","L_Latitude", "L_Longitude", "Year","meanTP_yearly", "Secchi.depth_avg", "Lake.Area"))



