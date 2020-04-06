####Shortest distance calculation between lake and house location

##Calculate shortest distance between house and lake station
install.packages(geosphere)
library(geosphere)
install.packages("data.table")
library(data.table)

##Import lake wq data (Final_wq_data_1) and housing data with real price (clean_HPI_HD_real_price)

v_lake <- read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/wq/Final_wq_data_1.csv", header = T)

##I insert a new column "ID" in "clean_HPI_HD_real_price" and then import
w_house <- read.csv("C:/Users/rar727/OneDrive - University of Saskatchewan/rash/final data/HPI/clean_HPI_HD_real_price.csv", header = T)


## create two different subset from v_lake & w_house data (one for house coordinates(w) & one for lake coordinates(v))

v <-subset(v_lake,select = c("Lake_ID","Latitude", "Longitude"))

w <-subset(w_house,select = c("ID","Latitude", "Longitude"))


library(data.table)
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
install.packages("rio")
library("rio")
export(Final_shortest_dist, "Final_shortest_dist.csv")

