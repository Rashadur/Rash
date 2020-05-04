# Calculates average WQ within certain threshold as well as closest WQ station by year

# Cleans environment
rm(list=ls(all=TRUE))

# load packages
library(tidyverse)
library(data.table)
library(spatialrisk)

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
v_lake <- read.csv("final data/wq/Final_wq_data_1.csv", header = T)
w_house <- read.csv("final data/HPI/clean_HPI_HD_real_price.csv", header = T)

df_wq <- v_lake %>%
  select(Year, Latitude, Longitude, meanTP_yearly, Secchi.depth_avg) %>%
  rename(lon = Longitude,
        lat = Latitude)

summary(df_wq)

# Split housing data into lists for use in purrr:map
df_house <- split(w_house, w_house$ID)


df_wq_link <- map_dfr(df_house, CalcWQVariables, 
                      df_wq = df_wq, 
                      dist_metres = dist_metres)

write_csv(df_wq_link, "final data/house data/wq_house_merged.csv")
