######################## Uber and Lyft Rides in Chicago ########################
############################### Data preparation################################

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(RSocrata)
library(lubridate)
library(systemfit)

##### Import authentication #####

source("Chicago_Key.R")


##### API call #####

# create vector to store all URLs for API call
start_date <- mdy("06-30-19")
end_date <- mdy("09-30-19")
n_days <- interval(start_date, end_date) / days(1)
timestamps <- format(start_date + days(0:n_days), format='%Y-%m-%dT00:00:00')
t_len <- length(timestamps)
urls <- paste0(
  "https://data.cityofchicago.org/resource/m6dm-c72p.json?$where=trip_start_timestamp between '",
  timestamps[1:(t_len-1)],
  "' and '",
  timestamps[2:t_len],
  "' and fare >= 2.5 and fare <= 50 and pickup_community_area is not null and dropoff_community_area is not null and trip_seconds is not null and trip_miles is not null and pickup_census_tract is not null and dropoff_census_tract is not null"
  )


# get data for first day
df <- read.socrata(
  url = urls[1],
  app_token = app_t,
  email = email,
  password = passw
)

df <- df %>%
  select(trip_id, trip_start_timestamp, trip_end_timestamp, trip_seconds, trip_miles, pickup_community_area, dropoff_community_area, fare, tip, trip_total, shared_trip_authorized, trips_pooled)

write.csv(df, "data/chicagoRidesFullSample.csv", row.names = FALSE)

chicagoRidesFullSample <- df

print(
  paste0(
    'Current number of rows: ', nrow(chicagoRidesFullSample))
)


rm(df)
invisible(gc())

# append other days
for (i in 2:(t_len-1)) {
  
  df <- read.socrata(
    url = urls[i],
    app_token = app_t,
    email = email,
    password = passw
  )
  
  df <- df %>%
    select(trip_id, trip_start_timestamp, trip_end_timestamp, trip_seconds, trip_miles, pickup_community_area, dropoff_community_area, fare, tip, trip_total, shared_trip_authorized, trips_pooled)
  
  chicagoRidesFullSample <- rbind(chicagoRidesFullSample, df)
  
  rm(df)
  invisible(gc())
  
  print(
    paste0(
      'Current number of rows: ', nrow(chicagoRidesFullSample))
  )
  
  if (i == 20){
    write.csv(df, "data/chicagoRidesFullSample.csv", row.names = FALSE)
  }
  
}


write.csv(chicagoRidesFullSample, "data/chicagoRidesFullSample.csv", row.names = FALSE)
  
  

  
  
  