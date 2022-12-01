######################## Uber and Lyft Rides in Chicago ########################
############################### Data preparation ###############################

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(lubridate)

##### Read in data #####

df <- readr::read_csv("data/chicagoRidesFullSample.csv")


##### Data cleaning #####

### convert some variables to numeric variables
df <- df %>%
  mutate(
    trip_seconds = as.numeric(trip_seconds),
    trip_miles = as.numeric(trip_miles),
    fare = as.numeric(fare),
    tip = as.numeric(tip),
    trip_total = as.numeric(trip_total),
    trips_pooled = as.numeric(trips_pooled)
  )


### define variables of interest

# treatment variable X (sharing authorized)
df$shared_trip_authorized <- ifelse(df$shared_trip_authorized == "FALSE", 0, 1)

# mediator variable M (ride actually shared)
df$shared_trip <- ifelse(df$trips_pooled > 1, 1, 0)

# tip as share of fare
df$tip_share <- df$tip / df$fare

# tip dummy (extensive margin)
df$tip_dummy <- ifelse(df$tip > 0, 1, 0)

# inverse hyperbolic sine transformation of tip (intensive margin)
df$tip_asinh <- asinh(df$tip)

# inverse hyperbolic sine transformation of tip as share of fare (intensive margin)
df$tip_share_asinh <- asinh(df$tip_share)


### define control variables

# origin–destination community pairs
pickup <- unique(df[c("pickup_community_area")])$pickup_community_area
dropoff <- unique(df[c("dropoff_community_area")])$dropoff_community_area
setdiff(pickup, dropoff)
setdiff(dropoff, pickup)

df$origin_destination_pairs <- do.call(paste, c(df[c("pickup_community_area", "dropoff_community_area")], sep="-"))

# date
df$trip_start_date <- as.Date(df$trip_start_timestamp, tz = "")
# hour
df$trip_hour <- as.numeric(format(df$trip_start_timestamp, "%H"))
# weekday
df$trip_weekday <- wday(df$trip_start_date, week_start = 1)
# weekday-hour
df$trip_weekday_hour <- do.call(paste, c(df[c("trip_weekday", "trip_hour")], sep="-"))



# write to csv
write.csv(df, "data/chicagoRidesCleaned.csv", row.names = FALSE)




# # two hour time slot
# df$trip_two_hour_bracket <- floor(df$trip_hour / 2)
# df$two_hour_time_slot <- do.call(paste, c(df[c("trip_start_date", "trip_two_hour_bracket")], sep="-"))
