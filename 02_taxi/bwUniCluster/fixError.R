######################## Uber and Lyft Rides in Chicago ########################
############################### Data preparation ###############################

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(lubridate)

##### Read in data #####

df <- readr::read_csv("data/chicagoRidesCleaned.csv")

summary(df)

df <- df %>%
  rename(trip_weekday_hour = two_hour_time_slot)

summary(df)


write.csv(df, "data/chicagoRidesCleaned.csv", row.names = FALSE)
