######################## Uber and Lyft Rides in Chicago ########################

##### Set-Up #####

setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing/02_empirical/frontdoor_criterion/02_taxi")

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

# get data
df <- read.socrata(
  "https://data.cityofchicago.org/resource/m6dm-c72p.json?$where=trip_start_timestamp between '2019-06-30T22:00:00' and '2019-07-01T04:00:00' and fare >= 2.5 and fare <= 50 and pickup_community_area is not null and dropoff_community_area is not null and trip_seconds is not null and trip_miles is not null and pickup_census_tract is not null and dropoff_census_tract is not null",
  app_token = app_t,
  email = email,
  password = passw
)




##### Data preprocessing and storage #####

# delete unnecessary columns
df <- df %>%
  select(trip_id, trip_start_timestamp, trip_end_timestamp, trip_seconds, trip_miles, pickup_community_area, dropoff_community_area, fare, tip, trip_total, shared_trip_authorized, trips_pooled)

# check missing values
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count

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

# two hour time slot
# get date
df$trip_start_date <- as.Date(df$trip_start_timestamp, tz = "")
# get hour
df$trip_hour <- as.numeric(format(df$trip_start_timestamp, "%H"))
df$trip_two_hour_bracket <- floor(df$trip_hour / 2)
df$two_hour_time_slot <- do.call(paste, c(df[c("trip_start_date", "trip_two_hour_bracket")], sep="-"))


# get weekday (monday=1, sunday=7)
df$trip_weekday <- wday(df$trip_start_date, week_start = 1)




# write to csv
write.csv(df, "chicagoRides.csv", row.names = FALSE)

##### Descriptives and visualisation #####

# get summary statistics
summary(df)

# scatterplot
unique(df[c("tip", "tip_asinh")]) %>%
  ggplot(mapping = aes(x=tip, y=tip_asinh)) +
  geom_point(shape=19, color="#953807", alpha=1) +
  labs(x="Tip", y="Inverse Hyperbolic Sine of Tip",
       title="Inverse Hyperbolic Since Transformation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

ggsave("asinh_tip_scatterplot.png", plot=last_plot())

# histogram level
df %>%
  ggplot(mapping = aes(x=tip)) +
  geom_histogram(position="identity", fill="#953807", alpha=0.7, bins=30) +
  labs(x="Tip", y="Frequency",
       title="Histogram - Tip") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

# histogram asinh
df %>%
  ggplot(mapping = aes(x=tip_asinh)) +
  geom_histogram(position="identity", fill="#953807", alpha=0.7, bins=30) +
  labs(x="Inverse Hyperbolic Sine of Tip", y="Frequency",
       title="Histogram - Inverse Hyperbolic Sine of Tip") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))


# descriptives full sample
n_sample <- nrow(df)
df %>%
  group_by(shared_trip_authorized) %>%
  summarise(
    fare_mean = mean(fare),
    fare_sd = sd(fare),
    tip_mean = mean(tip),
    tip_sd = sd(tip),
    tip_dummy_mean = mean(tip_dummy),
    tip_dummy_sd = sd(tip_dummy),
    n = n(),,
    share = n()/n_sample
  ) %>%
  print()

n_sample_sharing <- sum(df$shared_trip_authorized == 1)
df %>%
  filter(shared_trip_authorized == 1) %>%
  group_by(shared_trip) %>%
  summarise(
    fare_mean = mean(fare),
    fare_sd = sd(fare),
    tip_mean = mean(tip),
    tip_sd = sd(tip),
    tip_dummy_mean = mean(tip_dummy),
    tip_dummy_sd = sd(tip_dummy),
    n = n(),,
    share = n()/n_sample_sharing
  ) %>%
  print()


##### Causal identification through the front door #####

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}

# first stage
f1 <- formula(shared_trip ~ shared_trip_authorized + fare + as.factor(trip_start_date) + as.factor(trip_hour) + as.factor(origin_destination_pairs))
f2 <- formula(tip_dummy ~ shared_trip + shared_trip_authorized + fare + as.factor(trip_start_date) + as.factor(trip_hour) + as.factor(origin_destination_pairs))
model_input <- list(first = f1, second = f2)

model_fd <- systemfit(model_input, method = "SUR", data = df)
model_fd_summary <- summary(model_fd)

model_fd_summary$coefficients[c("first_(Intercept)", "first_shared_trip_authorized", "first_fare"),]
model_fd_summary[["eq"]][[1]][["r.squared"]]
model_fd_summary[["eq"]][[1]][["adj.r.squared"]]
model_fd_summary$coefficients[c("second_(Intercept)", "second_shared_trip", "second_shared_trip_authorized", "first_fare"),]
model_fd_summary[["eq"]][[2]][["r.squared"]]
model_fd_summary[["eq"]][[2]][["adj.r.squared"]]
model_fd_summary$ols.r.squared

model_fd_summary


# computation of ATE by multiplying coefficients
ate_fd <- as.numeric(model_fd$coefficients["first_shared_trip_authorized"] * model_fd$coefficients["second_shared_trip"])
# get variance-covariance matrix of coefficients
ate_fd_cov <- model_fd$coefCov[c("first_shared_trip_authorized", "second_shared_trip"), c("first_shared_trip_authorized", "second_shared_trip")]
# get jacobian for Delta-method
jacobian <- as.matrix(rev(model_fd$coefficients[c("first_shared_trip_authorized", "second_shared_trip")]))
# apply Delta-method to compute ATE standard error
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0): ', round(pval(coef=ate_fd, cv=0, se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)



