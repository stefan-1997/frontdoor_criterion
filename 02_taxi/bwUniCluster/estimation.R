######################## Uber and Lyft Rides in Chicago ########################
################################## Estimation ##################################

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(systemfit)

df <- readr::read_csv("data/chicagoRidesCleaned.csv")


# df <- df %>%
#   sample_n(890000)


##### Descriptives #####

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
  print(width=Inf)

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
    n = n(),
    share = n()/n_sample_sharing
  ) %>%
  print(width=Inf)



##### Seemingly unrelated regressions #####

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
f1 <- formula(shared_trip ~ shared_trip_authorized + fare + as.factor(trip_start_date) + as.factor(trip_hour) + as.factor(trip_weekday_hour) + as.factor(origin_destination_pairs))
# second stage
f2 <- formula(tip_dummy ~ shared_trip + shared_trip_authorized + fare + as.factor(trip_start_date) + as.factor(trip_hour) + as.factor(trip_weekday_hour) + as.factor(origin_destination_pairs))
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
