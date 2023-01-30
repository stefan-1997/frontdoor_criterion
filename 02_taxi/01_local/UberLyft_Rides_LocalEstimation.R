######################## Uber and Lyft Rides in Chicago ########################
################################## Estimation ##################################

setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing/02_empirical/frontdoor_criterion/02_taxi")

options(scipen=999)

rm(list=ls())
invisible(gc())


library(tidyverse)
library(fixest)

set.seed(79)

pval <- function(coef, cv, se, df){
  q = (coef-cv) / se
  p <- pt(q = q, df = df, lower.tail = !(q > 0))*2
  return(p)
}


df <- readr::read_csv("03_data/chicagoRidesCleaned.csv")

n_sample <- 20000
df <- df %>%
  sample_n(n_sample)
n_sample <- nrow(df)



##### Descriptives #####

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



##### Estimation of ATT #####


### data preparation ###

df$trip_start_date <- as.factor(df$trip_start_date)
df$trip_hour <- as.factor(df$trip_hour)
df$trip_weekday_hour <- as.factor(df$trip_weekday_hour)
df$origin_destination_pairs <- as.factor(df$origin_destination_pairs)
df$fare_full <- df$trip_total - df$tip
df$pairs_X_start_date <- as.factor(do.call(paste, c(df[c("origin_destination_pairs", "trip_start_date")], sep="_")))


### formulas ###

# first stage
f1 <- formula(shared_trip ~ shared_trip_authorized + fare_full |
                trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs + pairs_X_start_date)
# second stage
f2 <- formula(tip_asinh ~ shared_trip + shared_trip_authorized +  fare_full |
                trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs + pairs_X_start_date)
# naive
f3 <- formula(tip_asinh ~ shared_trip_authorized + fare_full |
                trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs + pairs_X_start_date)


model_f1 <- fixest::feols(fml = f1, data = df)
model_f1_summary <- summary(model_f1, vcov = "hetero")

model_f2 <- fixest::feols(fml = f2, data = df)
model_f2_summary <- summary(model_f2, vcov = "hetero")

model_naive <- fixest::feols(fml = f3, data = df)
model_naive_summary <- summary(model_naive, vcov = "hetero")

ate_fd <- as.numeric(coef(model_f1)["shared_trip_authorized"] * coef(model_f2)["shared_trip"])
ate_fd_cov <- matrix(c(model_f1_summary$se["shared_trip_authorized"]^2, 0, 0, model_f2_summary$se["shared_trip"]^2), nrow=2)
jacobian <- as.matrix(c(coef(model_f2)["shared_trip"], coef(model_f1)["shared_trip_authorized"]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

d_free_fd <- model_f1$nobs*2 - model_f1$nparams - model_f2$nparams
ate_fd_p <- pval(coef=ate_fd, cv=0, se=ate_fd_se, df=d_free_fd)

ate_naive <- coef(model_naive)["shared_trip_authorized"]
ate_naive_se <- model_naive_summary$se["shared_trip_authorized"]
d_free_naive <- model_naive$nobs - model_naive$nparams
ate_naive_p <- pval(coef=ate_naive, cv=0, se=ate_naive_se, df=d_free_naive)

# elasticity recovery arcsinh–linear specification with dummy variable

# model fitting
model_naive_base <- lm(tip_asinh ~ shared_trip_authorized + fare_full + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs + pairs_X_start_date, data = df)

# procedure A
full_spec <- coef(model_naive_base)
full_spec <- full_spec[which(!is.na(full_spec))]
reduced_spec <- full_spec[which(names(full_spec) != "shared_trip_authorized")]
sinh(sum(full_spec)) / sinh(sum(reduced_spec)) - 1
sinh(abs(sum(full_spec))) / sinh(abs(sum(reduced_spec))) - 1

# procedure B
df_X1 <- df %>%
  mutate(shared_trip_authorized = 1)
df_X0 <- df %>%
  mutate(shared_trip_authorized = 0)

pred_X1 <- predict(model_naive, df_X1)
pred_X0 <- predict(model_naive, df_X0)
mean(pred_X1) / mean(pred_X0) - 1



collection <- data.frame(
  n = model_f1$nobs,
  df_fd = d_free_fd,
  df_naive = d_free_naive,
  f1_shared_trip_authorized = coef(model_f1)["shared_trip_authorized"],
  f1_shared_trip_authorized_se = model_f1_summary$se["shared_trip_authorized"],
  f1_fare = coef(model_f1)["fare_full"],
  f1_fare_se = model_f1_summary$se["fare_full"],
  f2_shared_trip_authorized = coef(model_f2)["shared_trip_authorized"],
  f2_shared_trip_authorized_se = model_f2_summary$se["shared_trip_authorized"],
  f2_shared_trip = coef(model_f2)["shared_trip"],
  f2_shared_trip_se = model_f2_summary$se["shared_trip"],
  f2_fare = coef(model_f2)["fare_full"],
  f2_fare_se = model_f2_summary$se["fare_full"],
  f1_r2 = 1 - (sum(model_f1$residuals^2) / sum((df$shared_trip - mean(df$shared_trip))^2)),
  f2_r2 = 1 - (sum(model_f2$residuals^2) / sum((df$tip_dummy - mean(df$tip_dummy))^2)),
  naive_r2 = 1 - (sum(model_naive$residuals^2) / sum((df$tip_dummy - mean(df$tip_dummy))^2)),
  ate_fd = ate_fd,
  ate_fd_se = ate_fd_se,
  ate_fd_p = ate_fd_p,
  ate_naive = ate_naive,
  ate_naive_se = ate_naive_se,
  ate_naive_p = ate_naive_p
)


