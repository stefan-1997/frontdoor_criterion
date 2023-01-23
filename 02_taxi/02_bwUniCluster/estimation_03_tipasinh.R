######################## Uber and Lyft Rides in Chicago ########################
################################## Estimation ##################################

options(scipen=999)

rm(list=ls())
invisible(gc())


library(tidyverse)
library(fixest)

set.seed(79)

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}


df <- readr::read_csv("data/chicagoRidesCleaned.csv")



### definition of criteria ###

# define chunk size
n_sample <- 15000000

df <- df %>%
  sample_n(n_sample)

n <- 500000
k <- ceiling(n_sample/n)



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
f1 <- formula(shared_trip ~ shared_trip_authorized + fare | trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)
# second stage
f2 <- formula(tip_asinh ~ shared_trip + shared_trip_authorized +  fare | trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)

# naive
f3 <- formula(tip_asinh ~ shared_trip_authorized + fare | trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)



### initial model fit ###

ch <- df[1:n,]

model_f1 <- fixest::feols(fml = f1, data = ch)
model_f1_summary <- summary(model_f1, vcov = "iid")

model_f2 <- fixest::feols(fml = f2, data = ch)
model_f2_summary <- summary(model_f2, vcov = "iid")

model_naive <- fixest::feols(fml = f3, data = ch)
model_naive_summary <- summary(model_naive, vcov = "iid")

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


collection <- data.frame(
  n = model_f1$nobs,
  df_fd = d_free_fd,
  df_naive = d_free_naive,
  f1_shared_trip_authorized = coef(model_f1)["shared_trip_authorized"],
  f1_shared_trip_authorized_se = model_f1_summary$se["shared_trip_authorized"],
  f1_fare = coef(model_f1)["fare"],
  f1_fare_se = model_f1_summary$se["fare"],
  f2_shared_trip_authorized = coef(model_f2)["shared_trip_authorized"],
  f2_shared_trip_authorized_se = model_f2_summary$se["shared_trip_authorized"],
  f2_shared_trip = coef(model_f2)["shared_trip"],
  f2_shared_trip_se = model_f2_summary$se["shared_trip"],
  f2_fare = coef(model_f2)["fare"],
  f2_fare_se = model_f2_summary$se["fare"],
  f1_r2 = 1 - (sum(model_f1$residuals^2) / sum((ch$shared_trip - mean(ch$shared_trip))^2)),
  f2_r2 = 1 - (sum(model_f2$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
  naive_r2 = 1 - (sum(model_naive$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
  ate_fd = ate_fd,
  ate_fd_se = ate_fd_se,
  ate_fd_p = ate_fd_p,
  ate_naive = ate_naive,
  ate_naive_se = ate_naive_se,
  ate_naive_p = ate_naive_p
)



### subsequent model fits ###

for (i in 2:(k-1)){
  
  ch <- df[1:(i*n),]
  
  model_f1 <- fixest::feols(fml = f1, data = ch)
  model_f1_summary <- summary(model_f1, vcov = "iid")
  
  model_f2 <- fixest::feols(fml = f2, data = ch)
  model_f2_summary <- summary(model_f2, vcov = "iid")
  
  model_naive <- fixest::feols(fml = f3, data = ch)
  model_naive_summary <- summary(model_naive, vcov = "iid")
  
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
  
  new_collection <- c(
    model_f1$nobs,
    d_free_fd,
    d_free_naive,
    coef(model_f1)["shared_trip_authorized"],
    model_f1_summary$se["shared_trip_authorized"],
    coef(model_f1)["fare"],
    model_f1_summary$se["fare"],
    coef(model_f2)["shared_trip_authorized"],
    model_f2_summary$se["shared_trip_authorized"],
    coef(model_f2)["shared_trip"],
    model_f2_summary$se["shared_trip"],
    coef(model_f2)["fare"],
    model_f2_summary$se["fare"],
    1 - (sum(model_f1$residuals^2) / sum((ch$shared_trip - mean(ch$shared_trip))^2)),
    1 - (sum(model_f2$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
    1 - (sum(model_naive$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
    ate_fd,
    ate_fd_se,
    ate_fd_p,
    ate_naive,
    ate_naive_se,
    ate_naive_p
  )
  
  collection <- rbind(collection, new_collection)
  
  rm(ch); invisible(gc())

  write.csv(collection, "data/estimationResultsBaseTipAsinh.csv", row.names = FALSE)

  print(model_f1$nobs)
  
}



### final model fit ###

ch <- df

model_f1 <- fixest::feols(fml = f1, data = ch)
model_f1_summary <- summary(model_f1, vcov = "iid")

model_f2 <- fixest::feols(fml = f2, data = ch)
model_f2_summary <- summary(model_f2, vcov = "iid")

model_naive <- fixest::feols(fml = f3, data = ch)
model_naive_summary <- summary(model_naive, vcov = "iid")

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

new_collection <- c(
  model_f1$nobs,
  d_free_fd,
  d_free_naive,
  coef(model_f1)["shared_trip_authorized"],
  model_f1_summary$se["shared_trip_authorized"],
  coef(model_f1)["fare"],
  model_f1_summary$se["fare"],
  coef(model_f2)["shared_trip_authorized"],
  model_f2_summary$se["shared_trip_authorized"],
  coef(model_f2)["shared_trip"],
  model_f2_summary$se["shared_trip"],
  coef(model_f2)["fare"],
  model_f2_summary$se["fare"],
  1 - (sum(model_f1$residuals^2) / sum((ch$shared_trip - mean(ch$shared_trip))^2)),
  1 - (sum(model_f2$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
  1 - (sum(model_naive$residuals^2) / sum((ch$tip_dummy - mean(ch$tip_dummy))^2)),
  ate_fd,
  ate_fd_se,
  ate_fd_p,
  ate_naive,
  ate_naive_se,
  ate_naive_p
)

collection <- rbind(collection, new_collection)

rm(ch); invisible(gc())

write.csv(collection, "data/estimationResultsBaseTipAsinh.csv", row.names = FALSE)


