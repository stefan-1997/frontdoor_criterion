######################## Uber and Lyft Rides in Chicago ########################
################################## Estimation ##################################

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)

df <- readr::read_csv("data/chicagoRidesCleaned.csv")


df <- df %>%
  sample_n(13111)


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

df$trip_start_date <- as.factor(df$trip_start_date)
df$trip_hour <- as.factor(df$trip_hour)
df$trip_weekday_hour <- as.factor(df$trip_weekday_hour)
df$origin_destination_pairs <- as.factor(df$origin_destination_pairs)


# first stage
f1 <- formula(shared_trip ~ shared_trip_authorized + fare + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)
# second stage
f2 <- formula(tip_dummy ~ shared_trip + shared_trip_authorized + fare + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)


# define chunk size
n <- 1000
k <- ceiling(n_sample/n)

# first model fit
ch <- df[1:n,]
model_f1 <- biglm::biglm(f1, ch)
model_f2 <- biglm::biglm(f2, ch)
rm(ch); invisible(gc())

# subsequent model fits
for (i in 2:(k-1)){
  
  ch <- df[((i-1)*n+1):(i*n),]
  model_f1 <- update(model_f1, ch)
  model_f2 <- update(model_f2, ch)
  
  rm(ch); invisible(gc())
  
}

# final model fit
ch <- df[((k-1)*n+1):n_sample,]
model_f1 <- update(model_f1, ch)
model_f2 <- update(model_f2, ch)

rm(ch); invisible(gc())


# model_f1 <- biglm::biglm(f1, df)
model_f1_summary <- summary(model_f1)
se_f1 <- sqrt(diag(vcov(model_f1)))

# model_f2 <- biglm::biglm(f2, df)
model_f2_summary <- summary(model_f2)
se_f2 <- sqrt(diag(vcov(model_f2)))


coef(model_f1)[c("(Intercept)", "shared_trip_authorized", "fare")]
se_f1[c("(Intercept)", "shared_trip_authorized", "fare")]
model_f1_summary$rsq
coef(model_f2)[c("(Intercept)", "shared_trip", "shared_trip_authorized", "fare")]
model_f2_summary$rsq
se_f2[c("(Intercept)", "shared_trip", "shared_trip_authorized", "fare")]


# computation of ATE by multiplying coefficients
ate_fd <- as.numeric(coef(model_f1)["shared_trip_authorized"] * coef(model_f2)["shared_trip"])
# get variance-covariance matrix of coefficients (covariance=0)
ate_fd_cov <- matrix(c(se_f1["shared_trip_authorized"]^2, 0, 0, se_f2["shared_trip"]^2), nrow=2)
# get jacobian for Delta-method
jacobian <- as.matrix(c(coef(model_f2)["shared_trip"], coef(model_f1)["shared_trip_authorized"]))
# apply Delta-method to compute ATE standard error
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)
# degrees of freedom
d_free <- n_sample*2 - length(model_f1_summary[["obj"]][["names"]]) - length(model_f2_summary[["obj"]][["names"]])

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0): ', round(pval(coef=ate_fd, cv=0, se=ate_fd_se, df=d_free), 4)
)





