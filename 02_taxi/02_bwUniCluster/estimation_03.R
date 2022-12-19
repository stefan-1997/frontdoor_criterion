######################## Uber and Lyft Rides in Chicago ########################
################################## Estimation ##################################

options(scipen=999)

rm(list=ls())
invisible(gc())


library(tidyverse)
library(biglm)

df <- readr::read_csv("data/chicagoRidesCleaned.csv")

df <- df %>%
  sample_n(3500000)

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}



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



##### Estimation of ATT #####


### data preparation ###

df$trip_start_date <- as.factor(df$trip_start_date)
df$trip_hour <- as.factor(df$trip_hour)
df$trip_weekday_hour <- as.factor(df$trip_weekday_hour)
df$origin_destination_pairs <- as.factor(df$origin_destination_pairs)



### formulas ###

# first stage
f1 <- formula(shared_trip ~ shared_trip_authorized + fare + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)
# f1 <- formula(shared_trip ~ shared_trip_authorized + fare + trip_start_date + trip_hour + origin_destination_pairs)
# second stage
f2 <- formula(tip_dummy ~ shared_trip + shared_trip_authorized + fare + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)
# f2 <- formula(tip_dummy ~ shared_trip + shared_trip_authorized + fare + trip_start_date + trip_hour + origin_destination_pairs)

# naive
f3 <- formula(tip_dummy ~ shared_trip_authorized + fare + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs)
# f3 <- formula(tip_dummy ~ shared_trip_authorized + fare + trip_start_date + trip_hour + origin_destination_pairs)



### definition of criteria ###

# define chunk size
n <- 110000
k <- ceiling(n_sample/n)



### initial model fit ###

ch <- df[1:n,]
model_f1 <- biglm::biglm(f1, ch)
model_f2 <- biglm::biglm(f2, ch)

model_naive <- biglm::biglm(f3, ch)

rm(ch); invisible(gc())

model_f1_summary <- summary(model_f1)
se_f1 <- sqrt(diag(vcov(model_f1)))

model_f2_summary <- summary(model_f2)
se_f2 <- sqrt(diag(vcov(model_f2)))

model_naive_summary <- summary(model_naive)
se_naive <- sqrt(diag(vcov(model_naive)))


ate_fd <- as.numeric(coef(model_f1)["shared_trip_authorized"] * coef(model_f2)["shared_trip"])
ate_fd_cov <- matrix(c(se_f1["shared_trip_authorized"]^2, 0, 0, se_f2["shared_trip"]^2), nrow=2)
jacobian <- as.matrix(c(coef(model_f2)["shared_trip"], coef(model_f1)["shared_trip_authorized"]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

d_free_fd <- model_f1[["n"]]*2 - length(model_f1_summary[["obj"]][["names"]]) - length(model_f2_summary[["obj"]][["names"]])
ate_fd_p <- pval(coef=ate_fd, cv=0, se=ate_fd_se, df=d_free_fd)

ate_naive <- coef(model_naive)["shared_trip_authorized"]
ate_naive_se <- se_naive["shared_trip_authorized"]
d_free_naive <- model_naive[["n"]] - length(model_naive_summary[["obj"]][["names"]])
ate_naive_p <- pval(coef=ate_naive, cv=0, se=ate_naive_se, df=d_free_naive)


collection <- data.frame(
  n = model_f1[["n"]],
  df_fd = d_free_fd,
  df_naive = d_free_naive,
  f1_shared_trip_authorized = coef(model_f1)["shared_trip_authorized"],
  f1_shared_trip_authorized_se = se_f1["shared_trip_authorized"],
  f1_fare = coef(model_f1)["fare"],
  f1_fare_se = se_f1["fare"],
  f2_shared_trip_authorized = coef(model_f2)["shared_trip_authorized"],
  f2_shared_trip_authorized_se = se_f2["shared_trip_authorized"],
  f2_shared_trip = coef(model_f2)["shared_trip"],
  f2_shared_trip_se = se_f2["shared_trip"],
  f2_fare = coef(model_f2)["fare"],
  f2_fare_se = se_f2["fare"],
  f1_r2 = model_f1_summary$rsq,
  f2_r2 = model_f2_summary$rsq,
  naive_r2 = model_naive_summary$rsq,
  ate_fd = ate_fd,
  ate_fd_se = ate_fd_se,
  ate_fd_p = ate_fd_p,
  ate_naive = ate_naive,
  ate_naive_se = ate_naive_se,
  ate_naive_p = ate_naive_p
)



### subsequent model fits ###

for (i in 2:(k-1)){
  
  ch <- df[((i-1)*n+1):(i*n),]
  model_f1 <- update(model_f1, ch)
  model_f2 <- update(model_f2, ch)
  
  model_naive <- update(model_naive, ch)
  
  rm(ch); invisible(gc())
  
  model_f1_summary <- summary(model_f1)
  se_f1 <- sqrt(diag(vcov(model_f1)))
  
  model_f2_summary <- summary(model_f2)
  se_f2 <- sqrt(diag(vcov(model_f2)))
  
  model_naive_summary <- summary(model_naive)
  se_naive <- sqrt(diag(vcov(model_naive)))
  
  
  ate_fd <- as.numeric(coef(model_f1)["shared_trip_authorized"] * coef(model_f2)["shared_trip"])
  ate_fd_cov <- matrix(c(se_f1["shared_trip_authorized"]^2, 0, 0, se_f2["shared_trip"]^2), nrow=2)
  jacobian <- as.matrix(c(coef(model_f2)["shared_trip"], coef(model_f1)["shared_trip_authorized"]))
  ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)
  
  d_free_fd <- model_f1[["n"]]*2 - length(model_f1_summary[["obj"]][["names"]]) - length(model_f2_summary[["obj"]][["names"]])
  ate_fd_p <- pval(coef=ate_fd, cv=0, se=ate_fd_se, df=d_free_fd)
  
  ate_naive <- coef(model_naive)["shared_trip_authorized"]
  ate_naive_se <- se_naive["shared_trip_authorized"]
  d_free_naive <- model_naive[["n"]] - length(model_naive_summary[["obj"]][["names"]])
  ate_naive_p <- pval(coef=ate_naive, cv=0, se=ate_naive_se, df=d_free_naive)
  
  
  new_collection <- c(
    model_f1[["n"]],
    d_free_fd,
    d_free_naive,
    coef(model_f1)["shared_trip_authorized"],
    se_f1["shared_trip_authorized"],
    coef(model_f1)["fare"],
    se_f1["fare"],
    coef(model_f2)["shared_trip_authorized"],
    se_f2["shared_trip_authorized"],
    coef(model_f2)["shared_trip"],
    se_f2["shared_trip"],
    coef(model_f2)["fare"],
    se_f2["fare"],
    model_f1_summary$rsq,
    model_f2_summary$rsq,
    model_naive_summary$rsq,
    ate_fd,
    ate_fd_se,
    ate_fd_p,
    ate_naive,
    ate_naive_se,
    ate_naive_p
  )
  
  collection <- rbind(collection, new_collection)

  write.csv(collection, "data/estimationResultsMajor.csv", row.names = FALSE)

  print(model_f1[["n"]])
  
}



### final model fit ###

ch <- df[((k-1)*n+1):n_sample,]
model_f1 <- update(model_f1, ch)
model_f2 <- update(model_f2, ch)

model_naive <- update(model_naive, ch)

rm(ch); invisible(gc())

model_f1_summary <- summary(model_f1)
se_f1 <- sqrt(diag(vcov(model_f1)))

model_f2_summary <- summary(model_f2)
se_f2 <- sqrt(diag(vcov(model_f2)))

model_naive_summary <- summary(model_naive)
se_naive <- sqrt(diag(vcov(model_naive)))


ate_fd <- as.numeric(coef(model_f1)["shared_trip_authorized"] * coef(model_f2)["shared_trip"])
ate_fd_cov <- matrix(c(se_f1["shared_trip_authorized"]^2, 0, 0, se_f2["shared_trip"]^2), nrow=2)
jacobian <- as.matrix(c(coef(model_f2)["shared_trip"], coef(model_f1)["shared_trip_authorized"]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

d_free_fd <- model_f1[["n"]]*2 - length(model_f1_summary[["obj"]][["names"]]) - length(model_f2_summary[["obj"]][["names"]])
ate_fd_p <- pval(coef=ate_fd, cv=0, se=ate_fd_se, df=d_free_fd)

ate_naive <- coef(model_naive)["shared_trip_authorized"]
ate_naive_se <- se_naive["shared_trip_authorized"]
d_free_naive <- model_naive[["n"]] - length(model_naive_summary[["obj"]][["names"]])
ate_naive_p <- pval(coef=ate_naive, cv=0, se=ate_naive_se, df=d_free_naive)


new_collection <- c(
  model_f1[["n"]],
  d_free_fd,
  d_free_naive,
  coef(model_f1)["shared_trip_authorized"],
  se_f1["shared_trip_authorized"],
  coef(model_f1)["fare"],
  se_f1["fare"],
  coef(model_f2)["shared_trip_authorized"],
  se_f2["shared_trip_authorized"],
  coef(model_f2)["shared_trip"],
  se_f2["shared_trip"],
  coef(model_f2)["fare"],
  se_f2["fare"],
  model_f1_summary$rsq,
  model_f2_summary$rsq,
  model_naive_summary$rsq,
  ate_fd,
  ate_fd_se,
  ate_fd_p,
  ate_naive,
  ate_naive_se,
  ate_naive_p
)

collection <- rbind(collection, new_collection)


write.csv(collection, "data/estimationResultsMajor.csv", row.names = FALSE)








