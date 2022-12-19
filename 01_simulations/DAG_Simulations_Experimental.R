#################### DAG Simulations - Experimental Script #####################

##### Set-Up #####

setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing/02_empirical/frontdoor_criterion/01_simulations")

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(systemfit)
library(KRLS)

set.seed(79)

n <- 1000

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}



##### Simple FDC set-up using KRLS #####

U <- rnorm(n, mean = 0, sd = 1)
Z <- runif(n, min = 0, max = 1)
e_X <- rnorm(n, mean = 0, sd = 1)
e_M <- rnorm(n, mean = 0, sd = 1)
e_Y <- rnorm(n, mean = 0, sd = 1)

X <- 0.5*U + e_X
M <- Z*X + e_M
Y <- 0.5*M + 0.5*U + e_Y

d <- data.frame(cbind(U, X, M, Y))

# kernel-based regularized least squares
# first stage
kmod_1 <- krls(y = d$M, X = d$X)
kmod_1_summary <- summary(kmod_1)
kmod_1_summary
# second stage
kmod_2 <- krls(y = d$Y, X = d[,c('X', 'M')])
kmod_2_summary <- summary(kmod_2)
kmod_2_summary



##### Treatment totally defined by mediator (with unobserved confounder) #####

U <- rbinom(n, size = 1, prob = 0.5)
Z_x <- rbinom(n, size = 1, prob = 0.5)
Z_m <- rbinom(n, size = 1, prob = 0.5)

e_Y <- rnorm(n, mean = 0, sd = 1)

X <- Z_x * U
M <- Z_m * X + X
Y <- 0.5*M + 0.5*U + e_Y

# proof treatment is totally defined by mediator
table(M, X) / n

d <- data.frame(cbind(U, X, M, Y))

# benchmark model
model_bench <- lm(Y ~ X + U, d)
summary(model_bench)

# fdc model
model_fd <- systemfit(list(first = M ~ X, second = Y ~ M + X), method = "SUR", data = d)
model_fd_summary <- summary(model_fd)
model_fd_summary

ate_fd <- as.numeric(model_fd$coefficients["first_X"] * model_fd$coefficients["second_M"])
ate_fd_cov <- model_fd$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(rev(model_fd$coefficients[c("first_X", "second_M")]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0.75): ', round(pval(coef=ate_fd, cv=0.75, se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)



##### Treatment totally defined by mediator (without unobserved confounder) #####

X <- rbinom(n, size = 1, prob = 0.75)
Z_m <- rbinom(n, size = 1, prob = 0.5)

e_Y <- rnorm(n, mean = 0, sd = 1)

M <- Z_m * X + X
Y <- 0.5*M + e_Y

table(M, X) / n

d <- data.frame(cbind(X, M, Y))

# benchmark model
model_bench <- lm(Y ~ X, d)
summary(model_bench)

# fdc model
model_fd <- systemfit(list(first = M ~ X, second = Y ~ M), method = "SUR", data = d)
model_fd_summary <- summary(model_fd)
model_fd_summary

ate_fd <- as.numeric(model_fd$coefficients["first_X"] * model_fd$coefficients["second_M"])
ate_fd_cov <- model_fd$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(rev(model_fd$coefficients[c("first_X", "second_M")]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0.75): ', round(pval(coef=ate_fd, cv=0.75, se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)



##### BDC = FDC? #####
# no arrow from U to X, but arrow from U to Z

b <- 0.5
U <- rnorm(n, mean = 0, sd = 1)

e_X <- rnorm(n, mean = 0, sd = 1)
e_Z <- rnorm(n, mean = 0, sd = 1)
e_M <- rnorm(n, mean = 0, sd = 1)
e_Y <- rnorm(n, mean = 0, sd = 1)

Z <- b*U + e_Z
X <- b*Z + e_X
M <- b*X + b*Z + e_M
Y <- b*M + b*Z + b*U + e_Y

d <- data.frame(cbind(X, M, Y))

# benchmark model
model_bench <- lm(Y ~ X + Z + U, d)
summary(model_bench)

# benchmark model
model_bd <- lm(Y ~ X + Z, d)
summary(model_bd)

# fdc model
model_fd <- systemfit(list(first = M ~ X + Z, second = Y ~ M + Z), method = "SUR", data = d)
model_fd_summary <- summary(model_fd)
model_fd_summary

ate_fd <- as.numeric(model_fd$coefficients["first_X"] * model_fd$coefficients["second_M"])
ate_fd_cov <- model_fd$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(rev(model_fd$coefficients[c("first_X", "second_M")]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0.25): ', round(pval(coef=ate_fd, cv=0.25, se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)






