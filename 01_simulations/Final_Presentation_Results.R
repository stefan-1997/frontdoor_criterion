options(scipen=0)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(systemfit)

set.seed(43)

n <- 100000

pval <- function(coef, cv, se, df){
  t <- (coef-cv) / se
  p <- 2 * pt(abs(t), n, lower=FALSE)
  return(p)
}

sigmoid <- function(x){
  p <- 1/(1 + exp(-x))
  return(p)
}


# unobserved confounder
U <- rnorm(n, mean=0, sd=1)
# observed confounder
Z <- rnorm(n, mean=0, sd=1)

e_X <- rnorm(n, mean=0, sd=1)
e_M <- rnorm(n, mean=0, sd=1)
e_Y <- rnorm(n, mean=0, sd=1)

# fixed effect size
b <- 0.25

X <- 1*(b*U + b*Z + b*e_X > 0)
M <- 1*(b*Z + X + b*e_M > 0.8)
Y <- b*Z + b*U + 0.5*M + e_Y

table(X, M) / n


model_bench <- lm(Y ~ X + U + Z)
model_bench_summary <- summary(model_bench)
model_bench_summary


model_naive <- lm(Y ~ X + Z)
model_naive_summary <- summary(model_naive, vcov = "hetero")
model_naive_summary

model_fd <- systemfit(list(first = M ~ X + Z, second = Y ~ M + X + Z), method = "SUR")
model_fd_summary <- summary(model_fd, vcov = "hetero")
model_fd_summary

ate_fd <- as.numeric(model_fd$coefficients["first_X"] * model_fd$coefficients["second_M"])
ate_fd_cov <- model_fd$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(rev(model_fd$coefficients[c("first_X", "second_M")]))
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE:       ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value: ', round(pval(coef=ate_fd, cv=model_bench_summary$coefficients["X",1], se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)
