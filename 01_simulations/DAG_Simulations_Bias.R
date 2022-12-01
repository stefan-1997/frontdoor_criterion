####################### DAG Simulations - Bias formulas ########################



##### Set-Up #####

setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing")

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(systemfit)
library(MASS)
library(latex2exp)

set.seed(79)

n <- 50000

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}



##### DGP #####

Z <- rnorm(n, mean = 0, sd = 1)
U <- rnorm(n, mean = 0, sd = 1)
V <- rnorm(n, mean = 0, sd = 1)

e_X <- rnorm(n, mean = 0, sd = 1)
e_M <- rnorm(n, mean = 0, sd = 1)
e_Y <- rnorm(n, mean = 0, sd = 1)

X <- 0.5*U + 0.25*Z + e_X
M <- 0.5*X + 0.2*Z + 0.4*V + e_M
Y <- 0.5*M + 0.3*Z + 0.5*U + 0.4*V + e_Y

d <- data.frame(cbind(X, M, Y, Z, U, V))



##### Benchmark #####

# benchmark regression (incl. unobserved & observed confounders)
model_bench <- lm(Y ~ X + Z + U + V, d)
model_bench_summary <- summary(model_bench)
model_bench_summary



##### Naive #####

# naive regression (excl. unobserved confounder  V, incl. unobserved confounder  U & incl. observed confounder)
model_naive <- lm(Y ~ X + Z + V, d)
model_naive_summary <- summary(model_naive)
model_naive_summary



##### FDC #####

model_fd <- systemfit(list(first = M ~ X + Z, second = Y ~ M + X + Z), method = "SUR", data = d)
model_fd_summary <- summary(model_fd)
model_fd_summary

ate_fd <- as.numeric(model_fd$coefficients["first_X"] * model_fd$coefficients["second_M"])
ate_fd_cov <- model_fd$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(model_fd$coefficients[c("second_M", "first_X")])
# apply Delta-method to compute ATE standard error
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0.25): ', round(pval(coef=ate_fd, cv=0.25, se=ate_fd_se, df=model_fd_summary$df[2]), 4)
)



##### Bias simulation #####

taus <- seq(0, 2, 0.025)
l_tau <- length(taus)
X_mu <- rep(0, 10)
n <- 5000
replications <- 100
models <- c("benchmark", "naive", "fdc")
l_models <- length(models)

collection <- data.frame(
  model = factor(rep(models, l_tau), levels = models),
  tau = NA,
  effect = NA
)

for (tau in 1:l_tau){
  
  mat_rep <- matrix(NA, ncol = l_models, nrow = replications)
  
  for (r in 1:replications){
    
    Z <- rnorm(n, mean = 0, sd = 1)
    U <- rnorm(n, mean = 0, sd = 1)
    V <- rnorm(n, mean = 0, sd = 1)
    
    e_X <- rnorm(n, mean = 0, sd = 1)
    e_M <- rnorm(n, mean = 0, sd = 1)
    e_Y <- rnorm(n, mean = 0, sd = 1)
    
    X <- 0.5*Z + 0.5*U + e_X
    M <- 0.5*X + 0.5*Z + taus[tau]*V + e_M
    Y <- 0.5*M + 0.5*Z + 0.5*U + 0.5*V + e_Y
    
    d <- data.frame(cbind(X, M, Y, Z, U, V))
    
    bench <- lm(Y ~ X + Z + U + V, d)$coefficients['X']
    naive <- lm(Y ~ X + Z, d)$coefficients['X']
    fdc_model <- systemfit(list(first = M ~ X + Z, second = Y ~ M + X + Z), method = "SUR", data = d)
    fdc <- as.numeric(fdc_model$coefficients["first_X"] * fdc_model$coefficients["second_M"])
    
    mat_rep[r,] <- c(bench, naive, fdc)
    
  }
  
  ind <- (tau*l_models - l_models + 1):(tau*l_models)
  
  collection[ind, 2] <- taus[tau]
  collection[ind, 3] <- colMeans(mat_rep)
  
}

collection <- collection %>%
  mutate(effect = round(effect, 4))

ggplot(data = collection, aes(x = tau, y = effect, colour = model)) +
  geom_point(shape = 16) +
  labs(
    x = TeX(r'($\tau$)'),
    y = TeX(r'($\hat{ATE}$)'),
    title = "ATEs for confounded mediator",
    caption = "Based on simulated data",
    color='Model'
    ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))




