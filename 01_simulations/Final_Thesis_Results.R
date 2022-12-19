setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing/02_empirical/frontdoor_criterion/01_simulations")

options(scipen=999)

rm(list=ls())
invisible(gc())

library(tidyverse)
library(latex2exp)
library(systemfit)

pval <- function(coef, cv, se, df){
  
  q = (coef-cv) / se
  
  p <- pt(
    q = q,
    df = df,
    lower.tail = !(q > 0)
  )*2
  
  return(p)
  
}

set.seed(79)

n <- 100000
b <- 0.5



### correlation of Z and U ###

U <- rnorm(n, mean = 0, sd = 1)

e_X <- rnorm(n, mean = 0, sd = 1)
e_Z <- rnorm(n, mean = 0, sd = 1)
e_M <- rnorm(n, mean = 0, sd = 1)
e_Y <- rnorm(n, mean = 0, sd = 1)

Z <- b*U + e_Z
X <- b*Z + b*U + e_X
M <- b*X + b*Z + e_M
Y <- b*M + b*Z + b*U + e_Y

d <- data.frame(cbind(X, Z, M, Y, U))

bench <- lm(Y ~ X + Z + U, d)
summary(bench)
naive <- lm(Y ~ X + Z, d)
summary(naive)
fdc_model <- systemfit(list(first = M ~ X + Z, second = Y ~ M + X + Z), method = "SUR", data = d)
fdc_model_summary <- summary(fdc_model)
fdc_model_summary

ate_fd <- as.numeric(fdc_model$coefficients["first_X"] * fdc_model$coefficients["second_M"])
ate_fd_cov <- fdc_model$coefCov[c("first_X", "second_M"), c("first_X", "second_M")]
jacobian <- as.matrix(fdc_model$coefficients[c("second_M", "first_X")])
# apply Delta-method to compute ATE standard error
ate_fd_se <- sqrt(t(jacobian) %*% ate_fd_cov %*% jacobian)

cat(
  'ATE: ', round(ate_fd, 4),
  '\nse(ATE): ', round(ate_fd_se, 4),
  '\np-value(ATE = 0.25): ', round(pval(coef=ate_fd, cv=0.25, se=ate_fd_se, df=fdc_model_summary$df[2]), 4)
)



### unobserved confounder V of M and Y ###

n <- 10000
replications <- 25

phis <- seq(-1.5, 1.5, 0.025)
len_phis <- length(phis)

V_confounds_M <- data.frame(
  iter = seq(len_phis),
  phi = NA,
  ate_bench = NA,
  ate_naive = NA,
  ate_fdc = NA,
  first_X = NA,
  second_M = NA,
  effect_MV = NA,
  effect_XU = NA,
  effect_MY = NA,
  effect_UY = NA,
  effect_VY = NA
)

for (p in 1:len_phis){
  
  vec_ate_bench <- rep(NA, replications)
  vec_ate_naive <- rep(NA, replications)
  vec_ate_fdc <- rep(NA, replications)
  vec_first_X <- rep(NA, replications)
  vec_second_M <- rep(NA, replications)
  vec_effect_MV <- rep(NA, replications)
  vec_effect_XU <- rep(NA, replications)
  vec_effect_MY <- rep(NA, replications)
  vec_effect_UY <- rep(NA, replications)
  vec_effect_VY <- rep(NA, replications)
  
  for (r in 1:replications){
    
    U <- rnorm(n, mean = 0, sd = 1)
    V <- rnorm(n, mean = 0, sd = 1)
    
    e_X <- rnorm(n, mean = 0, sd = 1)
    e_Z <- rnorm(n, mean = 0, sd = 1)
    e_M <- rnorm(n, mean = 0, sd = 1)
    e_Y <- rnorm(n, mean = 0, sd = 1)
    
    Z <- b*U + e_Z
    X <- b*Z + b*U + e_X
    M <- b*X + b*Z + phis[p]*V + e_M
    Y <- b*M + b*Z + b*U + b*V + e_Y
    
    d <- data.frame(cbind(X, Z, M, Y, U, V))
    
    bench <- lm(Y ~ X + Z + U + V, d)
    naive <- lm(Y ~ X + Z, d)
    fdc_model <- systemfit(list(first = M ~ X + Z, second = Y ~ M + X + Z), method = "SUR", data = d)
    
    ate <- as.numeric(fdc_model$coefficients["first_X"] * fdc_model$coefficients["second_M"])
    
    model_second <- lm(Y ~ M + Z + U + V, d)

    vec_ate_bench[r] <- bench$coefficients['X']
    vec_ate_naive[r] <- naive$coefficients['X']
    vec_ate_fdc[r] <- ate
    vec_first_X[r] <- fdc_model$coefficients["first_X"]
    vec_second_M[r] <- fdc_model$coefficients["second_M"]
    vec_effect_MV[r] <- lm(V ~ M + Z + X, d)$coefficients["M"]
    vec_effect_XU[r] <- lm(U ~ X + Z, d)$coefficients["X"]
    vec_effect_MY[r] <- model_second$coefficients["M"]
    vec_effect_UY[r] <- model_second$coefficients["U"]
    vec_effect_VY[r] <- model_second$coefficients["V"]
    
  }
  
  V_confounds_M[p,2] <- phis[p]
  V_confounds_M[p,3] <- mean(vec_ate_bench)
  V_confounds_M[p,4] <- mean(vec_ate_naive)
  V_confounds_M[p,5] <- mean(vec_ate_fdc)
  V_confounds_M[p,6] <- mean(vec_first_X)
  V_confounds_M[p,7] <- mean(vec_second_M)
  V_confounds_M[p,8] <- mean(vec_effect_MV)
  V_confounds_M[p,9] <- mean(vec_effect_XU)
  V_confounds_M[p,10] <- mean(vec_effect_MY)
  V_confounds_M[p,11] <- mean(vec_effect_UY)
  V_confounds_M[p,12] <- mean(vec_effect_VY)
  
}


V_confounds_M <- V_confounds_M %>%
  mutate(
    ate_naive_rep = first_X * effect_MY + effect_UY * effect_XU,
    ate_fdc_rep = first_X * effect_MY + first_X * effect_VY * effect_MV
  )

VM_plot <- V_confounds_M %>%
  select(phi, ate_bench, ate_naive, ate_fdc, ate_naive_rep, ate_fdc_rep) %>%
  pivot_longer(c(ate_bench, ate_naive, ate_fdc, ate_naive_rep, ate_fdc_rep), names_to = "estimator", values_to = "estimate") %>%
  mutate(
    replication = as.factor(ifelse(estimator %in% c("ate_naive_rep", "ate_fdc_rep"), "Replicated (by OVB)", "Estimated")),
    model = as.factor(
      ifelse(estimator == "ate_bench", "Benchmark",
             ifelse(estimator %in% c("ate_fdc", "ate_fdc_rep"), "FDC", "Naive")))
  )

ggplot(data = VM_plot, aes(x = phi, y = estimate, shape = replication, color = model)) +
  geom_hline(yintercept = b*b, color = "#E44030") +
  geom_point(alpha = 0.7) +
  labs(
    x = TeX(r'($\phi$)'),
    y = TeX(r'($\hat{ATE}$)'),
    color = "Estimator",
    shape = ""
  ) +
  scale_color_manual(values = c("#F39627", "#207E4C", "#306FE4")) +
  scale_shape_manual(values=c(4, 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave(filename = "04_visualisation/VM_plot.png", plot = last_plot(), width = 10, height = 4)

