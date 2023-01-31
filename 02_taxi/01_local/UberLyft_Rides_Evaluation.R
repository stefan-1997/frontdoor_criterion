###################### Evaluation of bwUniCluster Results ######################

options(scipen=999)

rm(list=ls())
invisible(gc())

setwd("C:/Users/stefan/OneDrive - bwedu/03_semester/Master_Seminar_Marketing/02_empirical/frontdoor_criterion/02_taxi")

library(tidyverse)
library(latex2exp)

# # extensive margin
# results <- readr::read_csv("03_data/estimationResultsBaseXXL.csv")
# results_alt <- readr::read_csv("03_data/estimationResultsAlternativeXXL.csv")

# # intensive margin
results <- readr::read_csv("03_data/estimationResultsBasetipAsinh.csv")
results_alt <- readr::read_csv("03_data/estimationResultsAlternativetipAsinh.csv")

# different elasticities
# results_asinh_base <- readr::read_csv("03_data/estimationResultsBasetipAsinh.csv")
# results_asinh <- readr::read_csv("03_data/estimationResultsAlternativetipAsinh.csv")
# results_level <- readr::read_csv("03_data/estimationResultsAlternativetipLevel.csv")
# results_log <- readr::read_csv("03_data/estimationResultsAlternativetipLog.csv")
# 
# results_alt <- readr::read_csv("03_data/estimationResultsAlternativetipAsinhFinal.csv")

plot_01_data <- cbind(
  results_alt %>%
    select(n, ate_fd, ate_naive) %>%
    pivot_longer(cols = c("ate_fd", "ate_naive"), names_to = "estimator", "values_to" = "estimate"),
  results %>%
    select(ate_fd_se, ate_naive_se) %>%
    pivot_longer(cols = c("ate_fd_se", "ate_naive_se"), names_to = "se_estimator", values_to = "se_estimate")
)

plot_01 <- ggplot(plot_01_data, aes(x = n, y = estimate)) +
  geom_point(stat="identity", aes(color = estimator)) +
  geom_errorbar(aes(ymin = estimate - 2*se_estimate, ymax = estimate + 2*se_estimate, color = estimator), width = 50000) +
  ylim(c(min(results$ate_naive - 2*results$ate_naive_se),0)) +
  labs(x = "Number of observations", y = "ATT", color = "Estimator") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_colour_discrete(labels=c("FDC", "Naive")) +
  theme_bw()

plot_01
ggsave("04_visualisation/ATTs_by_Sample.png", plot=plot_01)

plot_02 <- ggplot(results, aes(x = n, y = ate_fd)) +
  geom_point(stat="identity", color = "#953807") +
  geom_errorbar(aes(ymin = ate_fd - 2*ate_fd_se, ymax = ate_fd + 2*ate_fd_se), width = 50000) +
  ylim(c(min(results$ate_fd - 2*results$ate_fd_se),0)) +
  labs(x = "Number of observations", y = TeX(r'($ATT_{FDC}$)')) +
  theme_bw()

plot_02
ggsave("04_visualisation/FDC_ATTs_by_Sample.png", plot=plot_02)







# # elasticity estimation (experimental)
# 
# model_naive_base <- lm(tip_asinh ~ shared_trip_authorized + fare_full + trip_start_date + trip_hour + trip_weekday_hour + origin_destination_pairs + pairs_X_start_date, data = df)
# 
# # procedure A
# full_spec <- coef(model_naive_base)
# full_spec <- full_spec[which(!is.na(full_spec))]
# reduced_spec <- full_spec[which(names(full_spec) != "shared_trip_authorized")]
# sinh(sum(full_spec)) / sinh(sum(reduced_spec)) - 1
# sinh(abs(sum(full_spec))) / sinh(abs(sum(reduced_spec))) - 1
# 
# # procedure B
# df_X1 <- df %>%
#   mutate(shared_trip_authorized = 1)
# df_X0 <- df %>%
#   mutate(shared_trip_authorized = 0)
# 
# pred_X1 <- predict(model_naive, df_X1)
# pred_X0 <- predict(model_naive, df_X0)
# mean(pred_X1) / mean(pred_X0) - 1

