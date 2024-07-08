# Effect of Correlation on Family-Wise Error Rate Corrections
# Gabriel Odom
# 2024-07-02

library(tidyverse)



######  Simulation Setup  #####################################################
n1 <- n2 <- 15L
N <- n1 + n2
y <- c(rep("a", n1), rep("b", n2))
p <- 1000L

# set.seed(911) # way too many significant p-values for correlated tests
set.seed(20150516) # no significant p-values for correlated tests



######  Independent t-Tests  ##################################################

# Draw Random Independent Data
indep_mat <- matrix(
  data = rnorm(N * p),
  nrow = N,
  ncol = p
)
round(cor(indep_mat[, 1:10]), 2)


# Model dataframe
indepData_df <- 
  indep_mat %>% 
  as.data.frame() %>% 
  bind_cols(tibble(treatment = y), .)


# Loop
indepRes_num <- map_dbl(
  .x = seq_len(p) + 1,
  .f = ~{
    # Split the data into two samples based on the response
    data_ls <- list(
      a = indepData_df[indepData_df$treatment == "a", .x, drop = TRUE],
      b = indepData_df[indepData_df$treatment == "b", .x, drop = TRUE]
    )
    # Extract the p-value
    t.test(x = data_ls[[1]], data_ls[[2]])$p.value
  }
)


# Inspect Results
# This should be roughly uniform
hist(indepRes_num)
# This should be very close to 5%
mean(indepRes_num < 0.05)



######  Correlated t-Tests  ###################################################

# Correlation/Covariance Matrix
rho <- 0.9
I_mat <- diag(p)
M_mat <- matrix(1, nrow = p, ncol = p)
Sigma_mat <- (1 - rho) * I_mat + rho * M_mat
Sigma_mat[1:10, 1:10]


# Draw Random Correlated Data
dep_mat <- MASS::mvrnorm(
  n = N,
  mu = rep(0, p),
  Sigma = Sigma_mat
)
# NOTE: because we are simulating from a multivariate standard normal, the mean
# and variance will still be {0,1}, respectively
round(cor(dep_mat[, 1:10]), 2)


# Model dataframe
depData_df <- 
  dep_mat %>% 
  as.data.frame() %>% 
  bind_cols(tibble(treatment = y), .)

# Loop
depRes_num <- map_dbl(
  .x = seq_len(p) + 1,
  .f = ~{
    # Split the data into two samples based on the response
    data_ls <- list(
      a = depData_df[depData_df$treatment == "a", .x, drop = TRUE],
      b = depData_df[depData_df$treatment == "b", .x, drop = TRUE]
    )
    # Extract the p-value
    t.test(x = data_ls[[1]], data_ls[[2]])$p.value
  }
)

# Inspect Results
# This should be roughly uniform
hist(depRes_num)
# This should be very close to 5%
mean(depRes_num < 0.05)



######  Family-Wise Error Rate vs FDR Adjustments  ############################
# FWER is more conservative (use if there are real costs to a false positive)
# FDR is more powerful (use if you are building models)

###  Bonferroni  ###
sum(indepRes_num < 0.05)
sum(p.adjust(indepRes_num, method = "bonferroni") < 0.05)
sum(depRes_num < 0.05)
sum(p.adjust(depRes_num, method = "bonferroni") < 0.05)


###  BY (Benjamini & Yekutieli)  ###
sum(indepRes_num < 0.05)
sum(p.adjust(indepRes_num, method = "BY") < 0.05)
sum(depRes_num < 0.05)
sum(p.adjust(depRes_num, method = "BY") < 0.05)


###  FDR  ###
sum(indepRes_num < 0.05)
sum(p.adjust(indepRes_num, method = "fdr") < 0.05)
sum(depRes_num < 0.05)
sum(p.adjust(depRes_num, method = "fdr") < 0.05)
