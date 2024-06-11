# Ranks for Non-Normal Data
# Gabriel Odom
# 2024-06-06

library(tidyverse)


###  Simulate Data  ###
nx_int <- 15
ny_int <- 15

xGamma_num <- rgamma(n = nx_int, shape = 3, rate = 3)
hist(xGamma_num)

yNorm_num <- rnorm(n = ny_int, mean = 2, sd = 1)
hist(yNorm_num)



###  Combine and Plot  ###
hist(c(xGamma_num, yNorm_num))

ranks_df <- tibble(
  group = c(rep("x", nx_int), rep("y", ny_int)),
  value = c(xGamma_num, yNorm_num),
  ranks = rank(value)
)

ggplot(data = ranks_df) + 
  aes(x = value, y = ranks, colour = group) + 
  geom_col()


###  Test  ###
wilcox.test(value ~ group, data = ranks_df, conf.int = TRUE, exact = TRUE)

# How? We can use a normal approximation. See
#   https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test
Rx <- ranks_df %>% 
  filter(group == "x") %>% 
  pull(ranks) %>% 
  sum()
Ry <- ranks_df %>% 
  filter(group == "y") %>% 
  pull(ranks) %>% 
  sum()

Ux <- nx_int * ny_int + ny_int * (ny_int - 1) / 2 - Ry
Uy <- nx_int * ny_int + nx_int * (nx_int - 1) / 2 - Rx

U <- min(Ux, Uy) 

# Asymptotic Distribution
mU <- nx_int * ny_int / 2
sU <- sqrt(
  nx_int * ny_int * (nx_int + ny_int + 1) / 12
)

pnorm(q = U, mean = mU, sd = sU)
# This approximation is poor because our sample sizes are small.


