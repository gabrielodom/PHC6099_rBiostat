# Subset BRFSS Data
# Gemma Galvez
# 2024-07-12

library(tidyverse)

BRFSS_df <- read_csv("extdata/BRFSS_raw_2013.csv")

BRFSS_df1 <- 
  BRFSS_df %>%
  select(sex, X_racegr3, marital, educa, income2, employ1, genhlth)

write_csv(BRFSS_df1, file = "data/05_brfss_subset.csv")
