# Jitter HEPESE Study Data
# Gabriel Odom
# 2024-05-10

# The data that Srijana and Arturo used for the Mann-Whitney U lesson is 
#   not public and cannot be shared. This script will take in the original data
#   and create a synthetic variant of a subset of it, specifically the sex and
#   age at Dx columns.


######  Setup  ################################################################
library(haven)
library(tidyverse)

original_df <- read_sav("data/ICPSR_36578/DS0001/36578-0001-Data.sav")

toJitter_df <- 
  original_df %>% 
  mutate(isMale = SEX8 == 1) %>% 
  select(isMale, ageAtDx = MDIAB82) %>% 
  # Strip attributes
  mutate(ageAtDx = as.integer(ageAtDx))



######  Impute Missing Age at Dx and Jitter  ##################################
summary(toJitter_df$ageAtDx)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   20.00   60.00   70.00   67.31   78.00   91.00     544

# Normally I would throw a fit at the idea of imputing this many missing values,
#   but the data are going to be fake anyway, so w/e.
nonMissingAge_int <-
  toJitter_df %>% 
  drop_na(ageAtDx) %>% 
  pull(ageAtDx)
hist(nonMissingAge_int)

# Randomly add or subtract some years
# set.seed(20150516)
# Normally, I would set a seed, but I don't actually want anyone to be able to
#   reconstruct the original data.
multiplier_int <- sample(c(-1L, 1L), size = nrow(toJitter_df), replace = TRUE)
yearsOff_int <- rpois(n = nrow(toJitter_df), lambda = 1)

jittered_df <- 
  toJitter_df %>% 
  mutate(
    willJitter_lgl = sample(
      x = c(TRUE, FALSE), size = nrow(.), replace = TRUE, prob = c(0.1, 0.9)
    )
  ) %>% 
  mutate(
    isMaleJ = if_else(willJitter_lgl, true = !isMale, false = isMale)
  ) %>% 
  rowwise() %>% 
  mutate(
    ageAtDxImp = case_when(
      is.na(ageAtDx) ~ sample(nonMissingAge_int, size = 1),
      !is.na(ageAtDx) ~ ageAtDx
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    ageAtDxJ = ageAtDxImp + (multiplier_int * yearsOff_int)
  )

table(jittered_df$ageAtDxImp - jittered_df$ageAtDxJ)



######  Clean Up and Save  ####################################################
jittered_df %>% 
  select(isMaleJ, ageAtDxJ) %>% 
  rename(isMale = isMaleJ, ageAtDx = ageAtDxJ) %>% 
  write_csv(file = "data/03_HEPESE_synthetic_20240510.csv")
