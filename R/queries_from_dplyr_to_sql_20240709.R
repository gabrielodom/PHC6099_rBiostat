library(tidyverse)
library(dbplyr)
library(nycflights13)

flights %>% 
  # Setup for dbplyr (and name the table properly)
  dbplyr::tbl_lazy(name = "flights") %>% 
  filter(month == 7, day == 9) %>% 
  select(-year, -month, -day) %>% 
  select(dep_time, origin, dest, dep_delay) %>% 
  arrange(desc(dep_delay)) %>% 
  mutate(is_late = dep_delay > 30) %>% 
  # generate the SQL code
  dbplyr::remote_query()
