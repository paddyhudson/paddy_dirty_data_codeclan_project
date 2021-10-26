library(tidyverse)
library(janitor)

#read in data
decathlon <- read_rds("raw_data/decathlon.rds") %>% clean_names()

#remove rownames
decathlon_clean <- decathlon %>% 
  rownames_to_column(var = "name") %>% 
#fix column names 
  rename(
    "100m" = x100m,
    "400m" = x400m,
    hurdles_110m = x110m_hurdle,
    javelin = javeline,
    "1500m" = x1500m
  ) %>% 
#pivot longer to create tidy data
  pivot_longer(
    cols = "100m":"1500m",
    names_to = "event",
    values_to = "result"
  ) %>% 
#make name column all lower case
  mutate(name = str_to_lower(name))

#write to csv

write_csv(decathlon_clean, "clean_data/decathlon_clean.csv")
remove(decathlon_clean)
remove(decathlon)
