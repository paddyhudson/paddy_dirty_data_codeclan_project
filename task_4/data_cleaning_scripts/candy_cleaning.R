#load libraries

library(tidyverse)
library(janitor)
library(readxl)
library(here)

#read in each of the three datasets and clean names

candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx")) %>%  clean_names()
candy_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx")) %>%  clean_names()
candy_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx")) %>%  clean_names()

#start cleaning 2015 data

candy_2015_working <- candy_2015 %>% 
  #drop useless columns
  select(2:96) %>% 
  #rename columns
  rename(age = how_old_are_you, 
         going_out = are_you_going_actually_going_trick_or_treating_yourself) %>% 
  #convert to long format
  pivot_longer(3:95, names_to = "candy", values_to = "rating") %>%
  #add year and other missing columns present in 2016/2017 data
  mutate(country= NA, year = 2015, gender = NA, .after = going_out)

#start cleaning 2016 data

candy_2016_working <- candy_2016 %>% 
  #drop useless columns
  select(2:106, -which_state_province_county_do_you_live_in) %>% 
  #rename columns
  rename(age = how_old_are_you, 
         going_out = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in) %>%
  #convert to long format
  pivot_longer(5:104, names_to = "candy", values_to = "rating") %>% 
  #add year
  mutate(year = 2016, .after = going_out)

#start cleaning 2017 data

candy_2017_working <- candy_2017 %>% 
  #drop useless columns
  select(2:109, -q5_state_province_county_etc) %>% 
  #rename columns
  rename(age = q3_age, 
         going_out = q1_going_out,
         gender = q2_gender,
         country = q4_country) %>%
  #convert to long format
  pivot_longer(5:107, names_to = "candy", values_to = "rating", names_prefix = "q[0-9]_") %>% 
  #add year
  mutate(year = 2017, .after = going_out)

#combine the three datasets

candy_all <- candy_2015_working %>% 
  bind_rows(candy_2016_working) %>% 
  bind_rows(candy_2017_working)

#clean the age column

candy_all_working <- candy_all %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age = if_else(between(age, 5, 98), age, 0)) %>% 
  mutate(age = na_if(age, 0))

#checked distinct values of going_out, year, gender and rating in console - all good

#clean country column

candy_all_working <- candy_all_working %>%
  mutate(
    country = str_to_lower(country),
    country = case_when(
      str_detect(country, "usa") ~ "usa",
      str_detect(country, "america") ~ "usa",
      str_detect(country, "states") ~ "usa",
      TRUE ~country),
    country = recode(country,
                    "us" = "usa",
                    "united states" = "usa",
                    "'merica" = "usa",
                    "united states of america"= "usa",
                    "u.s.a." = "usa",
                    "ussa" = "usa",
                    "murica" = "usa",
                    "england" = "uk",
                    "united kingdom" = "uk",
                    "u.s." = "usa",
                    "scotland" = "uk",
                    "us of a" = "usa",
                    "united state" = "usa",
                    "united stated" = "usa",
                    "united ststes" = "usa"
                     )
    ) %>% 
  group_by(country) %>% 
  mutate(count = n()) %>% 
  mutate(country = if_else(count > 203, country, "")) %>% 
  mutate(country = na_if(country, "")) %>% 
  select(-count)

write_csv(candy_all_working, here("clean_data/candy_clean.csv"))

rm(list = ls())