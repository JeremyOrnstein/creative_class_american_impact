
library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(readxl)

# I used read_csv to turn the necessary documents into tibbles. 

latlong_raw <- read_csv("raw-data/latlong.csv") %>% mutate(Long = str_remove_all(Long, "[–°]")) %>% 
  mutate(Long = str_c("-", Long)) %>% 
  mutate(Lat = str_remove_all(Lat, "[+°]")) %>% mutate(Lat = as.numeric(Lat)) %>% 
  mutate(Long = as.numeric(Long))

people <- read_csv("raw-data/People.csv")
income <- read_csv("raw-data/income.csv")
creatives <- read_csv("raw-data/creative_class_clean.csv")

# Out of the coordinate file, I only needed 3 columns. 
# I used paste to add a "0" to the FIPS column, which is a county identifier number, to universalize the FIPS columns between tibbles

latlong_raw1 <- latlong_raw %>% 
  select("Lat", "Long", "FIPS") %>% 
  mutate(FIPS = str_pad(FIPS, 5, side = "left", pad = "0"))

# mutate FIPS with if else as length where FIPS is 4 digit add a leading zero, otherwise leave it as it is 

creatives <- creatives %>% mutate(FIPS = str_pad(FIPS, 5, side = "left", pad = "0"))

creative_people <- left_join(creatives, people, by = "FIPS")

first_creative_people_income <- left_join(creative_people, income, by = "FIPS")

# I used join to pull the two tibbles together 

second_creative_people_income <- first_creative_people_income %>% 
  mutate(metro93 = `metro 1993 definition (1=metro, 0=nonmetro)`) %>% 
  mutate(metro03 = `metro 2003 definition (1=metro, 0=nonmetro)`) %>%
  mutate(difference = Creative2000N - Creative1990N)

# I changed columns to be neater for later use, and created a difference column with mutate.
# Then, I joined the coordinate tibble with the other tibble. 

creative_people_income <- left_join(second_creative_people_income, latlong_raw1, by = "FIPS")

creative_people_income_first <- left_join(first_creative_people_income, latlong_raw1, by = "FIPS")

write_rds(creative_people_income, path = "creative.web.app/creative_people_income.rds")

write_rds(creative_people_income_first, path = "creative.web.app/test.rds")

# I wrote it into an rds to use in my app.r.




