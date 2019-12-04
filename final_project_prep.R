
library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(readxl)

latlong_raw <- read_csv("raw-data/latlong.csv")
people <- read_csv("raw-data/People.csv")
income <- read_csv("raw-data/income.csv")
creatives <- read_csv("raw-data/creative_class_clean.csv")

latlong_raw1 <- latlong_raw %>% select("Lat", "Long", "FIPS")

latlong_raw1$FIPS <- paste0("0", latlong_raw$FIPS)

creatives$FIPS <- paste0("0", creatives$FIPS)

creative_people <- left_join(creatives, people, by = "FIPS")

first_creative_people_income <- left_join(creative_people, income, by = "FIPS")

second_creative_people_income <- first_creative_people_income %>% 
  mutate(metro93 = `metro 1993 definition (1=metro, 0=nonmetro)`) %>% 
  mutate(metro03 = `metro 2003 definition (1=metro, 0=nonmetro)`) %>%
  mutate(difference = Creative2000N - Creative1990N)

creative_people_income <- left_join(second_creative_people_income, latlong_raw1, by = "FIPS")

write_rds(creative_people_income, path = "final_project_draft/creative_people_income.rds")
