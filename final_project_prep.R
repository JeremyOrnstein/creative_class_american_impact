
library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)

people <- read_csv("raw-data/People.csv")
income <- read_csv("raw-data/income.csv")
creatives <- read_csv("raw-data/creative_class_clean.csv")

creatives$FIPS <- paste0("0", creatives$FIPS)

inner_join(creatives, income, by = "County")

inner_join(creatives, income)

creative_income <- merge(creatives, income, by = "FIPS", all.x=TRUE)

creative_people <- merge(creatives, people, by = "FIPS", all.x=TRUE)

creative_people_income_old <- merge(creative_people, income, by = "FIPS", all.x=TRUE) 

creative_people_income <- creative_people_income_old %>% 
  mutate(metro93 = `metro 1993 definition (1=metro, 0=nonmetro)`) %>% 
  mutate(metro03 = `metro 2003 definition (1=metro, 0=nonmetro)`) %>%
  mutate(difference = Creative2000N - Creative1990N)

write_rds(creative_people_income, path = "final_project_draft/creative_people_income.rds")

model <- lm(data = creative_people_income, formula = PerCapitaInc ~ PopDensity2010)
print(model)

# How does population density affect creative class? 
change <- creative_people_income %>% mutate(difference = Creative2000N - Creative2000S)
change %>% ggplot(aes(PopDensity2010, difference)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + 
  xlab("Population Density") + ylab("Change in Creative Class 1990-2000, Share of Pop") + scale_y_log10() + scale_x_log10() + labs(title = "Relationship between Population Density and Creative Class")
model_pop <- lm(data = creative_people_income, formula = change$difference ~ PopDensity2010)
print(model_pop)

# Higher per capita income corresponds to more creative people
creative_income %>% 
  ggplot((aes(x = PerCapitaInc, Creative2000S))) + geom_point() + geom_smooth(method = "lm", se = TRUE) + xlab("Per Capita Income") + ylab("Creative Class, Share of Pop") + labs(title = "Relationship between Per Capita Income and Creative Class")
model_inc <- lm(data = creative_people_income, formula = Creative2000S ~ PerCapitaInc)
print(model_inc)

# Higher per capita income brings in more creative people 
change %>% ggplot(aes(PerCapitaInc, difference)) + geom_point() + geom_smooth(method = "lm", se = TRUE) +
  xlab("Per Capita Income") + ylab("Change in Creative Class 1990-2000, Share of Pop") + scale_y_log10() + scale_x_log10() + labs(title = "Relationship between Per Capita Income and Change in Creative Class")
model_change <- lm(data = creative_people_income, formula = Creative2000S ~ PerCapitaInc)
print(model)

# More migration means more creative people.
# bohemian <- creative_people_income %>% filter(Bohemian2000S < .03)
#  bohemian%>%
  creative_people_income %>% ggplot((aes(x = Bohemian2000S, NetMigrationRate1018))) + geom_point() + geom_smooth(method = "lm", se = TRUE) +xlab("Bohemians, Share of Pop") + ylab("Net Migration Rate") + labs(title = "Relationship between Migration and Bohemians")
model_migration <- lm(data = creative_people_income, formula = NetMigrationRate1018 ~ Bohemian2000S)
print(model_migration)
model

# Metro's impact on creative class
creative_people_income %>% ggplot(aes(x = `metro 1993 definition (1=metro, 0=nonmetro)`, Creative2000S)) + geom_jitter() + geom_smooth(method = "lm", se = TRUE) + xlab("Metro Status") + ylab("Creative Class") + labs(title = "Relationship between Metro Status and Creative Class")
model_metro <- lm(data = creative_people_income, formula = Creative2000S ~ `metro 1993 definition (1=metro, 0=nonmetro)`)
print(model_metro)
```