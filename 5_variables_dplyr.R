#import tidyverse
library(tidyverse)

#read csv
gapminder2010 <- read_csv('D:\\R\\r-workshop\\data\\raw\\gapminder1960to2010_socioeconomic.csv', na = '')

#select columns
select(gapminder2010, country, year)
select(gapminder2010, contains('life'))
select(gapminder2010, contains('life'), contains('_per_'))
select(gapminder2010, country : year)
select(gapminder2010, -country, -life_expectancy)

#Renaming columns, rename(my_table, new_column_name = old_column_name)
rename(gapminder2010, continent = world_region, gdp_per_capita = income_per_person)

#Creating and modifying columns:
mutate(gapminder2010, population_millions = population/1e6)
#The new column is attached to the end of the table.
# to change the object, you have to assign (<-) to an object:
gapminder2010 <- mutate(gapminder2010, population_millions = population/1e6)
select(gapminder2010, population, population_millions)

#Chaining commands with pipe: %>%
gapminder2010 %>%
  mutate(population = population/1e6) %>%
  select(country, world_region, population) %>%
  rename(continent = world_region)

#Using . with %>% to specify exactly where the input goes to.
gapminder2010 %>% select(., country, world_region)
#Cases where . needs to be used explicitly with pipes:
gapminder2010 %>% lm(formula = life_expectancy ~ income_per_person, data = .)

#Using pipe, change life_expectancy_female to numeric
gapminder2010 <- gapminder2010 %>% 
  mutate(life_expectancy_female = as.numeric(life_expectancy_female))
class(gapminder2010$life_expectancy_female)

#Crete a plot using pipes: difference of male - female life expectancies.
gapminder2010 %>%
  mutate(life_expectancy_female_minus_male = life_expectancy_female - life_expectancy_male) %>%
  ggplot(aes(life_expectancy_female_minus_male)) +
  geom_histogram(binwidth = 1)

#Create a line plot, for the change in income.
gapminder2010 %>%
  mutate(income_total = income_per_person * population) %>%
  ggplot(aes(year, income_total)) + 
  geom_line(aes(group = country, colour = world_region))

#Clear packages
detach("package:datasets", unload = TRUE)

#Clear plots
dev.off()

#Clear Console
cat("\014")

#clear objects
rm(list = ls())
