# Grouped operations using dplyr

#Load library and file:
library(tidyverse)
gapminder2010 <- read_csv('D:\\R\\r-workshop\\data\\raw\\gapminder1960to2010_socioeconomic.csv', na ='')

#Summarise data, and calculate mean and SD for life expectancy:
gapminder2010 %>%
  summarise(life_expectancy_mean = mean(life_expectancy, na.rm=TRUE),
            life_expectancy_sd = sd(life_expectancy, na.rm=TRUE))
#na.rm option sets to ignore missing values in the calculation.

#List of summary functions: all have the na.rm option.
# mean(x) - arithmetic mean
# median(x) - median
# sd(x) - standard deviation
# var(x) - variance
# IQR(x) - interquartile range
# mad(x) - median absolute deviation
# min(x) and max(x) - minimum and maximum
# quantile(x, probs = 0.75) - quantile (use the probs option to set the quantile of your choosing)
# sum(x) - addition of all values in "x"
# n_distinct(x) (from dplyr) - the number of distinct values in the vector "x"

#Grouped summaries: Combine groupby() and summarise()
#summarise each income group:
gapminder2010 %>%
  group_by(income_groups) %>%
  summarise(life_expectancy_mean = mean(life_expectancy, na.rm = TRUE), 
            life_expectancy_sd = sd(life_expectancy, na.rm = TRUE))

#Median life expectancy for each year and region, and create a line plot:
gapminder2010 %>%
  group_by(year, world_region) %>%
  summarise(life_expectancy_median = median(life_expectancy, na.rm = TRUE)) %>%
  ggplot(aes(year, life_expectancy_median)) +
  geom_line(aes(colour = world_region)) +
  theme_minimal()

#Ribbon graph for children per woman for median, 50% and 90% percentiles:
gapminder2010 %>%
  filter(!is.na(children_per_woman)) %>%
  group_by(year) %>%
  summarise(q5 = quantile(children_per_woman, probs = 0.05),
            q25 = quantile(children_per_woman, probs = 0.25),
            median = median(children_per_woman),
            q75 = quantile(children_per_woman, probs = 0.75),
            q95 = quantile(children_per_woman, probs = 0.95)) %>%
  ggplot(aes(year, median)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
  geom_line() +
  theme_minimal() +
  labs(x = 'Year', y = 'Children per Woman',
       title = 'Median, 50% and 90% percentiles')

#Counting observations per group
#Count how many rows for each group, using n() within summirise()
gapminder2010 %>%
  group_by(income_groups) %>%
  summarise(life_expectancy_mean = mean(life_expectancy, na.rm=TRUE),
            life_expectancy_sd = sd(life_expectancy, na.rm=TRUE),
            observations = n())
#This shows the number of observation points per group, including NA values.

gapminder2010 %>%
  filter(!is.na(life_expectancy))%>%  #Filter to exclude NA values...
  group_by(income_groups) %>%
  summarise(life_expectancy_mean = mean(life_expectancy, na.rm=TRUE),
            life_expectancy_sd = sd(life_expectancy, na.rm=TRUE),
            observations = n())

#Include na and non na in the same table:
gapminder2010 %>%
  group_by(income_groups) %>%
  summarise(life_expectancy_mean = mean(life_expectancy, na.rm=TRUE),
            life_expectancy_sd = sd(life_expectancy, na.rm=TRUE),
            observations = n(),
            observations_non_NA = sum(!is.na(life_expectancy)))
# Remember that !is.na() returns a logical vector for sum() function to count.

income_summary <- gapminder2010 %>%
  group_by(year, world_region) %>%
  summarise(income_per_person_mean = mean(income_per_person, na.rm=TRUE),
            income_per_person_sd = sd(income_per_person, na.rm=TRUE),
            number_countries = n(),
            data_income_per_person = sum(!is.na(income_per_person)),
            below_2usd_day = sum(income_per_person < 2*365, na.rm = TRUE)) 
#Graph
income_summary %>%
  ggplot(aes(year, below_2usd_day/data_income_per_person)) + 
  geom_line(aes(colour = world_region))

#Function count = group_by() + summarise(n())
gapminder2010 %>%
  count(year, world_region)

#Grouped filter: group_by() combined with filter()
#Get the lowest income for each year.
gapminder2010 %>%
  group_by(year) %>%
  filter(income_per_person == min(income_per_person, na.rm = TRUE))

#Adding new columns: mutate() function
#Calculate the population of each country as a % of total population / year
gapminder2010 %>%
  group_by(year) %>%
  mutate(perc_population = population/sum(population, na.rm = TRUE)*100) %>%
  select(country, year, population, perc_population) %>%
  ggplot(aes(year, perc_population)) +
  geom_line(aes(group = country))

#Child mortality centered on the mean of each year.
gapminder2010 %>%
  filter(!is.na(child_mortality)) %>%
  group_by(year) %>%
  mutate(child_mortality_centered = child_mortality - mean(child_mortality)) %>%
  ggplot(aes(year, child_mortality_centered)) +
  geom_line(aes(group = country)) + 
  geom_hline(yintercept = 0, colour = 'red', size=2, linetype = 'dashed')

#Removing groups with ungroup():
#Always remove groups, so you do not do operations within groups later on.
#Calculate the total income of each country and save to a new object:
total_incomes <- gapminder2010 %>%
  group_by(world_region, year) %>%
  summarise(total_income = sum(income_per_person))
#New column %_total_income
total_incomes <- total_incomes %>%
  mutate(perc_total_income = total_income/sum(total_income)*100)
sum(total_incomes$perc_total_income)  # sum is 600, which is wrong, shoud be 100
#Remove groups before sum...
total_incomes <- gapminder2010 %>%
  group_by(world_region, year) %>%
  summarise(total_income = sum(income_per_person)) %>%
  ungroup() %>%
  mutate(perc_total_income = total_income/sum(total_income)*100)
sum(total_incomes$perc_total_income)

#lead() and lag() functions:
#shift vectors by one value, to compare values ahead or behind the current value. 
#Calculate the change in child mortality in the current year and previous one.
gapminder2010 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(diff_child_mortality = child_mortality - lag(child_mortality)) %>%
  ggplot(aes(year, diff_child_mortality)) +
  geom_line(aes(group = country), alpha=0.1) +
  geom_hline(yintercept = 0, colour = 'red')

#cumsum: Cummulative sum.
gapminder2010 %>%
  filter(!is.na(income_per_person)) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(cumulative_income = cumsum(income_per_person)) %>%
  ggplot(aes(year, cumulative_income)) +
  geom_line(aes(group = country)) +
  scale_y_continuous(trans='log10')

#Using [] inside functions: Find countries with the least income per person.
gapminder2010 %>%
  filter(!is.na(income_per_person)) %>%
  group_by(year) %>%
  summarise(income_min = min(income_per_person), 
            country = country[income_per_person == min(income_per_person)]) %>%
  ggplot(aes(year, income_min)) +
  geom_line() +
  geom_point(aes(colour = country))

#Clear packages
detach("package:datasets", unload = TRUE)

#Clear plots
dev.off()

#Clear Console
cat("\014")

#clear objects
rm(list = ls())
