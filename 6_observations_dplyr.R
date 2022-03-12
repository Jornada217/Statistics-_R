#import tidyverse
library(tidyverse)

#read csv
gapminder2010 <- read_csv('D:\\R\\r-workshop\\data\\raw\\gapminder1960to2010_socioeconomic.csv', na = '')

#Ordering rows by year using arrange():
gapminder2010 %>%
  select(country, world_region, year) %>%
  arrange(year)

#Ordering rows by continent in descending order:
gapminder2010 %>%
  select(country, world_region, year) %>%
  arrange(desc(world_region))

#Including several arrangment orders:
gapminder2010 %>%
  select(country, world_region, year) %>%
  arrange(year, desc(world_region), desc(country))

#Retain unique rows using distinct():
gapminder2010 %>%
  distinct(main_religion, world_region)

#Filtering rows based on conditions, using filter():
gapminder2010 %>%
  select(country, year) %>%
  filter(year > 2000)
# Bear in mind that by using a condition, the output is a logical vector.
select_years <- c(1985, 1990, 2020, 2001, 1975)
select_years < 2000  #Output: [1]  TRUE  TRUE FALSE FALSE  TRUE

#Using multiple conditions with &(and) and |(or):
select_years > 1990 & select_years <2000

#Some operators to use in conditions:
#   > is greater than
#   >= is greater than or equal to
#   < is less than
#   <= is less than or equal to
#   == is equal to
#   != is different from
#   %in% is contained in
#   ! to negate a logical condition:
    #    !is.na(x) returns TRUE if a value in x is NOT missing
    #    !(x %in% y) returns TRUE if a value in x is NOT present in y

#Combination operators and functions:
#   & AND
#   | OR
#   is,na(x)   Returns TRUE if x is null value.

# Retain specif countries and plot a function of income_per_person in time:
gapminder2010 %>%
  filter(country %in% c('Uganda', 'Brazil', 'South Korea')) %>%
  arrange(country, year) %>%
  ggplot(aes(income_per_person, child_mortality)) +
  geom_path(aes(colour = country), arrow = arrow())

#Scatter plot for life expectancy and fertility reate:
gapminder2010 %>% 
  mutate(population_millions = population/1e6) %>% 
  filter(year %in% c(1960, 1990, 2010)) %>% 
  ggplot(aes(children_per_woman, life_expectancy)) +
  geom_point(aes(size = population_millions, colour = world_region)) +
  facet_grid(cols = vars(year)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Fertility Rate (children per woman)", y = "Life Expectancy (years)", 
       size = "Total Population (millions)", colour = "Region")

#Set multiple conditions for filter
gapminder2010 %>%
  filter(year == 1960 & children_per_woman < 5 & life_expectancy <35) %>%
  select(country, year, children_per_woman, life_expectancy)

#Chart of life expectancy change (China and Portugal)
gapminder2010 %>%
  filter(country %in% c('China', 'Portugal')) %>%
  ggplot(aes(x = year, y = life_expectancy)) + 
  geom_line(aes(colour = country))

#Filtering by is.na(): Return all rows where religion is not missing
gapminder2010 %>%
  filter(!is.na(main_religion))

#Count how many observations for both life_expectancy and children_per_woman:
gapminder2010 %>%
  filter(!is.na(life_expectancy) & !is.na(children_per_woman)) %>%
  nrow()

# conditionals IFELSE, 
  # modify variables or highlight certain observations in the data
gapminder2010 %>%
  mutate(is_brazil = country == 'Brazil') %>%
  ggplot(aes(x = year, y = income_per_person)) +
  geom_line(aes(group = country, colour = is_brazil))
# Highlight two countries and compare to other countries in the chart:
gapminder2010 %>%
  mutate(income_total = population*income_per_person,
         country_highlight = ifelse(country %in% c('China', 'United States', 'Brazil'),
                                    country, 'Others')) %>%
  ggplot(aes(year, income_total)) +
  geom_line(aes(group = country, colour = country_highlight))
#ifelse() works in a vectorised manner, 
  #meaning that it evaluates the condition at each value 
    #and returns the respective result we ask for.

#Using IFELSE to correct outliers: 
  #In this case, min life expectancy for male is -999
summary(gapminder2010$life_expectancy_male)
gapminder2010 <- gapminder2010 %>%
  mutate(life_expectancy_male = ifelse(life_expectancy_male == -999, NA, life_expectancy_male))
  #Run summary again, and min expectancy for male is 16.29 now.


#Clear packages
detach("package:datasets", unload = TRUE)

#Clear plots
dev.off()

#Clear Console
cat("\014")

#clear objects
rm(list = ls())
