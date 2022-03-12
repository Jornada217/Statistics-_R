#Reading data into R

#import tidyverse
library(tidyverse)

#read csv
gapminder2010 <- read_csv('D:\\R\\r-workshop\\data\\raw\\gapminder1960to2010_socioeconomic.csv', na = '')

#Data control commands: typeof(), class(), str(), summary().
  # length(unique()), is.na()

typeof(gapminder2010)
class(gapminder2010)
ncol(gapminder2010)
nrow(gapminder2010)
str(gapminder2010)
summary(gapminder2010)

gapminder2010$country
unique(gapminder2010$world_region)
length(unique(gapminder2010$world_region))

table(gapminder2010$world_region)

unique(gapminder2010$main_religion)
table(gapminder2010$main_religion)

class(gapminder2010$life_expectancy_female)

#Accessing rows and columns
gapminder2010[1, 3:6]
gapminder2010[, c('country', 'income_per_person')]

# skim() is a package that do all quality checks in one go.
install.packages('ggbeeswarm')
install.packages('skimr')
library(skimr)
skim(gapminder2010)

#Clear packages
detach("package:datasets", unload = TRUE)

#Clear Console
cat("\014")

#clear objects
rm(list = ls())
