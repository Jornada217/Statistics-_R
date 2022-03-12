library(tidyverse, skimr)

gapminder <- read_csv("data/raw/gapminder2010_socioeconomic.csv",
                      na = '')

skim(gapminder)

#Ploting 
ggplot(data = gapminder,
       mapping = aes(x = children_per_woman, y = life_expectancy, colour = world_region)) +
  geom_point(alpha = 0.9, size = 2, shape = 6)


#Use naniar to replace null values:
library(naniar)
#geo_miss_point from naniar can be used with ggplot to fill NA with 
  #values 10% lower than the min in the variable:
ggplot(data = gapminder,
       mapping = aes(x = children_per_woman, y = life_expectancy)) +
  geom_miss_point()

#Boxplot of children_per_woman for world_region(x) filled by income groups
ggplot(data=gapminder,
       aes(x = world_region, y = children_per_woman, fill=income_groups)) +
  geom_boxplot()

#Violin plot of children_per_woman for world_region(x)
#Scaling violins by width rather than area (area is default)
#Adding a boxplot and making it thinner to fit in the violins
ggplot(gapminder, aes(x = world_region, y = children_per_woman)) +
  geom_violin(aes(fill=world_region), scale = 'width') +
  geom_boxplot(width = 0.2)


#Changing how geometries look like:
?geom_point


#Clear packages
detach("package:datasets", unload = TRUE)

#Clear plots
dev.off()

#Clear Console
cat("\014")

#clear objects
rm(list = ls())
