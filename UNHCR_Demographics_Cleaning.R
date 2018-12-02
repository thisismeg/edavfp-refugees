library(tidyverse)
library(reshape)

#setwd("AY2018-2019/Fall 2018/Classes/Edav/FinalProject")

dem <- read_csv('UNHCR_Demographics.csv')
colnames(dem)[colnames(dem) == 
                     "Country / territory of asylum/residence"] <- "Country"


dem_long <- gather(dem,demographic,number,-Year, -'Country',-'Location Name')
dem_long <- dem_long %>%
  transform(dem = colsplit(demographic, split = "\\ ", names = c('sex', 'age')))

dem_long_filtered <- dem_long %>%
  filter(Country == 'South Sudan') %>% 
  filter(Year >= 2010 & Year <= 2016) %>%
  filter(dem.sex %in% c("Female","Male")) %>%
  group_by(Country, Year, dem.sex, dem.age) %>%
  summarise(number = sum(number))

dem_long_filtered_sex <- dem_long_filtered %>%
  group_by(dem.sex, Year, Country) %>%
  summarise(number = sum(number))
# South Sudan refugees by sex
ggplot(dem_long_filtered_sex, aes(x = Year, y = number, fill = dem.sex)) +
  geom_col(position = "dodge")

dem_long_filtered_age <- dem_long_filtered %>%
  filter(Year == 2016) 
# South Sudan refugees by age
ggplot(dem_long_filtered_age, aes(x = dem.age, y = number)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~dem.sex)
