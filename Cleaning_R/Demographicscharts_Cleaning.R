library(tidyverse)
library(reshape)

#setwd("AY2018-2019/Fall 2018/Classes/Edav/FinalProject")

dem <- read_csv('UNHCR_Demographics.csv')
colnames(dem)[colnames(dem) == 
                     "Country / territory of asylum/residence"] <- "Country"


dem_long <- gather(dem,demographic,number,-Year, -'Country',-'Location Name')
dem_long <- dem_long %>%
  transform(dem = colsplit(demographic, split = "\\ ", names = c('sex', 'age')))

# Recoding confidential data points to 0
dem_long$number[is.na(dem_long$number)] <- 0

### Sudan
# Cleaning for Sudan
dem_sudan <- dem_long %>%
  filter(Country == 'South Sudan') %>% 
  filter(Year >= 2011 & Year <= 2017) %>%
  filter(dem.sex %in% c("Female","Male")) %>%
  group_by(Country, Year, dem.sex, dem.age) %>%
  summarise(number = sum(number))

# The age 5-17 category was not used in tracking persons of concern from South Sudan
dem_sudan<-dem_sudan[!(dem_sudan$dem.age =="5-17"),]

# Reordering the factor categories
dem_sudan$dem.age <- factor(dem_sudan$dem.age, levels = c("0-4", "5-11", "12-17", "18-59", "60+"))

# Demographic bar chart
ggplot(dem_sudan, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "stack") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in South Sudan')

# Demographic bar chart
ggplot(dem_sudan, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "fill") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in South Sudan')

# Demographic variable width bar chart
dem_sudan_sum <- dem_sudan %>% 
  filter(Year == 2016) %>%
  group_by(Country, Year, dem.age, dem.sex) %>%
  summarise(number = sum(number))

dem_sudan_sum$binwidth <- recode(dem_sudan_sum$dem.age, "0-4" = 4, "5-11" = 6, "12-17" = 5, "18-59" = 41, "60+" = 20)
dem_sudan_sum$center <- recode(dem_sudan_sum$dem.age, "0-4" = 2, "5-11" = 8, "12-17" = 14.5, "18-59" = 38.5, "60+" = 70)
dem_sudan_sum$breaks <- recode(dem_sudan_sum$dem.age, "0-4" = 4, "5-11" = 12, "12-17" = 17, "18-59" = 59, "60+" = 80)
cats <- c("0-4","0-4","5-11", "5-11","12-17","12-17", "18-59", "18-59", "60+", "60+")
total <- sum(dem_sudan_sum$number)
ggplot(dem_sudan_sum, aes(x = center, y = number / binwidth, width = binwidth)) +
  geom_col(color = "lightblue", fill = "lightblue") +
  scale_x_continuous(labels = cats, breaks = dem_sudan_sum$center) +
  facet_grid(.~dem.sex) +
  xlab('Age group') +
  ylab('Chart Area = population of persons of concern') +
  ggtitle('Women and their children are disproportionately affected in South Sudan') +
  theme(#axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
# 
# # Proportion bar chart
# ggplot(dem_sudan, aes(x = factor(Year), y = number / 1000)) +
#   geom_bar(aes(fill = dem.age),stat = "identity", position = "fill") +
#   facet_wrap(.~dem.sex) +
#   xlab('Year') +
#   ylab('Population of Persons of Concern (in thousands)') +
#   ggtitle('Demographic breakdown of persons of concern in South Sudan')

### Syria
# Cleaning for Syria
dem_syria <- dem_long %>%
  filter(Country == 'Syrian Arab Rep.') %>% 
  filter(Year >= 2011 & Year <= 2017) %>%
  filter(dem.sex %in% c("Female","Male")) %>%
  group_by(Country, Year, dem.sex, dem.age) %>%
  summarise(number = sum(number))

# The age 5-17 category was not used in tracking persons of concern from South Sudan
dem_syria<-dem_syria[!(dem_syria$dem.age =="5-17"),]

# Reordering the factor categories
dem_syria$dem.age <- factor(dem_syria$dem.age, levels = c("0-4", "5-11", "12-17", "18-59", "60+"))

# Failed bar plot
ggplot(dem_syria, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "stack") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in Syrian Arab Republic')

# Failed bar plot (proportions)
ggplot(dem_syria, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "fill") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in Syrian Arab Republic')

### Afghanistan
# Cleaning for afghanistan
dem_afghan <- dem_long %>%
  filter(Country == 'Afghanistan') %>% 
  filter(Year >= 2010 & Year <= 2017) %>%
  filter(dem.sex %in% c("Female","Male")) %>%
  group_by(Country, Year, dem.sex, dem.age) %>%
  summarise(number = sum(number))

# The age 5-17 category was not used in tracking persons of concern from South Sudan
dem_afghan<-dem_afghan[!(dem_afghan$dem.age =="5-17"),]

# Reordering the factor categories
dem_afghan$dem.age <- factor(dem_afghan$dem.age, levels = c("0-4", "5-11", "12-17", "18-59", "60+"))

# Demographic bar chart
ggplot(dem_afghan, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "stack") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in Afghanistan')

# Demographic variable width bar chart
dem_afghan_sum <- dem_afghan %>% 
  filter(Year == 2016) %>%
  arrange(dem.age)

dem_afghan_sum$binwidth <- recode(dem_afghan_sum$dem.age, "0-4" = 4, "5-11" = 6, "12-17" = 5, "18-59" = 41, "60+" = 20)
dem_afghan_sum$center <- recode(dem_afghan_sum$dem.age, "0-4" = 2, "5-11" = 8, "12-17" = 14.5, "18-59" = 38.5, "60+" = 70)
dem_afghan_sum$breaks <- recode(dem_afghan_sum$dem.age, "0-4" = 4, "5-11" = 12, "12-17" = 17, "18-59" = 59, "60+" = 80)
cats <- c("0-4","0-4","5-11", "5-11","12-17","12-17", "18-59", "18-59", "60+", "60+")
total <- sum(dem_afghan_sum$number)
ggplot(dem_afghan_sum, aes(x = center, y = number / binwidth, width = binwidth)) +
  geom_col(color = "lightblue", fill = "lightblue") +
  scale_x_continuous(labels = cats, breaks = dem_afghan_sum$center) +
  facet_grid(.~dem.sex) +
  xlab('Age group') +
  ylab('Chart Area = population of persons of concern') +
  ggtitle('Children are disproportionately affected in Afghanistan') +
  theme(#axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

### Dem Rep of Congo
# Cleaning for Congo
dem_congo <- dem_long %>%
  filter(Country == 'Dem. Rep. of the Congo') %>% 
  filter(Year >= 2010 & Year <= 2017) %>%
  filter(dem.sex %in% c("Female","Male")) %>%
  group_by(Country, Year, dem.sex, dem.age) %>%
  summarise(number = sum(number))

# The age 5-17 category was not used in tracking persons of concern from South Sudan
dem_congo<-dem_congo[!(dem_congo$dem.age =="5-17"),]

# Reordering the factor categories
dem_congo$dem.age <- factor(dem_congo$dem.age, levels = c("0-4", "5-11", "12-17", "18-59", "60+"))

# Demographic bar chart
ggplot(dem_congo, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "stack") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in Democratic Republic of Congo')

# Demographic bar chart (proportions)
ggplot(dem_congo, aes(x = factor(Year), y = number / 1000)) +
  geom_bar(aes(fill = dem.age),stat = "identity", position = "fill") +
  facet_wrap(.~dem.sex) +
  xlab('Year') +
  ylab('Population of Persons of Concern (in thousands)') +
  ggtitle('Demographic breakdown of persons of concern in Democratic Republic of Congo')

# Demographic variable width bar chart
dem_congo_sum <- dem_congo %>% 
  filter(Year == 2016) %>%
  arrange(dem.age)

dem_congo_sum$binwidth <- recode(dem_congo_sum$dem.age, "0-4" = 4, "5-11" = 6, "12-17" = 5, "18-59" = 41, "60+" = 20)
dem_congo_sum$center <- recode(dem_congo_sum$dem.age, "0-4" = 2, "5-11" = 8, "12-17" = 14.5, "18-59" = 38.5, "60+" = 70)
dem_congo_sum$breaks <- recode(dem_congo_sum$dem.age, "0-4" = 4, "5-11" = 12, "12-17" = 17, "18-59" = 59, "60+" = 80)
cats <- c("0-4","0-4","5-11", "5-11","12-17","12-17", "18-59", "18-59", "60+", "60+")
total <- sum(dem_congo_sum$number)
ggplot(dem_congo_sum, aes(x = center, y = number / binwidth, width = binwidth)) +
  geom_col(color = "lightblue", fill = "lightblue") +
  scale_x_continuous(labels = cats, breaks = dem_congo_sum$center) +
  facet_grid(.~dem.sex) +
  xlab('Age group') +
  ylab('Chart Area = population of persons of concern') +
  ggtitle('Children are disproportionately affected in Democratic Republic of Congo') +
  theme(#axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
