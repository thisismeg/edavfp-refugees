library(tidyverse)
library(alluvial)
library(ggalluvial)

RefugeeCountries <- read.csv('RefugeePoplnInCountries.csv')

RefugeeCountries_filter <- RefugeeCountries %>%
  filter(Year == 2016) %>% 
  filter(Origin %in% CrisesOriginCountries) %>%
  group_by(Origin, Country) %>%
  summarise(Value = sum(Value)) %>%
  filter(Value >= 5000)
p1 <- alluvial(RefugeeCountries_filter[,c(1,2)], freq = RefugeeCountries_filter$Value, col ="orange", border = "orange", hide = RefugeeCountries_filter$Value < 2000)

ggplot(RefugeeCountries_filter, aes(y = Value, axis1 = Origin, axis2 = Country)) +
  geom_alluvium(aes(fill = Origin)) +
  geom_stratum(width = 1/2)+
  #geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Origin", "Country"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") 
