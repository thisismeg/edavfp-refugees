library(tidyverse)
library(alluvial)
library(ggalluvial)
library(RColorBrewer)

RefugeeCountries <- read.csv('RefugeePoplnInCountries.csv')

RefugeeCountries_filter <- RefugeeCountries %>%
  filter(Year == 2016) %>% 
  filter(Origin %in% CrisesOriginCountries) %>%
  group_by(Origin, Country) %>%
  summarise(Value = sum(Value)) %>%
  filter(Value >= 5000)
p1 <- alluvial(RefugeeCountries_filter[,c(1,2)], freq = RefugeeCountries_filter$Value, col ="orange", border = "orange", hide = RefugeeCountries_filter$Value < 2000)

colourCount = length(unique(RefugeeCountries_filter$Origin))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ylab <- c(2.0, 4.0, 6.0, 8.0)
ggplot(RefugeeCountries_filter, aes(y = Value, axis1 = Origin, axis2 = Country)) +
  geom_alluvium(aes(fill = Origin), color = "black") +
  geom_stratum(width = 1/8)+
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Origin", "Country"), expand = c(.05, .05)) +
  #scale_fill_manual(values = getPalette(colourCount)) +
  ylab('Population in 2016 (millions)') + 
  ggtitle('Refugee population and origin in various countries, 2016') +
  scale_y_continuous(labels = ylab, breaks = 10^6 * ylab)
