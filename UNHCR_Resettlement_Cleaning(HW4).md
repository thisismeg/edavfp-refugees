EDAV HW4Q3
================
Nanshan Li (UNI: nl2643)
November 14, 2018

EDAV HW 4 Q3
------------

The dataset my group chose was from the United Nations High Commissioner for Refugees (UNHCR), and the variable I was tasked to look at was the resettlement of refugees from 1959 to 2017.

``` r
library(tidyverse)
library(GGally)
options(scipen=999)
resettlement <- read_csv('UNHCR_Resettlement.csv')
colnames(resettlement)[colnames(resettlement) == 
                         "Country / territory of asylum/residence"] <- "Country"
```

The column name `Country / territory of asylum/residence` was renamed so that the variable can be accessed more easily. The countries to which the most refugees were resettled in were extracted and a time series chart was plotted for the data.

Plot 1
------

``` r
resettlement_bycountry <- resettlement %>%
  group_by(Country, Year) %>%
  summarise(Value = sum(Value))

topcountries <- c("United States of America", "Canada", "Australia", 
                  "United Kingdom of Great Britain and Northern Ireland", "Sweden")

resettlement_bycountry_subset <- resettlement_bycountry %>%
  filter(Country %in% topcountries)
```

``` r
ggplot(resettlement_bycountry_subset, aes(x = Year, y = Value, col = Country)) + 
  geom_line(size = 1) +
  scale_x_continuous(limits=c(1982,2016)) + 
  ggtitle('Top 5 countries in which refugees resettle over 1982 to 2016') +
  ylab('Number of people')
```

![](UNHCR_Resettlement_Cleaning_files/figure-markdown_github/unnamed-chunk-3-1.png) From this plot, USA seems to accept far more refugees for resettlement that all other countries in the dataset. Additionally, Canada and Australia also accept significantly more refugees than the 4th and 5th ranked country, although far less than that of USA in the dataset. It will be interesting to plot the remaining countries to spot further "bins" of values.

Plot 2
------

``` r
resettlement_byorigin <- resettlement %>%
  group_by(Origin, Year) %>%
  summarise(Value = sum(Value))

resettlement_byorigin_subset <- resettlement_byorigin %>%
  group_by(Origin) %>%
  summarise(Value = sum(Value)) %>%
  arrange(desc(Value))

top10countries <- c("Viet Nam", "Russian Federation", "Iraq", "Various/unknown", 
                    "Myanmar", "Bosnia and Herzegovina", "Somalia", "Lao People's Dem. Rep.", 
                    "Iran (Islamic Rep. of)", "Afghanistan")

resettlement_byorigin_subset <- resettlement_byorigin %>%
  filter(Origin %in% top10countries)
```

``` r
ggplot(resettlement_byorigin_subset, aes(x = Year, y = Value, col = Origin)) +
  geom_line(size = 0.8) + 
  ggtitle('Top 10 countries in which resettling refugees originate from (1959 - 2017)') +
  ylab('Number of people')
```

![](UNHCR_Resettlement_Cleaning_files/figure-markdown_github/unnamed-chunk-5-1.png) This plot shows the origin country of resettling refugees in a time series. Through this plot, the years in which various countries face different situations that result in a refugee situation can be seen. For example, after the Vietnam War, Cold War, the civil war in Bosnia and Herzegovina and Laos and the Iraq war.

Plot 3
------

``` r
resettlement_flow <- resettlement %>%
  group_by(Country, Origin) %>%
  summarise(Value = sum(Value))

resettlement_flow_subset <- resettlement %>%
  filter(Country %in% topcountries) %>%
  filter(Origin %in% top10countries) %>%
  filter(!(Origin %in% "Various/unknown")) %>%
  arrange(Year)

library(alluvial)
```

Origin and Destination countries of resettled refugees
======================================================

Orange: resettlement year after 2000 Gray: resettlement year before or in 2000

``` r
alluvial(resettlement_flow_subset[,c(2,1)], freq = resettlement_flow_subset$Value, col = ifelse(resettlement_flow_subset$Year > 2000, "orange", "grey"), border = ifelse(resettlement_flow_subset$Year > 2000, "orange", "grey"))
```

![](UNHCR_Resettlement_Cleaning_files/figure-markdown_github/unnamed-chunk-7-1.png)

This alluvial plot shows the origin and destination of refugees that were resettled. From this plot, the proportion of refugees from each country that resettled in the top 5 countries for refugee resettlements can be seen. USA took in the majority of resettled refugees from all countries other than Afghanistan.
