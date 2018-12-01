# Final cleaning

library(tidyverse)
Refugees <- read.csv('TimeSeries.csv')

#rename column for easier access
colnames(Refugees)[colnames(Refugees) == 
                         "Country...territory.of.asylum.residence"] <- "Country"

# Where refugees are coming from
CrisesOriginCountries <- c("Syrian Arab Rep.", "South Sudan", "Afghanistan", "Dem. Rep. of the Congo", "Myanmar")

CrisesOriginRefugees <- Refugees %>%
  filter(Origin %in% CrisesOriginCountries) %>%
  filter(Population.type %in% c("Refugees (incl. refugee-like situations)","Asylum-seekers", "Internally displaced persons")) 

write.csv(CrisesOriginRefugees,'CrisesOrigin.csv')

# Where refugees are going now
LDRefugeeCountries <- c("Turkey", "Jordan", "Pakistan", "Lebanon","Iran", "Ethiopia", "Sudan", "Kenya", "Dem. Rep. of the Congo", "Chad")
MDRefugeeCountries <- c("Germany", "Sweden")
LargestPoplnInWorld <- c("India", "China", "United States of America") 
MediterranianCountries <- c("Greece", "Italy")

RefugeeCountries <- Refugees %>%
  filter(Country %in% MediterranianCountries | Country %in% LDRefugeeCountries | Country %in% MDRefugeeCountries | Country %in% LargestPoplnInWorld) %>%
  filter(Population.type %in% c("Refugees (incl. refugee-like situations)","Asylum-seekers")) #%>% # cannot add IDP to this list
  #filter(Year >= 2000) %>%
  #group_by(Country,Year) %>%
  #summarize(Value = sum(Value))

write.csv(RefugeeCountries,'RefugeePoplnInCountries.csv')

#ggplot(RefugeeCountries, aes(Year, Value, col = Country)) +
#  geom_line()

