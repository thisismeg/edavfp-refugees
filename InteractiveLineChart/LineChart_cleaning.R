library(tidyverse)
Refugees <- read.csv('/Users/megalakannan/Documents/EDAV/Edav Project/TimeSeries.csv')
Refugees$Value <- as.numeric(Refugees$Value)
#rename column for easier access
colnames(Refugees)[colnames(Refugees) == 
                     "Country...territory.of.asylum.residence"] <- "Country"

# Where refugees are coming from
CrisesOriginCountries <- c("Syrian Arab Rep.", "South Sudan", "Afghanistan", "Dem. Rep. of the Congo", "Myanmar")

CrisesOriginRefugees <- Refugees %>%
  filter(Origin %in% CrisesOriginCountries) %>%
  filter(Population.type %in% c("Refugees (incl. refugee-like situations)","Asylum-seekers", "Internally displaced persons")) 

Origin_Syria <- CrisesOriginRefugees %>% filter(Origin == "Syrian Arab Rep.") %>% group_by(Year) %>% summarize(Value= sum(Value))
Origin_Sudan <- CrisesOriginRefugees %>% filter(Origin == "South Sudan") %>% group_by(Year) %>% summarize(Value= sum(Value))
Origin_Afghanistan <- CrisesOriginRefugees %>% filter(Origin == "Afghanistan") %>% group_by(Year) %>% summarize(Value= sum(Value))
Origin_Congo <- CrisesOriginRefugees %>% filter(Origin == "Dem. Rep. of the Congo") %>% group_by(Year) %>% summarize(Value= sum(Value))
Origin_Myanmar <- CrisesOriginRefugees %>% filter(Origin == "Myanmar") %>% group_by(Year) %>% summarize(Value= sum(Value))

write.csv(CrisesOriginRefugees,'/Users/megalakannan/Documents/EDAV/Edav Project/CrisesOrigin.csv')
write.csv(Origin_Syria,'/Users/megalakannan/Documents/EDAV/Edav Project/SyriaCrises.csv')
write.csv(Origin_Congo,'/Users/megalakannan/Documents/EDAV/Edav Project/CongoCrises.csv')
write.csv(Origin_Afghanistan,'/Users/megalakannan/Documents/EDAV/Edav Project/AfghanistanCrises.csv')
write.csv(Origin_Sudan,'/Users/megalakannan/Documents/EDAV/Edav Project/SudanCrises.csv')
write.csv(Origin_Myanmar,'/Users/megalakannan/Documents/EDAV/Edav Project/MyanmarCrises.csv')

# Where refugees are going now
LDRefugeeCountries <- c("Turkey", "Jordan", "Pakistan", "Lebanon","Iran", "Ethiopia", "Sudan", "Kenya", "Dem. Rep. of the Congo", "Chad", "Bangladesh")
MDRefugeeCountries <- c("Germany", "Sweden")
LargestPoplnInWorld <- c("India", "China", "United States of America") 
MediterranianCountries <- c("Greece", "Italy")

RefugeeCountries <- Refugees %>%
  filter(Country_of_asylum %in% MediterranianCountries | Country_of_asylum %in% LDRefugeeCountries | Country_of_asylum %in% MDRefugeeCountries | Country_of_asylum %in% LargestPoplnInWorld) 

RefugeeCountries <-  RefugeeCountries %>% filter(Population.type %in% c("Refugees (incl. refugee-like situations)","Asylum-seekers")) %>%
  filter(Year >= 2000) %>%
  group_by(Country_of_asylum,Year) %>%
  summarize(Value = sum(Value))

write.csv(RefugeeCountries,'/Users/megalakannan/Documents/EDAV/Edav Project/RefugeePoplnInCountries.csv')

#ggplot(RefugeeCountries, aes(Year, Value, col = Country)) +
#  geom_line()