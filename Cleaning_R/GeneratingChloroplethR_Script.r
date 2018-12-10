library("ggplot2")
library("knitr")
library("cluster")
library("tidyverse")
library("dplyr")
library("extracat")
library("RColorBrewer")
library("choroplethr")
library("choroplethrAdmin1")
library("choroplethrMaps")
library("gridExtra")

setwd("C:/Work/Columbia/EDAV/Final Project")
df<-read.csv(file="unhcr_popstats_export_time_series_all_data.csv", header=TRUE, sep=",")
continent<-read.csv(file="Country_Continent.csv", header=TRUE, sep=",")
df<-as.tbl(df)
df<-merge(df,continent,by.x ="Country_of_asylum", by.y = "Country", all.x=TRUE)
names(df)[6]<-"Continent_of_asylum"
names(df)[7]<-"Asylum_Maps"
df<-merge(df,continent,by.x ="Origin", by.y = "Country", all.x=TRUE)
names(df)[8]<-"Continent_Origin"
names(df)[9]<-"Origin_Maps"
df$Value <- as.numeric(df$Value)

CrisesOriginCountries <- c("Syrian Arab Rep.", "South Sudan", "Afghanistan", "Dem. Rep. of the Congo", "Myanmar")

CrisesOriginRefugees <- df %>%
  filter(Origin %in% CrisesOriginCountries) %>%
  filter(Population.type %in% c("Refugees (incl. refugee-like situations)","Asylum-seekers", "Internally displaced persons"))

data("country.regions");

for(country in c('afghanistan')) {
  for(i in 1990:2017) {
    df_map<- CrisesOriginRefugees %>%
      filter (Origin_Maps == country & Year == i & Asylum_Maps %in% country.regions$region) %>%
      group_by(Asylum_Maps) %>%
      summarize(value=sum(Value))
    names(df_map)[1]<-"region"
    png(paste("Images/",country,"_",i,".png",sep = ""), width = 800, height = 600)
    country_choropleth(df_map, title = "", legend = "Displaced Population", num_colors = 7,zoom=df_map$region)
    dev.off()
  }
}


