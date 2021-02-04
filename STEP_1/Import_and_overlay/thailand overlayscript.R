library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay thailand")

geo2_th90 <- read_sf("geo2_th1990.shp")
geo2_th00 <- read_sf("geo2_th2000.shp")
centroid_th90 <- st_centroid(geo2_th90)
centroid_th00 <- st_centroid(geo2_th00)
AUE_bangkok <- read_sf("Bangkok_studyArea.shp")
AUE_bangkok  <- st_transform(AUE_bangkok , 4326)

bangkok_90 <- geo2_th90[st_intersection(AUE_bangkok,centroid_th90),]
bangkok_90['CITY']='bangkok'
bangkok_00 <- geo2_th00[st_intersection(AUE_bangkok,centroid_th00),]
bangkok_00['CITY']='bangkok'

plot(st_geometry(bangkok_00), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_th00[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00049.xml")
thailand <- read_ipums_micro(ddi)

names(thailand)

##Creating field for join

thailand$IPUM1990 <- as.integer(thailand$GEO2_TH1990)
thailand$IPUM2000 <- as.integer(thailand$GEO2_TH2000)
bangkok_90$IPUM1990 <- as.integer(bangkok_90$IPUM1990)
bangkok_00$IPUM2000 <- as.integer(bangkok_00$IPUM2000)


##Joining by year

bankok_90 <- thailand %>% inner_join(bankok_90, by="IPUM1990")
bankok_00 <- thailand %>% inner_join(bankok_00, by="IPUM2000")

names(bankok_90)
names(bankok_00)

bankok_90 <- select(bankok_90, -c(DIST1990))
bankok_00 <- select(bankok_00, -c(DIST2000))

##Merging all years into one table
thailand_full <- rbind(bankok_90,bankok_00)
names(thailand_full)

##Excluding specific columns for the unifeied dataset
thailand_full<- select(thailand_full, -c(GEO2_TH1990,GEO2_TH2000,IPUM1990,IPUM2000))
table(thailand_full$CITY)
save(thailand_full,file="thailand_full.Rda")




