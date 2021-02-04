library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay philippines")

geo2_ph90 <- read_sf("geo2_ph1990.shp")
geo2_ph00 <- read_sf("geo2_ph2000.shp")
geo2_ph10 <- read_sf("geo2_ph2010.shp")
centroid_ph90 <- st_centroid(geo2_ph90)
centroid_ph00 <- st_centroid(geo2_ph00)
centroid_ph10 <- st_centroid(geo2_ph10)
AUE_manila <- read_sf("Manila_studyArea.shp")
AUE_cebucity <- read_sf("Cebu_City_studyArea.shp")
AUE_bacolod <- read_sf("Bacolod_studyArea.shp")
AUE_manila  <- st_transform(AUE_manila , 4326)
AUE_cebucity  <- st_transform(AUE_cebucity , 4326)
AUE_bacolod  <- st_transform(AUE_bacolod , 4326)

bacolod_90 <- geo2_ph90[st_intersection(AUE_bacolod,centroid_ph90),]
bacolod_90['CITY']='bacolod'
bacolod_00 <- geo2_ph00[st_intersection(AUE_bacolod,centroid_ph00),]
bacolod_00['CITY']='bacolod'
bacolod_10 <- geo2_ph10[st_intersection(AUE_bacolod,centroid_ph10),]
bacolod_10['CITY']='bacolod'

cebu_90 <- geo2_ph90[st_intersection(AUE_cebucity,centroid_ph90),]
cebu_90['CITY']='cebu'
cebu_00 <-  geo2_ph00[st_intersection(AUE_cebucity,centroid_ph00),]
cebu_00['CITY']='cebu'
cebu_10 <- geo2_ph10[st_intersection(AUE_cebucity,centroid_ph10),]
cebu_10['CITY']='cebu'

manila_90 <- geo2_ph90[st_intersection(AUE_manila,centroid_ph90),]
manila_90['CITY']='manila'
manila_00 <- geo2_ph00[st_intersection(AUE_manila,centroid_ph00),]
manila_00['CITY']='manila'
manila_10 <-  geo2_ph10[st_intersection(AUE_manila,centroid_ph10),]
manila_10['CITY']='manila'

philippines_90 <- rbind(cebu_90,bacolod_90,manila_90)
philippines_00 <- rbind(cebu_00,bacolod_00,manila_00)
philippines_10 <- rbind(cebu_10,bacolod_10,manila_10)

plot(st_geometry(philippines_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ph10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00043.xml")
philippines <- read_ipums_micro(ddi)

names(philippines)

##Creating field for join

philippines$IPUM1990 <- as.integer(philippines$GEO2_PH1990)
philippines$IPUM2000 <- as.integer(philippines$GEO2_PH2000)
philippines$IPUM2010 <- as.integer(philippines$GEO2_PH2010)
philippines_90$IPUM1990 <- as.integer(philippines_90$IPUM1990)
philippines_00$IPUM2000 <- as.integer(philippines_00$IPUM2000)
philippines_10$IPUM2010 <- as.integer(philippines_10$IPUM2010)

##Joining by year

philippines_90 <- philippines %>% inner_join(philippines_90, by="IPUM1990")
philippines_00 <- philippines %>% inner_join(philippines_00, by="IPUM2000")
philippines_10 <- philippines %>% inner_join(philippines_10, by="IPUM2010")

names(philippines_90)
names(philippines_00)
names(philippines_10)

philippines_90 <- select(philippines_90, -c(MUNI1990))
philippines_00 <- select(philippines_00, -c(MUNI2000))
philippines_10 <- select(philippines_10, -c(MUNI2010))

##Merging all years into one table
philippines_full <- rbind(philippines_90,philippines_00,philippines_10)
names(philippines_full)

##Excluding specific columns for the unifeied dataset
philippines_full<- select(philippines_full, -c(GEO2_PH1990,GEO2_PH2000,GEO2_PH2010,IPUM1990,IPUM2000,IPUM2010))
table(philippines_full$CITY)
save(philippines_full,file="philippines_full.Rda")



