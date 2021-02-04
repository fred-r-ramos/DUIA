library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ng07 <- read_sf("geo2_ng2007.shp")
geo2_ng10 <- read_sf("geo2_ng2010.shp")
centroid_ng07 <- st_centroid(geo2_ng07)
centroid_ng10 <- st_centroid(geo2_ng10)
AUE_gombe <- read_sf("Gombe_studyArea.shp")
AUE_lagos <- read_sf("Lagos_studyArea.shp")
AUE_ibadan <- read_sf("Ibadan_studyArea.shp")
AUE_oyo <- read_sf("Oyo_studyArea.shp")
AUE_lagos  <- st_transform(AUE_lagos , 4326)
AUE_gombe  <- st_transform(AUE_gombe , 4326)
AUE_ibadan <- st_transform(AUE_ibadan , 4326)
AUE_oyo  <- st_transform(AUE_oyo , 4326)


gombe_07 <-  geo2_ng07[st_intersection(AUE_gombe,centroid_ng07),]
gombe_07['CITY']='gombe'
gombe_10 <- geo2_ng10[st_intersection(AUE_gombe,centroid_ng10),]
gombe_10['CITY']='gombe'

lagos_07 <- geo2_ng07[st_intersection(AUE_lagos,centroid_ng07),]
lagos_07['CITY']='lagos'
lagos_10 <- geo2_ng10[st_intersection(AUE_lagos,centroid_ng10),]
lagos_10['CITY']='lagos'

ibadan_07 <- geo2_ng07[st_intersection(AUE_ibadan,centroid_ng07),]
ibadan_07['CITY']='ibadan'
ibadan_10 <- geo2_ng10[st_intersection(AUE_ibadan,centroid_ng10),]
ibadan_10['CITY']='ibadan'

oyo_07 <- geo2_ng07[st_intersection(AUE_oyo,centroid_ng07),]
oyo_07['CITY']='oyo'
oyo_10 <- geo2_ng10[st_intersection(AUE_oyo,centroid_ng10),]
oyo_10['CITY']='oyo'

geo_nigeria_07 <- rbind(gombe_07,lagos_07,ibadan_07,oyo_07)
geo_nigeria_10 <- rbind(gombe_10,lagos_10,ibadan_10,oyo_10)

plot(st_geometry(geo_nigeria_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ng10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00041.xml")
nigeria <- read_ipums_micro(ddi)

names(nigeria)

##Creating field for join

nigeria$IPUM2007 <- as.integer(nigeria$GEO2_NG2007)
nigeria$IPUM2010 <- as.integer(nigeria$GEO2_NG2010)
geo_nigeria_07$IPUM2007 <- as.integer(geo_nigeria_07$IPUM2007)
geo_nigeria_10$IPUM2010 <- as.integer(geo_nigeria_10$IPUM2010)
##Joining by year

geo_nigeria_07 <- nigeria %>% inner_join(geo_nigeria_07, by="IPUM2007")
geo_nigeria_10 <- nigeria %>% inner_join(geo_nigeria_10, by="IPUM2010")

names(geo_nigeria_07)
names(geo_nigeria_10)

geo_nigeria_07 <- select(geo_nigeria_07, -c(LAG2007))
geo_nigeria_10 <- select(geo_nigeria_10, -c(LGA2010))

##Merging all years into one table
nigeria_full <- rbind(geo_nigeria_07,geo_nigeria_10)
names(nigeria_full)

##Excluding specific columns for the unifeied dataset
nigeria_full<- select(nigeria_full, -c(GEO2_NG2007,GEO2_NG2010,IPUM2007,IPUM2010))
table(nigeria_full$CITY)
save(nigeria_full,file="nigeria_full.Rda")
