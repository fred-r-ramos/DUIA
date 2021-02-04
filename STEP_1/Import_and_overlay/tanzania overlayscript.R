library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay tanzania")

geo2_tz88 <- read_sf("geo2_tz1988.shp")
geo2_tz02 <- read_sf("geo2_tz2002.shp")
geo2_tz12 <- read_sf("geo2_tz2012.shp")
centroid_tz88 <- st_centroid(geo2_tz88)
centroid_tz02 <- st_centroid(geo2_tz02)
centroid_tz12 <- st_centroid(geo2_tz12)
AUE_arusha <- read_sf("Arusha_studyArea.shp")
AUE_arusha  <- st_transform(AUE_arusha , 4326)

arusha_88 <- geo2_tz88[st_intersection(AUE_arusha,centroid_tz88),]
arusha_88['CITY']='arusha'
arusha_02 <- geo2_tz02[st_intersection(AUE_arusha,centroid_tz02),]
arusha_02['CITY']='arusha'
arusha_12 <- geo2_tz12[st_intersection(AUE_arusha,centroid_tz12),]
arusha_12['CITY']='arusha'

plot(st_geometry(arusha_12), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_tz12[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00048.xml")
tanzania <- read_ipums_micro(ddi)

names(tanzania)

##Creating field for join

tanzania$IPUM1988 <- as.integer(tanzania$GEO2_TZ1988)
tanzania$IPUM2002 <- as.integer(tanzania$GEO2_TZ2002)
tanzania$IPUM2012 <- as.integer(tanzania$GEO2_TZ2012)
arusha_88$IPUM1988 <- as.integer(arusha_88$IPUM1988)
arusha_02$IPUM2002 <- as.integer(arusha_02$IPUM2002)
arusha_12$IPUM2012 <- as.integer(arusha_12$IPUM2012)


##Joining by year

arusha_88 <- tanzania %>% inner_join(arusha_88, by="IPUM1988")
arusha_02 <- tanzania %>% inner_join(arusha_02, by="IPUM2002")
arusha_12 <- tanzania %>% inner_join(arusha_12, by="IPUM2012")

names(arusha_88)
names(arusha_02)
names(arusha_12)

arusha_88 <- select(arusha_88, -c(DIST1988))
arusha_02 <- select(arusha_02, -c(DIST2002))
arusha_12 <- select(arusha_12, -c(DIST2012))

##Merging all years into one table
tanzania_full <- rbind(arusha_88,arusha_02,arusha_12)
names(tanzania_full)

##Excluding specific columns for the unifeied dataset
tanzania_full<- select(tanzania_full, -c(GEO2_TZ1998,GEO2_TZ2002,GEO2_TZ2012,IPUM1988,IPUM2002,IPUM2012))
table(tanzania_full$CITY)
save(tanzania_full,file="tanzania_full.Rda")



