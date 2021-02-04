library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_gr91 <- read_sf("geo2_gr1991.shp")
geo2_gr01 <- read_sf("geo2_gr2001.shp")
geo2_gr11 <- read_sf("geo2_gr2011.shp")
AUE_t <- read_sf("Thessaloniki_studyArea.shp")
AUE_th <- st_transform(AUE_t, 4326)

centroid_gr91 <- st_centroid(geo2_gr91)
centroid_gr01 <- st_centroid(geo2_gr01)
centroid_gr11 <- st_centroid(geo2_gr11)

thessaloniki_91 <- st_intersection(AUE_th, geo2_gr91)
thessaloniki_91['CITY']='thessaloniki'
thessaloniki_01 <- st_intersection(AUE_th, geo2_gr01)
thessaloniki_01['CITY']='thessaloniki'
thessaloniki_11 <- st_intersection(AUE_th, geo2_gr11)
thessaloniki_11['CITY']='thessaloniki'

plot(st_geometry(thessaloniki_11), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_gr11[0], add = TRUE)

### how to include thessaliniki municipality? an we include a small part of the surrounding area?

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00033.xml")
greece <- read_ipums_micro(ddi)

names(greece)

##Creating field for join

greece$IPUM1991 <- as.integer(greece$GEO2_GR1991)
greece$IPUM2001 <- as.integer(greece$GEO2_GR2001)
greece$IPUM2011 <- as.integer(greece$GEO2_GR2011)
thessaloniki_91$IPUM1991 <- as.integer(thessaloniki_91$IPUM1991)
thessaloniki_01$IPUM2001 <- as.integer(thessaloniki_01$IPUM2001)
thessaloniki_11$IPUM2011 <- as.integer(thessaloniki_11$IPUM2011)


##Joining by year

thessaloniki_91 <- greece %>% inner_join(thessaloniki_91, by="IPUM1991")
thessaloniki_01 <- greece %>% inner_join(thessaloniki_01, by="IPUM2001")
thessaloniki_11 <- greece %>% inner_join(thessaloniki_11, by="IPUM2011")

names(thessaloniki_91)
names(thessaloniki_01)
names(thessaloniki_11)

thessaloniki_91 <- select(thessaloniki_91, -c(MUNI1991))
thessaloniki_01 <- select(thessaloniki_01, -c(MUNI2001))
thessaloniki_11 <- select(thessaloniki_11, -c(MUNI2011))

##Merging all years into one table
greece_full <- rbind(thessaloniki_91,thessaloniki_01,thessaloniki_11)
names(greece_full)

##Excluding specific columns for the unifeied dataset
greece_full<- select(greece_full, -c(GEO2_GR1991,GEO2_GR2001,GEO2_GR2011,IPUM1991,IPUM2001,IPUM2011))
table(greece_full$CITY)
save(greece_full,file="greece_full.Rda")

