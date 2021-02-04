library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_eg86 <- read_sf("geo2_eg1986.shp")
geo2_eg96 <- read_sf("geo2_eg1996.shp")
geo2_eg06 <- read_sf("geo2_eg2006.shp")

AUE_alexandria <- read_sf("Alexandria_studyArea.shp")
AUE_cairo <- read_sf("Cairo_studyArea.shp")
AUE_cairo <- st_transform(AUE_cairo, 4326)
AUE_alexandria <- st_transform(AUE_alexandria, 4326)

centroid_eg86 <- st_centroid(geo2_eg86)
centroid_eg96 <- st_centroid(geo2_eg96)
centroid_eg06 <- st_centroid(geo2_eg06)

cairo_86 <- geo2_eg86[st_intersection(AUE_cairo,centroid_eg86),]
cairo_86['CITY']='cairo'
cairo_96 <- geo2_eg96[st_intersection(AUE_cairo,centroid_eg96),]
cairo_96['CITY']='cairo'
cairo_06 <- geo2_eg06[st_intersection(AUE_cairo,centroid_eg06),]
cairo_06['CITY']='cairo'

alexandria_86 <- geo2_eg86[st_intersection(AUE_alexandria,centroid_eg86),]
alexandria_86['CITY']='alexandria'
alexandria_96 <- geo2_eg96[st_intersection(AUE_alexandria,centroid_eg96),]
alexandria_96['CITY']='alexandria'
alexandria_06 <- geo2_eg06[st_intersection(AUE_alexandria,centroid_eg06),]
alexandria_06['CITY']='alexandria'

geo_egypt_86 <- rbind(cairo_86,alexandria_86)
geo_egypt_96 <- rbind(cairo_96,alexandria_96)
geo_egypt_06 <- rbind(cairo_06,alexandria_06)

plot(st_geometry(geo_egypt_06), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_eg06[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00028.xml")
egypt <- read_ipums_micro(ddi)

names(egypt)

##Creating field for join

egypt$IPUM1986 <- as.integer(egypt$GEO2_EG1986)
egypt$IPUM1996 <- as.integer(egypt$GEO2_EG1996)
egypt$IPUM2006 <- as.integer(egypt$GEO2_EG2006)
geo_egypt_86$IPUM1986 <- as.integer(geo_egypt_86$IPUM1986)
geo_egypt_96$IPUM1996 <- as.integer(geo_egypt_96$IPUM1996)
geo_egypt_06$IPUM2006 <- as.integer(geo_egypt_06$IPUM2006)

##Joining by year

geo_egypt_86 <- egypt %>% inner_join(geo_egypt_86, by="IPUM1986")
geo_egypt_96 <- egypt %>% inner_join(geo_egypt_96, by="IPUM1996")
geo_egypt_06 <- egypt %>% inner_join(geo_egypt_06, by="IPUM2006")

names(geo_egypt_86)
names(geo_egypt_96)
names(geo_egypt_06)

geo_egypt_86 <- select(geo_egypt_86, -c(DIST1986))
geo_egypt_96 <- select(geo_egypt_96, -c(DIST1996))
geo_egypt_06 <- select(geo_egypt_06, -c(DIST2006))

##Merging all years into one table
egypt_full <- rbind(geo_egypt_86,geo_egypt_96,geo_egypt_06)
names(egypt_full)

##Excluding specific columns for the unifeied dataset
egypt_full<- select(egypt_full, -c(GEO2_EG1986,GEO2_EG2006,GEO2_EG1996,IPUM1986,IPUM1996,IPUM2006))
table(egypt_full$CITY)
save(egypt_full,file="egypt_full.Rda")

