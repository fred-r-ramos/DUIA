library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

###loading data and correcting projection
geo2_91 <- read_sf("geo2_ar1991.shp")
geo2_01 <- read_sf("geo2_ar2001.shp")
geo2_10 <- read_sf("geo2_ar2010.shp")
AUE_ba <- read_sf("Buenos_Aires_studyArea.shp")
AUE_co <- read_sf("Cordoba_studyArea.shp")
AUE_b <- st_transform(AUE_ba, 4326)
AUE_c <- st_transform(AUE_co, 4326)

### selecting by centroids
centroid_ar91 <- st_centroid(geo2_91)
centroid_ar01 <- st_centroid(geo2_01)
centroid_ar10 <- st_centroid(geo2_10)

buenos_aires_91 <- geo2_91[st_intersection(AUE_b, centroid_ar91),]
buenos_aires_91['CITY']='buenos aires'
buenos_aires_01 <- geo2_01[st_intersection(AUE_b, centroid_ar01),]
buenos_aires_01['CITY']='buenos aires'
buenos_aires_10 <- geo2_10[st_intersection(AUE_b, centroid_ar10),]
buenos_aires_10['CITY']='buenos aires'

cordoba_91 <- geo2_91[st_intersection(AUE_c, centroid_ar91),]
cordoba_91['CITY']='cordoba'
cordoba_01 <- geo2_01[st_intersection(AUE_c, centroid_ar01),]
cordoba_01['CITY']='cordoba'
cordoba_10 <- geo2_10[st_intersection(AUE_c, centroid_ar10),]
cordoba_10['CITY']='cordoba'

##### joining and plotting geometry

geo_argentina_91 <- rbind(buenos_aires_91,cordoba_91)
geo_argentina_01 <- rbind(buenos_aires_01,cordoba_01)
geo_argentina_10 <- rbind(buenos_aires_10,cordoba_10)

plot(st_geometry(geo_argentina_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00021.xml")
argentina <- read_ipums_micro(ddi)

names(argentina)

##Creating field for join

argentina$IPUM1991 <- as.integer(argentina$GEO2_AR1991)
argentina$IPUM2001 <- as.integer(argentina$GEO2_AR2001)
argentina$IPUM2010 <- as.integer(argentina$GEO2_AR2010)
geo_argentina_91$IPUM1991 <- as.integer(geo_argentina_91$IPUM1991)
geo_argentina_01$IPUM2001 <- as.integer(geo_argentina_01$IPUM2001)
geo_argentina_10$IPUM2010 <- as.integer(geo_argentina_10$IPUM2010)

##Joining by year

argentina_91 <- argentina %>% inner_join(geo_argentina_91, by="IPUM1991")
argentina_01 <- argentina %>% inner_join(geo_argentina_01, by="IPUM2001")
argentina_10 <- argentina %>% inner_join(geo_argentina_10, by="IPUM2010")

names(argentina_91)
names(argentina_01)
names(argentina_10)

argentina_91 <- select(argentina_91, -c(DEPT1991))
argentina_01 <- select(argentina_01, -c(DEPT2001))
argentina_10 <- select(argentina_10, -c(DEPT2010))

##Merging all years into one table
argentina_full <- rbind(argentina_91,argentina_01,argentina_10)
names(argentina_full)

##Excluding specific columns for the unifeied dataset
argentina_full<- select(argentina_full, -c(GEO2_AR1991,GEO2_AR2001,GEO2_AR2010,IPUM1991,IPUM2001,IPUM2010))
table(argentina_full$CITY)

##Creating the input file for merging
save(argentina_full,file="argentina_full.Rda")



