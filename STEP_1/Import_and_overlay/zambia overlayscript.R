library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_zm90 <- read_sf("geo2_zm1990.shp")
geo2_zm00 <- read_sf("geo2_zm2000.shp")
geo2_zm10 <- read_sf("geo2_zm2010.shp")
centroid_zm90 <- st_centroid(geo2_zm90)
centroid_zm00 <- st_centroid(geo2_zm00)
centroid_zm10 <- st_centroid(geo2_zm10)
AUE_ndola <- read_sf("Ndola_studyArea.shp")
AUE_ndola  <- st_transform(AUE_ndola , 4326)

ndola_90 <- geo2_zm90[st_intersection(AUE_ndola,centroid_zm90),]
ndola_90['CITY']='ndola'
ndola_00 <- geo2_zm00[st_intersection(AUE_ndola,centroid_zm00),]
ndola_00['CITY']='ndola'
ndola_10 <- geo2_zm10[st_intersection(AUE_ndola,centroid_zm10),]
ndola_10['CITY']='ndola'

plot(st_geometry(ndola_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_zm10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00053.xml")
zambia <- read_ipums_micro(ddi)

names(zambia)

##Creating field for join

zambia$IPUM1990 <- as.integer(zambia$GEO2_ZM1990)
zambia$IPUM2000 <- as.integer(zambia$GEO2_ZM2000)
zambia$IPUM2010 <- as.integer(zambia$GEO2_ZM2010)
ndola_90$IPUM1990 <- as.integer(ndola_90$IPUM1990)
ndola_00$IPUM2000 <- as.integer(ndola_00$IPUM2000)
ndola_10$IPUM2010 <- as.integer(ndola_10$IPUM2010)


##Joining by year

ndola_90 <- zambia %>% inner_join(ndola_90, by="IPUM1990")
ndola_00 <- zambia %>% inner_join(ndola_00, by="IPUM2000")
ndola_10 <- zambia %>% inner_join(ndola_10, by="IPUM2010")

names(ndola_90)
names(ndola_00)
names(ndola_10)

ndola_90 <- select(ndola_90, -c(DIST1990))
ndola_00 <- select(ndola_00, -c(DIST2000))
ndola_10 <- select(ndola_10, -c(DIST2010))

##Merging all years into one table
zambia_full <- rbind(ndola_90,ndola_00,ndola_10)
names(zambia_full)

##Excluding specific columns for the unifeied dataset
zambia_full<- select(zambia_full, -c(GEO2_ZM1990,GEO2_ZM2000,GEO2_ZM2010,IPUM1990,IPUM2000,IPUM2010))
table(zambia_full$CITY)
save(zambia_full,file="zambia_full.Rda")


