library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay turkey")

geo2_tr90 <- read_sf("geo2_tr1990.shp")
geo2_tr00 <- read_sf("geo2_tr2000.shp")
centroid_tr90 <- st_centroid(geo2_tr90)
centroid_tr00 <- st_centroid(geo2_tr00)
AUE_istanbul <- read_sf("Istanbul_studyArea.shp")
AUE_kayseri <- read_sf("Kayseri_studyArea.shp")
AUE_malatya <- read_sf("Malatya_studyArea.shp")
AUE_istanbul  <- st_transform(AUE_istanbul , 4326)
AUE_kayseri  <- st_transform(AUE_kayseri , 4326)
AUE_malatya  <- st_transform(AUE_malatya , 4326)

istanbul_90 <- geo2_tr90[st_intersection(AUE_istanbul,centroid_tr90),]
istanbul_90['CITY']='istanbul'

istanbul_00 <- geo2_tr00[st_intersection(AUE_istanbul,centroid_tr00),]
istanbul_00['CITY']='istanbul'

kayseri_90 <- geo2_tr90[st_intersection(AUE_kayseri,centroid_tr90),]
kayseri_90['CITY']='kayseri'

kayseri_00 <- geo2_tr00[st_intersection(AUE_kayseri,centroid_tr00),]
kayseri_00['CITY']='kayseri'

malatya_90 <- geo2_tr90[st_intersection(AUE_malatya,centroid_tr90),]
malatya_90['CITY']='malatya'

malatya_00 <- geo2_tr00[st_intersection(AUE_malatya,centroid_tr00),]
malatya_00['CITY']='malatya'

geo_turkey_90 <- rbind(istanbul_90,malatya_90,kayseri_90)
geo_turkey_00 <- rbind(istanbul_00,malatya_00,kayseri_00)

plot(st_geometry(geo_turkey_00), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_tr00[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00050.xml")
turkey <- read_ipums_micro(ddi)

names(turkey)

##Creating field for join

turkey$IPUM1990 <- as.integer(turkey$GEO2_TR1990)
turkey$IPUM2000 <- as.integer(turkey$GEO2_TR2000)
geo_turkey_90$IPUM1990 <- as.integer(geo_turkey_90$IPUM1990)
geo_turkey_00$IPUM2000 <- as.integer(geo_turkey_00$IPUM2000)

##Joining by year

geo_turkey_90  <- turkey %>% inner_join(geo_turkey_90, by="IPUM1990")
geo_turkey_00  <- turkey %>% inner_join(geo_turkey_00, by="IPUM2000")

names(geo_turkey_90)
names(geo_turkey_00)

geo_turkey_90 <- select(geo_turkey_90, -c(DIST1990))
geo_turkey_00 <- select(geo_turkey_00, -c(DIST2000))

##Merging all years into one table
turkey_full <- rbind(geo_turkey_90,geo_turkey_00)
names(turkey_full)

##Excluding specific columns for the unifeied dataset
turkey_full<- select(turkey_full, -c(GEO2_TR1990,GEO2_TR2000,IPUM1990,IPUM2000))
table(turkey_full$CITY)
save(turkey_full,file="turkey_full.Rda")




