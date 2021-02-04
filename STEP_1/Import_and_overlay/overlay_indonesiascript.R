library(sf)
library(tidyverse)
library(dplyr)
library(tmap)
setwd("C:/Users/Gebruiker/Documents/VIDI/microdata/indonesia/indonesia/overlayindonesia")

geo2_in90 <- read_sf("geo2_id1990.shp")
geo2_in00 <- read_sf("geo2_id2000.shp")
geo2_in10 <- read_sf("geo2_id2010.shp")
AUE_medan <- read_sf("Medan_study_area_IPUM.shp")
AUE_cirebon <- read_sf("Cirebon_study_area_IPUM.shp")
AUE_palembang <- read_sf("Palembang_study_area_IPUM.shp")

centroid_in90 <- st_centroid(geo2_in90)
centroid_in00 <- st_centroid(geo2_in00)
centroid_in10 <- st_centroid(geo2_in10)


medan_90 <- geo2_in90[st_intersection(AUE_medan, centroid_in90),]
medan_90['CITY']='medan'
medan_00 <- geo2_in00[st_intersection(AUE_medan, centroid_in00),]
medan_00['CITY']='medan'
medan_10 <- geo2_in10[st_intersection(AUE_medan, centroid_in10),]
medan_10['CITY']='medan'

cirebon_90 <- geo2_in90[st_intersection(AUE_cirebon, centroid_in90),]
cirebon_90['CITY']='cirebon'
cirebon_00 <- geo2_in00[st_intersection(AUE_cirebon, centroid_in00),]
cirebon_00['CITY']='cirebon'
cirebon_10 <- geo2_in10[st_intersection(AUE_cirebon, centroid_in10),]
cirebon_10['CITY']='cirebon'

palembang_90 <- geo2_in90[st_intersection(AUE_palembang, centroid_in90),]
palembang_90['CITY']='palembang'
palembang_00 <- geo2_in00[st_intersection(AUE_palembang, centroid_in00),]
palembang_00['CITY']='palembang'
palembang_10 <- geo2_in10[st_intersection(AUE_palembang, centroid_in10),]
palembang_10['CITY']='palembang'

geo_indonesia_90 <- rbind(medan_90,cirebon_90, palembang_90)
geo_indonesia_00 <- rbind(medan_00,cirebon_00, palembang_00)
geo_indonesia_10 <- rbind(medan_10,cirebon_10, palembang_10)

