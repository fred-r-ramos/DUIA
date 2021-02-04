library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay el salvador")

geo2_sv92 <- read_sf("geo2_sv1992.shp")
geo2_sv07 <- read_sf("geo2_sv2007.shp")
AUE_sansalvador <- read_sf("San_Salvador_studyArea.shp")
AUE_sansalvador  <- st_transform(AUE_sansalvador , 4326)


centroid_sv92 <- st_centroid(geo2_sv92)
centroid_sv07 <- st_centroid(geo2_sv07)

san_salvador_92 <- geo2_sv92[st_intersection(AUE_sansalvador,centroid_sv92),]
san_salvador_92['CITY']='san salvador'
san_salvador_07 <- geo2_sv07[st_intersection(AUE_sansalvador,centroid_sv07),]
san_salvador_07['CITY']='san salvador'


plot(st_geometry(san_salvador_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_sv07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00029.xml")
el_salvador <- read_ipums_micro(ddi)

names(el_salvador)

##Creating field for join

el_salvador$IPUM1992 <- as.integer(el_salvador$GEO2_SV1992)
el_salvador$IPUM2007 <- as.integer(el_salvador$GEO2_SV2007)
san_salvador_92$IPUM1992 <- as.integer(san_salvador_92$IPUM1992)
san_salvador_07$IPUM2007 <- as.integer(san_salvador_07$IPUM2007)

##Joining by year

san_salvador_92 <- el_salvador %>% inner_join(san_salvador_92, by="IPUM1992")
san_salvador_07 <- el_salvador %>% inner_join(san_salvador_07, by="IPUM2007")

names(san_salvador_92)
names(san_salvador_07)

san_salvador_92 <- select(san_salvador_92, -c(MUNI1992))
san_salvador_07 <- select(san_salvador_07, -c(MUNI2007))

##Merging all years into one table
el_salvador_full <- rbind(san_salvador_92,san_salvador_07)
names(el_salvador_full)

##Excluding specific columns for the unifeied dataset
el_salvador_full<- select(el_salvador_full, -c(GEO2_SV1992,GEO2_SV2007,IPUM1992,IPUM2007))
table(el_salvador_full$CITY)
save(el_salvador_full,file="el_salvador_full.Rda")