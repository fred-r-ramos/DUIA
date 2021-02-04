library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay mexico")

geo2_mx90 <- read_sf("geo2_mx1990.shp")
geo2_mx00 <- read_sf("geo2_mx2000.shp")
geo2_mx15 <- read_sf("geo2_mx2015.shp")
AUE_gu <- read_sf("Guadalajara_studyArea.shp")
AUE_me <- read_sf("Mexico_City_studyArea.shp")
AUE_re<- read_sf("reynosa_studyArea.shp")
AUE_ti <- read_sf("Tijuana_studyArea.shp")
AUE_gu <- st_transform(AUE_gu, 4326)
AUE_me <- st_transform(AUE_me, 4326)
AUE_re  <- st_transform(AUE_re, 4326)
AUE_ti  <- st_transform(AUE_ti, 4326)
 
centroid_mx90 <- st_centroid(geo2_mx90)
centroid_mx00 <- st_centroid(geo2_mx00)
centroid_mx15 <- st_centroid(geo2_mx15)

guadalajara_90 <- geo2_mx90[st_intersection(AUE_gu, centroid_mx90),]
guadalajara_90['CITY']='guadalajara'
guadalajara_00 <- geo2_mx00[st_intersection(AUE_gu, centroid_mx00),]
guadalajara_00['CITY']='guadalajara'
guadalajara_15 <- geo2_mx15[st_intersection(AUE_gu, centroid_mx15),]
guadalajara_15['CITY']='guadalajara'

mexico_90 <- geo2_mx90[st_intersection(AUE_me, centroid_mx90),]
mexico_90['CITY']='mexico'
mexico_00 <- geo2_mx00[st_intersection(AUE_me, centroid_mx00),]
mexico_00['CITY']='mexico'
mexico_15 <- geo2_mx15[st_intersection(AUE_me, centroid_mx15),]
mexico_15['CITY']='mexico'

reynosa_90 <- geo2_mx90[st_intersection(AUE_re, centroid_mx90),]
reynosa_90['CITY']='reynosa'
reynosa_00 <- geo2_mx00[st_intersection(AUE_re, centroid_mx00),]
reynosa_00['CITY']='reynosa'
reynosa_15 <- geo2_mx15[st_intersection(AUE_re, centroid_mx15),]
reynosa_15['CITY']='reynosa'

tijuana_90 <- geo2_mx90[st_intersection(AUE_ti, centroid_mx90),]
tijuana_90['CITY']='tijuana'
tijuana_00 <- geo2_mx00[st_intersection(AUE_ti, centroid_mx00),]
tijuana_00['CITY']='tijuana'
tijuana_15 <- geo2_mx15[st_intersection(AUE_ti, centroid_mx15),]
tijuana_15['CITY']='tijuana'

geo_mexico_90 <- rbind(reynosa_90,tijuana_90,mexico_90,guadalajara_90)
geo_mexico_00 <- rbind(reynosa_00,tijuana_00,mexico_00,guadalajara_00)
geo_mexico_15 <- rbind(reynosa_15,tijuana_15,mexico_15,guadalajara_15)

plot(st_geometry(geo_mexico_15), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_mx15[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00037.xml")
mexico <- read_ipums_micro(ddi)

names(mexico)

##Creating field for join

mexico$IPUM1990 <- as.integer(mexico$GEO2_MX1990)
mexico$IPUM2000 <- as.integer(mexico$GEO2_MX2000)
mexico$IPUM2015 <- as.integer(mexico$GEO2_MX2015)
geo_mexico_90$IPUM1990 <- as.integer(geo_mexico_90$IPUM1990)
geo_mexico_00$IPUM2000 <- as.integer(geo_mexico_00$IPUM2000)
geo_mexico_15$IPUM2015 <- as.integer(geo_mexico_15$IPUM2015)


##Joining by year

geo_mexico_90 <- mexico %>% inner_join(geo_mexico_90, by="IPUM1990")
geo_mexico_00 <- mexico %>% inner_join(geo_mexico_00, by="IPUM2000")
geo_mexico_15 <- mexico %>% inner_join(geo_mexico_15, by="IPUM2015")

names(geo_mexico_90)
names(geo_mexico_00)
names(geo_mexico_15)

geo_mexico_90 <- select(geo_mexico_90, -c(MUNI1990))
geo_mexico_00 <- select(geo_mexico_00, -c(MUNI2000))
geo_mexico_15 <- select(geo_mexico_15, -c(MUNI2015))

##Merging all years into one table
mexico_full <- rbind(geo_mexico_90,geo_mexico_00,geo_mexico_15)
names(mexico_full)

##Excluding specific columns for the unifeied dataset
mexico_full<- select(mexico_full, -c(GEO2_MX1990,GEO2_MX2000,GEO2_MX2015,IPUM1990,IPUM2000,IPUM2015))
table(mexico_full$CITY)
save(mexico_full,file="mexico_full.Rda")
