library(sf)
library(tidyverse)
library(dplyr)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay Bangladesh")

geo2_bd1991 <- read_sf("geo2_bd1991.shp")
geo2_bd2001 <- read_sf("geo2_bd2001.shp")
geo2_bd2011 <- read_sf("geo2_bd2011.shp")
AUE_dhaka <- read_sf("Dhaka_studyArea.shp")
AUE_rajshahi <- read_sf("Rajshahi_studyArea.shp")
AUE_dhaka <- st_transform(AUE_dhaka, 4326)
AUE_rajshahi <- st_transform(AUE_rajshahi, 4326)

centroid_bd1991 <- st_centroid(geo2_bd1991)
centroid_bd2001 <- st_centroid(geo2_bd2001)
centroid_bd2011 <- st_centroid(geo2_bd2011)

dhaka_91 <- geo2_bd1991[st_intersection(AUE_dhaka,centroid_bd1991),]
dhaka_91['CITY']='dhaka'
dhaka_01 <- geo2_bd2001[st_intersection(AUE_dhaka,centroid_bd2001),]
dhaka_01['CITY']='dhaka'
dhaka_11 <- geo2_bd2011[st_intersection(AUE_dhaka,centroid_bd2011),]
dhaka_11['CITY']='dhaka'
rajshahi_91 <- geo2_bd1991[st_intersection(AUE_rajshahi,centroid_bd1991),]
rajshahi_91['CITY']='rajshahi'
rajshahi_01 <- geo2_bd2001[st_intersection(AUE_rajshahi,centroid_bd2001),]
rajshahi_01['CITY']='rajshahi'
rajshahi_11 <- geo2_bd2011[st_intersection(AUE_rajshahi,centroid_bd2011),]
rajshahi_11['CITY']='rajshahi'

geo_bangladesh_91 <- rbind(dhaka_91,rajshahi_91)
geo_bangladesh_01 <- rbind(dhaka_01,rajshahi_01)
geo_bangladesh_11 <- rbind(dhaka_11,rajshahi_11)

plot(st_geometry(geo_bangladesh_11), expandBB = c(1, 0.1, 1, 0.1), col = "red", lwd = 3)
plot(geo2_bd2011[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00022.xml")
bangladesh <- read_ipums_micro(ddi)

##Creating field for join

bangladesh$IPUM1991 <- as.integer(bangladesh$GEO2_BD1991)
bangladesh$IPUM2001 <- as.integer(bangladesh$GEO2_BD2001)
bangladesh$IPUM2011 <- as.integer(bangladesh$GEO2_BD2011)

geo_bangladesh_91$IPUM1991 <- as.integer(geo_bangladesh_91$IPUM1991)
geo_bangladesh_01$IPUM2001 <- as.integer(geo_bangladesh_01$IPUM2001)
geo_bangladesh_11$IPUM2011 <- as.integer(geo_bangladesh_11$IPUM2011)

##Joining by year

bangladesh_91 <- bangladesh %>% inner_join(geo_bangladesh_91, by="IPUM1991")
bangladesh_01 <- bangladesh %>% inner_join(geo_bangladesh_01, by="IPUM2001")
bangladesh_11 <- bangladesh %>% inner_join(geo_bangladesh_11, by="IPUM2011")

names(bangladesh_91)
names(bangladesh_01)
names(bangladesh_11)

bangladesh_91 <- select(bangladesh_91, -c(ZILL1991))
bangladesh_01 <- select(bangladesh_01, -c(ZILL2001))
bangladesh_11 <- select(bangladesh_11, -c(ZILL2011))


##Merging all years into one table
bangladesh_full <- rbind(bangladesh_91,bangladesh_01,bangladesh_11)
names(bangladesh_full)

##Excluding specific columns for the unifeied dataset
bangladesh_full<- select(bangladesh_full, -c(GEO2_BD1991,GEO2_BD2001,GEO2_BD2011,IPUM1991,IPUM2001,IPUM2011))
table(bangladesh_full$CITY)
save(bangladesh_full,file="bangladesh_full.Rda")

