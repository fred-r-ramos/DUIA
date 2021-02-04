library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_et84 <- read_sf("geo2_et1984.shp")
geo2_et94 <- read_sf("geo2_et1994.shp")
geo2_et07 <- read_sf("geo2_et2007.shp")
AUE_addisababa <- read_sf("Addis_Ababa_studyArea.shp")
AUE_addisababa <- st_transform(AUE_addisababa, 4326)


centroid_et84 <- st_centroid(geo2_et84)
centroid_et94 <- st_centroid(geo2_et94)
centroid_et07 <- st_centroid(geo2_et07)

addis_ababa_84 <- geo2_et84[st_intersection(AUE_addisababa,centroid_et84),]
addis_ababa_84['CITY']='addis ababa'
addis_ababa_94 <-  geo2_et94[st_intersection(AUE_addisababa,centroid_et94),]
addis_ababa_94['CITY']='addis ababa'
addis_ababa_07 <-  geo2_et07[st_intersection(AUE_addisababa,centroid_et07),]
addis_ababa_07['CITY']='addis ababa'


plot(st_geometry(addis_ababa_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_et07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00030.xml")
ethiopia <- read_ipums_micro(ddi)

names(ethiopia)

##Creating field for join

ethiopia$IPUM1984 <- as.integer(ethiopia$GEO2_ET1984)
ethiopia$IPUM1994 <- as.integer(ethiopia$GEO2_ET1994)
ethiopia$IPUM2007 <- as.integer(ethiopia$GEO2_ET2007)
addis_ababa_84$IPUM1984 <- as.integer(addis_ababa_84$IPUM1984)
addis_ababa_94$IPUM1994 <- as.integer(addis_ababa_94$IPUM1994)
addis_ababa_07$IPUM2007 <- as.integer(addis_ababa_07$IPUM2007)


##Joining by year

addis_ababa_84 <- ethiopia %>% inner_join(addis_ababa_84, by="IPUM1984")
addis_ababa_94 <- ethiopia %>% inner_join(addis_ababa_94, by="IPUM1994")
addis_ababa_07 <- ethiopia %>% inner_join(addis_ababa_07, by="IPUM2007")

names(addis_ababa_84)
names(addis_ababa_94)
names(addis_ababa_07)

addis_ababa_84 <- select(addis_ababa_84, -c(AWRJ1984))
addis_ababa_94 <- select(addis_ababa_94, -c(ZONE1994))
addis_ababa_07 <- select(addis_ababa_07, -c(ZONE2007))

##Merging all years into one table
ethiopia_full <- rbind(addis_ababa_84,addis_ababa_94,addis_ababa_07)
names(ethiopia_full)


##Excluding specific columns for the unifeied dataset
ethiopia_full<- select(ethiopia_full, -c(GEO2_ET1984,GEO2_ET1994,GEO2_ET2007,IPUM1984,IPUM1994,IPUM2007))
table(ethiopia_full$CITY)
save(ethiopia_full,file="ethiopia_full.Rda")
