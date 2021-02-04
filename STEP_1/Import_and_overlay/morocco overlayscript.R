library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay morocco")
geo2_ma82 <- read_sf("geo2_ma1982.shp")
geo2_ma94 <- read_sf("geo2_ma1994.shp")
geo2_ma04 <- read_sf("geo2_ma2004.shp")
centroid_ma82 <- st_centroid(geo2_ma82)
centroid_ma94 <- st_centroid(geo2_ma94)
centroid_ma04 <- st_centroid(geo2_ma04)
AUE_marrakesh <- read_sf("Marrakesh_studyArea.shp")
AUE_marrakesh  <- st_transform(AUE_marrakesh , 4326)

marrakesh_82 <- geo2_ma82[st_intersection(AUE_marrakesh,centroid_ma82),]
marrakesh_82['CITY']='marrakesh'
marrakesh_94 <- geo2_ma94[st_intersection(AUE_marrakesh,centroid_ma94),]
marrakesh_94['CITY']='marrakesh'
marrakesh_04 <- geo2_ma04[st_intersection(AUE_marrakesh,centroid_ma04),]
marrakesh_04['CITY']='marrakesh'


plot(st_geometry(marrakesh_04), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ma04[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00038.xml")
moroco <- read_ipums_micro(ddi)

names(moroco)

##Creating field for join

moroco$IPUM1982 <- as.integer(moroco$GEO2_MA1982)
moroco$IPUM1994 <- as.integer(moroco$GEO2_MA1994)
moroco$IPUM2004 <- as.integer(moroco$GEO2_MA2004)
marrakesh_82$IPUM1982 <- as.integer(marrakesh_82$IPUM1982)
marrakesh_94$IPUM1994 <- as.integer(marrakesh_94$IPUM1994)
marrakesh_04$IPUM2004 <- as.integer(marrakesh_04$IPUM2004)


##Joining by year

marrakesh_94 <- moroco %>% inner_join(marrakesh_94, by="IPUM1994")
marrakesh_04 <- moroco %>% inner_join(marrakesh_04, by="IPUM2004")

names(marrakesh_94)
names(marrakesh_04)

marrakesh_94 <- select(marrakesh_94, -c(PROV1994))
marrakesh_04 <- select(marrakesh_04, -c(PROV2004))

##Merging all years into one table
moroco_full <- rbind(marrakesh_94,marrakesh_04)
names(moroco_full)

##Excluding specific columns for the unifeied dataset
moroco_full<- select(moroco_full, -c(GEO2_MA1994,GEO2_MA2004,IPUM1994,IPUM2004))
table(moroco_full$CITY)
save(moroco_full,file="moroco_full.Rda")


