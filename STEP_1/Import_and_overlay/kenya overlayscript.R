library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ke89 <- read_sf("geo2_ke1989.shp")
geo2_ke99 <- read_sf("geo2_ke1999.shp")
geo2_ke09 <- read_sf("geo2_ke2009.shp")
centroid_ke89 <- st_centroid(geo2_ke89)
centroid_ke99 <- st_centroid(geo2_ke99)
centroid_ke09 <- st_centroid(geo2_ke09)
AUE_nakuru <- read_sf("Nakuru_studyArea.shp")
AUE_nakuru <- st_transform(AUE_nakuru, 4326)


nakuru_1989 <- geo2_ke89[st_intersection(AUE_nakuru,centroid_ke89),]
nakuru_1989['CITY']='nakuru'
nakuru_1999 <- geo2_ke99[st_intersection(AUE_nakuru,centroid_ke99),]
nakuru_1999['CITY']='nakuru'
nakuru_2009 <- geo2_ke09[st_intersection(AUE_nakuru,centroid_ke09),]
nakuru_2009['CITY']='nakuru'

plot(st_geometry(nakuru_2009), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ke09[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00035.xml")
kenya <- read_ipums_micro(ddi)

names(kenya)

##Creating field for join

kenya$IPUM1989 <- as.integer(kenya$GEO2_KE1989)
kenya$IPUM1999 <- as.integer(kenya$GEO2_KE1999)
kenya$IPUM2009 <- as.integer(kenya$GEO2_KE2009)
nakuru_1989$IPUM1989 <- as.integer(nakuru_1989$IPUM1989)
nakuru_1999$IPUM1999 <- as.integer(nakuru_1999$IPUM1999)
nakuru_2009$IPUM2009 <- as.integer(nakuru_2009$IPUM2009)

##Joining by year

nakuru_1989 <- kenya %>% inner_join(nakuru_1989, by="IPUM1989")
nakuru_1999 <- kenya %>% inner_join(nakuru_1999, by="IPUM1999")
nakuru_2009 <- kenya %>% inner_join(nakuru_2009, by="IPUM2009")

names(nakuru_1989)
names(nakuru_1999)
names(nakuru_2009)

nakuru_1989 <- select(nakuru_1989, -c(DIST1989))
nakuru_1999 <- select(nakuru_1999, -c(DIST1999))
nakuru_2009 <- select(nakuru_2009, -c(DIST2009))

##Merging all years into one table
kenya_full <- rbind(nakuru_1989,nakuru_1999,nakuru_2009)
names(kenya_full)

##Excluding specific columns for the unifeied dataset
kenya_full<- select(kenya_full, -c(GEO2_KE1989,GEO2_KE1999,GEO2_KE2009,IPUM1989,IPUM1999,IPUM2009))
table(kenya_full$CITY)
save(kenya_full,file="kenya_full.Rda")
