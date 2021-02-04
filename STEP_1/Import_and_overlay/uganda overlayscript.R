library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ug91 <- read_sf("geo2_ug1991.shp")
geo2_ug02 <- read_sf("geo2_ug2002.shp")
centroid_ug91 <- st_centroid(geo2_ug91)
centroid_ug02 <- st_centroid(geo2_ug02)
AUE_kampala <- read_sf("Kampala_studyArea.shp")
AUE_kampala  <- st_transform(AUE_kampala , 4326)

kampala_91 <- geo2_ug91[st_intersection(AUE_kampala,centroid_ug91),]
kampala_91['CITY']='kampala'
kampala_02 <- geo2_ug02[st_intersection(AUE_kampala,centroid_ug02),]
kampala_02['CITY']='kampala'

plot(st_geometry(kampala_02), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ug02[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00051.xml")
uganda <- read_ipums_micro(ddi)

names(uganda)

##Creating field for join

uganda$IPUM1991 <- as.integer(uganda$GEO2_UG1991)
uganda$IPUM2002 <- as.integer(uganda$GEO2_UG2002)
kampala_91$IPUM1991 <- as.integer(kampala_91$IPUM1991)
kampala_02$IPUM2002 <- as.integer(kampala_02$IPUM2002)

##Joining by year

kampala_91 <- uganda %>% inner_join(kampala_91, by="IPUM1991")
kampala_02 <- uganda %>% inner_join(kampala_02, by="IPUM2002")

names(kampala_91)
names(kampala_02)

kampala_91 <- select(kampala_91, -c(CNTY1991))
kampala_02 <- select(kampala_02, -c(CNTY2002))

##Merging all years into one table
uganda_full <- rbind(kampala_91,kampala_02)
names(uganda_full)

##Excluding specific columns for the unifeied dataset
uganda_full<- select(uganda_full, -c(GEO2_UG1991,GEO2_UG2002,IPUM1991,IPUM2002))
table(uganda_full$CITY)
save(uganda_full,file="uganda_full.Rda")
