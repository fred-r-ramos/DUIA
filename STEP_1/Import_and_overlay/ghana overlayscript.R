library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_gh00 <- read_sf("geo2_gh2000.shp")
geo2_gh10 <- read_sf("geo2_gh2010.shp")
centroid_gh00 <- st_centroid(geo2_gh00)
centroid_gh10 <- st_centroid(geo2_gh10)
AUE_accra <- read_sf("Accra_studyArea.shp")
AUE_accra <- st_transform(AUE_accra, 4326)

accra_00 <- geo2_gh00[st_intersection(AUE_accra,centroid_gh00),]
accra_00['CITY']='accra'
accra_10 <- geo2_gh10[st_intersection(AUE_accra,centroid_gh10),]
accra_10['CITY']='accra'


plot(st_geometry(accra_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_gh10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00032.xml")
ghana <- read_ipums_micro(ddi)

names(ghana)

##Creating field for join

ghana$IPUM2000 <- as.integer(ghana$GEO2_GH2000)
ghana$IPUM2010 <- as.integer(ghana$GEO2_GH2010)

accra_00$IPUM2000 <- as.integer(accra_00$IPUM2000)
accra_10$IPUM2010 <- as.integer(accra_10$IPUM2010)

##Joining by year

accra_00 <- ghana %>% inner_join(accra_00, by="IPUM2000")
accra_10 <- ghana %>% inner_join(accra_10, by="IPUM2010")

names(accra_00)
names(accra_10)

accra_00 <- select(accra_00, -c(DIST2000))
accra_10 <- select(accra_10, -c(DIST2010))

##Merging all years into one table
ghana_full <- rbind(accra_00,accra_10)
names(ghana_full)

##Excluding specific columns for the unifeied dataset
ghana_full<- select(ghana_full, -c(GEO2_GH2000,GEO2_GH2010,IPUM2000,IPUM2010))
table(ghana_full$CITY)
save(ghana_full,file="ghana_full.Rda")



