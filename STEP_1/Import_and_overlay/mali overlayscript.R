library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ml87 <- read_sf("geo2_ml1987.shp")
geo2_ml98 <- read_sf("geo2_ml1998.shp")
geo2_ml09 <- read_sf("geo2_ml2009.shp")
centroid_ml87 <- st_centroid(geo2_ml87)
centroid_ml98 <- st_centroid(geo2_ml98)
centroid_ml09 <- st_centroid(geo2_ml09)
AUE_bamako <- read_sf("bamako_studyArea.shp")
AUE_bamako <- st_transform(AUE_bamako, 4326)


bamako_87 <- geo2_ml87[st_intersection(AUE_bamako,centroid_ml87),]
bamako_87['CITY']='bamako'
bamako_98 <- geo2_ml98[st_intersection(AUE_bamako,centroid_ml98),]
bamako_98['CITY']='bamako'
bamako_09 <- geo2_ml09[st_intersection(AUE_bamako,centroid_ml09),]
bamako_09['CITY']='bamako'


plot(st_geometry(bamako_09), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ml09[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00036.xml")
mali <- read_ipums_micro(ddi)

names(mali)

##Creating field for join

mali$IPUM1987 <- as.integer(mali$GEO2_ML1987)
mali$IPUM1998 <- as.integer(mali$GEO2_ML1998)
mali$IPUM2009 <- as.integer(mali$GEO2_ML2009)
bamako_87$IPUM1987 <- as.integer(bamako_87$IPUM1987)
bamako_98$IPUM1998 <- as.integer(bamako_98$IPUM1998)
bamako_09$IPUM2009 <- as.integer(bamako_09$IPUM2009)

##Joining by year

bamako_87 <- mali %>% inner_join(bamako_87, by="IPUM1987")
bamako_98 <- mali %>% inner_join(bamako_98, by="IPUM1998")
bamako_09 <- mali %>% inner_join(bamako_09, by="IPUM2009")

names(bamako_87)
names(bamako_98)
names(bamako_09)

bamako_87 <- select(bamako_87, -c(CIRC1987))
bamako_98 <- select(bamako_98, -c(CIRC1998))
bamako_09 <- select(bamako_09, -c(CIRC2009))

##Merging all years into one table
mali_full <- rbind(bamako_87,bamako_98,bamako_09)
names(mali_full)

##Excluding specific columns for the unifeied dataset
mali_full<- select(mali_full, -c(GEO2_ML1987,GEO2_ML1998,GEO2_ML2009,IPUM1987,IPUM1998,IPUM2009))
table(mali_full$CITY)
save(mali_full,file="mali_full.Rda")


