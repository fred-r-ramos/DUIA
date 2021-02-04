library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_es91 <- read_sf("geo2_es1991.shp")
geo2_es01 <- read_sf("geo2_es2001.shp")
geo2_es11 <- read_sf("geo2_es2011.shp")
centroid_es91 <- st_centroid(geo2_es91)
centroid_es01 <- st_centroid(geo2_es01)
centroid_es11 <- st_centroid(geo2_es11)

AUE_madrid <- read_sf("Madrid_studyArea.shp")
AUE_madrid  <- st_transform(AUE_madrid , 4326)

madrid_91 <- geo2_es91[st_intersection(AUE_madrid,centroid_es91),]
madrid_91['CITY']='madrid'
madrid_01 <- geo2_es01[st_intersection(AUE_madrid,centroid_es01),]
madrid_01['CITY']='madrid'
madrid_11 <- geo2_es11[st_intersection(AUE_madrid,centroid_es11),]
madrid_11['CITY']='madrid'

plot(st_geometry(madrid_11), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_es11[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00046.xml")
spain <- read_ipums_micro(ddi)

names(spain)

##Creating field for join

spain$IPUM1991 <- as.integer(spain$GEO2_ES1991)
spain$IPUM2001 <- as.integer(spain$GEO2_ES2001)
spain$IPUM2011 <- as.integer(spain$GEO2_ES2011)
madrid_91$IPUM1991 <- as.integer(madrid_91$IPUM1991)
madrid_01$IPUM2001 <- as.integer(madrid_01$IPUM2001)
madrid_11$IPUM2011 <- as.integer(madrid_11$IPUM2011)

##Joining by year

madrid_91 <- spain %>% inner_join(madrid_91, by="IPUM1991")
madrid_01 <- spain %>% inner_join(madrid_01, by="IPUM2001")
madrid_11 <- spain %>% inner_join(madrid_11, by="IPUM2011")


madrid_91 <- select(madrid_91, -c(PROV1991))
madrid_01 <- select(madrid_01, -c(PROV2001))
madrid_11 <- select(madrid_11, -c(PROV2011))

names(madrid_91)
names(madrid_01)
names(madrid_11)


##Merging all years into one table
spain_full <- rbind(madrid_91,madrid_01,madrid_11)
names(spain_full)

##Excluding specific columns for the unifeied dataset
spain_full<- select(spain_full, -c(GEO2_ES1991,GEO2_ES2001,GEO2_ES2011,IPUM1991,IPUM2001,IPUM2011))
table(spain_full$CITY)
save(spain_full,file="spain_full.Rda")


