library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ve90 <- read_sf("geo2_ve1990.shp")
geo2_ve01 <- read_sf("geo2_ve2001.shp")
centroid_ve90 <- st_centroid(geo2_ve90)
centroid_ve01 <- st_centroid(geo2_ve01)
AUE_cabimas <- read_sf("Cabimas_studyArea.shp")
AUE_cabimas  <- st_transform(AUE_cabimas , 4326)

cabimas_90 <- geo2_ve90[st_intersection(AUE_cabimas,centroid_ve90),]
cabimas_90['CITY']='cabimas'
cabimas_01 <- geo2_ve01[st_intersection(AUE_cabimas,centroid_ve01),]
cabimas_01['CITY']='cabimas'

plot(st_geometry(cabimas_01), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ve01[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00052.xml")
venezuela <- read_ipums_micro(ddi)

names(venezuela)

##Creating field for join

venezuela$IPUM1990 <- as.integer(venezuela$GEO2_VE1990)
venezuela$IPUM2001 <- as.integer(venezuela$GEO2_VE2001)
cabimas_90$IPUM1990 <- as.integer(cabimas_90$IPUM1990)
cabimas_01$IPUM2001 <- as.integer(cabimas_01$IPUM2001)

##Joining by year

cabimas_90 <- venezuela %>% inner_join(cabimas_90, by="IPUM1990")
cabimas_01 <- venezuela %>% inner_join(cabimas_01, by="IPUM2001")

names(cabimas_90)
names(cabimas_01)

cabimas_90 <- select(cabimas_90, -c(MUNI1990))
cabimas_01 <- select(cabimas_01, -c(MUNI2001))

##Merging all years into one table
venezuela_full <- rbind(cabimas_90,cabimas_01)
names(venezuela_full)

##Excluding specific columns for the unifeied dataset
venezuela_full<- select(venezuela_full, -c(GEO2_VE1990,GEO2_VE2001,IPUM1990,IPUM2001))
table(venezuela_full$CITY)
save(venezuela_full,file="venezuela_full.Rda")
