library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay sudan")

geo2_sd08 <- read_sf("geo2_sd2008.shp")
centroid_sd08 <- st_centroid(geo2_sd08)
AUE_khartoum <- read_sf("Khartoum_studyArea.shp")
AUE_khartoum  <- st_transform(AUE_khartoum , 4326)
khartoum_08 <- geo2_sd08[st_intersection(AUE_khartoum,centroid_sd08),]
khartoum_08['CITY']='khartoum'

plot(st_geometry(khartoum_08), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_sd08[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00047.xml")
sudan <- read_ipums_micro(ddi)

names(sudan)

##Creating field for join

sudan$IPUM2008 <- as.integer(sudan$GEO2_SD2008)
khartoum_08$IPUM2008 <- as.integer(khartoum_08$IPUM2008)

##Joining by year

khartoum_08 <- sudan %>% inner_join(khartoum_08, by="IPUM2008")

names(khartoum_08)

khartoum_08 <- select(khartoum_08, -c(CNTY2008))

sudan_full <- khartoum_08
names(sudan_full)

##Excluding specific columns for the unifeied dataset
sudan_full<- select(sudan_full, -c(GEO2_SD2008,IPUM2008))
table(sudan_full$CITY)
save(sudan_full,file="sudan_full.Rda")






