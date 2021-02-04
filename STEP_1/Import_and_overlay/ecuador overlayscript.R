library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_ec90 <- read_sf("geo2_ec1990.shp")
geo2_ec01 <- read_sf("geo2_ec2001.shp")
geo2_ec10 <- read_sf("geo2_ec2010.shp")
AUE_quito <- read_sf("quito_studyArea.shp")
AUE_quito <- st_transform(AUE_quito, 4326)

centroid_ec90 <- st_centroid(geo2_ec90)
centroid_ec01 <- st_centroid(geo2_ec01)
centroid_ec10 <- st_centroid(geo2_ec10)

quito_90 <- geo2_ec90[st_intersection(AUE_quito,centroid_ec90),]
quito_90['CITY']='quito'
quito_01 <- geo2_ec01[st_intersection(AUE_quito,centroid_ec01),]
quito_01['CITY']='quito'
quito_10 <- geo2_ec10[st_intersection(AUE_quito,centroid_ec10),]
quito_10['CITY']='quito'


plot(st_geometry(quito_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ec10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00027.xml")
ecuador <- read_ipums_micro(ddi)

names(ecuador)

##Creating field for join

ecuador$IPUM1990 <- as.integer(ecuador$GEO2_EC1990)
ecuador$IPUM2001 <- as.integer(ecuador$GEO2_EC2001)
ecuador$IPUM2010 <- as.integer(ecuador$GEO2_EC2010)
quito_90$IPUM1990 <- as.integer(quito_90$IPUM1990)
quito_01$IPUM2001 <- as.integer(quito_01$IPUM2001)
quito_10$IPUM2010 <- as.integer(quito_10$IPUM2010)

##Joining by year

quito_90 <- ecuador %>% inner_join(quito_90, by="IPUM1990")
quito_01 <- ecuador %>% inner_join(quito_01, by="IPUM2001")
quito_10 <- ecuador %>% inner_join(quito_10, by="IPUM2010")

names(quito_90)
names(quito_01)
names(quito_10)

quito_90 <- select(quito_90, -c(CANT1990))
quito_01 <- select(quito_01, -c(CANT2001))
quito_10 <- select(quito_10, -c(CANT2010))

##Merging all years into one table
ecuador_full <- rbind(quito_90,quito_01,quito_10)
names(ecuador_full)

##Excluding specific columns for the unifeied dataset
ecuador_full<- select(ecuador_full, -c(GEO2_EC1990,GEO2_EC2001,GEO2_EC2010,IPUM1990,IPUM2001,IPUM2010))
table(ecuador_full$CITY)
save(ecuador_full,file="ecuador_full.Rda")
