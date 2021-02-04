library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_fj86 <- read_sf("geo2_fj1986.shp")
geo2_fj96 <- read_sf("geo2_fj1996.shp")
geo2_fj07 <- read_sf("geo2_fj2007.shp")
centroid_fj86 <- st_centroid(geo2_fj86)
centroid_fj96 <- st_centroid(geo2_fj96)
centroid_fj07 <- st_centroid(geo2_fj07)
AUE_suva <- read_sf("Suva_studyArea.shp")
AUE_suva <- st_transform(AUE_suva, 4326)


suva_86 <- geo2_fj86[st_intersection(AUE_suva,centroid_fj86),]
suva_86['CITY']='suva'
suva_96 <- geo2_fj96[st_intersection(AUE_suva,centroid_fj96),]
suva_96['CITY']='suva'
suva_07 <- geo2_fj07[st_intersection(AUE_suva,centroid_fj07),]
suva_07['CITY']='suva'

plot(st_geometry(suva_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_fj07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00031.xml")
fiji <- read_ipums_micro(ddi)

names(fiji)

##Creating field for join

fiji$IPUM1986 <- as.integer(fiji$GEO2_FJ1986)
fiji$IPUM1996 <- as.integer(fiji$GEO2_FJ1996)
fiji$IPUM2007 <- as.integer(fiji$GEO2_FJ2007)
suva_86$IPUM1986 <- as.integer(suva_86$IPUM1986)
suva_96$IPUM1996 <- as.integer(suva_96$IPUM1996)
suva_07$IPUM2007 <- as.integer(suva_07$IPUM2007)


##Joining by year

suva_86 <- fiji %>% inner_join(suva_86, by="IPUM1986")
suva_96 <- fiji %>% inner_join(suva_96, by="IPUM1996")
suva_07 <- fiji %>% inner_join(suva_07, by="IPUM2007")

names(suva_86)
names(suva_96)
names(suva_07)

suva_86 <- select(suva_86, -c(PROV1986))
suva_96 <- select(suva_96, -c(PROV1996))
suva_07 <- select(suva_07, -c(PROV2007))

##Merging all years into one table
fiji_full <- rbind(suva_86,suva_96,suva_07)
names(fiji_full)

##Excluding specific columns for the unifeied dataset
fiji_full<- select(fiji_full, -c(GEO2_FJ1986,GEO2_FJ1996,GEO2_FJ2007,IPUM1986,IPUM1996,IPUM2007))
table(fiji_full$CITY)
save(fiji_full,file="fiji_full.Rda")
