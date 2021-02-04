library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay mozambique")
geo2_mz97 <- read_sf("geo2_mz1997.shp")
geo2_mz07 <- read_sf("geo2_mz2007.shp")
centroid_mz97 <- st_centroid(geo2_mz97)
centroid_mz07 <- st_centroid(geo2_mz07)
AUE_beira <- read_sf("Beira_studyArea.shp")
AUE_beira  <- st_transform(AUE_beira , 4326)


beira_97 <- geo2_mz97[st_intersection(AUE_beira,centroid_mz97),]
beira_97['CITY']='beira'
beira_07 <-  geo2_mz07[st_intersection(AUE_beira,centroid_mz07),]
beira_07['CITY']='beira'


plot(st_geometry(beira_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_mz07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00039.xml")
mozambique <- read_ipums_micro(ddi)

names(mozambique)

##Creating field for join

mozambique$IPUM1997 <- as.integer(mozambique$GEO2_MZ1997)
mozambique$IPUM2007 <- as.integer(mozambique$GEO2_MZ2007)
beira_97$IPUM1997 <- as.integer(beira_97$IPUM1997)
beira_07$IPUM2007 <- as.integer(beira_07$IPUM2007)

##Joining by year

beira_97 <- mozambique %>% inner_join(beira_97, by="IPUM1997")
beira_07 <- mozambique %>% inner_join(beira_07, by="IPUM2007")

names(beira_97)
names(beira_07)

beira_97 <- select(beira_97, -c(DIST1997))
beira_07 <- select(beira_07, -c(DIST2007))

##Merging all years into one table
mozambique_full <- rbind(beira_97,beira_07)
names(mozambique_full)

##Excluding specific columns for the unifeied dataset
mozambique_full<- select(mozambique_full, -c(GEO2_MZ1997,GEO2_MZ2007,IPUM1997,IPUM2007))
table(mozambique_full$CITY)
save(mozambique_full,file="mozambique_full.Rda")
