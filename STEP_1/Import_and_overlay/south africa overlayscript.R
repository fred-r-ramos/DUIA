library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay south africa")

geo2_za96 <- read_sf("geo2_za1996.shp")
geo2_za01 <- read_sf("geo2_za2001.shp")
geo2_za07 <- read_sf("geo2_za2007.shp")
geo2_za11 <- read_sf("geo2_za2011.shp")
centroid_za96 <- st_centroid(geo2_za96)
centroid_za01 <- st_centroid(geo2_za01)
centroid_za07 <- st_centroid(geo2_za07)
centroid_za11 <- st_centroid(geo2_za11)
AUE_johannesburg <- read_sf("Johannesburg_studyArea.shp")
AUE_johannesburg  <- st_transform(AUE_johannesburg , 4326)


jburg_96 <- geo2_za96[st_intersection(AUE_johannesburg,centroid_za96),]
jburg_96['CITY']='johannesburg'
jburg_01 <- geo2_za01[st_intersection(AUE_johannesburg,centroid_za01),]
jburg_01['CITY']='johannesburg'
jburg_07 <- geo2_za07[st_intersection(AUE_johannesburg,centroid_za07),]
jburg_07['CITY']='johannesburg'
jburg_11 <- geo2_za11[st_intersection(AUE_johannesburg,centroid_za11),]
jburg_11['CITY']='johannesburg'

plot(st_geometry(jburg_11), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_za11[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00045.xml")
southafrica <- read_ipums_micro(ddi)

names(southafrica)

##Creating field for join

southafrica$IPUM1996 <- as.integer(southafrica$GEO2_ZA1996)
southafrica$IPUM2001 <- as.integer(southafrica$GEO2_ZA2001)
southafrica$IPUM2007 <- as.integer(southafrica$GEO2_ZA2007)
southafrica$IPUM2011 <- as.integer(southafrica$GEO2_ZA2011)


##Joining by year

jburg_96 <- southafrica %>% inner_join(jburg_96, by="IPUM1996")
jburg_01 <- southafrica %>% inner_join(jburg_01, by="IPUM2001")
jburg_07 <- southafrica %>% inner_join(jburg_07, by="IPUM2007")
jburg_11 <- southafrica %>% inner_join(jburg_11, by="IPUM2011")

names(jburg_96)
names(jburg_01)
names(jburg_07)
names(jburg_11)

jburg_96 <- select(jburg_96, -c(DIST1996))
jburg_01 <- select(jburg_01, -c(DIST2001))
jburg_07 <- select(jburg_01, -c(DIST2007))
jburg_11 <- select(jburg_11, -c(DIST2011))

##Merging all years into one table
southafrica_full <- rbind(jburg_96,jburg_01, jburg_07, jburg_11)
names(southafrica_full)

##Excluding specific columns for the unifeied dataset
southafrica_full<- select(southafrica_full, -c(GEO2_ZA1996,GEO2_ZA2001,GEO2_ZA2011,GEO2_ZA2007,IPUM1996,IPUM2001,IPUM2011))
table(southafrica_full$CITY)
save(southafrica_full,file="southafrica_full.Rda")


