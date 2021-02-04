library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay chile")

geo2_cl1992 <- read_sf("geo2_cl1992.shp")
geo2_cl2002 <- read_sf("geo2_cl2002.shp")
AUE_santiago <- read_sf("Santiago_studyArea.shp")
AUE_santiago <- st_transform(AUE_santiago, 4326)

centroid_cl1992 <- st_centroid(geo2_cl1992)
centroid_cl2002 <- st_centroid(geo2_cl2002)

santiago_92 <- geo2_cl1992[st_intersection(AUE_santiago,centroid_cl1992),]
santiago_92['CITY']='santiago'
santiago_02 <- geo2_cl2002[st_intersection(AUE_santiago,centroid_cl2002),]
santiago_02['CITY']='santiago'


plot(st_geometry(santiago_02), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_cl2002[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00025.xml")
chile <- read_ipums_micro(ddi)

##Creating field for join

chile$IPUM1992 <- as.integer(chile$GEO2_CL1992)
chile$IPUM2002 <- as.integer(chile$GEO2_CL2002)
santiago_92$IPUM1992 <- as.integer(santiago_92$IPUM1992)
santiago_02$IPUM2002 <- as.integer(santiago_02$IPUM2002)

##Joining by year

chile_92 <- chile %>% inner_join(santiago_92, by="IPUM1992")
chile_02 <- chile %>% inner_join(santiago_02, by="IPUM2002")

names(chile_92)
names(chile_02)

chile_92 <- select(chile_92, -c(MUNI1992))
chile_02 <- select(chile_02, -c(MUNI2002))

##Merging all years into one table
chile_full <- rbind(chile_92,chile_02)
names(chile_full)


##Excluding specific columns for the unifeied dataset
chile_full<- select(chile_full, -c(GEO2_CL1992,GEO2_CL2002,IPUM1992,IPUM2002))
table(chile_full$CITY)
save(chile_full,file="chile_full.Rda")
