library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_co85 <- read_sf("geo2_co1985.shp")
geo2_co93 <- read_sf("geo2_co1993.shp")
geo2_co05 <- read_sf("geo2_co2005.shp")
AUE_bogota <- read_sf("Bogota_studyArea.shp")
AUE_valledupar <- read_sf("Valledupar_studyArea.shp")
AUE_bogota <- st_transform(AUE_bogota, 4326)
AUE_valledupar <- st_transform(AUE_valledupar, 4326)

centroid_co85 <- st_centroid(geo2_co85)
centroid_co93 <- st_centroid(geo2_co93)
centroid_co05 <- st_centroid(geo2_co05)

bogota_85 <- geo2_co85[st_intersection(AUE_bogota,centroid_co85),]
bogota_85['CITY']='bogota'
bogota_93 <- geo2_co93[st_intersection(AUE_bogota,centroid_co93),]
bogota_93['CITY']='bogota'
bogota_05 <- geo2_co05[st_intersection(AUE_bogota,centroid_co05),]
bogota_05['CITY']='bogota'

valledupar_85 <- geo2_co85[st_intersection(AUE_valledupar,centroid_co85),]
valledupar_85['CITY']='valledupar'
valledupar_93 <- geo2_co93[st_intersection(AUE_valledupar,centroid_co93),]
valledupar_93['CITY']='valledupar'
valledupar_05 <- geo2_co05[st_intersection(AUE_valledupar,centroid_co05),]
valledupar_05['CITY']='valledupar'

geo_colombia_85 <- rbind(bogota_85,valledupar_85)
geo_colombia_93 <- rbind(bogota_93,valledupar_93)
geo_colombia_05 <- rbind(bogota_05,valledupar_05)


plot(st_geometry(geo_colombia_05), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_co05[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00026.xml")
colombia <- read_ipums_micro(ddi)

names(colombia)

##Creating field for join

colombia$IPUM1985 <- as.integer(colombia$GEO2_CO1985)
colombia$IPUM1993 <- as.integer(colombia$GEO2_CO1993)
colombia$IPUM2005 <- as.integer(colombia$GEO2_CO2005)
geo_colombia_85$IPUM1985 <- as.integer(geo_colombia_85$IPUM1985)
geo_colombia_93$IPUM1993 <- as.integer(geo_colombia_93$IPUM1993)
geo_colombia_05$IPUM2005 <- as.integer(geo_colombia_05$IPUM2005)

##Joining by year

geo_colombia_85 <- colombia %>% inner_join(geo_colombia_85, by="IPUM1985")
geo_colombia_93 <- colombia %>% inner_join(geo_colombia_93, by="IPUM1993")
geo_colombia_05 <- colombia %>% inner_join(geo_colombia_05, by="IPUM2005")

names(geo_colombia_85)
names(geo_colombia_93)
names(geo_colombia_05)

geo_colombia_85 <- select(geo_colombia_85, -c(MUNI1985))
geo_colombia_93 <- select(geo_colombia_93, -c(MUNI1993))
geo_colombia_05 <- select(geo_colombia_05, -c(MUNI2005))

##Merging all years into one table
colombia_full <- rbind(geo_colombia_85,geo_colombia_93,geo_colombia_05)
names(colombia_full)

##Excluding specific columns for the unifeied dataset
colombia_full<- select(colombia_full, -c(GEO2_CO1985,GEO2_CO1993,GEO2_CO2005,IPUM1985,IPUM1993,IPUM2005))
table(colombia_full$CITY)
save(colombia_full,file="colombia_full.Rda")


