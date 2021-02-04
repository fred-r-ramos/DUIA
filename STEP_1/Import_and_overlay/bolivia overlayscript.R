library(sf)
library(tidyverse)
library(dplyr)
setwd("C:/Users/Gebruiker/Dropbox/Countriesoverlay David/overlay bolivia")

geo2_bo1992 <- read_sf("geo2_bo1992.shp")
geo2_bo2001 <- read_sf("geo2_bo2001.shp")
AUE_cb <- read_sf("Cochabamba_studyArea.shp")
AUE_cochabamba <- st_transform(AUE_cb, 4326)

centroid_bo1992 <- st_centroid(geo2_bo1992)
centroid_bo2001 <- st_centroid(geo2_bo2001)

cochabamba_92 <- geo2_bo1992[st_intersection(AUE_cochabamba,centroid_bo1992),]
cochabamba_92['CITY']='cochabamba'
cochabamba_01 <- geo2_bo2001[st_intersection(AUE_cochabamba,centroid_bo2001),]
cochabamba_01['CITY']='cochabamba'

plot(st_geometry(cochabamba_01), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_bo2001[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00023.xml")
bolivia <- read_ipums_micro(ddi)

names(bolivia)

##Creating field for join

bolivia$IPUM1992 <- as.integer(bolivia$GEO2_BO1992)
bolivia$IPUM2001 <- as.integer(bolivia$GEO2_BO2001)
cochabamba_92$IPUM1992 <- as.integer(cochabamba_92$IPUM1992)
cochabamba_01$IPUM2001 <- as.integer(cochabamba_01$IPUM2001)

##Joining by year

cochabamba_92 <- bolivia %>% inner_join(cochabamba_92, by="IPUM1992")
cochabamba_01 <- bolivia %>% inner_join(cochabamba_01, by="IPUM2001")

names(cochabamba_92)
names(cochabamba_01)

cochabamba_92 <- select(cochabamba_92, -c(PROV1992))
cochabamba_01 <- select(cochabamba_01, -c(PROV2001))


##Merging all years into one table
bolivia_full <- rbind(cochabamba_92,cochabamba_01)
names(bolivia_full)

##Excluding specific columns for the unifeied dataset
bolivia_full<- select(bolivia_full, -c(GEO2_BO1992,GEO2_BO2001,IPUM1992,IPUM2001))
table(bolivia_full$CITY)
save(bolivia_full,file="bolivia_full.Rda")

