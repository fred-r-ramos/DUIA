library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_pk81 <- read_sf("geo2_pk1981.shp")
geo2_pk98 <- read_sf("geo2_pk1998.shp")
centroid_pk81 <- st_centroid(geo2_pk81)
centroid_pk98 <- st_centroid(geo2_pk98)
AUE_karachi <- read_sf("Karachi_studyArea.shp")
AUE_lahore <- read_sf("Lahore_studyArea.shp")
AUE_karachi  <- st_transform(AUE_karachi , 4326)
AUE_lahore  <- st_transform(AUE_lahore , 4326)



karachi_81 <- geo2_pk81[st_intersection(AUE_karachi,centroid_pk81),]
karachi_81['CITY']='karachi'
karachi_98 <- geo2_pk98[st_intersection(AUE_karachi,centroid_pk98),]
karachi_98['CITY']='karachi'

lahore_81 <- st_intersection(AUE_lahore, geo2_pk81)
lahore_81['CITY']='lahore'
lahore_81 <- lahore_81 %>% select (-c(1))
lahore_98 <-  st_intersection(AUE_lahore, geo2_pk98)
lahore_98['CITY']='lahore'
lahore_98 <- lahore_98 %>% select (-c(1))


geo_pakistan_81 <- rbind(karachi_81,lahore_81)
geo_pakistan_98 <- rbind(karachi_98,lahore_98)

plot(st_geometry(geo_pakistan_98), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_pk98[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00042.xml")
pakistan <- read_ipums_micro(ddi)

names(pakistan)

##Creating field for join

pakistan$IPUM1981 <- as.integer(pakistan$GEO2_PK1981)
pakistan$IPUM1998 <- as.integer(pakistan$GEO2_PK1998)
geo_pakistan_81$IPUM1981 <- as.integer(geo_pakistan_81$IPUM1981)
geo_pakistan_98$IPUM1998 <- as.integer(geo_pakistan_98$IPUM1998)

##Joining by year

geo_pakistan_81 <- pakistan %>% inner_join(geo_pakistan_81, by="IPUM1981")
geo_pakistan_98 <- pakistan %>% inner_join(geo_pakistan_98, by="IPUM1998")

names(geo_pakistan_81)
names(geo_pakistan_98)

geo_pakistan_81 <- select(geo_pakistan_81, -c(DIVI1981))
geo_pakistan_98 <- select(geo_pakistan_98, -c(DIVI1998))

##Merging all years into one table
pakistan_full <- rbind(geo_pakistan_81,geo_pakistan_98)
names(pakistan_full)

##Excluding specific columns for the unifeied dataset
pakistan_full<- select(pakistan_full, -c(GEO2_PK1981,GEO2_PK1998,IPUM1981,IPUM1998))
table(pakistan_full$CITY)
save(pakistan_full,file="pakistan_full.Rda")

