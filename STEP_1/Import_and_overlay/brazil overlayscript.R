library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("   ")

geo2_br1991 <- read_sf("geo2_br1991.shp")
geo2_br2000 <- read_sf("geo2_br2000.shp")
geo2_br2010 <- read_sf("geo2_br2010.shp")

AUE_BH <- read_sf("Belo_Horizonte_studyArea.shp")
AUE_ilheus <- read_sf("Ilheus_studyArea.shp")
AUE_jequie <- read_sf("Jequie_studyArea.shp")
AUE_palmas <- read_sf("Palmas_studyArea.shp")
AUE_ribeirao <- read_sf("Ribeirao_Preto_studyArea.shp")
AUE_curitiba <- read_sf("Curitiba_studyArea.shp")
AUE_SP  <- read_sf("Sao_Paulo_studyArea.shp")
AUE_florianopolis <- read_sf("Florianopolis_studyArea.shp")
AUE_BH <- st_transform(AUE_BH, 4326)
AUE_SP <- st_transform(AUE_SP, 4326)
AUE_ilheus <- st_transform(AUE_ilheus, 4326)
AUE_jequie <- st_transform(AUE_jequie, 4326)
AUE_palmas <- st_transform(AUE_palmas, 4326)
AUE_ribeirao <- st_transform(AUE_ribeirao, 4326)
AUE_curitiba <- st_transform(AUE_curitiba, 4326)
AUE_florianopolis <- st_transform(AUE_florianopolis, 4326)


centroid_br1991 <- st_centroid(geo2_br1991)
centroid_br2000 <- st_centroid(geo2_br2000)
centroid_br2010 <- st_centroid(geo2_br2010)

BH_91 <- geo2_br1991[st_intersection(AUE_BH,centroid_br1991),]
BH_91['CITY']='belo horizonte'
BH_00 <- geo2_br2000[st_intersection(AUE_BH,centroid_br2000),]
BH_00['CITY']='belo horizonte'
BH_10 <- geo2_br2010[st_intersection(AUE_BH,centroid_br2010),]
BH_10['CITY']='belo horizonte'

SP_91 <- geo2_br1991[st_intersection(AUE_SP,centroid_br1991),]
SP_91['CITY']='sao paulo'
SP_00 <- geo2_br2000[st_intersection(AUE_SP,centroid_br2000),]
SP_00['CITY']='sao paulo'
SP_10 <- geo2_br2010[st_intersection(AUE_SP,centroid_br2010),]
SP_10['CITY']='sao paulo'

ilheus_91 <- geo2_br1991[st_intersection(AUE_ilheus,centroid_br1991),]
ilheus_91['CITY']='ilheus'
ilheus_00 <- geo2_br2000[st_intersection(AUE_ilheus,centroid_br2000),]
ilheus_00['CITY']='ilheus'
ilheus_10 <- geo2_br2010[st_intersection(AUE_ilheus,centroid_br2010),]
ilheus_10['CITY']='ilheus'

jequie_91 <- geo2_br1991[st_intersection(AUE_jequie,centroid_br1991),]
jequie_91['CITY']='jequie'
jequie_00 <- geo2_br2000[st_intersection(AUE_jequie,centroid_br2000),]
jequie_00['CITY']='jequie'
jequie_10 <- geo2_br2010[st_intersection(AUE_jequie,centroid_br2010),]
jequie_10['CITY']='jequie'

palmas_91 <- geo2_br1991[st_intersection(AUE_palmas,centroid_br1991),]
palmas_91['CITY']='palmas'
palmas_00 <- geo2_br2000[st_intersection(AUE_palmas,centroid_br2000),]
palmas_00['CITY']='palmas'
palmas_10 <- geo2_br2010[st_intersection(AUE_palmas,centroid_br2010),]
palmas_10['CITY']='palmas'

ribeirao_91 <- geo2_br1991[st_intersection(AUE_ribeirao,centroid_br1991),]
ribeirao_91['CITY']='ribeirao preto'
ribeirao_00 <- geo2_br2000[st_intersection(AUE_ribeirao,centroid_br2000),]
ribeirao_00['CITY']='ribeirao preto'
ribeirao_10 <- geo2_br2010[st_intersection(AUE_ribeirao,centroid_br2010),]
ribeirao_10['CITY']='ribeirao preto'


curitiba_91 <- geo2_br1991[st_intersection(AUE_curitiba,centroid_br1991),]
curitiba_91['CITY']='curitiba'
curitiba_00 <- geo2_br2000[st_intersection(AUE_curitiba,centroid_br2000),]
curitiba_00['CITY']='curitiba'
curitiba_10 <- geo2_br2010[st_intersection(AUE_curitiba,centroid_br2010),]
curitiba_10['CITY']='curitiba'

floripa_91 <- geo2_br1991[st_intersection(AUE_florianopolis,centroid_br1991),]
floripa_91['CITY']='florianopolis'
floripa_00 <- geo2_br2000[st_intersection(AUE_florianopolis,centroid_br2000),]
floripa_00['CITY']='florianopolis'
floripa_10 <- geo2_br2010[st_intersection(AUE_florianopolis,centroid_br2010),]
floripa_10['CITY']='florianopolis'

geo_brazil_91 <- rbind(BH_91,floripa_91, ilheus_91,jequie_91,palmas_91,ribeirao_91,SP_91,curitiba_91)
geo_brazil_00 <- rbind(BH_00,floripa_00, ilheus_00,jequie_00,palmas_00,ribeirao_00,SP_00,curitiba_00)
geo_brazil_10 <- rbind(BH_10,floripa_10, ilheus_10,jequie_10,palmas_10,ribeirao_10,SP_10,curitiba_10)

plot(st_geometry(geo_brazil_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_br2010[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00024.xml")
brasil <- read_ipums_micro(ddi)

names(brasil)

##Creating field for join

brasil$IPUM1991 <- as.integer(brasil$GEO2_BR1991)
brasil$IPUM2000 <- as.integer(brasil$GEO2_BR2000)
brasil$IPUM2010 <- as.integer(brasil$GEO2_BR2010)
geo_brazil_91$IPUM1991 <- as.integer(geo_brazil_91$IPUM1991)
geo_brazil_01$IPUM2001 <- as.integer(geo_brazil_01$IPUM2001)
geo_brazil_10$IPUM2010 <- as.integer(geo_brazil_10$IPUM2010)

geo_brazil_91 <- brasil %>% inner_join(geo_brazil_91, by="IPUM1991")
geo_brazil_00 <- brasil %>% inner_join(geo_brazil_00, by="IPUM2000")
geo_brazil_10 <- brasil %>% inner_join(geo_brazil_10, by="IPUM2010")

names(geo_brazil_91)
names(geo_brazil_00)
names(geo_brazil_10)

geo_brazil_91 <- select(geo_brazil_91, -c(MUNI1991))
geo_brazil_00 <- select(geo_brazil_00, -c(MUNI2000))
geo_brazil_10 <- select(geo_brazil_10, -c(MUNI2010))


##Merging all years into one table
brazil_full <- rbind(geo_brazil_91,geo_brazil_00,geo_brazil_10)
names(brazil_full)

##Excluding specific columns for the unifeied dataset
brazil_full<- select(brazil_full, -c(GEO2_BR1991,GEO2_BR2000,GEO2_BR2010,IPUM1991,IPUM2000,IPUM2010))
table(brazil_full$CITY)
save(brazil_full,file="brazil_full.Rda")
