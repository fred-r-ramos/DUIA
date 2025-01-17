library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ineq)

gc()

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/brazil")



geo2_br1991 <- read_sf("geo2_br1991.shp")
geo2_br2000 <- read_sf("geo2_br2000.shp")
geo2_br2010 <- read_sf("geo2_br2010.shp")

sf::sf_use_s2(FALSE)

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

#ddi <-read_ipums_ddi("ipumsi_00225.xml")
ddi <-read_ipums_ddi("ipumsi_00244.xml")
brasil <- read_ipums_micro(ddi)
gc()

rm(geo2_br1991,geo2_br2000,geo2_br2010)

names(brasil)

######################creating binary variables for logit regressions

names(brasil)


table(brasil$OWNERSHIP)
brasil$owner_b <- ifelse(brasil$OWNERSHIP ==1,1,0)
table(brasil$ELECTRIC)
brasil$eletric_b <- ifelse(brasil$ELECTRIC ==1,1,0)
table(brasil$WATSUP)
brasil$water_b <- ifelse(brasil$WATSUP ==11,1,0)
table(brasil$SEWAGE)
brasil$sewage_b <- ifelse(brasil$SEWAGE ==11|brasil$SEWAGE ==12,1,0)
table(brasil$TRASH)
brasil$trash_b <- ifelse(brasil$TRASH ==11|brasil$TRASH ==12,1,0)
table(brasil$AUTOS)
brasil$autos_b <- ifelse(brasil$AUTOS !=0,1,0)
table(brasil$REFRIG)
brasil$refrig_b <- ifelse(brasil$REFRIG ==2,1,0)
table(brasil$TV)
brasil$tv_b <- ifelse(brasil$TV !=10,1,0)
table(brasil$BATH)
brasil$bath_b <- ifelse(brasil$BATH ==2,1,0)

names(brasil)
table(brasil$YEAR)
gc()

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL

#checking if the variables are available in both years
table(brasil$owner_b,brasil$YEAR)
table(brasil$eletric_b,brasil$YEAR)
table(brasil$water_b,brasil$YEAR)
table(brasil$sewage_b,brasil$YEAR)
table(brasil$trash_b,brasil$YEAR)
table(brasil$autos_b,brasil$YEAR)
table(brasil$refrig_b,brasil$YEAR)
table(brasil$tv_b,brasil$YEAR)
table(brasil$bath_b,brasil$YEAR)

brasil$PUBl_ASSET<- ifelse(is.na(brasil$water_b), 0, brasil$water_b) +
  ifelse(is.na(brasil$sewage_b), 0, brasil$sewage_b) +
  ifelse(is.na(brasil$eletric_b), 0, brasil$eletric_b) +
  ifelse(is.na(brasil$trash_b), 0, brasil$trash_b)
brasil %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

brasil$PRIV_ASSET <-  ifelse(is.na(brasil$owner_b), 0, brasil$owner_b) +
  ifelse(is.na(brasil$refrig_b), 0, brasil$refrig_b) +
  ifelse(is.na(brasil$bath_b), 0, brasil$bath_b)
brasil %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

brasil$TOTAL_ASSET <- ifelse(is.na(brasil$water_b), 0, brasil$water_b) +
  ifelse(is.na(brasil$sewage_b), 0, brasil$sewage_b) +
  ifelse(is.na(brasil$eletric_b), 0, brasil$eletric_b) +
  ifelse(is.na(brasil$trash_b), 0, brasil$trash_b)+
  ifelse(is.na(brasil$owner_b), 0, brasil$owner_b) +
  ifelse(is.na(brasil$refrig_b), 0, brasil$refrig_b) +
  ifelse(is.na(brasil$bath_b), 0, brasil$bath_b)
assets<-brasil %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

brasil$TOTAL_ASSET_gini <- ifelse(is.na(brasil$water_b), 0, brasil$water_b) +
  ifelse(is.na(brasil$sewage_b), 0, brasil$sewage_b) +
  ifelse(is.na(brasil$eletric_b), 0, brasil$eletric_b) +
  ifelse(is.na(brasil$trash_b), 0, brasil$trash_b)+
  ifelse(is.na(brasil$owner_b), 0, brasil$owner_b) +
  ifelse(is.na(brasil$refrig_b), 0, brasil$refrig_b) 
assets<-brasil %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

brasil$IPUM1991 <- as.integer(brasil$GEO2_BR1991)
brasil$IPUM2000 <- as.integer(brasil$GEO2_BR2000)
brasil$IPUM2010 <- as.integer(brasil$GEO2_BR2010)
geo_brazil_91$IPUM1991 <- as.integer(geo_brazil_91$IPUM1991)
geo_brazil_00$IPUM2000 <- as.integer(geo_brazil_00$IPUM2000)
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
table(brazil_full$YEAR)

gc()

##Excluding specific columns for the unifeied dataset
brazil_full<- select(brazil_full, -c(GEO2_BR1991,GEO2_BR2000,GEO2_BR2010,IPUM1991,IPUM2000,IPUM2010,geometry))
table(brazil_full$CITY)
table(brazil_full$CITY)
names(brazil_full)

table(brazil_full$YEAR)

#brazil_full <- brazil_full %>%  filter (brazil_full$YRSCHOOL < 90)

#brazil_full <- brazil_full %>%  filter (brazil_full$AGE >15)

#brazil_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(brazil_full$YRSCHOOL)
summary(brazil_full$YRSCHOOL)
table(brazil_full$CITY)

####belo horizonte
belo_horizonte_full <- brazil_full %>%  filter (CITY=="belo horizonte")

belo_horizonte_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(belo_horizonte_full$YRSCHOOL)
summary(belo_horizonte_full$AGE)

belo_horizonte_fu91 <- belo_horizonte_full %>%  filter (YEAR==1991)
belo_horizonte_fu00 <- belo_horizonte_full %>%  filter (YEAR==2000)
belo_horizonte_fu10 <- belo_horizonte_full %>%  filter (YEAR==2010)

Gini(belo_horizonte_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(belo_horizonte_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(belo_horizonte_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(belo_horizonte_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(belo_horizonte_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_bh91 <- Lc(belo_horizonte_fu91$YRSCHOOL, n = rep(1,length(belo_horizonte_fu91$YRSCHOOL)), plot = TRUE)
Lc_bh00 <- Lc(belo_horizonte_fu00$YRSCHOOL, n = rep(1,length(belo_horizonte_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_bh91,col='blue', main = "Lorenz Curve - Belo Horizonte")
lines(Lc_bh00, col='red')

gc()

####curitiba
curitiba_full <- brazil_full %>%  filter (CITY=="curitiba")


curitiba_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(curitiba_full$YRSCHOOL)
summary(curitiba_full$AGE)

curitiba_fu91 <- curitiba_full %>%  filter (YEAR==1991)
curitiba_fu00 <- curitiba_full %>%  filter (YEAR==2000)
curitiba_fu10 <- curitiba_full %>%  filter (YEAR==2010)

Gini(curitiba_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(curitiba_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(curitiba_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(curitiba_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(curitiba_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_cur91 <- Lc(curitiba_fu91$YRSCHOOL, n = rep(1,length(curitiba_fu91$YRSCHOOL)), plot = TRUE)
Lc_cur00 <- Lc(curitiba_fu00$YRSCHOOL, n = rep(1,length(curitiba_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_cur91,col='blue', main = "Lorenz Curve - Curitiba")
lines(Lc_cur00, col='red')

####florianopolis
florianopolis_full <- brazil_full %>%  filter (CITY=="florianopolis")

florianopolis_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(florianopolis_full$YRSCHOOL)
summary(florianopolis_full$AGE)

florianopolis_fu91 <- florianopolis_full %>%  filter (YEAR==1991)
florianopolis_fu00 <- florianopolis_full %>%  filter (YEAR==2000)
florianopolis_fu10 <- florianopolis_full %>%  filter (YEAR==2010)

Gini(florianopolis_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(florianopolis_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(florianopolis_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(florianopolis_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(florianopolis_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_flo91 <- Lc(florianopolis_fu91$YRSCHOOL, n = rep(1,length(florianopolis_fu91$YRSCHOOL)), plot = TRUE)
Lc_flo00 <- Lc(florianopolis_fu00$YRSCHOOL, n = rep(1,length(florianopolis_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_flo91,col='blue', main = "Lorenz Curve - florianopolis")
lines(Lc_flo00, col='red')

####ilheus
ilheus_full <- brazil_full %>%  filter (CITY=="ilheus")

ilheus_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(ilheus_full$YRSCHOOL)
summary(ilheus_full$AGE)

ilheus_fu91 <- ilheus_full %>%  filter (YEAR==1991)
ilheus_fu00 <- ilheus_full %>%  filter (YEAR==2000)
ilheus_fu10 <- ilheus_full %>%  filter (YEAR==2010)

Gini(ilheus_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(ilheus_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(ilheus_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ilheus_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ilheus_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_ilh91 <- Lc(ilheus_fu91$YRSCHOOL, n = rep(1,length(ilheus_fu91$YRSCHOOL)), plot = TRUE)
Lc_ilh00 <- Lc(ilheus_fu00$YRSCHOOL, n = rep(1,length(ilheus_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_ilh91,col='blue', main = "Lorenz Curve - ilheus")
lines(Lc_ilh00, col='red')

####jequie
jequie_full <- brazil_full %>%  filter (CITY=="jequie")

jequie_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(jequie_full$YRSCHOOL)
summary(jequie_full$AGE)

jequie_fu91 <- jequie_full %>%  filter (YEAR==1991)
jequie_fu00 <- jequie_full %>%  filter (YEAR==2000)
jequie_fu10 <- jequie_full %>%  filter (YEAR==2010)


Gini(jequie_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(jequie_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(jequie_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(jequie_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(jequie_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_jeq91 <- Lc(jequie_fu91$YRSCHOOL, n = rep(1,length(jequie_fu91$YRSCHOOL)), plot = TRUE)
Lc_jeq00 <- Lc(jequie_fu00$YRSCHOOL, n = rep(1,length(jequie_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_jeq91,col='blue', main = "Lorenz Curve - jequie")
lines(Lc_jeq00, col='red')

####palmas
palmas_full <- brazil_full %>%  filter (CITY=="palmas")

palmas_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(palmas_full$YRSCHOOL)
summary(palmas_full$AGE)

palmas_fu91 <- palmas_full %>%  filter (YEAR==1991)
palmas_fu00 <- palmas_full %>%  filter (YEAR==2000)
palmas_fu10 <- palmas_full %>%  filter (YEAR==2010)

Gini(palmas_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(palmas_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(palmas_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(palmas_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(palmas_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_pal91 <- Lc(palmas_fu91$YRSCHOOL, n = rep(1,length(palmas_fu91$YRSCHOOL)), plot = TRUE)
Lc_pal00 <- Lc(palmas_fu00$YRSCHOOL, n = rep(1,length(palmas_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_pal91,col='blue', main = "Lorenz Curve - palmas")
lines(Lc_pal00, col='red')

####ribeirao_preto
ribeirao_preto_full <- brazil_full %>%  filter (CITY=="ribeirao preto")

ribeirao_preto_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(ribeirao_preto_full$YRSCHOOL)
summary(ribeirao_preto_full$AGE)

ribeirao_preto_fu91 <- ribeirao_preto_full %>%  filter (YEAR==1991)
ribeirao_preto_fu00 <- ribeirao_preto_full %>%  filter (YEAR==2000)
ribeirao_preto_fu10 <- ribeirao_preto_full %>%  filter (YEAR==2010)

Gini(ribeirao_preto_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(ribeirao_preto_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(ribeirao_preto_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ribeirao_preto_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ribeirao_preto_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_rip91 <- Lc(ribeirao_preto_fu91$YRSCHOOL, n = rep(1,length(ribeirao_preto_fu91$YRSCHOOL)), plot = TRUE)
Lc_rip00 <- Lc(ribeirao_preto_fu00$YRSCHOOL, n = rep(1,length(ribeirao_preto_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_rip91,col='blue', main = "Lorenz Curve - Ribeirão Preto")
lines(Lc_rip00, col='red')

####sao_paulo
sao_paulo_full <- brazil_full %>%  filter (CITY=="sao paulo")


sao_paulo_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(sao_paulo_full$YRSCHOOL)
summary(sao_paulo_full$AGE)

sao_paulo_fu91 <- sao_paulo_full %>%  filter (YEAR==1991)
sao_paulo_fu00 <- sao_paulo_full %>%  filter (YEAR==2000)
sao_paulo_fu10 <- sao_paulo_full %>%  filter (YEAR==2010)
Gini(sao_paulo_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(sao_paulo_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(sao_paulo_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(sao_paulo_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(sao_paulo_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_sp91 <- Lc(sao_paulo_fu91$YRSCHOOL, n = rep(1,length(sao_paulo_fu91$YRSCHOOL)), plot = TRUE)
Lc_sp00 <- Lc(sao_paulo_fu00$YRSCHOOL, n = rep(1,length(sao_paulo_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_sp91,col='blue', main = "Lorenz Curve - São Paulo")
lines(Lc_sp00, col='red')

####Brazil
names(brasil)

table(brasil$YEAR)

brasil <- brasil %>%  filter (brasil$YRSCHOOL < 90)

brasil <- brasil %>%  filter (brasil$AGE >15)

brasil %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(brasil$YRSCHOOL)
summary(brasil$YRSCHOOL)

brasil_fu91 <- brasil %>%  filter (YEAR==1991)
brasil_fu00 <- brasil %>%  filter (YEAR==2000)

Gini(brasil_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(brasil_fu00 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves
Lc_br91 <- Lc(brasil_fu91$YRSCHOOL, n = rep(1,length(brasil_fu91$YRSCHOOL)), plot = TRUE)
Lc_br00 <- Lc(brasil_fu00$YRSCHOOL, n = rep(1,length(brasil_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_br91,col='blue', main = "Lorenz Curve - Brazil")
lines(Lc_br00, col='red')

names(brazil_full)

table(brazil_full$OCCISCO)


brazil_full$OCCISCO_b <- ifelse(brazil_full$OCCISCO ==1|brazil_full$OCCISCO ==2,1,0)
brazil_full$OCCISCO_b <- ifelse(brazil_full$OCCISCO ==3|brazil_full$OCCISCO ==4|brazil_full$OCCISCO ==5,2,brazil_full$OCCISCO_b)
brazil_full$OCCISCO_b <- ifelse(brazil_full$OCCISCO ==6|brazil_full$OCCISCO ==7|brazil_full$OCCISCO ==8|brazil_full$OCCISCO ==9,3,brazil_full$OCCISCO_b)
table(brazil_full$OCCISCO_b)

brazil_full_OCCISCO_b <- brazil_full %>% select(YEAR,CITY,OCCISCO_b,PERWT)
brazil_full_OCCISCO_b <- brazil_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(brazil_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- brazil_full_OCCISCO_b %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- brazil_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- brazil_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- brazil_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

brazil_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR","CITY"))
brazil_OCCISCO_b<- brazil_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR","CITY"))
brazil_OCCISCO_b<- brazil_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR","CITY"))

#write.csv(brazil_OCCISCO_b,file="brasil_ocup.csv")

# for PUBl_ASSET
belo_horizonte_PUBl_ASSET<-belo_horizonte_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- belo_horizonte_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
belo_horizonte_PUBl_ASSET <- belo_horizonte_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
belo_horizonte_PUBl_ASSET <- belo_horizonte_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
belo_horizonte_PUBl_ASSET$CITY<-"belo_horizonte"
# for PRIV_ASSET
belo_horizonte_PRIV_ASSET<-belo_horizonte_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- belo_horizonte_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
belo_horizonte_PRIV_ASSET <- belo_horizonte_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
belo_horizonte_PRIV_ASSET <- belo_horizonte_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
belo_horizonte_PRIV_ASSET$CITY<-"belo_horizonte"
# for TOTAL_ASSET
belo_horizonte_TOTAL_ASSET<-belo_horizonte_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- belo_horizonte_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
belo_horizonte_TOTAL_ASSET <- belo_horizonte_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
belo_horizonte_TOTAL_ASSET <- belo_horizonte_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
belo_horizonte_TOTAL_ASSET$CITY<-"belo_horizonte"
write.csv(belo_horizonte_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/belo_horizonte_PUBl_ASSET.csv", row.names = TRUE)
write.csv(belo_horizonte_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/belo_horizonte_PRIV_ASSET.csv", row.names = TRUE)
write.csv(belo_horizonte_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/belo_horizonte_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
curitiba_PUBl_ASSET<-curitiba_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- curitiba_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
curitiba_PUBl_ASSET <- curitiba_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
curitiba_PUBl_ASSET <- curitiba_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
curitiba_PUBl_ASSET$CITY<-"curitiba"
# for PRIV_ASSET
curitiba_PRIV_ASSET<-curitiba_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- curitiba_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
curitiba_PRIV_ASSET <- curitiba_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
curitiba_PRIV_ASSET <- curitiba_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
curitiba_PRIV_ASSET$CITY<-"curitiba"
# for TOTAL_ASSET
curitiba_TOTAL_ASSET<-curitiba_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- curitiba_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
curitiba_TOTAL_ASSET <- curitiba_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
curitiba_TOTAL_ASSET <- curitiba_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
curitiba_TOTAL_ASSET$CITY<-"curitiba"
write.csv(curitiba_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/curitiba_PUBl_ASSET.csv", row.names = TRUE)
write.csv(curitiba_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/curitiba_PRIV_ASSET.csv", row.names = TRUE)
write.csv(curitiba_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/curitiba_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
florianopolis_PUBl_ASSET<-florianopolis_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- florianopolis_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
florianopolis_PUBl_ASSET <- florianopolis_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
florianopolis_PUBl_ASSET <- florianopolis_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
florianopolis_PUBl_ASSET$CITY<-"florianopolis"
# for PRIV_ASSET
florianopolis_PRIV_ASSET<-florianopolis_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- florianopolis_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
florianopolis_PRIV_ASSET <- florianopolis_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
florianopolis_PRIV_ASSET <- florianopolis_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
florianopolis_PRIV_ASSET$CITY<-"florianopolis"
# for TOTAL_ASSET
florianopolis_TOTAL_ASSET<-florianopolis_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- florianopolis_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
florianopolis_TOTAL_ASSET <- florianopolis_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
florianopolis_TOTAL_ASSET <- florianopolis_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
florianopolis_TOTAL_ASSET$CITY<-"florianopolis"
write.csv(florianopolis_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/florianopolis_PUBl_ASSET.csv", row.names = TRUE)
write.csv(florianopolis_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/florianopolis_PRIV_ASSET.csv", row.names = TRUE)
write.csv(florianopolis_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/florianopolis_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
ilheus_PUBl_ASSET<-ilheus_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ilheus_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ilheus_PUBl_ASSET <- ilheus_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ilheus_PUBl_ASSET <- ilheus_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ilheus_PUBl_ASSET$CITY<-"ilheus"
# for PRIV_ASSET
ilheus_PRIV_ASSET<-ilheus_full %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ilheus_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ilheus_PRIV_ASSET <- ilheus_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ilheus_PRIV_ASSET <- ilheus_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ilheus_PRIV_ASSET$CITY<-"ilheus"
# for TOTAL_ASSET
ilheus_TOTAL_ASSET<-ilheus_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ilheus_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ilheus_TOTAL_ASSET <- ilheus_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ilheus_TOTAL_ASSET <- ilheus_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ilheus_TOTAL_ASSET$CITY<-"ilheus"
write.csv(ilheus_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ilheus_PUBl_ASSET.csv", row.names = TRUE)
write.csv(ilheus_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ilheus_PRIV_ASSET.csv", row.names = TRUE)
write.csv(ilheus_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ilheus_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
jequie_PUBl_ASSET<-jequie_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- jequie_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
jequie_PUBl_ASSET <- jequie_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
jequie_PUBl_ASSET <- jequie_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
jequie_PUBl_ASSET$CITY<-"jequie"
# for PRIV_ASSET
jequie_PRIV_ASSET<-jequie_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- jequie_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
jequie_PRIV_ASSET <- jequie_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
jequie_PRIV_ASSET <- jequie_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
jequie_PRIV_ASSET$CITY<-"jequie"
# for TOTAL_ASSET
jequie_TOTAL_ASSET<-jequie_full %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- jequie_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
jequie_TOTAL_ASSET <- jequie_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
jequie_TOTAL_ASSET <- jequie_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
jequie_TOTAL_ASSET$CITY<-"jequie"
write.csv(jequie_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/jequie_PUBl_ASSET.csv", row.names = TRUE)
write.csv(jequie_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/jequie_PRIV_ASSET.csv", row.names = TRUE)
write.csv(jequie_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/jequie_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
palmas_PUBl_ASSET<-palmas_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- palmas_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
palmas_PUBl_ASSET <- palmas_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
palmas_PUBl_ASSET <- palmas_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
palmas_PUBl_ASSET$CITY<-"palmas"
# for PRIV_ASSET
palmas_PRIV_ASSET<-palmas_full %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- palmas_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
palmas_PRIV_ASSET <- palmas_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
palmas_PRIV_ASSET <- palmas_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
palmas_PRIV_ASSET$CITY<-"palmas"
# for TOTAL_ASSET
palmas_TOTAL_ASSET<-palmas_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- palmas_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
palmas_TOTAL_ASSET <- palmas_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
palmas_TOTAL_ASSET <- palmas_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
palmas_TOTAL_ASSET$CITY<-"palmas"
write.csv(palmas_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/palmas_PUBl_ASSET.csv", row.names = TRUE)
write.csv(palmas_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/palmas_PRIV_ASSET.csv", row.names = TRUE)
write.csv(palmas_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/palmas_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
ribeirao_preto_PUBl_ASSET<-ribeirao_preto_full %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ribeirao_preto_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ribeirao_preto_PUBl_ASSET <- ribeirao_preto_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ribeirao_preto_PUBl_ASSET <- ribeirao_preto_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ribeirao_preto_PUBl_ASSET$CITY<-"ribeirao_preto"
# for PRIV_ASSET
ribeirao_preto_PRIV_ASSET<-ribeirao_preto_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ribeirao_preto_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ribeirao_preto_PRIV_ASSET <- ribeirao_preto_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ribeirao_preto_PRIV_ASSET <- ribeirao_preto_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ribeirao_preto_PRIV_ASSET$CITY<-"ribeirao_preto"
# for TOTAL_ASSET
ribeirao_preto_TOTAL_ASSET<-ribeirao_preto_full %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ribeirao_preto_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ribeirao_preto_TOTAL_ASSET <- ribeirao_preto_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ribeirao_preto_TOTAL_ASSET <- ribeirao_preto_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ribeirao_preto_TOTAL_ASSET$CITY<-"ribeirao_preto"
write.csv(ribeirao_preto_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ribeirao_preto_PUBl_ASSET.csv", row.names = TRUE)
write.csv(ribeirao_preto_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ribeirao_preto_PRIV_ASSET.csv", row.names = TRUE)
write.csv(ribeirao_preto_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ribeirao_preto_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
sao_paulo_PUBl_ASSET<-sao_paulo_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- sao_paulo_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
sao_paulo_PUBl_ASSET <- sao_paulo_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
sao_paulo_PUBl_ASSET <- sao_paulo_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
sao_paulo_PUBl_ASSET$CITY<-"sao_paulo"
# for PRIV_ASSET
sao_paulo_PRIV_ASSET<-sao_paulo_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- sao_paulo_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
sao_paulo_PRIV_ASSET <- sao_paulo_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
sao_paulo_PRIV_ASSET <- sao_paulo_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
sao_paulo_PRIV_ASSET$CITY<-"sao_paulo"
# for TOTAL_ASSET
sao_paulo_TOTAL_ASSET<-sao_paulo_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- sao_paulo_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
sao_paulo_TOTAL_ASSET <- sao_paulo_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
sao_paulo_TOTAL_ASSET <- sao_paulo_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
sao_paulo_TOTAL_ASSET$CITY<-"sao_paulo"
write.csv(sao_paulo_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/sao_paulo_PUBl_ASSET.csv", row.names = TRUE)
write.csv(sao_paulo_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/sao_paulo_PRIV_ASSET.csv", row.names = TRUE)
write.csv(sao_paulo_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/sao_paulo_TOTAL_ASSET.csv", row.names = TRUE)


######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

belo_horizonte_WATER <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(belo_horizonte_WATER$WATSUP)
belo_horizonte_WATER <- belo_horizonte_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(belo_horizonte_WATER$WATSUP)
table(belo_horizonte_WATER$water_b)

belo_horizonte_WATER$water_b <- factor(belo_horizonte_WATER$water_b)

belo_horizonte_WATER$YEAR <- factor(belo_horizonte_WATER$YEAR)
belo_horizonte_WATER_00 <- belo_horizonte_WATER %>% filter(belo_horizonte_WATER$YEAR==2000)
belo_horizonte_WATER_10 <- belo_horizonte_WATER %>% filter(belo_horizonte_WATER$YEAR==2010)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = belo_horizonte_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

belo_horizonte_OWN <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(belo_horizonte_OWN$OWNERSHIP)
belo_horizonte_OWN <- belo_horizonte_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(belo_horizonte_OWN$OWNERSHIP)
table(belo_horizonte_OWN$owner_b)

belo_horizonte_OWN$owner_b <- factor(belo_horizonte_OWN$owner_b)

belo_horizonte_OWN$YEAR <- factor(belo_horizonte_OWN$YEAR)
belo_horizonte_OWN_00 <- belo_horizonte_OWN %>% filter(belo_horizonte_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = belo_horizonte_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

belo_horizonte_ELECTRIC <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(belo_horizonte_ELECTRIC$ELECTRIC)
belo_horizonte_ELECTRIC <- belo_horizonte_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(belo_horizonte_ELECTRIC$ELECTRIC)
table(belo_horizonte_ELECTRIC$eletric_b)

belo_horizonte_ELECTRIC$eletric_b <- factor(belo_horizonte_ELECTRIC$eletric_b)

belo_horizonte_ELECTRIC$YEAR <- factor(belo_horizonte_ELECTRIC$YEAR)
belo_horizonte_ELECTRIC_00 <- belo_horizonte_ELECTRIC %>% filter(belo_horizonte_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = belo_horizonte_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_0))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

belo_horizonte_SEWG <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(belo_horizonte_SEWG$SEWAGE)
belo_horizonte_SEWG <- belo_horizonte_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(belo_horizonte_SEWG$SEWAGE)
table(belo_horizonte_SEWG$sewage_b)

belo_horizonte_SEWG$sewage_b <- factor(belo_horizonte_SEWG$sewage_b)

belo_horizonte_SEWG$YEAR <- factor(belo_horizonte_SEWG$YEAR)
belo_horizonte_SEWG_00 <- belo_horizonte_SEWG %>% filter(belo_horizonte_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = belo_horizonte_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR TRASH ##################################

belo_horizonte_TRASH <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(belo_horizonte_TRASH$TRASH)
belo_horizonte_TRASH <- belo_horizonte_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(belo_horizonte_TRASH$TRASH)
table(belo_horizonte_TRASH$trash_b)

belo_horizonte_TRASH$trash_b <- factor(belo_horizonte_TRASH$trash_b)

belo_horizonte_TRASH$YEAR <- factor(belo_horizonte_TRASH$YEAR)
belo_horizonte_TRASH_00 <- belo_horizonte_TRASH %>% filter(belo_horizonte_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = belo_horizonte_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

belo_horizonte_AUTOS <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(belo_horizonte_AUTOS$AUTOS)
belo_horizonte_AUTOS <- belo_horizonte_AUTOS %>% filter(AUTOS!=9)

table(belo_horizonte_AUTOS$AUTOS)
table(belo_horizonte_AUTOS$autos_b)

belo_horizonte_AUTOS$autos_b <- factor(belo_horizonte_AUTOS$autos_b)

belo_horizonte_AUTOS$YEAR <- factor(belo_horizonte_AUTOS$YEAR)
belo_horizonte_AUTOS_00 <- belo_horizonte_AUTOS %>% filter(belo_horizonte_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = belo_horizonte_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

belo_horizonte_REFRIG <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(belo_horizonte_REFRIG$REFRIG)
belo_horizonte_REFRIG <- belo_horizonte_REFRIG %>% filter(REFRIG!=0)

table(belo_horizonte_REFRIG$REFRIG)
table(belo_horizonte_REFRIG$refrig_b)

belo_horizonte_REFRIG$refrig_b <- factor(belo_horizonte_REFRIG$refrig_b)

belo_horizonte_REFRIG$YEAR <- factor(belo_horizonte_REFRIG$YEAR)
belo_horizonte_REFRIG_00 <- belo_horizonte_REFRIG %>% filter(belo_horizonte_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = belo_horizonte_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

belo_horizonte_TV <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(belo_horizonte_TV$TV)
belo_horizonte_TV <- belo_horizonte_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(belo_horizonte_TV$TV)
table(belo_horizonte_TV$tv_b)

belo_horizonte_TV$tv_b <- factor(belo_horizonte_TV$tv_b)

belo_horizonte_TV$YEAR <- factor(belo_horizonte_TV$YEAR)
belo_horizonte_TV_00 <- belo_horizonte_TV %>% filter(belo_horizonte_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = belo_horizonte_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}


#################################### FOR TOILET ##################################

belo_horizonte_BATH <- belo_horizonte_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(belo_horizonte_BATH$BATH)
belo_horizonte_BATH <- belo_horizonte_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(belo_horizonte_BATH$BATH)
table(belo_horizonte_BATH$bath_b)

belo_horizonte_BATH$bath_b <- factor(belo_horizonte_BATH$bath_b)

belo_horizonte_BATH$YEAR <- factor(belo_horizonte_BATH$YEAR)
belo_horizonte_BATH_00 <- belo_horizonte_BATH %>% filter(belo_horizonte_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = belo_horizonte_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}


#################################### consolidating data for belo_horizonte ##################################

BH_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
BH_logit$CITY <- "Belo Horizonte"

gc()

#################################### FOR ilheus ##################################
#################################### FOR WATSUP ##################################

ilheus_WATER <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(ilheus_WATER$WATSUP)
ilheus_WATER <- ilheus_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(ilheus_WATER$WATSUP)
table(ilheus_WATER$water_b)

ilheus_WATER$water_b <- factor(ilheus_WATER$water_b)

ilheus_WATER$YEAR <- factor(ilheus_WATER$YEAR)
ilheus_WATER_00 <- ilheus_WATER %>% filter(ilheus_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = ilheus_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

ilheus_OWN <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(ilheus_OWN$OWNERSHIP)
ilheus_OWN <- ilheus_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(ilheus_OWN$OWNERSHIP)
table(ilheus_OWN$owner_b)

ilheus_OWN$owner_b <- factor(ilheus_OWN$owner_b)

ilheus_OWN$YEAR <- factor(ilheus_OWN$YEAR)
ilheus_OWN_00 <- ilheus_OWN %>% filter(ilheus_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = ilheus_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

ilheus_ELECTRIC <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(ilheus_ELECTRIC$ELECTRIC)
ilheus_ELECTRIC <- ilheus_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(ilheus_ELECTRIC$ELECTRIC)
table(ilheus_ELECTRIC$eletric_b)

ilheus_ELECTRIC$eletric_b <- factor(ilheus_ELECTRIC$eletric_b)

ilheus_ELECTRIC$YEAR <- factor(ilheus_ELECTRIC$YEAR)
ilheus_ELECTRIC_00 <- ilheus_ELECTRIC %>% filter(ilheus_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = ilheus_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

ilheus_SEWG <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(ilheus_SEWG$SEWAGE)
ilheus_SEWG <- ilheus_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(ilheus_SEWG$SEWAGE)
table(ilheus_SEWG$sewage_b)

ilheus_SEWG$sewage_b <- factor(ilheus_SEWG$sewage_b)

ilheus_SEWG$YEAR <- factor(ilheus_SEWG$YEAR)
ilheus_SEWG_00 <- ilheus_SEWG %>% filter(ilheus_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = ilheus_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR TRASH ##################################

ilheus_TRASH <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(ilheus_TRASH$TRASH)
ilheus_TRASH <- ilheus_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(ilheus_TRASH$TRASH)
table(ilheus_TRASH$trash_b)

ilheus_TRASH$trash_b <- factor(ilheus_TRASH$trash_b)

ilheus_TRASH$YEAR <- factor(ilheus_TRASH$YEAR)
ilheus_TRASH_00 <- ilheus_TRASH %>% filter(ilheus_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = ilheus_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

ilheus_AUTOS <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(ilheus_AUTOS$AUTOS)
ilheus_AUTOS <- ilheus_AUTOS %>% filter(AUTOS!=9)

table(ilheus_AUTOS$AUTOS)
table(ilheus_AUTOS$autos_b)

ilheus_AUTOS$autos_b <- factor(ilheus_AUTOS$autos_b)

ilheus_AUTOS$YEAR <- factor(ilheus_AUTOS$YEAR)
ilheus_AUTOS_00 <- ilheus_AUTOS %>% filter(ilheus_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = ilheus_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

ilheus_REFRIG <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(ilheus_REFRIG$REFRIG)
ilheus_REFRIG <- ilheus_REFRIG %>% filter(REFRIG!=0)

table(ilheus_REFRIG$REFRIG)
table(ilheus_REFRIG$refrig_b)

ilheus_REFRIG$refrig_b <- factor(ilheus_REFRIG$refrig_b)

ilheus_REFRIG$YEAR <- factor(ilheus_REFRIG$YEAR)
ilheus_REFRIG_00 <- ilheus_REFRIG %>% filter(ilheus_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = ilheus_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

ilheus_TV <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(ilheus_TV$TV)
ilheus_TV <- ilheus_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(ilheus_TV$TV)
table(ilheus_TV$tv_b)

ilheus_TV$tv_b <- factor(ilheus_TV$tv_b)

ilheus_TV$YEAR <- factor(ilheus_TV$YEAR)
ilheus_TV_00 <- ilheus_TV %>% filter(ilheus_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = ilheus_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

ilheus_BATH <- ilheus_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(ilheus_BATH$BATH)
ilheus_BATH <- ilheus_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(ilheus_BATH$BATH)
table(ilheus_BATH$bath_b)

ilheus_BATH$bath_b <- factor(ilheus_BATH$bath_b)

ilheus_BATH$YEAR <- factor(ilheus_BATH$YEAR)
ilheus_BATH_00 <- ilheus_BATH %>% filter(ilheus_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = ilheus_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}

#################################### consolidating data for ilheus ##################################

ILHEUS_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
ILHEUS_logit$CITY <- "Ilheus"

gc()

#################################### FOR jequie ##################################
#################################### FOR WATSUP ##################################

jequie_WATER <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(jequie_WATER$WATSUP)
jequie_WATER <- jequie_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(jequie_WATER$WATSUP)
table(jequie_WATER$water_b)

jequie_WATER$water_b <- factor(jequie_WATER$water_b)

jequie_WATER$YEAR <- factor(jequie_WATER$YEAR)
jequie_WATER_00 <- jequie_WATER %>% filter(jequie_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = jequie_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

jequie_OWN <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(jequie_OWN$OWNERSHIP)
jequie_OWN <- jequie_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(jequie_OWN$OWNERSHIP)
table(jequie_OWN$owner_b)

jequie_OWN$owner_b <- factor(jequie_OWN$owner_b)

jequie_OWN$YEAR <- factor(jequie_OWN$YEAR)
jequie_OWN_00 <- jequie_OWN %>% filter(jequie_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = jequie_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

jequie_ELECTRIC <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(jequie_ELECTRIC$ELECTRIC)
jequie_ELECTRIC <- jequie_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(jequie_ELECTRIC$ELECTRIC)
table(jequie_ELECTRIC$eletric_b)

jequie_ELECTRIC$eletric_b <- factor(jequie_ELECTRIC$eletric_b)

jequie_ELECTRIC$YEAR <- factor(jequie_ELECTRIC$YEAR)
jequie_ELECTRIC_00 <- jequie_ELECTRIC %>% filter(jequie_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = jequie_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

jequie_SEWG <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(jequie_SEWG$SEWAGE)
jequie_SEWG <- jequie_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(jequie_SEWG$SEWAGE)
table(jequie_SEWG$sewage_b)

jequie_SEWG$sewage_b <- factor(jequie_SEWG$sewage_b)

jequie_SEWG$YEAR <- factor(jequie_SEWG$YEAR)
jequie_SEWG_00 <- jequie_SEWG %>% filter(jequie_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = jequie_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}


#################################### FOR TRASH ##################################

jequie_TRASH <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(jequie_TRASH$TRASH)
jequie_TRASH <- jequie_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(jequie_TRASH$TRASH)
table(jequie_TRASH$trash_b)

jequie_TRASH$trash_b <- factor(jequie_TRASH$trash_b)

jequie_TRASH$YEAR <- factor(jequie_TRASH$YEAR)
jequie_TRASH_00 <- jequie_TRASH %>% filter(jequie_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = jequie_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}


#################################### FOR AUTOS ##################################

jequie_AUTOS <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(jequie_AUTOS$AUTOS)
jequie_AUTOS <- jequie_AUTOS %>% filter(AUTOS!=9)

table(jequie_AUTOS$AUTOS)
table(jequie_AUTOS$autos_b)

jequie_AUTOS$autos_b <- factor(jequie_AUTOS$autos_b)

jequie_AUTOS$YEAR <- factor(jequie_AUTOS$YEAR)
jequie_AUTOS_00 <- jequie_AUTOS %>% filter(jequie_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = jequie_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

jequie_REFRIG <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(jequie_REFRIG$REFRIG)
jequie_REFRIG <- jequie_REFRIG %>% filter(REFRIG!=0)

table(jequie_REFRIG$REFRIG)
table(jequie_REFRIG$refrig_b)

jequie_REFRIG$refrig_b <- factor(jequie_REFRIG$refrig_b)

jequie_REFRIG$YEAR <- factor(jequie_REFRIG$YEAR)
jequie_REFRIG_00 <- jequie_REFRIG %>% filter(jequie_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = jequie_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

jequie_TV <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(jequie_TV$TV)
jequie_TV <- jequie_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(jequie_TV$TV)
table(jequie_TV$tv_b)

jequie_TV$tv_b <- factor(jequie_TV$tv_b)

jequie_TV$YEAR <- factor(jequie_TV$YEAR)
jequie_TV_00 <- jequie_TV %>% filter(jequie_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = jequie_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

jequie_BATH <- jequie_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(jequie_BATH$BATH)
jequie_BATH <- jequie_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(jequie_BATH$BATH)
table(jequie_BATH$bath_b)

jequie_BATH$bath_b <- factor(jequie_BATH$bath_b)

jequie_BATH$YEAR <- factor(jequie_BATH$YEAR)
jequie_BATH_00 <- jequie_BATH %>% filter(jequie_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = jequie_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}

#################################### consolidating data for jequie ##################################

JEQUIE_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
JEQUIE_logit$CITY <- "Jequie"

gc()


#################################### FOR palmas ##################################
#################################### FOR WATSUP ##################################

palmas_WATER <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(palmas_WATER$WATSUP)
palmas_WATER <- palmas_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(palmas_WATER$WATSUP)
table(palmas_WATER$water_b)

palmas_WATER$water_b <- factor(palmas_WATER$water_b)

palmas_WATER$YEAR <- factor(palmas_WATER$YEAR)
palmas_WATER_00 <- palmas_WATER %>% filter(palmas_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = palmas_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}



#################################### FOR OWNERSHIP ##################################

palmas_OWN <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(palmas_OWN$OWNERSHIP)
palmas_OWN <- palmas_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(palmas_OWN$OWNERSHIP)
table(palmas_OWN$owner_b)

palmas_OWN$owner_b <- factor(palmas_OWN$owner_b)

palmas_OWN$YEAR <- factor(palmas_OWN$YEAR)
palmas_OWN_00 <- palmas_OWN %>% filter(palmas_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = palmas_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

palmas_ELECTRIC <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(palmas_ELECTRIC$ELECTRIC)
palmas_ELECTRIC <- palmas_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(palmas_ELECTRIC$ELECTRIC)
table(palmas_ELECTRIC$eletric_b)

palmas_ELECTRIC$eletric_b <- factor(palmas_ELECTRIC$eletric_b)

palmas_ELECTRIC$YEAR <- factor(palmas_ELECTRIC$YEAR)
palmas_ELECTRIC_00 <- palmas_ELECTRIC %>% filter(palmas_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = palmas_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

palmas_SEWG <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(palmas_SEWG$SEWAGE)
palmas_SEWG <- palmas_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(palmas_SEWG$SEWAGE)
table(palmas_SEWG$sewage_b)

palmas_SEWG$sewage_b <- factor(palmas_SEWG$sewage_b)

palmas_SEWG$YEAR <- factor(palmas_SEWG$YEAR)
palmas_SEWG_00 <- palmas_SEWG %>% filter(palmas_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = palmas_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR TRASH ##################################

palmas_TRASH <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(palmas_TRASH$TRASH)
palmas_TRASH <- palmas_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(palmas_TRASH$TRASH)
table(palmas_TRASH$trash_b)

palmas_TRASH$trash_b <- factor(palmas_TRASH$trash_b)

palmas_TRASH$YEAR <- factor(palmas_TRASH$YEAR)
palmas_TRASH_00 <- palmas_TRASH %>% filter(palmas_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = palmas_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

palmas_AUTOS <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(palmas_AUTOS$AUTOS)
palmas_AUTOS <- palmas_AUTOS %>% filter(AUTOS!=9)

table(palmas_AUTOS$AUTOS)
table(palmas_AUTOS$autos_b)

palmas_AUTOS$autos_b <- factor(palmas_AUTOS$autos_b)

palmas_AUTOS$YEAR <- factor(palmas_AUTOS$YEAR)
palmas_AUTOS_00 <- palmas_AUTOS %>% filter(palmas_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = palmas_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

palmas_REFRIG <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(palmas_REFRIG$REFRIG)
palmas_REFRIG <- palmas_REFRIG %>% filter(REFRIG!=0)

table(palmas_REFRIG$REFRIG)
table(palmas_REFRIG$refrig_b)

palmas_REFRIG$refrig_b <- factor(palmas_REFRIG$refrig_b)

palmas_REFRIG$YEAR <- factor(palmas_REFRIG$YEAR)
palmas_REFRIG_00 <- palmas_REFRIG %>% filter(palmas_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = palmas_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

palmas_TV <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(palmas_TV$TV)
palmas_TV <- palmas_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(palmas_TV$TV)
table(palmas_TV$tv_b)

palmas_TV$tv_b <- factor(palmas_TV$tv_b)

palmas_TV$YEAR <- factor(palmas_TV$YEAR)
palmas_TV_00 <- palmas_TV %>% filter(palmas_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = palmas_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

palmas_BATH <- palmas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(palmas_BATH$BATH)
palmas_BATH <- palmas_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(palmas_BATH$BATH)
table(palmas_BATH$bath_b)

palmas_BATH$bath_b <- factor(palmas_BATH$bath_b)

palmas_BATH$YEAR <- factor(palmas_BATH$YEAR)
palmas_BATH_00 <- palmas_BATH %>% filter(palmas_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = palmas_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}

#################################### consolidating data for palmas ##################################

palmas_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
palmas_logit$CITY <- "Palmas"

gc()

#################################### FOR ribeirao_preto ##################################
#################################### FOR WATSUP ##################################

ribeirao_preto_WATER <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(ribeirao_preto_WATER$WATSUP)
ribeirao_preto_WATER <- ribeirao_preto_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(ribeirao_preto_WATER$WATSUP)
table(ribeirao_preto_WATER$water_b)

ribeirao_preto_WATER$water_b <- factor(ribeirao_preto_WATER$water_b)

ribeirao_preto_WATER$YEAR <- factor(ribeirao_preto_WATER$YEAR)
ribeirao_preto_WATER_00 <- ribeirao_preto_WATER %>% filter(ribeirao_preto_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = ribeirao_preto_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

ribeirao_preto_OWN <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(ribeirao_preto_OWN$OWNERSHIP)
ribeirao_preto_OWN <- ribeirao_preto_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(ribeirao_preto_OWN$OWNERSHIP)
table(ribeirao_preto_OWN$owner_b)

ribeirao_preto_OWN$owner_b <- factor(ribeirao_preto_OWN$owner_b)

ribeirao_preto_OWN$YEAR <- factor(ribeirao_preto_OWN$YEAR)
ribeirao_preto_OWN_00 <- ribeirao_preto_OWN %>% filter(ribeirao_preto_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = ribeirao_preto_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

ribeirao_preto_ELECTRIC <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(ribeirao_preto_ELECTRIC$ELECTRIC)
ribeirao_preto_ELECTRIC <- ribeirao_preto_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(ribeirao_preto_ELECTRIC$ELECTRIC)
table(ribeirao_preto_ELECTRIC$eletric_b)

ribeirao_preto_ELECTRIC$eletric_b <- factor(ribeirao_preto_ELECTRIC$eletric_b)

ribeirao_preto_ELECTRIC$YEAR <- factor(ribeirao_preto_ELECTRIC$YEAR)
ribeirao_preto_ELECTRIC_00 <- ribeirao_preto_ELECTRIC %>% filter(ribeirao_preto_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = ribeirao_preto_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

ribeirao_preto_SEWG <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(ribeirao_preto_SEWG$SEWAGE)
ribeirao_preto_SEWG <- ribeirao_preto_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(ribeirao_preto_SEWG$SEWAGE)
table(ribeirao_preto_SEWG$sewage_b)

ribeirao_preto_SEWG$sewage_b <- factor(ribeirao_preto_SEWG$sewage_b)

ribeirao_preto_SEWG$YEAR <- factor(ribeirao_preto_SEWG$YEAR)
ribeirao_preto_SEWG_00 <- ribeirao_preto_SEWG %>% filter(ribeirao_preto_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = ribeirao_preto_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR TRASH ##################################

ribeirao_preto_TRASH <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(ribeirao_preto_TRASH$TRASH)
ribeirao_preto_TRASH <- ribeirao_preto_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(ribeirao_preto_TRASH$TRASH)
table(ribeirao_preto_TRASH$trash_b)

ribeirao_preto_TRASH$trash_b <- factor(ribeirao_preto_TRASH$trash_b)

ribeirao_preto_TRASH$YEAR <- factor(ribeirao_preto_TRASH$YEAR)
ribeirao_preto_TRASH_00 <- ribeirao_preto_TRASH %>% filter(ribeirao_preto_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = ribeirao_preto_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

ribeirao_preto_AUTOS <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(ribeirao_preto_AUTOS$AUTOS)
ribeirao_preto_AUTOS <- ribeirao_preto_AUTOS %>% filter(AUTOS!=9)

table(ribeirao_preto_AUTOS$AUTOS)
table(ribeirao_preto_AUTOS$autos_b)

ribeirao_preto_AUTOS$autos_b <- factor(ribeirao_preto_AUTOS$autos_b)

ribeirao_preto_AUTOS$YEAR <- factor(ribeirao_preto_AUTOS$YEAR)
ribeirao_preto_AUTOS_00 <- ribeirao_preto_AUTOS %>% filter(ribeirao_preto_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = ribeirao_preto_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

ribeirao_preto_REFRIG <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(ribeirao_preto_REFRIG$REFRIG)
ribeirao_preto_REFRIG <- ribeirao_preto_REFRIG %>% filter(REFRIG!=0)

table(ribeirao_preto_REFRIG$REFRIG)
table(ribeirao_preto_REFRIG$refrig_b)

ribeirao_preto_REFRIG$refrig_b <- factor(ribeirao_preto_REFRIG$refrig_b)

ribeirao_preto_REFRIG$YEAR <- factor(ribeirao_preto_REFRIG$YEAR)
ribeirao_preto_REFRIG_00 <- ribeirao_preto_REFRIG %>% filter(ribeirao_preto_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = ribeirao_preto_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

ribeirao_preto_TV <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(ribeirao_preto_TV$TV)
ribeirao_preto_TV <- ribeirao_preto_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(ribeirao_preto_TV$TV)
table(ribeirao_preto_TV$tv_b)

ribeirao_preto_TV$tv_b <- factor(ribeirao_preto_TV$tv_b)

ribeirao_preto_TV$YEAR <- factor(ribeirao_preto_TV$YEAR)
ribeirao_preto_TV_00 <- ribeirao_preto_TV %>% filter(ribeirao_preto_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = ribeirao_preto_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

ribeirao_preto_BATH <- ribeirao_preto_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(ribeirao_preto_BATH$BATH)
ribeirao_preto_BATH <- ribeirao_preto_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(ribeirao_preto_BATH$BATH)
table(ribeirao_preto_BATH$bath_b)

ribeirao_preto_BATH$bath_b <- factor(ribeirao_preto_BATH$bath_b)

ribeirao_preto_BATH$YEAR <- factor(ribeirao_preto_BATH$YEAR)
ribeirao_preto_BATH_00 <- ribeirao_preto_BATH %>% filter(ribeirao_preto_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = ribeirao_preto_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}


#################################### consolidating data for ribeirao_preto ##################################

ribeirao_preto_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
ribeirao_preto_logit$CITY <- "Ribeirao Preto"

gc()

#################################### FOR curitiba ##################################
#################################### FOR WATSUP ##################################

curitiba_WATER <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(curitiba_WATER$WATSUP)
curitiba_WATER <- curitiba_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(curitiba_WATER$WATSUP)
table(curitiba_WATER$water_b)

curitiba_WATER$water_b <- factor(curitiba_WATER$water_b)

curitiba_WATER$YEAR <- factor(curitiba_WATER$YEAR)
curitiba_WATER_00 <- curitiba_WATER %>% filter(curitiba_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = curitiba_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

curitiba_OWN <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(curitiba_OWN$OWNERSHIP)
curitiba_OWN <- curitiba_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(curitiba_OWN$OWNERSHIP)
table(curitiba_OWN$owner_b)

curitiba_OWN$owner_b <- factor(curitiba_OWN$owner_b)

curitiba_OWN$YEAR <- factor(curitiba_OWN$YEAR)
curitiba_OWN_00 <- curitiba_OWN %>% filter(curitiba_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = curitiba_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

curitiba_ELECTRIC <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(curitiba_ELECTRIC$ELECTRIC)
curitiba_ELECTRIC <- curitiba_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(curitiba_ELECTRIC$ELECTRIC)
table(curitiba_ELECTRIC$eletric_b)

curitiba_ELECTRIC$eletric_b <- factor(curitiba_ELECTRIC$eletric_b)

curitiba_ELECTRIC$YEAR <- factor(curitiba_ELECTRIC$YEAR)
curitiba_ELECTRIC_00 <- curitiba_ELECTRIC %>% filter(curitiba_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = curitiba_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

curitiba_SEWG <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(curitiba_SEWG$SEWAGE)
curitiba_SEWG <- curitiba_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(curitiba_SEWG$SEWAGE)
table(curitiba_SEWG$sewage_b)

curitiba_SEWG$sewage_b <- factor(curitiba_SEWG$sewage_b)

curitiba_SEWG$YEAR <- factor(curitiba_SEWG$YEAR)
curitiba_SEWG_00 <- curitiba_SEWG %>% filter(curitiba_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = curitiba_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR TRASH ##################################

curitiba_TRASH <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(curitiba_TRASH$TRASH)
curitiba_TRASH <- curitiba_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(curitiba_TRASH$TRASH)
table(curitiba_TRASH$trash_b)

curitiba_TRASH$trash_b <- factor(curitiba_TRASH$trash_b)

curitiba_TRASH$YEAR <- factor(curitiba_TRASH$YEAR)
curitiba_TRASH_00 <- curitiba_TRASH %>% filter(curitiba_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = curitiba_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

curitiba_AUTOS <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(curitiba_AUTOS$AUTOS)
curitiba_AUTOS <- curitiba_AUTOS %>% filter(AUTOS!=9)

table(curitiba_AUTOS$AUTOS)
table(curitiba_AUTOS$autos_b)

curitiba_AUTOS$autos_b <- factor(curitiba_AUTOS$autos_b)

curitiba_AUTOS$YEAR <- factor(curitiba_AUTOS$YEAR)
curitiba_AUTOS_00 <- curitiba_AUTOS %>% filter(curitiba_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = curitiba_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

curitiba_REFRIG <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(curitiba_REFRIG$REFRIG)
curitiba_REFRIG <- curitiba_REFRIG %>% filter(REFRIG!=0)

table(curitiba_REFRIG$REFRIG)
table(curitiba_REFRIG$refrig_b)

curitiba_REFRIG$refrig_b <- factor(curitiba_REFRIG$refrig_b)

curitiba_REFRIG$YEAR <- factor(curitiba_REFRIG$YEAR)
curitiba_REFRIG_00 <- curitiba_REFRIG %>% filter(curitiba_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = curitiba_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

curitiba_TV <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(curitiba_TV$TV)
curitiba_TV <- curitiba_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(curitiba_TV$TV)
table(curitiba_TV$tv_b)

curitiba_TV$tv_b <- factor(curitiba_TV$tv_b)

curitiba_TV$YEAR <- factor(curitiba_TV$YEAR)
curitiba_TV_00 <- curitiba_TV %>% filter(curitiba_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = curitiba_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

curitiba_BATH <- curitiba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(curitiba_BATH$BATH)
curitiba_BATH <- curitiba_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(curitiba_BATH$BATH)
table(curitiba_BATH$bath_b)

curitiba_BATH$bath_b <- factor(curitiba_BATH$bath_b)

curitiba_BATH$YEAR <- factor(curitiba_BATH$YEAR)
curitiba_BATH_00 <- curitiba_BATH %>% filter(curitiba_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = curitiba_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}


#################################### consolidating data for curitiba ##################################

curitiba_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
curitiba_logit$CITY <- "Curitiba"

gc()

#################################### FOR florianopolis ##################################
#################################### FOR WATSUP ##################################

florianopolis_WATER <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(florianopolis_WATER$WATSUP)
florianopolis_WATER <- florianopolis_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(florianopolis_WATER$WATSUP)
table(florianopolis_WATER$water_b)

florianopolis_WATER$water_b <- factor(florianopolis_WATER$water_b)

florianopolis_WATER$YEAR <- factor(florianopolis_WATER$YEAR)
florianopolis_WATER_00 <- florianopolis_WATER %>% filter(florianopolis_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = florianopolis_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

florianopolis_OWN <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(florianopolis_OWN$OWNERSHIP)
florianopolis_OWN <- florianopolis_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(florianopolis_OWN$OWNERSHIP)
table(florianopolis_OWN$owner_b)

florianopolis_OWN$owner_b <- factor(florianopolis_OWN$owner_b)

florianopolis_OWN$YEAR <- factor(florianopolis_OWN$YEAR)
florianopolis_OWN_00 <- florianopolis_OWN %>% filter(florianopolis_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = florianopolis_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

florianopolis_ELECTRIC <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(florianopolis_ELECTRIC$ELECTRIC)
florianopolis_ELECTRIC <- florianopolis_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(florianopolis_ELECTRIC$ELECTRIC)
table(florianopolis_ELECTRIC$eletric_b)

florianopolis_ELECTRIC$eletric_b <- factor(florianopolis_ELECTRIC$eletric_b)

florianopolis_ELECTRIC$YEAR <- factor(florianopolis_ELECTRIC$YEAR)
florianopolis_ELECTRIC_00 <- florianopolis_ELECTRIC %>% filter(florianopolis_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = florianopolis_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

florianopolis_SEWG <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(florianopolis_SEWG$SEWAGE)
florianopolis_SEWG <- florianopolis_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(florianopolis_SEWG$SEWAGE)
table(florianopolis_SEWG$sewage_b)

florianopolis_SEWG$sewage_b <- factor(florianopolis_SEWG$sewage_b)

florianopolis_SEWG$YEAR <- factor(florianopolis_SEWG$YEAR)
florianopolis_SEWG_00 <- florianopolis_SEWG %>% filter(florianopolis_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = florianopolis_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}


#################################### FOR TRASH ##################################

florianopolis_TRASH <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(florianopolis_TRASH$TRASH)
florianopolis_TRASH <- florianopolis_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(florianopolis_TRASH$TRASH)
table(florianopolis_TRASH$trash_b)

florianopolis_TRASH$trash_b <- factor(florianopolis_TRASH$trash_b)

florianopolis_TRASH$YEAR <- factor(florianopolis_TRASH$YEAR)
florianopolis_TRASH_00 <- florianopolis_TRASH %>% filter(florianopolis_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = florianopolis_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

florianopolis_AUTOS <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(florianopolis_AUTOS$AUTOS)
florianopolis_AUTOS <- florianopolis_AUTOS %>% filter(AUTOS!=9)

table(florianopolis_AUTOS$AUTOS)
table(florianopolis_AUTOS$autos_b)

florianopolis_AUTOS$autos_b <- factor(florianopolis_AUTOS$autos_b)

florianopolis_AUTOS$YEAR <- factor(florianopolis_AUTOS$YEAR)
florianopolis_AUTOS_00 <- florianopolis_AUTOS %>% filter(florianopolis_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = florianopolis_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

florianopolis_REFRIG <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(florianopolis_REFRIG$REFRIG)
florianopolis_REFRIG <- florianopolis_REFRIG %>% filter(REFRIG!=0)

table(florianopolis_REFRIG$REFRIG)
table(florianopolis_REFRIG$refrig_b)

florianopolis_REFRIG$refrig_b <- factor(florianopolis_REFRIG$refrig_b)

florianopolis_REFRIG$YEAR <- factor(florianopolis_REFRIG$YEAR)
florianopolis_REFRIG_00 <- florianopolis_REFRIG %>% filter(florianopolis_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = florianopolis_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}


#################################### FOR TV ##################################

florianopolis_TV <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(florianopolis_TV$TV)
florianopolis_TV <- florianopolis_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(florianopolis_TV$TV)
table(florianopolis_TV$tv_b)

florianopolis_TV$tv_b <- factor(florianopolis_TV$tv_b)

florianopolis_TV$YEAR <- factor(florianopolis_TV$YEAR)
florianopolis_TV_00 <- florianopolis_TV %>% filter(florianopolis_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = florianopolis_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}


#################################### FOR TOILET ##################################

florianopolis_BATH <- florianopolis_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(florianopolis_BATH$BATH)
florianopolis_BATH <- florianopolis_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(florianopolis_BATH$BATH)
table(florianopolis_BATH$bath_b)

florianopolis_BATH$bath_b <- factor(florianopolis_BATH$bath_b)

florianopolis_BATH$YEAR <- factor(florianopolis_BATH$YEAR)
florianopolis_BATH_00 <- florianopolis_BATH %>% filter(florianopolis_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = florianopolis_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}

#################################### consolidating data for florianopolis ##################################

florianopolis_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
florianopolis_logit$CITY <- "Florianopolis"

gc()

#################################### FOR sao_paulo ##################################
#################################### FOR WATSUP ##################################

sao_paulo_WATER <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(sao_paulo_WATER$WATSUP)
sao_paulo_WATER <- sao_paulo_WATER %>% filter(WATSUP!=00)%>% filter(WATSUP!=99)

table(sao_paulo_WATER$WATSUP)
table(sao_paulo_WATER$water_b)

sao_paulo_WATER$water_b <- factor(sao_paulo_WATER$water_b)

sao_paulo_WATER$YEAR <- factor(sao_paulo_WATER$YEAR)
sao_paulo_WATER_00 <- sao_paulo_WATER %>% filter(sao_paulo_WATER$YEAR==2000)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = sao_paulo_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

sao_paulo_OWN <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(sao_paulo_OWN$OWNERSHIP)
sao_paulo_OWN <- sao_paulo_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(sao_paulo_OWN$OWNERSHIP)
table(sao_paulo_OWN$owner_b)

sao_paulo_OWN$owner_b <- factor(sao_paulo_OWN$owner_b)

sao_paulo_OWN$YEAR <- factor(sao_paulo_OWN$YEAR)
sao_paulo_OWN_00 <- sao_paulo_OWN %>% filter(sao_paulo_OWN$YEAR==2000)

mylogit_00 <- glm(owner_b ~ YRSCHOOL, data = sao_paulo_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

sao_paulo_ELECTRIC <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(sao_paulo_ELECTRIC$ELECTRIC)
sao_paulo_ELECTRIC <- sao_paulo_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(sao_paulo_ELECTRIC$ELECTRIC)
table(sao_paulo_ELECTRIC$eletric_b)

sao_paulo_ELECTRIC$eletric_b <- factor(sao_paulo_ELECTRIC$eletric_b)

sao_paulo_ELECTRIC$YEAR <- factor(sao_paulo_ELECTRIC$YEAR)
sao_paulo_ELECTRIC_00 <- sao_paulo_ELECTRIC %>% filter(sao_paulo_ELECTRIC$YEAR==2000)

mylogit_00 <- glm(eletric_b ~ YRSCHOOL, data = sao_paulo_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

sao_paulo_SEWG <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(sao_paulo_SEWG$SEWAGE)
sao_paulo_SEWG <- sao_paulo_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(sao_paulo_SEWG$SEWAGE)
table(sao_paulo_SEWG$sewage_b)

sao_paulo_SEWG$sewage_b <- factor(sao_paulo_SEWG$sewage_b)

sao_paulo_SEWG$YEAR <- factor(sao_paulo_SEWG$YEAR)
sao_paulo_SEWG_00 <- sao_paulo_SEWG %>% filter(sao_paulo_SEWG$YEAR==2000)

mylogit_00 <- glm(sewage_b ~ YRSCHOOL, data = sao_paulo_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}


#################################### FOR TRASH ##################################

sao_paulo_TRASH <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(sao_paulo_TRASH$TRASH)
sao_paulo_TRASH <- sao_paulo_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(sao_paulo_TRASH$TRASH)
table(sao_paulo_TRASH$trash_b)

sao_paulo_TRASH$trash_b <- factor(sao_paulo_TRASH$trash_b)

sao_paulo_TRASH$YEAR <- factor(sao_paulo_TRASH$YEAR)
sao_paulo_TRASH_00 <- sao_paulo_TRASH %>% filter(sao_paulo_TRASH$YEAR==2000)

mylogit_00 <- glm(trash_b ~ YRSCHOOL, data = sao_paulo_TRASH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TRASH_T2 <- summary(mylogit_00)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

sao_paulo_AUTOS <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(sao_paulo_AUTOS$AUTOS)
sao_paulo_AUTOS <- sao_paulo_AUTOS %>% filter(AUTOS!=9)

table(sao_paulo_AUTOS$AUTOS)
table(sao_paulo_AUTOS$autos_b)

sao_paulo_AUTOS$autos_b <- factor(sao_paulo_AUTOS$autos_b)

sao_paulo_AUTOS$YEAR <- factor(sao_paulo_AUTOS$YEAR)
sao_paulo_AUTOS_00 <- sao_paulo_AUTOS %>% filter(sao_paulo_AUTOS$YEAR==2000)

mylogit_00 <- glm(autos_b ~ YRSCHOOL, data = sao_paulo_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

sao_paulo_REFRIG <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(sao_paulo_REFRIG$REFRIG)
sao_paulo_REFRIG <- sao_paulo_REFRIG %>% filter(REFRIG!=0)

table(sao_paulo_REFRIG$REFRIG)
table(sao_paulo_REFRIG$refrig_b)

sao_paulo_REFRIG$refrig_b <- factor(sao_paulo_REFRIG$refrig_b)

sao_paulo_REFRIG$YEAR <- factor(sao_paulo_REFRIG$YEAR)
sao_paulo_REFRIG_00 <- sao_paulo_REFRIG %>% filter(sao_paulo_REFRIG$YEAR==2000)

mylogit_00 <- glm(refrig_b ~ YRSCHOOL, data = sao_paulo_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TV ##################################

sao_paulo_TV <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(sao_paulo_TV$TV)
sao_paulo_TV <- sao_paulo_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(sao_paulo_TV$TV)
table(sao_paulo_TV$tv_b)

sao_paulo_TV$tv_b <- factor(sao_paulo_TV$tv_b)

sao_paulo_TV$YEAR <- factor(sao_paulo_TV$YEAR)
sao_paulo_TV_00 <- sao_paulo_TV %>% filter(sao_paulo_TV$YEAR==2000)

mylogit_00 <- glm(tv_b ~ YRSCHOOL, data = sao_paulo_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}


#################################### FOR TOILET ##################################

sao_paulo_BATH <- sao_paulo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,BATH,bath_b,YRSCHOOL,CITY)
table(sao_paulo_BATH$BATH)
sao_paulo_BATH <- sao_paulo_BATH %>% filter(BATH!=0) %>% filter(BATH!=99)

table(sao_paulo_BATH$BATH)
table(sao_paulo_BATH$bath_b)

sao_paulo_BATH$bath_b <- factor(sao_paulo_BATH$bath_b)

sao_paulo_BATH$YEAR <- factor(sao_paulo_BATH$YEAR)
sao_paulo_BATH_00 <- sao_paulo_BATH %>% filter(sao_paulo_BATH$YEAR==2000)

mylogit_00 <- glm(bath_b ~ YRSCHOOL, data = sao_paulo_BATH_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

BATH_T2 <- summary(mylogit_00)$coefficients
BATH_T2_z <- BATH_T2[2,1:4]
BATH_T2_z<-as.data.frame(BATH_T2_z)
if (BATH_T2_z[4,1] > 0.05) {BATH_T2_z[1,1] <- 0}


#################################### consolidating data for sao_paulo ##################################

sao_paulo_logit <- cbind(WATER_T2_z,OWN_T2_z,ELECTRIC_T2_z,SEWG_T2_z,TRASH_T2_z,AUTOS_T2_z,REFRIG_T2_z,TV_T2_z,BATH_T2_z)
sao_paulo_logit$CITY <- "Sao Paulo"

gc()

brasil_logit <- rbind(sao_paulo_logit, florianopolis_logit,BH_logit,curitiba_logit,ILHEUS_logit, JEQUIE_logit,ribeirao_preto_logit,palmas_logit,sep = ".") 
write.csv(brasil_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/brasil_logit.csv"))

