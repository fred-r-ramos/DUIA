library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/mexico")

geo2_mx90 <- read_sf("geo2_mx1990.shp")
geo2_mx00 <- read_sf("geo2_mx2000.shp")
geo2_mx15 <- read_sf("geo2_mx2015.shp")
AUE_gu <- read_sf("Guadalajara_studyArea.shp")
AUE_me <- read_sf("Mexico_City_studyArea.shp")
AUE_re<- read_sf("reynosa_studyArea.shp")
AUE_ti <- read_sf("Tijuana_studyArea.shp")
AUE_gu <- st_transform(AUE_gu, 4326)
AUE_me <- st_transform(AUE_me, 4326)
AUE_re  <- st_transform(AUE_re, 4326)
AUE_ti  <- st_transform(AUE_ti, 4326)

sf::sf_use_s2(FALSE)

centroid_mx90 <- st_centroid(geo2_mx90)
centroid_mx00 <- st_centroid(geo2_mx00)
centroid_mx15 <- st_centroid(geo2_mx15)

guadalajara_90 <- geo2_mx90[st_intersection(AUE_gu, centroid_mx90),]
guadalajara_90['CITY']='guadalajara'
guadalajara_00 <- geo2_mx00[st_intersection(AUE_gu, centroid_mx00),]
guadalajara_00['CITY']='guadalajara'
guadalajara_15 <- geo2_mx15[st_intersection(AUE_gu, centroid_mx15),]
guadalajara_15['CITY']='guadalajara'

mexico_90 <- geo2_mx90[st_intersection(AUE_me, centroid_mx90),]
mexico_90['CITY']='mexico'
mexico_00 <- geo2_mx00[st_intersection(AUE_me, centroid_mx00),]
mexico_00['CITY']='mexico'
mexico_15 <- geo2_mx15[st_intersection(AUE_me, centroid_mx15),]
mexico_15['CITY']='mexico'

reynosa_90 <- geo2_mx90[st_intersection(AUE_re, centroid_mx90),]
reynosa_90['CITY']='reynosa'
reynosa_00 <- geo2_mx00[st_intersection(AUE_re, centroid_mx00),]
reynosa_00['CITY']='reynosa'
reynosa_15 <- geo2_mx15[st_intersection(AUE_re, centroid_mx15),]
reynosa_15['CITY']='reynosa'

tijuana_90 <- geo2_mx90[st_intersection(AUE_ti, centroid_mx90),]
tijuana_90['CITY']='tijuana'
tijuana_00 <- geo2_mx00[st_intersection(AUE_ti, centroid_mx00),]
tijuana_00['CITY']='tijuana'
tijuana_15 <- geo2_mx15[st_intersection(AUE_ti, centroid_mx15),]
tijuana_15['CITY']='tijuana'

geo_mexico_90 <- rbind(reynosa_90,tijuana_90,mexico_90,guadalajara_90)
geo_mexico_00 <- rbind(reynosa_00,tijuana_00,mexico_00,guadalajara_00)
geo_mexico_15 <- rbind(reynosa_15,tijuana_15,mexico_15,guadalajara_15)

plot(st_geometry(geo_mexico_15), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_mx15[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00230.xml")
ddi <-read_ipums_ddi("ipumsi_00256.xml")
mexico <- read_ipums_micro(ddi)

names(mexico)

######################creating binary variables for logit regressions

names(mexico)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(mexico$OWNERSHIP)
mexico$owner_b <- ifelse(mexico$OWNERSHIP ==1,1,0)
table(mexico$ELECTRIC)
mexico$eletric_b <- ifelse(mexico$ELECTRIC ==1,1,0)
table(mexico$WATSUP)
mexico$water_b <- ifelse(mexico$WATSUP ==11,1,0)
table(mexico$SEWAGE)
mexico$sewage_b <- ifelse(mexico$SEWAGE ==11|mexico$SEWAGE ==12,1,0)
table(mexico$TRASH)
mexico$trash_b <- ifelse(mexico$TRASH ==11|mexico$TRASH ==12,1,0)
table(mexico$AUTOS)
mexico$autos_b <- ifelse(mexico$AUTOS ==7,1,0)
table(mexico$REFRIG)
mexico$refrig_b <- ifelse(mexico$REFRIG ==2,1,0)
table(mexico$TV)
mexico$tv_b <- ifelse(mexico$TV ==20|mexico$TV ==30,1,0)
table(mexico$TOILET)
mexico$toilet_b <- ifelse(mexico$TOILET==20|mexico$TOILET==21|mexico$TOILET==22,1,0)
table(mexico$FLOOR)
mexico$floor_b <- ifelse(mexico$FLOOR ==202|mexico$FLOOR ==236,1,0)
table(mexico$ROOF)
mexico$roof_b <- ifelse(mexico$ROOF==10|mexico$ROOF==14|mexico$ROOF==21|mexico$ROOF==26|mexico$ROOF==28,1,0)

names(mexico)
gc()

#checking if the variables are available in both years
table(mexico$owner_b,mexico$YEAR)
table(mexico$eletric_b,mexico$YEAR)
table(mexico$water_b,mexico$YEAR)
table(mexico$sewage_b,mexico$YEAR)
table(mexico$trash_b,mexico$YEAR)
table(mexico$autos_b,mexico$YEAR)
table(mexico$refrig_b,mexico$YEAR)
table(mexico$tv_b,mexico$YEAR)
table(mexico$toilet_b,mexico$YEAR)
table(mexico$floor_b,mexico$YEAR)
table(mexico$roof_b,mexico$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
mexico$PUBl_ASSET <- ifelse(is.na(mexico$eletric_b), 0, mexico$eletric_b) +
  ifelse(is.na(mexico$water_b), 0, mexico$water_b) +
  ifelse(is.na(mexico$sewage_b), 0, mexico$sewage_b) +
  ifelse(is.na(mexico$trash_b), 0, mexico$trash_b)
mexico %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mexico$PRIV_ASSET <- ifelse(is.na(mexico$owner_b), 0, mexico$owner_b) +
  ifelse(is.na(mexico$autos_b), 0, mexico$autos_b) +
  ifelse(is.na(mexico$refrig_b), 0, mexico$refrig_b) +
  ifelse(is.na(mexico$tv_b), 0, mexico$tv_b)+
  ifelse(is.na(mexico$toilet_b), 0, mexico$toilet_b)+
  ifelse(is.na(mexico$floor_b), 0, mexico$floor_b)+
  ifelse(is.na(mexico$roof_b), 0, mexico$roof_b)
mexico %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mexico$TOTAL_ASSET <- ifelse(is.na(mexico$owner_b), 0, mexico$owner_b) +
  ifelse(is.na(mexico$autos_b), 0, mexico$autos_b) +
  ifelse(is.na(mexico$refrig_b), 0, mexico$refrig_b) +
  ifelse(is.na(mexico$tv_b), 0, mexico$tv_b)+
  ifelse(is.na(mexico$toilet_b), 0, mexico$toilet_b)+
  ifelse(is.na(mexico$floor_b), 0, mexico$floor_b)+
  ifelse(is.na(mexico$roof_b), 0, mexico$roof_b)+
  ifelse(is.na(mexico$eletric_b), 0, mexico$eletric_b) +
  ifelse(is.na(mexico$water_b), 0, mexico$water_b) +
  ifelse(is.na(mexico$sewage_b), 0, mexico$sewage_b) +
  ifelse(is.na(mexico$trash_b), 0, mexico$trash_b)
assets<-mexico %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mexico$TOTAL_ASSET_gini <- ifelse(is.na(mexico$owner_b), 0, mexico$owner_b) +
  ifelse(is.na(mexico$toilet_b), 0, mexico$toilet_b)+
  ifelse(is.na(mexico$floor_b), 0, mexico$floor_b)+
  ifelse(is.na(mexico$roof_b), 0, mexico$roof_b)+
  ifelse(is.na(mexico$eletric_b), 0, mexico$eletric_b) +
  ifelse(is.na(mexico$water_b), 0, mexico$water_b) +
  ifelse(is.na(mexico$sewage_b), 0, mexico$sewage_b) 

assets<-mexico %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

mexico$IPUM1990 <- as.integer(mexico$GEO2_MX1990)
mexico$IPUM2000 <- as.integer(mexico$GEO2_MX2000)
mexico$IPUM2015 <- as.integer(mexico$GEO2_MX2015)
geo_mexico_90$IPUM1990 <- as.integer(geo_mexico_90$IPUM1990)
geo_mexico_00$IPUM2000 <- as.integer(geo_mexico_00$IPUM2000)
geo_mexico_15$IPUM2015 <- as.integer(geo_mexico_15$IPUM2015)


##Joining by year

geo_mexico_90 <- mexico %>% inner_join(geo_mexico_90, by="IPUM1990")
geo_mexico_00 <- mexico %>% inner_join(geo_mexico_00, by="IPUM2000")
geo_mexico_15 <- mexico %>% inner_join(geo_mexico_15, by="IPUM2015")

names(geo_mexico_90)
names(geo_mexico_00)
names(geo_mexico_15)

geo_mexico_90 <- select(geo_mexico_90, -c(MUNI1990))
geo_mexico_00 <- select(geo_mexico_00, -c(MUNI2000))
geo_mexico_15 <- select(geo_mexico_15, -c(MUNI2015))

##Merging all years into one table
mexico_full <- rbind(geo_mexico_90,geo_mexico_00,geo_mexico_15)
names(mexico_full)

table(mexico_full$YEAR)

# mexico_full <- mexico_full %>%  filter (mexico_full$YRSCHOOL < 90)
# 
# mexico_full <- mexico_full %>%  filter (mexico_full$AGE >15)
# 
# mexico_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

rm(mexico)
gc()

##Excluding specific columns for the unifeied dataset
mexico_full<- select(mexico_full, -c(GEO2_MX1990,GEO2_MX2000,GEO2_MX2015,IPUM1990,IPUM2000,IPUM2015,geometry))
table(mexico_full$CITY)

####guadalajara
guadalajara_full <- mexico_full %>%  filter (CITY=="guadalajara")

summary(guadalajara_full$YRSCHOOL)
summary(guadalajara_full$AGE)

guadalajara_fu90 <- guadalajara_full %>%  filter (YEAR==1990)
guadalajara_fu00 <- guadalajara_full %>%  filter (YEAR==2000)
guadalajara_fu15 <- guadalajara_full %>%  filter (YEAR==2015)


Gini(guadalajara_fu90$YRSCHOOL,na.rm = TRUE)
Gini(guadalajara_fu00$YRSCHOOL,na.rm = TRUE)
Gini(guadalajara_fu15$YRSCHOOL,na.rm = TRUE)
Gini(guadalajara_fu90$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(guadalajara_fu00$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(guadalajara_fu15$TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_guad90 <- Lc(guadalajara_fu90$YRSCHOOL, n = rep(1,length(guadalajara_fu90$YRSCHOOL)), plot = TRUE)
Lc_guad00 <- Lc(guadalajara_fu00$YRSCHOOL, n = rep(1,length(guadalajara_fu00$YRSCHOOL)), plot = TRUE)
Lc_guad15 <- Lc(guadalajara_fu15$YRSCHOOL, n = rep(1,length(guadalajara_fu15$YRSCHOOL)), plot = TRUE)

plot(Lc_guad90,col='blue', main = "Lorenz Curve - Guadalajara")
lines(Lc_guad00, col='red')
lines(Lc_guad15, col='green')

####mexico
mexico_c_full <- mexico_full %>%  filter (CITY=="mexico")

summary(mexico_c_full$YRSCHOOL)
summary(mexico_c_full$AGE)

mexico_c_fu90 <- mexico_c_full %>%  filter (YEAR==1990)
mexico_c_fu00 <- mexico_c_full %>%  filter (YEAR==2000)
mexico_c_fu15 <- mexico_c_full %>%  filter (YEAR==2015)


Gini(mexico_c_fu90$YRSCHOOL,na.rm = TRUE)
Gini(mexico_c_fu00$YRSCHOOL,na.rm = TRUE)
Gini(mexico_c_fu15$YRSCHOOL,na.rm = TRUE)
Gini(mexico_c_fu90$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(mexico_c_fu00$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(mexico_c_fu15$TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_mexc90 <- Lc(mexico_c_fu90$YRSCHOOL, n = rep(1,length(mexico_c_fu90$YRSCHOOL)), plot = TRUE)
Lc_mexc00 <- Lc(mexico_c_fu00$YRSCHOOL, n = rep(1,length(mexico_c_fu00$YRSCHOOL)), plot = TRUE)
Lc_mexc15 <- Lc(mexico_c_fu15$YRSCHOOL, n = rep(1,length(mexico_c_fu15$YRSCHOOL)), plot = TRUE)

plot(Lc_mexc90,col='blue', main = "Lorenz Curve - Mexico City")
lines(Lc_mexc00, col='red')
lines(Lc_mexc15, col='green')

####reynosa
reynosa_full <- mexico_full %>%  filter (CITY=="reynosa")

summary(reynosa_full$YRSCHOOL)
summary(reynosa_full$AGE)

reynosa_fu90 <- reynosa_full %>%  filter (YEAR==1990)
reynosa_fu00 <- reynosa_full %>%  filter (YEAR==2000)
reynosa_fu15 <- reynosa_full %>%  filter (YEAR==2015)


Gini(reynosa_fu90$YRSCHOOL,na.rm = TRUE)
Gini(reynosa_fu00$YRSCHOOL,na.rm = TRUE)
Gini(reynosa_fu15$YRSCHOOL,na.rm = TRUE)
Gini(reynosa_fu90$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(reynosa_fu00$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(reynosa_fu15$TOTAL_ASSET_gini,na.rm = TRUE)


## compute the Lorenz curves
Lc_reyn90 <- Lc(reynosa_fu90$YRSCHOOL, n = rep(1,length(reynosa_fu90$YRSCHOOL)), plot = TRUE)
Lc_reyn00 <- Lc(reynosa_fu00$YRSCHOOL, n = rep(1,length(reynosa_fu00$YRSCHOOL)), plot = TRUE)
Lc_reyn15 <- Lc(reynosa_fu15$YRSCHOOL, n = rep(1,length(reynosa_fu15$YRSCHOOL)), plot = TRUE)

plot(Lc_reyn90,col='blue', main = "Lorenz Curve - Reynosa")
lines(Lc_reyn00, col='red')
lines(Lc_reyn15, col='green')

####tijuana
tijuana_full <- mexico_full %>%  filter (CITY=="tijuana")

summary(tijuana_full$YRSCHOOL)
summary(tijuana_full$AGE)

tijuana_fu90 <- tijuana_full %>%  filter (YEAR==1990)
tijuana_fu00 <- tijuana_full %>%  filter (YEAR==2000)
tijuana_fu15 <- tijuana_full %>%  filter (YEAR==2015)


Gini(tijuana_fu90$YRSCHOOL,na.rm = TRUE)
Gini(tijuana_fu00$YRSCHOOL,na.rm = TRUE)
Gini(tijuana_fu15$YRSCHOOL,na.rm = TRUE)
Gini(tijuana_fu90$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(tijuana_fu00$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(tijuana_fu15$TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_tiju90 <- Lc(tijuana_fu90$YRSCHOOL, n = rep(1,length(tijuana_fu90$YRSCHOOL)), plot = TRUE)
Lc_tiju00 <- Lc(tijuana_fu00$YRSCHOOL, n = rep(1,length(tijuana_fu00$YRSCHOOL)), plot = TRUE)
Lc_tiju15 <- Lc(tijuana_fu15$YRSCHOOL, n = rep(1,length(tijuana_fu15$YRSCHOOL)), plot = TRUE)

plot(Lc_tiju90,col='blue', main = "Lorenz Curve - Tijuana")
lines(Lc_tiju00, col='red')
lines(Lc_tiju15, col='green')


names(mexico_full )

table(mexico_full $OCCISCO)


mexico_full $OCCISCO_b <- ifelse(mexico_full $OCCISCO ==1|mexico_full $OCCISCO ==2,1,0)
mexico_full $OCCISCO_b <- ifelse(mexico_full $OCCISCO ==3|mexico_full $OCCISCO ==4|mexico_full $OCCISCO ==5,2,mexico_full $OCCISCO_b)
mexico_full $OCCISCO_b <- ifelse(mexico_full $OCCISCO ==6|mexico_full $OCCISCO ==7|mexico_full $OCCISCO ==8|mexico_full $OCCISCO ==9,3,mexico_full $OCCISCO_b)
table(mexico_full $OCCISCO_b)

mexico_full_OCCISCO_b <- mexico_full  %>% select(YEAR,CITY,OCCISCO_b,PERWT)
mexico_full_OCCISCO_b <- mexico_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(mexico_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- mexico_full_OCCISCO_b %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- mexico_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- mexico_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- mexico_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

MEXICO_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR","CITY"))
MEXICO_OCCISCO_b<- MEXICO_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR","CITY"))
MEXICO_OCCISCO_b<- MEXICO_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR","CITY"))

MEXICO_OCCISCO_b

# for PUBl_ASSET
guadalajara_PUBl_ASSET<-guadalajara_full %>%  
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- guadalajara_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
guadalajara_PUBl_ASSET <- guadalajara_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
guadalajara_PUBl_ASSET <- guadalajara_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
guadalajara_PUBl_ASSET$CITY<-"guadalajara"
# for PRIV_ASSET
guadalajara_PRIV_ASSET<-guadalajara_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- guadalajara_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
guadalajara_PRIV_ASSET <- guadalajara_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
guadalajara_PRIV_ASSET <- guadalajara_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
guadalajara_PRIV_ASSET$CITY<-"guadalajara"
# for TOTAL_ASSET
guadalajara_TOTAL_ASSET<-guadalajara_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- guadalajara_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
guadalajara_TOTAL_ASSET <- guadalajara_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
guadalajara_TOTAL_ASSET <- guadalajara_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
guadalajara_TOTAL_ASSET$CITY<-"guadalajara"
write.csv(guadalajara_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/guadalajara_PUBl_ASSET.csv", row.names = TRUE)
write.csv(guadalajara_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/guadalajara_PRIV_ASSET.csv", row.names = TRUE)
write.csv(guadalajara_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/guadalajara_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
mexico_c_PUBl_ASSET<-mexico_c_full %>%  
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- mexico_c_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
mexico_c_PUBl_ASSET <- mexico_c_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
mexico_c_PUBl_ASSET <- mexico_c_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
mexico_c_PUBl_ASSET$CITY<-"mexico_c"
# for PRIV_ASSET
mexico_c_PRIV_ASSET<-mexico_c_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- mexico_c_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
mexico_c_PRIV_ASSET <- mexico_c_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
mexico_c_PRIV_ASSET <- mexico_c_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
mexico_c_PRIV_ASSET$CITY<-"mexico_c"
# for TOTAL_ASSET
mexico_c_TOTAL_ASSET<-mexico_c_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- mexico_c_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
mexico_c_TOTAL_ASSET <- mexico_c_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
mexico_c_TOTAL_ASSET <- mexico_c_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
mexico_c_TOTAL_ASSET$CITY<-"mexico_c"
write.csv(mexico_c_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/mexico_c_PUBl_ASSET.csv", row.names = TRUE)
write.csv(mexico_c_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/mexico_c_PRIV_ASSET.csv", row.names = TRUE)
write.csv(mexico_c_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/mexico_c_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
reynosa_PUBl_ASSET<-reynosa_full %>%  
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- reynosa_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
reynosa_PUBl_ASSET <- reynosa_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
reynosa_PUBl_ASSET <- reynosa_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
reynosa_PUBl_ASSET$CITY<-"reynosa"
# for PRIV_ASSET
reynosa_PRIV_ASSET<-reynosa_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- reynosa_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
reynosa_PRIV_ASSET <- reynosa_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
reynosa_PRIV_ASSET <- reynosa_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
reynosa_PRIV_ASSET$CITY<-"reynosa"
# for TOTAL_ASSET
reynosa_TOTAL_ASSET<-reynosa_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- reynosa_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
reynosa_TOTAL_ASSET <- reynosa_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
reynosa_TOTAL_ASSET <- reynosa_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
reynosa_TOTAL_ASSET$CITY<-"reynosa"
write.csv(reynosa_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/reynosa_PUBl_ASSET.csv", row.names = TRUE)
write.csv(reynosa_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/reynosa_PRIV_ASSET.csv", row.names = TRUE)
write.csv(reynosa_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/reynosa_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
tijuana_PUBl_ASSET<-tijuana_full %>%  
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- tijuana_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
tijuana_PUBl_ASSET <- tijuana_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
tijuana_PUBl_ASSET <- tijuana_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
tijuana_PUBl_ASSET$CITY<-"tijuana"
# for PRIV_ASSET
tijuana_PRIV_ASSET<-tijuana_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- tijuana_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
tijuana_PRIV_ASSET <- tijuana_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
tijuana_PRIV_ASSET <- tijuana_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
tijuana_PRIV_ASSET$CITY<-"tijuana"
# for TOTAL_ASSET
tijuana_TOTAL_ASSET<-tijuana_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- tijuana_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
tijuana_TOTAL_ASSET <- tijuana_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
tijuana_TOTAL_ASSET <- tijuana_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
tijuana_TOTAL_ASSET$CITY<-"tijuana"
write.csv(tijuana_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/tijuana_PUBl_ASSET.csv", row.names = TRUE)
write.csv(tijuana_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/tijuana_PRIV_ASSET.csv", row.names = TRUE)
write.csv(tijuana_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/tijuana_TOTAL_ASSET.csv", row.names = TRUE)

#write.csv2(MEXICO_OCCISCO_b,file='mex_ocuup.csv')

#################################### FOR WATSUP ##################################

guadalajara_WATER <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(guadalajara_WATER$WATSUP)
table(guadalajara_WATER$HHWT)
guadalajara_WATER <- guadalajara_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(guadalajara_WATER$WATSUP)
table(guadalajara_WATER$water_b)

guadalajara_WATER$water_b <- factor(guadalajara_WATER$water_b)

guadalajara_WATER$YEAR <- factor(guadalajara_WATER$YEAR)
guadalajara_WATER_01 <- guadalajara_WATER %>% filter(guadalajara_WATER$YEAR==2000)
guadalajara_WATER_12 <- guadalajara_WATER %>% filter(guadalajara_WATER$YEAR==2015)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = guadalajara_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = guadalajara_WATER_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

guadalajara_OWN <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(guadalajara_OWN$OWNERSHIP)
guadalajara_OWN <- guadalajara_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(guadalajara_OWN$OWNERSHIP)
table(guadalajara_OWN$owner_b)

guadalajara_OWN$owner_b <- factor(guadalajara_OWN$owner_b)

guadalajara_OWN$YEAR <- factor(guadalajara_OWN$YEAR)
guadalajara_OWN_01 <- guadalajara_OWN %>% filter(guadalajara_OWN$YEAR==2000)
guadalajara_OWN_12 <- guadalajara_OWN %>% filter(guadalajara_OWN$YEAR==2015)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = guadalajara_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = guadalajara_OWN_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

guadalajara_SEWG <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(guadalajara_SEWG$SEWAGE)
guadalajara_SEWG <- guadalajara_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(guadalajara_SEWG$SEWAGE)
table(guadalajara_SEWG$sewage_b)

guadalajara_SEWG$sewage_b <- factor(guadalajara_SEWG$sewage_b)

guadalajara_SEWG$YEAR <- factor(guadalajara_SEWG$YEAR)
guadalajara_SEWG_01 <- guadalajara_SEWG %>% filter(guadalajara_SEWG$YEAR==2000)
guadalajara_SEWG_12 <- guadalajara_SEWG %>% filter(guadalajara_SEWG$YEAR==2015)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = guadalajara_SEWG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = guadalajara_SEWG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

guadalajara_REFRIG <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(guadalajara_REFRIG$REFRIG)
guadalajara_REFRIG <- guadalajara_REFRIG %>% filter(REFRIG!=0)

table(guadalajara_REFRIG$REFRIG)
table(guadalajara_REFRIG$refrig_b)

guadalajara_REFRIG$refrig_b <- factor(guadalajara_REFRIG$refrig_b)

guadalajara_REFRIG$YEAR <- factor(guadalajara_REFRIG$YEAR)
guadalajara_REFRIG_01 <- guadalajara_REFRIG %>% filter(guadalajara_REFRIG$YEAR==2000)
guadalajara_REFRIG_12 <- guadalajara_REFRIG %>% filter(guadalajara_REFRIG$YEAR==2015)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = guadalajara_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = guadalajara_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)

if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

guadalajara_TOILET <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(guadalajara_TOILET$TOILET)
guadalajara_TOILET <- guadalajara_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(guadalajara_TOILET$TOILET)
table(guadalajara_TOILET$toilet_b)

guadalajara_TOILET$toilet_b <- factor(guadalajara_TOILET$toilet_b)

guadalajara_TOILET$YEAR <- factor(guadalajara_TOILET$YEAR)
guadalajara_TOILET_01 <- guadalajara_TOILET %>% filter(guadalajara_TOILET$YEAR==2000)
guadalajara_TOILET_12 <- guadalajara_TOILET %>% filter(guadalajara_TOILET$YEAR==2015)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = guadalajara_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = guadalajara_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

guadalajara_ELECTRIC <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(guadalajara_ELECTRIC$ELECTRIC)
guadalajara_ELECTRIC <- guadalajara_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(guadalajara_ELECTRIC$ELECTRIC)
table(guadalajara_ELECTRIC$eletric_b)

guadalajara_ELECTRIC$eletric_b <- factor(guadalajara_ELECTRIC$eletric_b)

guadalajara_ELECTRIC$YEAR <- factor(guadalajara_ELECTRIC$YEAR)
guadalajara_ELECTRIC_01 <- guadalajara_ELECTRIC %>% filter(guadalajara_ELECTRIC$YEAR==2000)
guadalajara_ELECTRIC_12 <- guadalajara_ELECTRIC %>% filter(guadalajara_ELECTRIC$YEAR==2015)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = guadalajara_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = guadalajara_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)

if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

guadalajara_TRASH <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(guadalajara_TRASH$TRASH)
guadalajara_TRASH <- guadalajara_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(guadalajara_TRASH$TRASH)
table(guadalajara_TRASH$trash_b)

guadalajara_TRASH$trash_b <- factor(guadalajara_TRASH$trash_b)

guadalajara_TRASH$YEAR <- factor(guadalajara_TRASH$YEAR)
guadalajara_TRASH_01 <- guadalajara_TRASH %>% filter(guadalajara_TRASH$YEAR==2000)
guadalajara_TRASH_12 <- guadalajara_TRASH %>% filter(guadalajara_TRASH$YEAR==2015)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = guadalajara_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = guadalajara_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

guadalajara_AUTOS <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(guadalajara_AUTOS$AUTOS)
guadalajara_AUTOS <- guadalajara_AUTOS %>% filter(AUTOS!=99)

table(guadalajara_AUTOS$AUTOS)
table(guadalajara_AUTOS$autos_b)

guadalajara_AUTOS$autos_b <- factor(guadalajara_AUTOS$autos_b)

guadalajara_AUTOS$YEAR <- factor(guadalajara_AUTOS$YEAR)
guadalajara_AUTOS_01 <- guadalajara_AUTOS %>% filter(guadalajara_AUTOS$YEAR==2000)
guadalajara_AUTOS_12 <- guadalajara_AUTOS %>% filter(guadalajara_AUTOS$YEAR==2015)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = guadalajara_AUTOS_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = guadalajara_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

guadalajara_FLOOR <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(guadalajara_FLOOR$FLOOR)
guadalajara_FLOOR <- guadalajara_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(guadalajara_FLOOR$FLOOR)
table(guadalajara_FLOOR$floor_b)

guadalajara_FLOOR$floor_b <- factor(guadalajara_FLOOR$floor_b)

guadalajara_FLOOR$YEAR <- factor(guadalajara_FLOOR$YEAR)
guadalajara_FLOOR_01 <- guadalajara_FLOOR %>% filter(guadalajara_FLOOR$YEAR==2000)
guadalajara_FLOOR_12 <- guadalajara_FLOOR %>% filter(guadalajara_FLOOR$YEAR==2015)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = guadalajara_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = guadalajara_FLOOR_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)

if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

guadalajara_ROOF <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(guadalajara_ROOF$ROOF)
guadalajara_ROOF <- guadalajara_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(guadalajara_ROOF$ROOF)
table(guadalajara_ROOF$roof_b)

guadalajara_ROOF$roof_b <- factor(guadalajara_ROOF$roof_b)

guadalajara_ROOF$YEAR <- factor(guadalajara_ROOF$YEAR)
guadalajara_ROOF_01 <- guadalajara_ROOF %>% filter(guadalajara_ROOF$YEAR==2000)
guadalajara_ROOF_12 <- guadalajara_ROOF %>% filter(guadalajara_ROOF$YEAR==2015)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = guadalajara_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = guadalajara_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

guadalajara_TV <- guadalajara_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(guadalajara_TV$TV)
guadalajara_TV <- guadalajara_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(guadalajara_TV$TV)
table(guadalajara_TV$tv_b)

guadalajara_TV$tv_b <- factor(guadalajara_TV$tv_b)

guadalajara_TV$YEAR <- factor(guadalajara_TV$YEAR)
guadalajara_TV_01 <- guadalajara_TV %>% filter(guadalajara_TV$YEAR==2000)
guadalajara_TV_12 <- guadalajara_TV %>% filter(guadalajara_TV$YEAR==2015)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = guadalajara_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = guadalajara_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)

if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

guadalajara_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
guadalajara_logit$CITY<- "Guadalajara"
gc()

#################################### FOR WATSUP ##################################

mexico_c_WATER <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(mexico_c_WATER$WATSUP)
mexico_c_WATER <- mexico_c_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(mexico_c_WATER$WATSUP)
table(mexico_c_WATER$water_b)

mexico_c_WATER$water_b <- factor(mexico_c_WATER$water_b)

mexico_c_WATER$YEAR <- factor(mexico_c_WATER$YEAR)
mexico_c_WATER_01 <- mexico_c_WATER %>% filter(mexico_c_WATER$YEAR==2000)
mexico_c_WATER_12 <- mexico_c_WATER %>% filter(mexico_c_WATER$YEAR==2015)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = mexico_c_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = mexico_c_WATER_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

mexico_c_OWN <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(mexico_c_OWN$OWNERSHIP)
mexico_c_OWN <- mexico_c_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(mexico_c_OWN$OWNERSHIP)
table(mexico_c_OWN$owner_b)

mexico_c_OWN$owner_b <- factor(mexico_c_OWN$owner_b)

mexico_c_OWN$YEAR <- factor(mexico_c_OWN$YEAR)
mexico_c_OWN_01 <- mexico_c_OWN %>% filter(mexico_c_OWN$YEAR==2000)
mexico_c_OWN_12 <- mexico_c_OWN %>% filter(mexico_c_OWN$YEAR==2015)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = mexico_c_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = mexico_c_OWN_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

mexico_c_SEWG <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(mexico_c_SEWG$SEWAGE)
mexico_c_SEWG <- mexico_c_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(mexico_c_SEWG$SEWAGE)
table(mexico_c_SEWG$sewage_b)

mexico_c_SEWG$sewage_b <- factor(mexico_c_SEWG$sewage_b)

mexico_c_SEWG$YEAR <- factor(mexico_c_SEWG$YEAR)
mexico_c_SEWG_01 <- mexico_c_SEWG %>% filter(mexico_c_SEWG$YEAR==2000)
mexico_c_SEWG_12 <- mexico_c_SEWG %>% filter(mexico_c_SEWG$YEAR==2015)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = mexico_c_SEWG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = mexico_c_SEWG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

mexico_c_REFRIG <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(mexico_c_REFRIG$REFRIG)
mexico_c_REFRIG <- mexico_c_REFRIG %>% filter(REFRIG!=0)

table(mexico_c_REFRIG$REFRIG)
table(mexico_c_REFRIG$refrig_b)

mexico_c_REFRIG$refrig_b <- factor(mexico_c_REFRIG$refrig_b)

mexico_c_REFRIG$YEAR <- factor(mexico_c_REFRIG$YEAR)
mexico_c_REFRIG_01 <- mexico_c_REFRIG %>% filter(mexico_c_REFRIG$YEAR==2000)
mexico_c_REFRIG_12 <- mexico_c_REFRIG %>% filter(mexico_c_REFRIG$YEAR==2015)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = mexico_c_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = mexico_c_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

mexico_c_TOILET <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(mexico_c_TOILET$TOILET)
mexico_c_TOILET <- mexico_c_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(mexico_c_TOILET$TOILET)
table(mexico_c_TOILET$toilet_b)

mexico_c_TOILET$toilet_b <- factor(mexico_c_TOILET$toilet_b)

mexico_c_TOILET$YEAR <- factor(mexico_c_TOILET$YEAR)
mexico_c_TOILET_01 <- mexico_c_TOILET %>% filter(mexico_c_TOILET$YEAR==2000)
mexico_c_TOILET_12 <- mexico_c_TOILET %>% filter(mexico_c_TOILET$YEAR==2015)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = mexico_c_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = mexico_c_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

mexico_c_ELECTRIC <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(mexico_c_ELECTRIC$ELECTRIC)
mexico_c_ELECTRIC <- mexico_c_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(mexico_c_ELECTRIC$ELECTRIC)
table(mexico_c_ELECTRIC$eletric_b)

mexico_c_ELECTRIC$eletric_b <- factor(mexico_c_ELECTRIC$eletric_b)

mexico_c_ELECTRIC$YEAR <- factor(mexico_c_ELECTRIC$YEAR)
mexico_c_ELECTRIC_01 <- mexico_c_ELECTRIC %>% filter(mexico_c_ELECTRIC$YEAR==2000)
mexico_c_ELECTRIC_12 <- mexico_c_ELECTRIC %>% filter(mexico_c_ELECTRIC$YEAR==2015)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = mexico_c_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = mexico_c_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

mexico_c_TRASH <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(mexico_c_TRASH$TRASH)
mexico_c_TRASH <- mexico_c_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(mexico_c_TRASH$TRASH)
table(mexico_c_TRASH$trash_b)

mexico_c_TRASH$trash_b <- factor(mexico_c_TRASH$trash_b)

mexico_c_TRASH$YEAR <- factor(mexico_c_TRASH$YEAR)
mexico_c_TRASH_01 <- mexico_c_TRASH %>% filter(mexico_c_TRASH$YEAR==2000)
mexico_c_TRASH_12 <- mexico_c_TRASH %>% filter(mexico_c_TRASH$YEAR==2015)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = mexico_c_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = mexico_c_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

mexico_c_AUTOS <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(mexico_c_AUTOS$AUTOS)
mexico_c_AUTOS <- mexico_c_AUTOS %>% filter(AUTOS!=99)

table(mexico_c_AUTOS$AUTOS)
table(mexico_c_AUTOS$autos_b)

mexico_c_AUTOS$autos_b <- factor(mexico_c_AUTOS$autos_b)

mexico_c_AUTOS$YEAR <- factor(mexico_c_AUTOS$YEAR)
mexico_c_AUTOS_01 <- mexico_c_AUTOS %>% filter(mexico_c_AUTOS$YEAR==2000)
mexico_c_AUTOS_12 <- mexico_c_AUTOS %>% filter(mexico_c_AUTOS$YEAR==2015)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = mexico_c_AUTOS_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = mexico_c_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

mexico_c_FLOOR <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(mexico_c_FLOOR$FLOOR)
mexico_c_FLOOR <- mexico_c_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(mexico_c_FLOOR$FLOOR)
table(mexico_c_FLOOR$floor_b)

mexico_c_FLOOR$floor_b <- factor(mexico_c_FLOOR$floor_b)

mexico_c_FLOOR$YEAR <- factor(mexico_c_FLOOR$YEAR)
mexico_c_FLOOR_01 <- mexico_c_FLOOR %>% filter(mexico_c_FLOOR$YEAR==2000)
mexico_c_FLOOR_12 <- mexico_c_FLOOR %>% filter(mexico_c_FLOOR$YEAR==2015)


mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = mexico_c_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = mexico_c_FLOOR_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

mexico_c_ROOF <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(mexico_c_ROOF$ROOF)
mexico_c_ROOF <- mexico_c_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(mexico_c_ROOF$ROOF)
table(mexico_c_ROOF$roof_b)

mexico_c_ROOF$roof_b <- factor(mexico_c_ROOF$roof_b)

mexico_c_ROOF$YEAR <- factor(mexico_c_ROOF$YEAR)
mexico_c_ROOF_01 <- mexico_c_ROOF %>% filter(mexico_c_ROOF$YEAR==2000)
mexico_c_ROOF_12 <- mexico_c_ROOF %>% filter(mexico_c_ROOF$YEAR==2015)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = mexico_c_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = mexico_c_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)

if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

mexico_c_TV <- mexico_c_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(mexico_c_TV$TV)
mexico_c_TV <- mexico_c_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(mexico_c_TV$TV)
table(mexico_c_TV$tv_b)

mexico_c_TV$tv_b <- factor(mexico_c_TV$tv_b)

mexico_c_TV$YEAR <- factor(mexico_c_TV$YEAR)
mexico_c_TV_01 <- mexico_c_TV %>% filter(mexico_c_TV$YEAR==2000)
mexico_c_TV_12 <- mexico_c_TV %>% filter(mexico_c_TV$YEAR==2015)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = mexico_c_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = mexico_c_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

mexico_c_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
mexico_c_logit$CITY<- "Mexico City"
gc()

#################################### FOR WATSUP ##################################

reynosa_WATER <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(reynosa_WATER$WATSUP)
reynosa_WATER <- reynosa_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(reynosa_WATER$WATSUP)
table(reynosa_WATER$water_b)

reynosa_WATER$water_b <- factor(reynosa_WATER$water_b)

reynosa_WATER$YEAR <- factor(reynosa_WATER$YEAR)
reynosa_WATER_01 <- reynosa_WATER %>% filter(reynosa_WATER$YEAR==2000)
reynosa_WATER_12 <- reynosa_WATER %>% filter(reynosa_WATER$YEAR==2015)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = reynosa_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = reynosa_WATER_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

reynosa_OWN <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(reynosa_OWN$OWNERSHIP)
reynosa_OWN <- reynosa_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(reynosa_OWN$OWNERSHIP)
table(reynosa_OWN$owner_b)

reynosa_OWN$owner_b <- factor(reynosa_OWN$owner_b)

reynosa_OWN$YEAR <- factor(reynosa_OWN$YEAR)
reynosa_OWN_01 <- reynosa_OWN %>% filter(reynosa_OWN$YEAR==2000)
reynosa_OWN_12 <- reynosa_OWN %>% filter(reynosa_OWN$YEAR==2015)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = reynosa_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = reynosa_OWN_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

reynosa_SEWG <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(reynosa_SEWG$SEWAGE)
reynosa_SEWG <- reynosa_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(reynosa_SEWG$SEWAGE)
table(reynosa_SEWG$sewage_b)

reynosa_SEWG$sewage_b <- factor(reynosa_SEWG$sewage_b)

reynosa_SEWG$YEAR <- factor(reynosa_SEWG$YEAR)
reynosa_SEWG_01 <- reynosa_SEWG %>% filter(reynosa_SEWG$YEAR==2000)
reynosa_SEWG_12 <- reynosa_SEWG %>% filter(reynosa_SEWG$YEAR==2015)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = reynosa_SEWG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = reynosa_SEWG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

reynosa_REFRIG <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(reynosa_REFRIG$REFRIG)
reynosa_REFRIG <- reynosa_REFRIG %>% filter(REFRIG!=0)

table(reynosa_REFRIG$REFRIG)
table(reynosa_REFRIG$refrig_b)

reynosa_REFRIG$refrig_b <- factor(reynosa_REFRIG$refrig_b)

reynosa_REFRIG$YEAR <- factor(reynosa_REFRIG$YEAR)
reynosa_REFRIG_01 <- reynosa_REFRIG %>% filter(reynosa_REFRIG$YEAR==2000)
reynosa_REFRIG_12 <- reynosa_REFRIG %>% filter(reynosa_REFRIG$YEAR==2015)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = reynosa_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = reynosa_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

reynosa_TOILET <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(reynosa_TOILET$TOILET)
reynosa_TOILET <- reynosa_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(reynosa_TOILET$TOILET)
table(reynosa_TOILET$toilet_b)

reynosa_TOILET$toilet_b <- factor(reynosa_TOILET$toilet_b)

reynosa_TOILET$YEAR <- factor(reynosa_TOILET$YEAR)
reynosa_TOILET_01 <- reynosa_TOILET %>% filter(reynosa_TOILET$YEAR==2000)
reynosa_TOILET_12 <- reynosa_TOILET %>% filter(reynosa_TOILET$YEAR==2015)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = reynosa_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = reynosa_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

reynosa_ELECTRIC <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(reynosa_ELECTRIC$ELECTRIC)
reynosa_ELECTRIC <- reynosa_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(reynosa_ELECTRIC$ELECTRIC)
table(reynosa_ELECTRIC$eletric_b)

reynosa_ELECTRIC$eletric_b <- factor(reynosa_ELECTRIC$eletric_b)

reynosa_ELECTRIC$YEAR <- factor(reynosa_ELECTRIC$YEAR)
reynosa_ELECTRIC_01 <- reynosa_ELECTRIC %>% filter(reynosa_ELECTRIC$YEAR==2000)
reynosa_ELECTRIC_12 <- reynosa_ELECTRIC %>% filter(reynosa_ELECTRIC$YEAR==2015)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = reynosa_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = reynosa_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

reynosa_TRASH <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(reynosa_TRASH$TRASH)
reynosa_TRASH <- reynosa_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(reynosa_TRASH$TRASH)
table(reynosa_TRASH$trash_b)

reynosa_TRASH$trash_b <- factor(reynosa_TRASH$trash_b)

reynosa_TRASH$YEAR <- factor(reynosa_TRASH$YEAR)
reynosa_TRASH_01 <- reynosa_TRASH %>% filter(reynosa_TRASH$YEAR==2000)
reynosa_TRASH_12 <- reynosa_TRASH %>% filter(reynosa_TRASH$YEAR==2015)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = reynosa_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = reynosa_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

reynosa_AUTOS <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(reynosa_AUTOS$AUTOS)
reynosa_AUTOS <- reynosa_AUTOS %>% filter(AUTOS!=99)

table(reynosa_AUTOS$AUTOS)
table(reynosa_AUTOS$autos_b)

reynosa_AUTOS$autos_b <- factor(reynosa_AUTOS$autos_b)

reynosa_AUTOS$YEAR <- factor(reynosa_AUTOS$YEAR)
reynosa_AUTOS_01 <- reynosa_AUTOS %>% filter(reynosa_AUTOS$YEAR==2000)
reynosa_AUTOS_12 <- reynosa_AUTOS %>% filter(reynosa_AUTOS$YEAR==2015)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = reynosa_AUTOS_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = reynosa_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

reynosa_FLOOR <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(reynosa_FLOOR$FLOOR)
reynosa_FLOOR <- reynosa_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(reynosa_FLOOR$FLOOR)
table(reynosa_FLOOR$floor_b)

reynosa_FLOOR$floor_b <- factor(reynosa_FLOOR$floor_b)

reynosa_FLOOR$YEAR <- factor(reynosa_FLOOR$YEAR)
reynosa_FLOOR_01 <- reynosa_FLOOR %>% filter(reynosa_FLOOR$YEAR==2000)
reynosa_FLOOR_12 <- reynosa_FLOOR %>% filter(reynosa_FLOOR$YEAR==2015)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = reynosa_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = reynosa_FLOOR_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

reynosa_ROOF <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(reynosa_ROOF$ROOF)
reynosa_ROOF <- reynosa_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(reynosa_ROOF$ROOF)
table(reynosa_ROOF$roof_b)

reynosa_ROOF$roof_b <- factor(reynosa_ROOF$roof_b)

reynosa_ROOF$YEAR <- factor(reynosa_ROOF$YEAR)
reynosa_ROOF_01 <- reynosa_ROOF %>% filter(reynosa_ROOF$YEAR==2000)
reynosa_ROOF_12 <- reynosa_ROOF %>% filter(reynosa_ROOF$YEAR==2015)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = reynosa_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = reynosa_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

reynosa_TV <- reynosa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(reynosa_TV$TV)
reynosa_TV <- reynosa_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(reynosa_TV$TV)
table(reynosa_TV$tv_b)

reynosa_TV$tv_b <- factor(reynosa_TV$tv_b)

reynosa_TV$YEAR <- factor(reynosa_TV$YEAR)
reynosa_TV_01 <- reynosa_TV %>% filter(reynosa_TV$YEAR==2000)
reynosa_TV_12 <- reynosa_TV %>% filter(reynosa_TV$YEAR==2015)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = reynosa_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = reynosa_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

reynosa_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
reynosa_logit$CITY <- "Reynosa"

gc()

#################################### FOR WATSUP ##################################

tijuana_WATER <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(tijuana_WATER$WATSUP)
tijuana_WATER <- tijuana_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(tijuana_WATER$WATSUP)
table(tijuana_WATER$water_b)

tijuana_WATER$water_b <- factor(tijuana_WATER$water_b)

tijuana_WATER$YEAR <- factor(tijuana_WATER$YEAR)
tijuana_WATER_01 <- tijuana_WATER %>% filter(tijuana_WATER$YEAR==2000)
tijuana_WATER_12 <- tijuana_WATER %>% filter(tijuana_WATER$YEAR==2015)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = tijuana_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = tijuana_WATER_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

tijuana_OWN <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(tijuana_OWN$OWNERSHIP)
tijuana_OWN <- tijuana_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(tijuana_OWN$OWNERSHIP)
table(tijuana_OWN$owner_b)

tijuana_OWN$owner_b <- factor(tijuana_OWN$owner_b)

tijuana_OWN$YEAR <- factor(tijuana_OWN$YEAR)
tijuana_OWN_01 <- tijuana_OWN %>% filter(tijuana_OWN$YEAR==2000)
tijuana_OWN_12 <- tijuana_OWN %>% filter(tijuana_OWN$YEAR==2015)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = tijuana_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = tijuana_OWN_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

tijuana_SEWG <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(tijuana_SEWG$SEWAGE)
tijuana_SEWG <- tijuana_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(tijuana_SEWG$SEWAGE)
table(tijuana_SEWG$sewage_b)

tijuana_SEWG$sewage_b <- factor(tijuana_SEWG$sewage_b)

tijuana_SEWG$YEAR <- factor(tijuana_SEWG$YEAR)
tijuana_SEWG_01 <- tijuana_SEWG %>% filter(tijuana_SEWG$YEAR==2000)
tijuana_SEWG_12 <- tijuana_SEWG %>% filter(tijuana_SEWG$YEAR==2015)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = tijuana_SEWG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = tijuana_SEWG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

tijuana_REFRIG <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(tijuana_REFRIG$REFRIG)
tijuana_REFRIG <- tijuana_REFRIG %>% filter(REFRIG!=0)

table(tijuana_REFRIG$REFRIG)
table(tijuana_REFRIG$refrig_b)

tijuana_REFRIG$refrig_b <- factor(tijuana_REFRIG$refrig_b)

tijuana_REFRIG$YEAR <- factor(tijuana_REFRIG$YEAR)
tijuana_REFRIG_01 <- tijuana_REFRIG %>% filter(tijuana_REFRIG$YEAR==2000)
tijuana_REFRIG_12 <- tijuana_REFRIG %>% filter(tijuana_REFRIG$YEAR==2015)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = tijuana_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = tijuana_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

tijuana_TOILET <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(tijuana_TOILET$TOILET)
tijuana_TOILET <- tijuana_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(tijuana_TOILET$TOILET)
table(tijuana_TOILET$toilet_b)

tijuana_TOILET$toilet_b <- factor(tijuana_TOILET$toilet_b)

tijuana_TOILET$YEAR <- factor(tijuana_TOILET$YEAR)
tijuana_TOILET_01 <- tijuana_TOILET %>% filter(tijuana_TOILET$YEAR==2000)
tijuana_TOILET_12 <- tijuana_TOILET %>% filter(tijuana_TOILET$YEAR==2015)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = tijuana_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = tijuana_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

tijuana_ELECTRIC <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(tijuana_ELECTRIC$ELECTRIC)
tijuana_ELECTRIC <- tijuana_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(tijuana_ELECTRIC$ELECTRIC)
table(tijuana_ELECTRIC$eletric_b)

tijuana_ELECTRIC$eletric_b <- factor(tijuana_ELECTRIC$eletric_b)

tijuana_ELECTRIC$YEAR <- factor(tijuana_ELECTRIC$YEAR)
tijuana_ELECTRIC_01 <- tijuana_ELECTRIC %>% filter(tijuana_ELECTRIC$YEAR==2000)
tijuana_ELECTRIC_12 <- tijuana_ELECTRIC %>% filter(tijuana_ELECTRIC$YEAR==2015)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = tijuana_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = tijuana_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

tijuana_TRASH <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(tijuana_TRASH$TRASH)
tijuana_TRASH <- tijuana_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(tijuana_TRASH$TRASH)
table(tijuana_TRASH$trash_b)

tijuana_TRASH$trash_b <- factor(tijuana_TRASH$trash_b)

tijuana_TRASH$YEAR <- factor(tijuana_TRASH$YEAR)
tijuana_TRASH_01 <- tijuana_TRASH %>% filter(tijuana_TRASH$YEAR==2000)
tijuana_TRASH_12 <- tijuana_TRASH %>% filter(tijuana_TRASH$YEAR==2015)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = tijuana_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = tijuana_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

tijuana_AUTOS <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(tijuana_AUTOS$AUTOS)
tijuana_AUTOS <- tijuana_AUTOS %>% filter(AUTOS!=99)

table(tijuana_AUTOS$AUTOS)
table(tijuana_AUTOS$autos_b)

tijuana_AUTOS$autos_b <- factor(tijuana_AUTOS$autos_b)

tijuana_AUTOS$YEAR <- factor(tijuana_AUTOS$YEAR)
tijuana_AUTOS_01 <- tijuana_AUTOS %>% filter(tijuana_AUTOS$YEAR==2000)
tijuana_AUTOS_12 <- tijuana_AUTOS %>% filter(tijuana_AUTOS$YEAR==2015)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = tijuana_AUTOS_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = tijuana_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

tijuana_FLOOR <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(tijuana_FLOOR$FLOOR)
tijuana_FLOOR <- tijuana_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(tijuana_FLOOR$FLOOR)
table(tijuana_FLOOR$floor_b)

tijuana_FLOOR$floor_b <- factor(tijuana_FLOOR$floor_b)

tijuana_FLOOR$YEAR <- factor(tijuana_FLOOR$YEAR)
tijuana_FLOOR_01 <- tijuana_FLOOR %>% filter(tijuana_FLOOR$YEAR==2000)
tijuana_FLOOR_12 <- tijuana_FLOOR %>% filter(tijuana_FLOOR$YEAR==2015)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = tijuana_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = tijuana_FLOOR_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

tijuana_ROOF <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(tijuana_ROOF$ROOF)
tijuana_ROOF <- tijuana_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(tijuana_ROOF$ROOF)
table(tijuana_ROOF$roof_b)

tijuana_ROOF$roof_b <- factor(tijuana_ROOF$roof_b)

tijuana_ROOF$YEAR <- factor(tijuana_ROOF$YEAR)
tijuana_ROOF_01 <- tijuana_ROOF %>% filter(tijuana_ROOF$YEAR==2000)
tijuana_ROOF_12 <- tijuana_ROOF %>% filter(tijuana_ROOF$YEAR==2015)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = tijuana_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = tijuana_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

tijuana_TV <- tijuana_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(tijuana_TV$TV)
tijuana_TV <- tijuana_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(tijuana_TV$TV)
table(tijuana_TV$tv_b)

tijuana_TV$tv_b <- factor(tijuana_TV$tv_b)

tijuana_TV$YEAR <- factor(tijuana_TV$YEAR)
tijuana_TV_01 <- tijuana_TV %>% filter(tijuana_TV$YEAR==2000)
tijuana_TV_12 <- tijuana_TV %>% filter(tijuana_TV$YEAR==2015)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = tijuana_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = tijuana_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

tijuana_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
tijuana_logit$CITY <- "Tijuana"


MEXICO_logit <- rbind(guadalajara_logit,mexico_c_logit,reynosa_logit,tijuana_logit,sep = ".") 
write.csv(MEXICO_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/MEXICO_logit.csv"))


