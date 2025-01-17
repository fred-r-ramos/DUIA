library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/philippines")

sf::sf_use_s2(FALSE)

geo2_ph90 <- read_sf("geo2_ph1990.shp")
geo2_ph00 <- read_sf("geo2_ph2000.shp")
geo2_ph10 <- read_sf("geo2_ph2010.shp")
centroid_ph90 <- st_centroid(geo2_ph90)
centroid_ph00 <- st_centroid(geo2_ph00)
centroid_ph10 <- st_centroid(geo2_ph10)
AUE_manila <- read_sf("Manila_studyArea.shp")
AUE_cebucity <- read_sf("Cebu_City_studyArea.shp")
AUE_bacolod <- read_sf("Bacolod_studyArea.shp")
AUE_manila  <- st_transform(AUE_manila , 4326)
AUE_cebucity  <- st_transform(AUE_cebucity , 4326)
AUE_bacolod  <- st_transform(AUE_bacolod , 4326)

bacolod_90 <- geo2_ph90[st_intersection(AUE_bacolod,centroid_ph90),]
bacolod_90['CITY']='bacolod'
bacolod_00 <- geo2_ph00[st_intersection(AUE_bacolod,centroid_ph00),]
bacolod_00['CITY']='bacolod'
bacolod_10 <- geo2_ph10[st_intersection(AUE_bacolod,centroid_ph10),]
bacolod_10['CITY']='bacolod'

cebu_90 <- geo2_ph90[st_intersection(AUE_cebucity,centroid_ph90),]
cebu_90['CITY']='cebu'
cebu_00 <-  geo2_ph00[st_intersection(AUE_cebucity,centroid_ph00),]
cebu_00['CITY']='cebu'
cebu_10 <- geo2_ph10[st_intersection(AUE_cebucity,centroid_ph10),]
cebu_10['CITY']='cebu'

manila_90 <- geo2_ph90[st_intersection(AUE_manila,centroid_ph90),]
manila_90['CITY']='manila'
manila_00 <- geo2_ph00[st_intersection(AUE_manila,centroid_ph00),]
manila_00['CITY']='manila'
manila_10 <-  geo2_ph10[st_intersection(AUE_manila,centroid_ph10),]
manila_10['CITY']='manila'

philippines_90 <- rbind(cebu_90,bacolod_90,manila_90)
philippines_00 <- rbind(cebu_00,bacolod_00,manila_00)
philippines_10 <- rbind(cebu_10,bacolod_10,manila_10)

plot(st_geometry(philippines_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ph10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 
#ddi <-read_ipums_ddi("ipumsi_00221.xml")
ddi <-read_ipums_ddi("ipumsi_00259.xml")
philippines <- read_ipums_micro(ddi)

names(philippines)

######################creating binary variables for logit regressions

names(philippines)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(philippines$OWNERSHIP)
philippines$owner_b <- ifelse(philippines$OWNERSHIP ==1,1,0)
table(philippines$ELECTRIC)
philippines$eletric_b <- ifelse(philippines$ELECTRIC ==1,1,0)
table(philippines$WATSUP)
philippines$water_b <- ifelse(philippines$WATSUP ==12,1,0)
table(philippines$TRASH)
philippines$trash_b <- ifelse(philippines$TRASH ==10|philippines$TRASH ==12,1,0)
table(philippines$AUTOS)
philippines$autos_b <- ifelse(philippines$AUTOS ==7,1,0)
table(philippines$REFRIG)
philippines$refrig_b <- ifelse(philippines$REFRIG ==2,1,0)
table(philippines$TV)
philippines$tv_b <- ifelse(philippines$TV ==20,1,0)
table(philippines$TOILET)
philippines$toilet_b <- ifelse(philippines$TOILET >19,1,0)
table(philippines$ROOF)
philippines$roof_b <- ifelse(philippines$ROOF==13|philippines$ROOF==16|philippines$ROOF==28,1,0)

names(philippines)
gc()

#checking if the variables are available in both years
table(philippines$owner_b,philippines$YEAR)
table(philippines$eletric_b,philippines$YEAR)
table(philippines$water_b,philippines$YEAR)
table(philippines$trash_b,philippines$YEAR)
table(philippines$autos_b,philippines$YEAR)
table(philippines$refrig_b,philippines$YEAR)
table(philippines$tv_b,philippines$YEAR)
table(philippines$toilet_b,philippines$YEAR)
table(philippines$roof_b,philippines$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
philippines$PUBl_ASSET <- ifelse(is.na(philippines$eletric_b), 0, philippines$eletric_b)+
  ifelse(is.na(philippines$water_b), 0, philippines$water_b)+
  ifelse(is.na(philippines$trash_b), 0, philippines$trash_b)
philippines %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

philippines$PRIV_ASSET <- ifelse(is.na(philippines$owner_b), 0, philippines$owner_b)+
  ifelse(is.na(philippines$refrig_b), 0, philippines$refrig_b)+
  ifelse(is.na(philippines$toilet_b), 0, philippines$toilet_b)+
  ifelse(is.na(philippines$roof_b), 0, philippines$roof_b)+
  ifelse(is.na(philippines$tv_b), 0, philippines$tv_b)
philippines %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

philippines$TOTAL_ASSET <- ifelse(is.na(philippines$owner_b), 0, philippines$owner_b)+
  ifelse(is.na(philippines$refrig_b), 0, philippines$refrig_b)+
  ifelse(is.na(philippines$toilet_b), 0, philippines$toilet_b)+
  ifelse(is.na(philippines$roof_b), 0, philippines$roof_b)+
  ifelse(is.na(philippines$tv_b), 0, philippines$tv_b)+
  ifelse(is.na(philippines$eletric_b), 0, philippines$eletric_b)+
  ifelse(is.na(philippines$water_b), 0, philippines$water_b)+
  ifelse(is.na(philippines$trash_b), 0, philippines$trash_b)
assets<-philippines %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join
philippines$IPUM1990 <- as.integer(philippines$GEO2_PH1990)
philippines$IPUM2000 <- as.integer(philippines$GEO2_PH2000)
philippines$IPUM2010 <- as.integer(philippines$GEO2_PH2010)
philippines_90$IPUM1990 <- as.integer(philippines_90$IPUM1990)
philippines_00$IPUM2000 <- as.integer(philippines_00$IPUM2000)
philippines_10$IPUM2010 <- as.integer(philippines_10$IPUM2010)

##Joining by year
philippines_90 <- philippines %>% inner_join(philippines_90, by="IPUM1990")
philippines_00 <- philippines %>% inner_join(philippines_00, by="IPUM2000")
philippines_10 <- philippines %>% inner_join(philippines_10, by="IPUM2010")
names(philippines_90)
names(philippines_00)
names(philippines_10)

philippines_90 <- select(philippines_90, -c(MUNI1990))
philippines_00 <- select(philippines_00, -c(MUNI2000))
philippines_10 <- select(philippines_10, -c(MUNI2010))

##Merging all years into one table
philippines_full <- rbind(philippines_90,philippines_00,philippines_10)
names(philippines_full)

table(philippines_full$CITY)

philippines_full <- philippines_full %>%  filter (philippines_full$YRSCHOOL < 90)

philippines_full <- philippines_full %>%  filter (philippines_full$AGE >15)

cebu_full<- philippines_full %>% filter(CITY=="cebu")
table(cebu_full$YEAR)

cebu_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

manila_full<- philippines_full %>% filter(CITY=="manila")
table(manila_full$YEAR)

manila_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

bacolod_full<- philippines_full %>% filter(CITY=="bacolod")
table(bacolod_full$YEAR)
bacolod_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

cebu_fu90 <- cebu_full %>%  filter (YEAR==1990)
cebu_fu00 <- cebu_full %>%  filter (YEAR==2000)
cebu_fu10 <- cebu_full %>%  filter (YEAR==2010)
Gini(cebu_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(cebu_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(cebu_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(cebu_fu90 $TOTAL_ASSET,na.rm = TRUE)
Gini(cebu_fu00 $TOTAL_ASSET,na.rm = TRUE)
Gini(cebu_fu10 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_cebu90 <- Lc(cebu_fu90$YRSCHOOL, n = rep(1,length(cebu_fu90$YRSCHOOL)), plot = TRUE)
Lc_cebu00 <- Lc(cebu_fu00$YRSCHOOL, n = rep(1,length(cebu_fu00$YRSCHOOL)), plot = TRUE)
Lc_cebu10 <- Lc(cebu_fu10$YRSCHOOL, n = rep(1,length(cebu_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_cebu90,col='blue', main = "Lorenz Curve - Cebu")
lines(Lc_cebu00, col='red')
lines(Lc_cebu10, col='green')

manila_fu90 <- manila_full %>%  filter (YEAR==1990)
manila_fu00 <- manila_full %>%  filter (YEAR==2000)
manila_fu10 <- manila_full %>%  filter (YEAR==2010)
Gini(manila_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(manila_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(manila_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(manila_fu90 $TOTAL_ASSET,na.rm = TRUE)
Gini(manila_fu00 $TOTAL_ASSET,na.rm = TRUE)
Gini(manila_fu10 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_manil90 <- Lc(manila_fu90$YRSCHOOL, n = rep(1,length(manila_fu90$YRSCHOOL)), plot = TRUE)
Lc_manil00 <- Lc(manila_fu00$YRSCHOOL, n = rep(1,length(manila_fu00$YRSCHOOL)), plot = TRUE)
Lc_manil10 <- Lc(manila_fu10$YRSCHOOL, n = rep(1,length(manila_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_manil90,col='blue', main = "Lorenz Curve - Manila")
lines(Lc_manil00, col='red')
lines(Lc_manil10, col='green')

bacolod_fu90 <- bacolod_full %>%  filter (YEAR==1990)
bacolod_fu00 <- bacolod_full %>%  filter (YEAR==2000)
bacolod_fu10 <- bacolod_full %>%  filter (YEAR==2010)
Gini(bacolod_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(bacolod_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(bacolod_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(bacolod_fu90 $TOTAL_ASSET,na.rm = TRUE)
Gini(bacolod_fu00 $TOTAL_ASSET,na.rm = TRUE)
Gini(bacolod_fu10 $TOTAL_ASSET,na.rm = TRUE)


## compute the Lorenz curves
Lc_bacol90 <- Lc(bacolod_fu90$YRSCHOOL, n = rep(1,length(bacolod_fu90$YRSCHOOL)), plot = TRUE)
Lc_bacol00 <- Lc(bacolod_fu00$YRSCHOOL, n = rep(1,length(bacolod_fu00$YRSCHOOL)), plot = TRUE)
Lc_bacol10 <- Lc(bacolod_fu10$YRSCHOOL, n = rep(1,length(bacolod_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_bacol90,col='blue', main = "Lorenz Curve - Bacolod")
lines(Lc_bacol00, col='red')
lines(Lc_bacol10, col='green')

philippines_edu <- philippines %>%  filter (philippines$YRSCHOOL < 90)

philippines_edu <- philippines_edu %>%  filter (philippines_edu$AGE >15)

phil_fu90 <- philippines_edu %>%  filter (YEAR==1990)
phil_fu00 <- philippines_edu %>%  filter (YEAR==2000)
phil_fu10 <- philippines_edu %>%  filter (YEAR==2010)
Gini(phil_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(phil_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(phil_fu10 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves
Lc_phil90 <- Lc(phil_fu90$YRSCHOOL, n = rep(1,length(phil_fu90$YRSCHOOL)), plot = TRUE)
Lc_phil00 <- Lc(phil_fu00$YRSCHOOL, n = rep(1,length(phil_fu00$YRSCHOOL)), plot = TRUE)
Lc_phil10 <- Lc(phil_fu10$YRSCHOOL, n = rep(1,length(phil_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_phil90,col='blue', main = "Lorenz Curve - Philipines")
lines(Lc_phil00, col='red')
lines(Lc_phil10, col='green')

names(philippines_full)

table(philippines_full$OCCISCO)

philippines_full$OCCISCO_b <- ifelse(philippines_full$OCCISCO ==1|philippines_full$OCCISCO ==2,1,0)
philippines_full$OCCISCO_b <- ifelse(philippines_full$OCCISCO ==3|philippines_full$OCCISCO ==4|philippines_full$OCCISCO ==5,2,philippines_full$OCCISCO_b)
philippines_full$OCCISCO_b <- ifelse(philippines_full$OCCISCO ==6|philippines_full$OCCISCO ==7|philippines_full$OCCISCO ==8|philippines_full$OCCISCO ==9,3,philippines_full$OCCISCO_b)
table(philippines_full$OCCISCO_b)

philippines_full_OCCISCO_b <- philippines_full %>% select(YEAR,CITY,OCCISCO_b,PERWT)
philippines_full_OCCISCO_b <- philippines_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(philippines_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- philippines_full_OCCISCO_b %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- philippines_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- philippines_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- philippines_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

philippines_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR","CITY"))
philippines_OCCISCO_b<- philippines_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR","CITY"))
philippines_OCCISCO_b<- philippines_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR","CITY"))

philippines_OCCISCO_b
gc()

# for PUBl_ASSET
cebu_PUBl_ASSET<-cebu_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cebu_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cebu_PUBl_ASSET <- cebu_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cebu_PUBl_ASSET <- cebu_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cebu_PUBl_ASSET$CITY<-"cebu"
# for PRIV_ASSET
cebu_PRIV_ASSET<-cebu_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cebu_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cebu_PRIV_ASSET <- cebu_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cebu_PRIV_ASSET <- cebu_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cebu_PRIV_ASSET$CITY<-"cebu"
# for TOTAL_ASSET
cebu_TOTAL_ASSET<-cebu_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cebu_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cebu_TOTAL_ASSET <- cebu_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cebu_TOTAL_ASSET <- cebu_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cebu_TOTAL_ASSET$CITY<-"cebu"
write.csv(cebu_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cebu_PUBl_ASSET.csv", row.names = TRUE)
write.csv(cebu_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cebu_PRIV_ASSET.csv", row.names = TRUE)
write.csv(cebu_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cebu_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
bacolod_PUBl_ASSET<-bacolod_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bacolod_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bacolod_PUBl_ASSET <- bacolod_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bacolod_PUBl_ASSET <- bacolod_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bacolod_PUBl_ASSET$CITY<-"bacolod"
# for PRIV_ASSET
bacolod_PRIV_ASSET<-bacolod_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bacolod_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bacolod_PRIV_ASSET <- bacolod_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bacolod_PRIV_ASSET <- bacolod_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bacolod_PRIV_ASSET$CITY<-"bacolod"
# for TOTAL_ASSET
bacolod_TOTAL_ASSET<-bacolod_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bacolod_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bacolod_TOTAL_ASSET <- bacolod_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bacolod_TOTAL_ASSET <- bacolod_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bacolod_TOTAL_ASSET$CITY<-"bacolod"
write.csv(bacolod_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bacolod_PUBl_ASSET.csv", row.names = TRUE)
write.csv(bacolod_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bacolod_PRIV_ASSET.csv", row.names = TRUE)
write.csv(bacolod_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bacolod_TOTAL_ASSET.csv", row.names = TRUE)


# for PUBl_ASSET
manila_PUBl_ASSET<-manila_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- manila_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
manila_PUBl_ASSET <- manila_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
manila_PUBl_ASSET <- manila_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
manila_PUBl_ASSET$CITY<-"manila"
# for PRIV_ASSET
manila_PRIV_ASSET<-manila_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- manila_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
manila_PRIV_ASSET <- manila_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
manila_PRIV_ASSET <- manila_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
manila_PRIV_ASSET$CITY<-"manila"
# for TOTAL_ASSET
manila_TOTAL_ASSET<-manila_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- manila_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
manila_TOTAL_ASSET <- manila_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
manila_TOTAL_ASSET <- manila_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
manila_TOTAL_ASSET$CITY<-"manila"
write.csv(manila_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/manila_PUBl_ASSET.csv", row.names = TRUE)
write.csv(manila_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/manila_PRIV_ASSET.csv", row.names = TRUE)
write.csv(manila_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/manila_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

manila_WATER <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(manila_WATER$WATSUP)
table(manila_WATER$HHWT)
manila_WATER <- manila_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(manila_WATER$WATSUP)
table(manila_WATER$water_b)

manila_WATER$water_b <- factor(manila_WATER$water_b)

manila_WATER$YEAR <- factor(manila_WATER$YEAR)
manila_WATER_01 <- manila_WATER %>% filter(manila_WATER$YEAR==2000)
manila_WATER_12 <- manila_WATER %>% filter(manila_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = manila_WATER_01, family = "binomial",weights = HHWT)
table(manila_WATER_01$water_b,manila_WATER_01$YRSCHOOL)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = manila_WATER_12, family = "binomial",weights = HHWT)
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

manila_OWN <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(manila_OWN$OWNERSHIP)
manila_OWN <- manila_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(manila_OWN$OWNERSHIP)
table(manila_OWN$owner_b)

manila_OWN$owner_b <- factor(manila_OWN$owner_b)

manila_OWN$YEAR <- factor(manila_OWN$YEAR)
manila_OWN_01 <- manila_OWN %>% filter(manila_OWN$YEAR==2000)
manila_OWN_12 <- manila_OWN %>% filter(manila_OWN$YEAR==2010)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = manila_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = manila_OWN_12, family = "binomial",weights = HHWT)
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

#################################### FOR REFRIG ##################################

manila_REFRIG <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(manila_REFRIG$REFRIG)
manila_REFRIG <- manila_REFRIG %>% filter(REFRIG!=0)

table(manila_REFRIG$REFRIG)
table(manila_REFRIG$refrig_b)

manila_REFRIG$refrig_b <- factor(manila_REFRIG$refrig_b)

manila_REFRIG$YEAR <- factor(manila_REFRIG$YEAR)
manila_REFRIG_01 <- manila_REFRIG %>% filter(manila_REFRIG$YEAR==2000)
manila_REFRIG_12 <- manila_REFRIG %>% filter(manila_REFRIG$YEAR==2010)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = manila_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = manila_REFRIG_12, family = "binomial",weights = HHWT)
table(manila_REFRIG_12$refrig_b,manila_REFRIG_12$YRSCHOOL)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

manila_TOILET <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(manila_TOILET$TOILET)
manila_TOILET <- manila_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(manila_TOILET$TOILET)
table(manila_TOILET$toilet_b)

manila_TOILET$toilet_b <- factor(manila_TOILET$toilet_b)

manila_TOILET$YEAR <- factor(manila_TOILET$YEAR)
manila_TOILET_01 <- manila_TOILET %>% filter(manila_TOILET$YEAR==2000)
manila_TOILET_12 <- manila_TOILET %>% filter(manila_TOILET$YEAR==2010)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = manila_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = manila_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_12)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

manila_ELECTRIC <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(manila_ELECTRIC$ELECTRIC)
manila_ELECTRIC <- manila_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(manila_ELECTRIC$ELECTRIC)
table(manila_ELECTRIC$eletric_b)

manila_ELECTRIC$eletric_b <- factor(manila_ELECTRIC$eletric_b)

manila_ELECTRIC$YEAR <- factor(manila_ELECTRIC$YEAR)
manila_ELECTRIC_01 <- manila_ELECTRIC %>% filter(manila_ELECTRIC$YEAR==2000)
manila_ELECTRIC_12 <- manila_ELECTRIC %>% filter(manila_ELECTRIC$YEAR==2010)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = manila_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = manila_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_12)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

manila_TRASH <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(manila_TRASH$TRASH)
manila_TRASH <- manila_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(manila_TRASH$TRASH)
table(manila_TRASH$trash_b)

manila_TRASH$trash_b <- factor(manila_TRASH$trash_b)

manila_TRASH$YEAR <- factor(manila_TRASH$YEAR)
manila_TRASH_01 <- manila_TRASH %>% filter(manila_TRASH$YEAR==2000)
manila_TRASH_12 <- manila_TRASH %>% filter(manila_TRASH$YEAR==2010)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = manila_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = manila_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

manila_AUTOS <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(manila_AUTOS$AUTOS)
manila_AUTOS <- manila_AUTOS %>% filter(AUTOS!=99)

table(manila_AUTOS$AUTOS)
table(manila_AUTOS$autos_b)

manila_AUTOS$autos_b <- factor(manila_AUTOS$autos_b)

manila_AUTOS$YEAR <- factor(manila_AUTOS$YEAR)
manila_AUTOS_12 <- manila_AUTOS %>% filter(manila_AUTOS$YEAR==2010)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = manila_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

manila_ROOF <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(manila_ROOF$ROOF)
manila_ROOF <- manila_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(manila_ROOF$ROOF)
table(manila_ROOF$roof_b)

manila_ROOF$roof_b <- factor(manila_ROOF$roof_b)

manila_ROOF$YEAR <- factor(manila_ROOF$YEAR)
manila_ROOF_01 <- manila_ROOF %>% filter(manila_ROOF$YEAR==2000)
manila_ROOF_12 <- manila_ROOF %>% filter(manila_ROOF$YEAR==2010)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = manila_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = manila_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

manila_TV <- manila_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(manila_TV$TV)
manila_TV <- manila_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(manila_TV$TV)
table(manila_TV$tv_b)

manila_TV$tv_b <- factor(manila_TV$tv_b)

manila_TV$YEAR <- factor(manila_TV$YEAR)
manila_TV_01 <- manila_TV %>% filter(manila_TV$YEAR==2000)
manila_TV_12 <- manila_TV %>% filter(manila_TV$YEAR==2010)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = manila_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = manila_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

manila_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
manila_logit$CITY <- "Manila"

#################################### FOR WATSUP ##################################

bacolod_WATER <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(bacolod_WATER$WATSUP)
bacolod_WATER <- bacolod_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(bacolod_WATER$WATSUP)
table(bacolod_WATER$water_b)

bacolod_WATER$water_b <- factor(bacolod_WATER$water_b)

bacolod_WATER$YEAR <- factor(bacolod_WATER$YEAR)
bacolod_WATER_01 <- bacolod_WATER %>% filter(bacolod_WATER$YEAR==2000)
bacolod_WATER_12 <- bacolod_WATER %>% filter(bacolod_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = bacolod_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = bacolod_WATER_12, family = "binomial",weights = HHWT)
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

bacolod_OWN <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(bacolod_OWN$OWNERSHIP)
bacolod_OWN <- bacolod_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(bacolod_OWN$OWNERSHIP)
table(bacolod_OWN$owner_b)

bacolod_OWN$owner_b <- factor(bacolod_OWN$owner_b)

bacolod_OWN$YEAR <- factor(bacolod_OWN$YEAR)
bacolod_OWN_01 <- bacolod_OWN %>% filter(bacolod_OWN$YEAR==2000)
bacolod_OWN_12 <- bacolod_OWN %>% filter(bacolod_OWN$YEAR==2010)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = bacolod_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = bacolod_OWN_12, family = "binomial",weights = HHWT)
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

#################################### FOR REFRIG ##################################

bacolod_REFRIG <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(bacolod_REFRIG$REFRIG)
bacolod_REFRIG <- bacolod_REFRIG %>% filter(REFRIG!=0)

table(bacolod_REFRIG$REFRIG)
table(bacolod_REFRIG$refrig_b)

bacolod_REFRIG$refrig_b <- factor(bacolod_REFRIG$refrig_b)

bacolod_REFRIG$YEAR <- factor(bacolod_REFRIG$YEAR)
bacolod_REFRIG_01 <- bacolod_REFRIG %>% filter(bacolod_REFRIG$YEAR==2000)
bacolod_REFRIG_12 <- bacolod_REFRIG %>% filter(bacolod_REFRIG$YEAR==2010)

mylogit <- glm(refrig_b ~ YRSCHOOL + YEAR, data = bacolod_REFRIG, family = "binomial",weights = HHWT)
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = bacolod_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = bacolod_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

bacolod_TOILET <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(bacolod_TOILET$TOILET)
bacolod_TOILET <- bacolod_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(bacolod_TOILET$TOILET)
table(bacolod_TOILET$toilet_b)

bacolod_TOILET$toilet_b <- factor(bacolod_TOILET$toilet_b)

bacolod_TOILET$YEAR <- factor(bacolod_TOILET$YEAR)
bacolod_TOILET_01 <- bacolod_TOILET %>% filter(bacolod_TOILET$YEAR==2000)
bacolod_TOILET_12 <- bacolod_TOILET %>% filter(bacolod_TOILET$YEAR==2010)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = bacolod_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = bacolod_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_12)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

bacolod_ELECTRIC <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(bacolod_ELECTRIC$ELECTRIC)
bacolod_ELECTRIC <- bacolod_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(bacolod_ELECTRIC$ELECTRIC)
table(bacolod_ELECTRIC$eletric_b)

bacolod_ELECTRIC$eletric_b <- factor(bacolod_ELECTRIC$eletric_b)

bacolod_ELECTRIC$YEAR <- factor(bacolod_ELECTRIC$YEAR)
bacolod_ELECTRIC_01 <- bacolod_ELECTRIC %>% filter(bacolod_ELECTRIC$YEAR==2000)
bacolod_ELECTRIC_12 <- bacolod_ELECTRIC %>% filter(bacolod_ELECTRIC$YEAR==2010)


mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = bacolod_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = bacolod_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_12)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

bacolod_TRASH <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(bacolod_TRASH$TRASH)
bacolod_TRASH <- bacolod_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(bacolod_TRASH$TRASH)
table(bacolod_TRASH$trash_b)

bacolod_TRASH$trash_b <- factor(bacolod_TRASH$trash_b)

bacolod_TRASH$YEAR <- factor(bacolod_TRASH$YEAR)
bacolod_TRASH_01 <- bacolod_TRASH %>% filter(bacolod_TRASH$YEAR==2000)
bacolod_TRASH_12 <- bacolod_TRASH %>% filter(bacolod_TRASH$YEAR==2010)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = bacolod_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = bacolod_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

bacolod_AUTOS <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(bacolod_AUTOS$AUTOS)
bacolod_AUTOS <- bacolod_AUTOS  %>% filter(AUTOS!=99)

table(bacolod_AUTOS$AUTOS)
table(bacolod_AUTOS$autos_b)

bacolod_AUTOS$autos_b <- factor(bacolod_AUTOS$autos_b)

bacolod_AUTOS$YEAR <- factor(bacolod_AUTOS$YEAR)
bacolod_AUTOS_12 <- bacolod_AUTOS %>% filter(bacolod_AUTOS$YEAR==2010)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = bacolod_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

bacolod_ROOF <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(bacolod_ROOF$ROOF)
bacolod_ROOF <- bacolod_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(bacolod_ROOF$ROOF)
table(bacolod_ROOF$roof_b)

bacolod_ROOF$roof_b <- factor(bacolod_ROOF$roof_b)

bacolod_ROOF$YEAR <- factor(bacolod_ROOF$YEAR)
bacolod_ROOF_01 <- bacolod_ROOF %>% filter(bacolod_ROOF$YEAR==2000)
bacolod_ROOF_12 <- bacolod_ROOF %>% filter(bacolod_ROOF$YEAR==2010)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = bacolod_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = bacolod_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

bacolod_TV <- bacolod_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(bacolod_TV$TV)
bacolod_TV <- bacolod_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(bacolod_TV$TV)
table(bacolod_TV$tv_b)

bacolod_TV$tv_b <- factor(bacolod_TV$tv_b)

bacolod_TV$YEAR <- factor(bacolod_TV$YEAR)
bacolod_TV_01 <- bacolod_TV %>% filter(bacolod_TV$YEAR==2000)
bacolod_TV_12 <- bacolod_TV %>% filter(bacolod_TV$YEAR==2010)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = bacolod_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = bacolod_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

bacolod_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
bacolod_logit$CITY <- "Bacolod"

########################### FOR WATSUP ##################################

cebu_WATER <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(cebu_WATER$WATSUP)
cebu_WATER <- cebu_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(cebu_WATER$WATSUP)
table(cebu_WATER$water_b)

cebu_WATER$water_b <- factor(cebu_WATER$water_b)

cebu_WATER$YEAR <- factor(cebu_WATER$YEAR)
cebu_WATER_01 <- cebu_WATER %>% filter(cebu_WATER$YEAR==2000)
cebu_WATER_12 <- cebu_WATER %>% filter(cebu_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = cebu_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = cebu_WATER_12, family = "binomial",weights = HHWT)
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

cebu_OWN <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(cebu_OWN$OWNERSHIP)
cebu_OWN <- cebu_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(cebu_OWN$OWNERSHIP)
table(cebu_OWN$owner_b)

cebu_OWN$owner_b <- factor(cebu_OWN$owner_b)

cebu_OWN$YEAR <- factor(cebu_OWN$YEAR)
cebu_OWN_01 <- cebu_OWN %>% filter(cebu_OWN$YEAR==2000)
cebu_OWN_12 <- cebu_OWN %>% filter(cebu_OWN$YEAR==2010)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = cebu_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = cebu_OWN_12, family = "binomial",weights = HHWT)
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

#################################### FOR REFRIG ##################################

cebu_REFRIG <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(cebu_REFRIG$REFRIG)
cebu_REFRIG <- cebu_REFRIG %>% filter(REFRIG!=0)

table(cebu_REFRIG$REFRIG)
table(cebu_REFRIG$refrig_b)

cebu_REFRIG$refrig_b <- factor(cebu_REFRIG$refrig_b)

cebu_REFRIG$YEAR <- factor(cebu_REFRIG$YEAR)
cebu_REFRIG_01 <- cebu_REFRIG %>% filter(cebu_REFRIG$YEAR==2000)
cebu_REFRIG_12 <- cebu_REFRIG %>% filter(cebu_REFRIG$YEAR==2010)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = cebu_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = cebu_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

cebu_TOILET <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(cebu_TOILET$TOILET)
cebu_TOILET <- cebu_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(cebu_TOILET$TOILET)
table(cebu_TOILET$toilet_b)

cebu_TOILET$toilet_b <- factor(cebu_TOILET$toilet_b)

cebu_TOILET$YEAR <- factor(cebu_TOILET$YEAR)
cebu_TOILET_01 <- cebu_TOILET %>% filter(cebu_TOILET$YEAR==2000)
cebu_TOILET_12 <- cebu_TOILET %>% filter(cebu_TOILET$YEAR==2010)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = cebu_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = cebu_TOILET_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_12)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

cebu_ELECTRIC <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(cebu_ELECTRIC$ELECTRIC)
cebu_ELECTRIC <- cebu_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(cebu_ELECTRIC$ELECTRIC)
table(cebu_ELECTRIC$eletric_b)

cebu_ELECTRIC$eletric_b <- factor(cebu_ELECTRIC$eletric_b)

cebu_ELECTRIC$YEAR <- factor(cebu_ELECTRIC$YEAR)
cebu_ELECTRIC_01 <- cebu_ELECTRIC %>% filter(cebu_ELECTRIC$YEAR==2000)
cebu_ELECTRIC_12 <- cebu_ELECTRIC %>% filter(cebu_ELECTRIC$YEAR==2010)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = cebu_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = cebu_ELECTRIC_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_12)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

cebu_TRASH <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(cebu_TRASH$TRASH)
cebu_TRASH <- cebu_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(cebu_TRASH$TRASH)
table(cebu_TRASH$trash_b)

cebu_TRASH$trash_b <- factor(cebu_TRASH$trash_b)

cebu_TRASH$YEAR <- factor(cebu_TRASH$YEAR)
cebu_TRASH_01 <- cebu_TRASH %>% filter(cebu_TRASH$YEAR==2000)
cebu_TRASH_12 <- cebu_TRASH %>% filter(cebu_TRASH$YEAR==2010)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = cebu_TRASH_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = cebu_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T2 <- summary(mylogit_01)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

cebu_AUTOS <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(cebu_AUTOS$AUTOS)
cebu_AUTOS <- cebu_AUTOS %>% filter(AUTOS!=99)

table(cebu_AUTOS$AUTOS)
table(cebu_AUTOS$autos_b)

cebu_AUTOS$autos_b <- factor(cebu_AUTOS$autos_b)

cebu_AUTOS$YEAR <- factor(cebu_AUTOS$YEAR)
cebu_AUTOS_12 <- cebu_AUTOS %>% filter(cebu_AUTOS$YEAR==2010)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = cebu_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

cebu_ROOF <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(cebu_ROOF$ROOF)
cebu_ROOF <- cebu_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(cebu_ROOF$ROOF)
table(cebu_ROOF$roof_b)

cebu_ROOF$roof_b <- factor(cebu_ROOF$roof_b)

cebu_ROOF$YEAR <- factor(cebu_ROOF$YEAR)
cebu_ROOF_01 <- cebu_ROOF %>% filter(cebu_ROOF$YEAR==2000)
cebu_ROOF_12 <- cebu_ROOF %>% filter(cebu_ROOF$YEAR==2010)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = cebu_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = cebu_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}
#################################### FOR TV ##################################

cebu_TV <- cebu_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(cebu_TV$TV)
cebu_TV <- cebu_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(cebu_TV$TV)
table(cebu_TV$tv_b)

cebu_TV$tv_b <- factor(cebu_TV$tv_b)

cebu_TV$YEAR <- factor(cebu_TV$YEAR)
cebu_TV_01 <- cebu_TV %>% filter(cebu_TV$YEAR==2000)
cebu_TV_12 <- cebu_TV %>% filter(cebu_TV$YEAR==2010)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = cebu_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = cebu_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

cebu_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
cebu_logit$CITY <- "Cebu"




#################################################Saving an unique table results join

PHILIPINAS_logit <- rbind(manila_logit,cebu_logit,bacolod_logit,sep = ".") 
write.csv(PHILIPINAS_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/PHILIPINAS_logit.csv"))


