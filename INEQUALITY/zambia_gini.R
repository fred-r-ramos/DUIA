library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/zambia")

geo2_zm90 <- read_sf("geo2_zm1990.shp")
geo2_zm00 <- read_sf("geo2_zm2000.shp")
geo2_zm10 <- read_sf("geo2_zm2010.shp")
centroid_zm90 <- st_centroid(geo2_zm90)
centroid_zm00 <- st_centroid(geo2_zm00)
centroid_zm10 <- st_centroid(geo2_zm10)
AUE_ndola <- read_sf("Ndola_studyArea.shp")
AUE_ndola  <- st_transform(AUE_ndola , 4326)

sf::sf_use_s2(FALSE)

ndola_90 <- geo2_zm90[st_intersection(AUE_ndola,centroid_zm90),]
ndola_90['CITY']='ndola'
ndola_00 <- geo2_zm00[st_intersection(AUE_ndola,centroid_zm00),]
ndola_00['CITY']='ndola'
ndola_10 <- geo2_zm10[st_intersection(AUE_ndola,centroid_zm10),]
ndola_10['CITY']='ndola'

plot(st_geometry(ndola_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_zm10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00236.xml")
ddi <-read_ipums_ddi("ipumsi_00265.xml")
zambia <- read_ipums_micro(ddi)

names(zambia)

######################creating binary variables for logit regressions

names(zambia)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(zambia$OWNERSHIP)
zambia$owner_b <- ifelse(zambia$OWNERSHIP ==1,1,0)
table(zambia$ELECTRIC)
zambia$eletric_b <- ifelse(zambia$ELECTRIC ==1,1,0)
table(zambia$WATSUP)
zambia$water_b <- ifelse(zambia$WATSUP ==11,1,0)
table(zambia$SEWAGE)
zambia$sewage_b <- ifelse(zambia$SEWAGE ==10,1,0)
table(zambia$TRASH)
zambia$trash_b <- ifelse(zambia$TRASH ==10,1,0)
table(zambia$AUTOS)
zambia$autos_b <- ifelse(zambia$AUTOS ==7,1,0)
table(zambia$REFRIG)
zambia$refrig_b <- ifelse(zambia$REFRIG ==2,1,0)
table(zambia$TV)
zambia$tv_b <- ifelse(zambia$TV>19 & zambia$TV<25,1,0)
table(zambia$TOILET)
zambia$toilet_b <- ifelse(zambia$TOILET >19 & zambia$TOILET <24,1,0)
table(zambia$FLOOR)
zambia$floor_b <- ifelse(zambia$FLOOR ==14,1,0)
table(zambia$ROOF)
zambia$roof_b <- ifelse(zambia$ROOF==14|zambia$ROOF==27,1,0)

names(zambia)
gc()

#checking if the variables are available in both years
table(zambia$owner_b,zambia$YEAR)
table(zambia$eletric_b,zambia$YEAR)
table(zambia$water_b,zambia$YEAR)
table(zambia$sewage_b,zambia$YEAR)
table(zambia$trash_b,zambia$YEAR)
table(zambia$autos_b,zambia$YEAR)
table(zambia$refrig_b,zambia$YEAR)
table(zambia$tv_b,zambia$YEAR)
table(zambia$toilet_b,zambia$YEAR)
table(zambia$floor_b,zambia$YEAR)
table(zambia$roof_b,zambia$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
zambia$PUBl_ASSET <- ifelse(is.na(zambia$eletric_b), 0, zambia$eletric_b) +
  ifelse(is.na(zambia$water_b), 0, zambia$water_b) +
  ifelse(is.na(zambia$sewage_b), 0, zambia$sewage_b)
zambia %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

zambia$PRIV_ASSET <- ifelse(is.na(zambia$owner_b), 0, zambia$owner_b) +
  ifelse(is.na(zambia$autos_b), 0, zambia$autos_b) +
  ifelse(is.na(zambia$refrig_b), 0, zambia$refrig_b) +
  ifelse(is.na(zambia$tv_b), 0, zambia$tv_b) +
  ifelse(is.na(zambia$toilet_b), 0, zambia$toilet_b) +
  ifelse(is.na(zambia$floor_b), 0, zambia$floor_b) +
  ifelse(is.na(zambia$roof_b), 0, zambia$roof_b)
zambia %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

zambia$TOTAL_ASSET <- ifelse(is.na(zambia$owner_b), 0, zambia$owner_b) +
  ifelse(is.na(zambia$autos_b), 0, zambia$autos_b) +
  ifelse(is.na(zambia$refrig_b), 0, zambia$refrig_b) +
  ifelse(is.na(zambia$tv_b), 0, zambia$tv_b) +
  ifelse(is.na(zambia$toilet_b), 0, zambia$toilet_b) +
  ifelse(is.na(zambia$floor_b), 0, zambia$floor_b) +
  ifelse(is.na(zambia$roof_b), 0, zambia$roof_b) +
  ifelse(is.na(zambia$eletric_b), 0, zambia$eletric_b) +
  ifelse(is.na(zambia$water_b), 0, zambia$water_b) +
  ifelse(is.na(zambia$sewage_b), 0, zambia$sewage_b) +
  ifelse(is.na(zambia$trash_b), 0, zambia$trash_b)
zambia %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

zambia$TOTAL_ASSET_gini <- ifelse(is.na(zambia$owner_b), 0, zambia$owner_b) +
  ifelse(is.na(zambia$tv_b), 0, zambia$tv_b) +
  ifelse(is.na(zambia$toilet_b), 0, zambia$toilet_b) +
  ifelse(is.na(zambia$roof_b), 0, zambia$roof_b) +
  ifelse(is.na(zambia$eletric_b), 0, zambia$eletric_b) +
  ifelse(is.na(zambia$water_b), 0, zambia$water_b) +
  ifelse(is.na(zambia$sewage_b), 0, zambia$sewage_b) 
assets<-zambia %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

zambia$IPUM1990 <- as.integer(zambia$GEO2_ZM1990)
zambia$IPUM2000 <- as.integer(zambia$GEO2_ZM2000)
zambia$IPUM2010 <- as.integer(zambia$GEO2_ZM2010)
ndola_90$IPUM1990 <- as.integer(ndola_90$IPUM1990)
ndola_00$IPUM2000 <- as.integer(ndola_00$IPUM2000)
ndola_10$IPUM2010 <- as.integer(ndola_10$IPUM2010)

##Joining by year

ndola_90 <- zambia %>% inner_join(ndola_90, by="IPUM1990")
ndola_00 <- zambia %>% inner_join(ndola_00, by="IPUM2000")
ndola_10 <- zambia %>% inner_join(ndola_10, by="IPUM2010")

names(ndola_90)
names(ndola_00)
names(ndola_10)

ndola_90 <- select(ndola_90, -c(DIST1990))
ndola_00 <- select(ndola_00, -c(DIST2000))
ndola_10 <- select(ndola_10, -c(DIST2010))

##Merging all years into one table
ndola_full <- rbind(ndola_90,ndola_00,ndola_10)
names(ndola_full)

##Excluding specific columns for the unifeied dataset
ndola_full<- select(ndola_full, -c(GEO2_ZM1990,GEO2_ZM2000,GEO2_ZM2010,IPUM1990,IPUM2000,IPUM2010,geometry))
table(ndola_full$CITY)

names(ndola_full)

table(ndola_full$YEAR)

ndola_full <- ndola_full %>%  filter (ndola_full$YRSCHOOL < 90)

ndola_full <- ndola_full %>%  filter (ndola_full$AGE >15)

ndola_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(ndola_full$YRSCHOOL)
summary(ndola_full$YRSCHOOL)

ndola_fu90 <- ndola_full %>%  filter (YEAR==1990)
ndola_fu00 <- ndola_full %>%  filter (YEAR==2000)
ndola_fu10 <- ndola_full %>%  filter (YEAR==2010)
Gini(ndola_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(ndola_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(ndola_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(ndola_fu90 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ndola_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(ndola_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)


## compute the Lorenz curves

## compute the Lorenz curves
Lc_ndol90 <- Lc(ndola_fu90$YRSCHOOL, n = rep(1,length(ndola_fu90$YRSCHOOL)), plot = TRUE)
Lc_ndol00 <- Lc(ndola_fu00$YRSCHOOL, n = rep(1,length(ndola_fu00$YRSCHOOL)), plot = TRUE)
Lc_ndol10 <- Lc(ndola_fu10$YRSCHOOL, n = rep(1,length(ndola_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_ndol90,col='blue', main = "Lorenz Curve - Ndola")
lines(Lc_ndol00, col='red')
lines(Lc_ndol10, col='green')

table(ndola_full$OCCISCO)


ndola_full$OCCISCO_b <- ifelse(ndola_full$OCCISCO ==1|ndola_full$OCCISCO ==2,1,0)
ndola_full$OCCISCO_b <- ifelse(ndola_full$OCCISCO ==3|ndola_full$OCCISCO ==4|ndola_full$OCCISCO ==5,2,ndola_full$OCCISCO_b)
ndola_full$OCCISCO_b <- ifelse(ndola_full$OCCISCO ==6|ndola_full$OCCISCO ==7|ndola_full$OCCISCO ==8|ndola_full$OCCISCO ==9,3,ndola_full$OCCISCO_b)
table(ndola_full$OCCISCO_b)

ndola_full_OCCISCO_b <- ndola_full %>% select(YEAR,OCCISCO_b,PERWT)
ndola_full_OCCISCO_b <- ndola_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(ndola_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- ndola_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- ndola_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- ndola_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- ndola_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

ndola_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
ndola_OCCISCO_b<- ndola_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
ndola_OCCISCO_b<- ndola_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

ndola_OCCISCO_b


# for PUBl_ASSET
ndola_PUBl_ASSET<-ndola_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ndola_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ndola_PUBl_ASSET <- ndola_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ndola_PUBl_ASSET <- ndola_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ndola_PUBl_ASSET$CITY<-"ndola"
# for PRIV_ASSET
ndola_PRIV_ASSET<-ndola_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ndola_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ndola_PRIV_ASSET <- ndola_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ndola_PRIV_ASSET <- ndola_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ndola_PRIV_ASSET$CITY<-"ndola"
# for TOTAL_ASSET
ndola_TOTAL_ASSET<-ndola_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- ndola_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
ndola_TOTAL_ASSET <- ndola_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
ndola_TOTAL_ASSET <- ndola_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
ndola_TOTAL_ASSET$CITY<-"ndola"
write.csv(ndola_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ndola_PUBl_ASSET.csv", row.names = TRUE)
write.csv(ndola_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ndola_PRIV_ASSET.csv", row.names = TRUE)
write.csv(ndola_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/ndola_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

ndola_WATER <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(ndola_WATER$WATSUP)
table(ndola_WATER$HHWT)
ndola_WATER <- ndola_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(ndola_WATER$WATSUP)
table(ndola_WATER$water_b)

ndola_WATER$water_b <- factor(ndola_WATER$water_b)

ndola_WATER$YEAR <- factor(ndola_WATER$YEAR)
ndola_WATER_01 <- ndola_WATER %>% filter(ndola_WATER$YEAR==2000)
ndola_WATER_12 <- ndola_WATER %>% filter(ndola_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = ndola_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = ndola_WATER_12, family = "binomial")
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

ndola_OWN <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(ndola_OWN$OWNERSHIP)
ndola_OWN <- ndola_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(ndola_OWN$OWNERSHIP)
table(ndola_OWN$owner_b)

ndola_OWN$owner_b <- factor(ndola_OWN$owner_b)

ndola_OWN$YEAR <- factor(ndola_OWN$YEAR)
ndola_OWN_01 <- ndola_OWN %>% filter(ndola_OWN$YEAR==2000)
ndola_OWN_12 <- ndola_OWN %>% filter(ndola_OWN$YEAR==2010)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = ndola_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = ndola_OWN_12, family = "binomial")
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

ndola_SEWG <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(ndola_SEWG$SEWAGE)
ndola_SEWG <- ndola_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(ndola_SEWG$SEWAGE)
table(ndola_SEWG$sewage_b)

ndola_SEWG$sewage_b <- factor(ndola_SEWG$sewage_b)

ndola_SEWG$YEAR <- factor(ndola_SEWG$YEAR)
ndola_SEWG_01 <- ndola_SEWG %>% filter(ndola_SEWG$YEAR==2000)
ndola_SEWG_12 <- ndola_SEWG %>% filter(ndola_SEWG$YEAR==2010)

mylogit <- glm(sewage_b ~ YRSCHOOL + YEAR, data = ndola_SEWG, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = ndola_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(sewage_b ~ YRSCHOOL, data = ndola_SEWG_12, family = "binomial")
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

ndola_REFRIG <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(ndola_REFRIG$REFRIG)
ndola_REFRIG <- ndola_REFRIG %>% filter(REFRIG!=0)

table(ndola_REFRIG$REFRIG)
table(ndola_REFRIG$refrig_b)

ndola_REFRIG$refrig_b <- factor(ndola_REFRIG$refrig_b)

ndola_REFRIG$YEAR <- factor(ndola_REFRIG$YEAR)
ndola_REFRIG_01 <- ndola_REFRIG %>% filter(ndola_REFRIG$YEAR==2000)
ndola_REFRIG_12 <- ndola_REFRIG %>% filter(ndola_REFRIG$YEAR==2010)

mylogit <- glm(refrig_b ~ YRSCHOOL + YEAR, data = ndola_REFRIG, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = ndola_REFRIG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = ndola_REFRIG_12, family = "binomial")
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

ndola_TOILET <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(ndola_TOILET$TOILET)
ndola_TOILET <- ndola_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(ndola_TOILET$TOILET)
table(ndola_TOILET$toilet_b)

ndola_TOILET$toilet_b <- factor(ndola_TOILET$toilet_b)

ndola_TOILET$YEAR <- factor(ndola_TOILET$YEAR)
ndola_TOILET_01 <- ndola_TOILET %>% filter(ndola_TOILET$YEAR==2000)
ndola_TOILET_12 <- ndola_TOILET %>% filter(ndola_TOILET$YEAR==2010)

mylogit <- glm(toilet_b ~ YRSCHOOL + YEAR, data = ndola_TOILET, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = ndola_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = ndola_TOILET_12, family = "binomial")
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

ndola_ELECTRIC <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(ndola_ELECTRIC$ELECTRIC)
ndola_ELECTRIC <- ndola_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(ndola_ELECTRIC$ELECTRIC)
table(ndola_ELECTRIC$eletric_b)

ndola_ELECTRIC$eletric_b <- factor(ndola_ELECTRIC$eletric_b)

ndola_ELECTRIC$YEAR <- factor(ndola_ELECTRIC$YEAR)
ndola_ELECTRIC_01 <- ndola_ELECTRIC %>% filter(ndola_ELECTRIC$YEAR==2000)
ndola_ELECTRIC_12 <- ndola_ELECTRIC %>% filter(ndola_ELECTRIC$YEAR==2010)

mylogit <- glm(eletric_b ~ YRSCHOOL + YEAR, data = ndola_ELECTRIC, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = ndola_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = ndola_ELECTRIC_12, family = "binomial")
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

ndola_TRASH <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(ndola_TRASH$TRASH)
ndola_TRASH <- ndola_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(ndola_TRASH$TRASH)
table(ndola_TRASH$trash_b)

ndola_TRASH$trash_b <- factor(ndola_TRASH$trash_b)

ndola_TRASH$YEAR <- factor(ndola_TRASH$YEAR)
ndola_TRASH_01 <- ndola_TRASH %>% filter(ndola_TRASH$YEAR==2000)
ndola_TRASH_12 <- ndola_TRASH %>% filter(ndola_TRASH$YEAR==2010)

mylogit <- glm(trash_b ~ YRSCHOOL + YEAR, data = ndola_TRASH, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = ndola_TRASH_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = ndola_TRASH_12, family = "binomial")
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

ndola_AUTOS <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(ndola_AUTOS$AUTOS)
ndola_AUTOS <- ndola_AUTOS %>% filter %>% filter(AUTOS!=99)

table(ndola_AUTOS$AUTOS)
table(ndola_AUTOS$autos_b)

ndola_AUTOS$autos_b <- factor(ndola_AUTOS$autos_b)

ndola_AUTOS$YEAR <- factor(ndola_AUTOS$YEAR)
ndola_AUTOS_01 <- ndola_AUTOS %>% filter(ndola_AUTOS$YEAR==2000)
ndola_AUTOS_12 <- ndola_AUTOS %>% filter(ndola_AUTOS$YEAR==2010)

mylogit <- glm(autos_b ~ YRSCHOOL + YEAR, data = ndola_AUTOS, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = ndola_AUTOS_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = ndola_AUTOS_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

ndola_FLOOR <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(ndola_FLOOR$FLOOR)
ndola_FLOOR <- ndola_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(ndola_FLOOR$FLOOR)
table(ndola_FLOOR$floor_b)

ndola_FLOOR$floor_b <- factor(ndola_FLOOR$floor_b)

ndola_FLOOR$YEAR <- factor(ndola_FLOOR$YEAR)
ndola_FLOOR_01 <- ndola_FLOOR %>% filter(ndola_FLOOR$YEAR==2000)
ndola_FLOOR_12 <- ndola_FLOOR %>% filter(ndola_FLOOR$YEAR==2010)

mylogit <- glm(floor_b ~ YRSCHOOL + YEAR, data = ndola_FLOOR, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = ndola_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = ndola_FLOOR_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_12)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

ndola_ROOF <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(ndola_ROOF$ROOF)
ndola_ROOF <- ndola_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(ndola_ROOF$ROOF)
table(ndola_ROOF$roof_b)

ndola_ROOF$roof_b <- factor(ndola_ROOF$roof_b)

ndola_ROOF$YEAR <- factor(ndola_ROOF$YEAR)
ndola_ROOF_01 <- ndola_ROOF %>% filter(ndola_ROOF$YEAR==2000)
ndola_ROOF_12 <- ndola_ROOF %>% filter(ndola_ROOF$YEAR==2010)

mylogit <- glm(roof_b ~ YRSCHOOL + YEAR, data = ndola_ROOF, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = ndola_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = ndola_ROOF_12, family = "binomial")
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

ndola_TV <- ndola_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(ndola_TV$TV)
ndola_TV <- ndola_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(ndola_TV$TV)
table(ndola_TV$tv_b)

ndola_TV$tv_b <- factor(ndola_TV$tv_b)

ndola_TV$YEAR <- factor(ndola_TV$YEAR)
ndola_TV_01 <- ndola_TV %>% filter(ndola_TV$YEAR==2000)
ndola_TV_12 <- ndola_TV %>% filter(ndola_TV$YEAR==2010)

mylogit <- glm(tv_b ~ YRSCHOOL + YEAR, data = ndola_TV, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = ndola_TV_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = ndola_TV_12, family = "binomial")
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

ndola_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
ndola_logit$CITY <- "Ndola"

ZAMBIA_logit <- rbind(ndola_logit,sep = ".") 
write.csv(ZAMBIA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/ZAMBIA_logit.csv"))

