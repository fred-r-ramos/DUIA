library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/mali")
sf::sf_use_s2(FALSE)
geo2_ml87 <- read_sf("geo2_ml1987.shp")
geo2_ml98 <- read_sf("geo2_ml1998.shp")
geo2_ml09 <- read_sf("geo2_ml2009.shp")
centroid_ml87 <- st_centroid(geo2_ml87)
centroid_ml98 <- st_centroid(geo2_ml98)
centroid_ml09 <- st_centroid(geo2_ml09)
AUE_bamako <- read_sf("bamako_studyArea.shp")
AUE_bamako <- st_transform(AUE_bamako, 4326)

sf::sf_use_s2(FALSE)

bamako_87 <- geo2_ml87[st_intersection(AUE_bamako,centroid_ml87),]
bamako_87['CITY']='bamako'
bamako_98 <- geo2_ml98[st_intersection(AUE_bamako,centroid_ml98),]
bamako_98['CITY']='bamako'
bamako_09 <- geo2_ml09[st_intersection(AUE_bamako,centroid_ml09),]
bamako_09['CITY']='bamako'

plot(st_geometry(bamako_09), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ml09[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00222.xml")
ddi <-read_ipums_ddi("ipumsi_00255.xml")
mali <- read_ipums_micro(ddi)

names(mali)

######################creating binary variables for logit regressions

names(mali)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(mali$OWNERSHIP)
mali$owner_b <- ifelse(mali$OWNERSHIP ==1,1,0)
table(mali$ELECTRIC)
mali$eletric_b <- ifelse(mali$ELECTRIC ==1,1,0)
table(mali$WATSUP)
mali$water_b <- ifelse(mali$WATSUP ==10,1,0)
table(mali$SEWAGE)
mali$sewage_b <- ifelse(mali$SEWAGE ==11|mali$SEWAGE ==12,1,0)
table(mali$TRASH)
mali$trash_b <- ifelse(mali$TRASH ==10|mali$TRASH ==12,1,0)
table(mali$TOILET)
mali$toilet_b <- ifelse(mali$TOILET==21|mali$TOILET==22,1,0)
table(mali$FLOOR)
mali$floor_b <- ifelse(mali$FLOOR ==201|mali$FLOOR ==202|mali$FLOOR ==222,1,0)
table(mali$ROOF)
mali$roof_b <- ifelse(mali$ROOF==11|mali$ROOF==14|mali$ROOF==29,1,0)

names(mali)
gc()

#checking if the variables are available in both years
table(mali$owner_b,mali$YEAR)
table(mali$eletric_b,mali$YEAR)
table(mali$water_b,mali$YEAR)
table(mali$sewage_b,mali$YEAR)
table(mali$trash_b,mali$YEAR)
table(mali$toilet_b,mali$YEAR)
table(mali$floor_b,mali$YEAR)
table(mali$roof_b,mali$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
mali$PUBl_ASSET <-ifelse(is.na(mali$eletric_b), 0, mali$eletric_b) +
  ifelse(is.na(mali$water_b), 0, mali$water_b) +
  ifelse(is.na(mali$sewage_b), 0, mali$sewage_b) + 
  ifelse(is.na(mali$trash_b), 0, mali$trash_b)
mali %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mali$PRIV_ASSET <- ifelse(is.na(mali$owner_b), 0, mali$owner_b) +
  ifelse(is.na(mali$toilet_b), 0, mali$toilet_b) +
  ifelse(is.na(mali$floor_b), 0, mali$floor_b) + 
  ifelse(is.na(mali$roof_b), 0, mali$roof_b)
mali %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mali$TOTAL_ASSET <- ifelse(is.na(mali$owner_b), 0, mali$owner_b) +
  ifelse(is.na(mali$toilet_b), 0, mali$toilet_b) +
  ifelse(is.na(mali$floor_b), 0, mali$floor_b) + 
  ifelse(is.na(mali$roof_b), 0, mali$roof_b)+
  ifelse(is.na(mali$eletric_b), 0, mali$eletric_b) +
  ifelse(is.na(mali$water_b), 0, mali$water_b) +
  ifelse(is.na(mali$sewage_b), 0, mali$sewage_b) + 
  ifelse(is.na(mali$trash_b), 0, mali$trash_b)
assets<-mali %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))


mali$TOTAL_ASSET_gini <- ifelse(is.na(mali$owner_b), 0, mali$owner_b) +
  ifelse(is.na(mali$toilet_b), 0, mali$toilet_b) +
  ifelse(is.na(mali$floor_b), 0, mali$floor_b) + 
  ifelse(is.na(mali$roof_b), 0, mali$roof_b)+
  ifelse(is.na(mali$eletric_b), 0, mali$eletric_b) 

assets<-mali %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

mali$IPUM1987 <- as.integer(mali$GEO2_ML1987)
mali$IPUM1998 <- as.integer(mali$GEO2_ML1998)
mali$IPUM2009 <- as.integer(mali$GEO2_ML2009)
bamako_87$IPUM1987 <- as.integer(bamako_87$IPUM1987)
bamako_98$IPUM1998 <- as.integer(bamako_98$IPUM1998)
bamako_09$IPUM2009 <- as.integer(bamako_09$IPUM2009)

##Joining by year

bamako_87 <- mali %>% inner_join(bamako_87, by="IPUM1987")
bamako_98 <- mali %>% inner_join(bamako_98, by="IPUM1998")
bamako_09 <- mali %>% inner_join(bamako_09, by="IPUM2009")

names(bamako_87)
names(bamako_98)
names(bamako_09)

bamako_87 <- select(bamako_87, -c(CIRC1987))
bamako_98 <- select(bamako_98, -c(CIRC1998))
bamako_09 <- select(bamako_09, -c(CIRC2009))

##Merging all years into one table
bamako_full <- rbind(bamako_87,bamako_98,bamako_09)
names(bamako_full)
table(bamako_full$YEAR)

bamako_full <- bamako_full %>%  filter (bamako_full$YRSCHOOL < 90)
# 
bamako_full <- bamako_full %>%  filter (bamako_full$AGE >15)
# 
bamako_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(bamako_full$YRSCHOOL)
summary(bamako_full$YRSCHOOL)

bamako_fu87 <- bamako_full %>%  filter (YEAR==1987)
bamako_fu98 <- bamako_full %>%  filter (YEAR==1998)
bamako_fu09 <- bamako_full %>%  filter (YEAR==2009)
Gini(bamako_fu98 $YRSCHOOL,na.rm = TRUE)
Gini(bamako_fu09 $YRSCHOOL,na.rm = TRUE)
Gini(bamako_fu87 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(bamako_fu98 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(bamako_fu09 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_bamk98 <- Lc(bamako_fu98$YRSCHOOL, n = rep(1,length(bamako_fu98$YRSCHOOL)), plot = TRUE)
Lc_baml09 <- Lc(bamako_fu09$YRSCHOOL, n = rep(1,length(bamako_fu09$YRSCHOOL)), plot = TRUE)

plot(Lc_bamk98,col='blue', main = "Lorenz Curve - Bamako")
lines(Lc_baml09, col='red')

##Merging all years into one table
mali_full <- mali
table(mali_full$YEAR)

mali_full <- mali_full %>%  filter (mali_full$YRSCHOOL < 90)

mali_full <- mali_full %>%  filter (mali_full$AGE >15)

mali_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(mali_full$YRSCHOOL)
summary(mali_full$YRSCHOOL)


mali_fu98 <- mali_full %>%  filter (YEAR==1998)
mali_fu09 <- mali_full %>%  filter (YEAR==2009)
Gini(mali_fu98 $YRSCHOOL,na.rm = TRUE)
Gini(mali_fu09 $YRSCHOOL,na.rm = TRUE)


## compute the Lorenz curves
Lc_mali98 <- Lc(mali_fu98$YRSCHOOL, n = rep(1,length(mali_fu98$YRSCHOOL)), plot = TRUE)
Lc_mali09 <- Lc(mali_fu09$YRSCHOOL, n = rep(1,length(mali_fu09$YRSCHOOL)), plot = TRUE)

plot(Lc_mali98,col='blue', main = "Lorenz Curve - Mali")
lines(Lc_mali09, col='red')

table(bamako_full$OCCISCO)
table(bamako_full$YEAR)

bamako_full$OCCISCO_b <- ifelse(bamako_full$OCCISCO ==1|bamako_full$OCCISCO ==2,1,0)
bamako_full$OCCISCO_b <- ifelse(bamako_full$OCCISCO ==3|bamako_full$OCCISCO ==4|bamako_full$OCCISCO ==5,2,bamako_full$OCCISCO_b)
bamako_full$OCCISCO_b <- ifelse(bamako_full$OCCISCO ==6|bamako_full$OCCISCO ==7|bamako_full$OCCISCO ==8|bamako_full$OCCISCO ==9,3,bamako_full$OCCISCO_b)
table(bamako_full$OCCISCO_b)

bamako_full_OCCISCO_b <- bamako_full %>% select(YEAR,OCCISCO_b,PERWT)
bamako_full_OCCISCO_b <- bamako_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(bamako_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- bamako_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- bamako_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- bamako_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- bamako_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

bamako_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
bamako_OCCISCO_b<- bamako_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
bamako_OCCISCO_b<- bamako_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

bamako_OCCISCO_b

# for PUBl_ASSET
bamako_PUBl_ASSET<-bamako_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bamako_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bamako_PUBl_ASSET <- bamako_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bamako_PUBl_ASSET <- bamako_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bamako_PUBl_ASSET$CITY<-"bamako"
# for PRIV_ASSET
bamako_PRIV_ASSET<-bamako_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bamako_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bamako_PRIV_ASSET <- bamako_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bamako_PRIV_ASSET <- bamako_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bamako_PRIV_ASSET$CITY<-"bamako"
# for TOTAL_ASSET
bamako_TOTAL_ASSET<-bamako_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bamako_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bamako_TOTAL_ASSET <- bamako_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bamako_TOTAL_ASSET <- bamako_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bamako_TOTAL_ASSET$CITY<-"bamako"
write.csv(bamako_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bamako_PUBl_ASSET.csv", row.names = TRUE)
write.csv(bamako_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bamako_PRIV_ASSET.csv", row.names = TRUE)
write.csv(bamako_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bamako_TOTAL_ASSET.csv", row.names = TRUE)

######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

bamako_WATER <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(bamako_WATER$WATSUP)
table(bamako_WATER$HHWT)
bamako_WATER <- bamako_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(bamako_WATER$WATSUP)
table(bamako_WATER$water_b)

bamako_WATER$water_b <- factor(bamako_WATER$water_b)

bamako_WATER$YEAR <- factor(bamako_WATER$YEAR)
bamako_WATER_12 <- bamako_WATER %>% filter(bamako_WATER$YEAR==2009)

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = bamako_WATER_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

#####saving z-value and beta

WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


gc()

#################################### FOR OWNERSHIP ##################################

bamako_OWN <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(bamako_OWN$OWNERSHIP)
bamako_OWN <- bamako_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(bamako_OWN$OWNERSHIP)
table(bamako_OWN$owner_b)

bamako_OWN$owner_b <- factor(bamako_OWN$owner_b)

bamako_OWN$YEAR <- factor(bamako_OWN$YEAR)
bamako_OWN_01 <- bamako_OWN %>% filter(bamako_OWN$YEAR==1998)
bamako_OWN_12 <- bamako_OWN %>% filter(bamako_OWN$YEAR==2009)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = bamako_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = bamako_OWN_12, family = "binomial")
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

bamako_SEWG <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(bamako_SEWG$SEWAGE)
bamako_SEWG <- bamako_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(bamako_SEWG$SEWAGE)
table(bamako_SEWG$sewage_b)

bamako_SEWG$sewage_b <- factor(bamako_SEWG$sewage_b)

bamako_SEWG$YEAR <- factor(bamako_SEWG$YEAR)
bamako_SEWG_01 <- bamako_SEWG %>% filter(bamako_SEWG$YEAR==1998)
bamako_SEWG_12 <- bamako_SEWG %>% filter(bamako_SEWG$YEAR==2009)

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = bamako_SEWG_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}


#################################### FOR TOILET ##################################

bamako_TOILET <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(bamako_TOILET$TOILET)
bamako_TOILET <- bamako_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(bamako_TOILET$TOILET)
table(bamako_TOILET$toilet_b)

bamako_TOILET$toilet_b <- factor(bamako_TOILET$toilet_b)

bamako_TOILET$YEAR <- factor(bamako_TOILET$YEAR)
bamako_TOILET_01 <- bamako_TOILET %>% filter(bamako_TOILET$YEAR==1998)
bamako_TOILET_12 <- bamako_TOILET %>% filter(bamako_TOILET$YEAR==2009)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = bamako_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = bamako_TOILET_12, family = "binomial")
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

bamako_ELECTRIC <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(bamako_ELECTRIC$ELECTRIC)
bamako_ELECTRIC <- bamako_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(bamako_ELECTRIC$ELECTRIC)
table(bamako_ELECTRIC$eletric_b)

bamako_ELECTRIC$eletric_b <- factor(bamako_ELECTRIC$eletric_b)

bamako_ELECTRIC$YEAR <- factor(bamako_ELECTRIC$YEAR)
bamako_ELECTRIC_01 <- bamako_ELECTRIC %>% filter(bamako_ELECTRIC$YEAR==1998)
bamako_ELECTRIC_12 <- bamako_ELECTRIC %>% filter(bamako_ELECTRIC$YEAR==2009)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = bamako_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = bamako_ELECTRIC_12, family = "binomial")
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

bamako_TRASH <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(bamako_TRASH$TRASH)
bamako_TRASH <- bamako_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(bamako_TRASH$TRASH)
table(bamako_TRASH$trash_b)

bamako_TRASH$trash_b <- factor(bamako_TRASH$trash_b)

bamako_TRASH$YEAR <- factor(bamako_TRASH$YEAR)
bamako_TRASH_12 <- bamako_TRASH %>% filter(bamako_TRASH$YEAR==2009)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = bamako_TRASH_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

bamako_FLOOR <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(bamako_FLOOR$FLOOR)
bamako_FLOOR <- bamako_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(bamako_FLOOR$FLOOR)
table(bamako_FLOOR$floor_b)

bamako_FLOOR$floor_b <- factor(bamako_FLOOR$floor_b)

bamako_FLOOR$YEAR <- factor(bamako_FLOOR$YEAR)
bamako_FLOOR_01 <- bamako_FLOOR %>% filter(bamako_FLOOR$YEAR==1998)
bamako_FLOOR_12 <- bamako_FLOOR %>% filter(bamako_FLOOR$YEAR==2009)

mylogit <- glm(floor_b ~ YRSCHOOL + YEAR, data = bamako_FLOOR, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = bamako_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = bamako_FLOOR_12, family = "binomial")
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

bamako_ROOF <- bamako_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(bamako_ROOF$ROOF)
bamako_ROOF <- bamako_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(bamako_ROOF$ROOF)
table(bamako_ROOF$roof_b)

bamako_ROOF$roof_b <- factor(bamako_ROOF$roof_b)

bamako_ROOF$YEAR <- factor(bamako_ROOF$YEAR)
bamako_ROOF_01 <- bamako_ROOF %>% filter(bamako_ROOF$YEAR==1998)
bamako_ROOF_12 <- bamako_ROOF %>% filter(bamako_ROOF$YEAR==2009)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = bamako_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = bamako_ROOF_12, family = "binomial")
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

bamako_logit <- cbind(WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z) 
bamako_logit$CITY <- "Bamako"


#################################################Saving an unique table results join

MALI_logit <- rbind(bamako_logit,sep = ".") 
write.csv(MALI_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/MALI_logit.csv"))
