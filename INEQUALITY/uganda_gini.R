library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/uganda")

geo2_ug91 <- read_sf("geo2_ug1991.shp")
geo2_ug02 <- read_sf("geo2_ug2002.shp")
geo2_ug14 <- read_sf("geo2_ug2014.shp")
centroid_ug91 <- st_centroid(geo2_ug91)
centroid_ug02 <- st_centroid(geo2_ug02)
centroid_ug14 <- st_centroid(geo2_ug14)

AUE_kampala <- read_sf("Kampala_studyArea.shp")
AUE_kampala  <- st_transform(AUE_kampala , 4326)

kampala_91 <- geo2_ug91[st_intersection(AUE_kampala,centroid_ug91),]
kampala_91['CITY']='kampala'
kampala_02 <- geo2_ug02[st_intersection(AUE_kampala,centroid_ug02),]
kampala_02['CITY']='kampala'
kampala_14 <- geo2_ug14[st_intersection(AUE_kampala,centroid_ug14),]
kampala_14['CITY']='kampala'

plot(st_geometry(kampala_14), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ug14[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00269.xml")
uganda <- read_ipums_micro(ddi)

names(uganda)

names(uganda)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(uganda$OWNERSHIP)
uganda$owner_b <- ifelse(uganda$OWNERSHIP ==1,1,0)
table(uganda$ELECTRIC)
uganda$eletric_b <- ifelse(uganda$ELECTRIC ==1,1,0)
table(uganda$WATSUP)
uganda$water_b <- ifelse(uganda$WATSUP ==11|uganda$WATSUP ==10,1,0)
table(uganda$TRASH)
uganda$trash_b <- ifelse(uganda$TRASH ==10|uganda$TRASH ==12,1,0)
table(uganda$AUTOS)
uganda$autos_b <- ifelse(uganda$AUTOS >0,1,0)
table(uganda$TV)
uganda$tv_b <- ifelse(uganda$TV >10,1,0)
table(uganda$TOILET)
uganda$toilet_b <- ifelse(uganda$TOILET==21|uganda$TOILET==22,1,0)
table(uganda$FLOOR)
uganda$floor_b <- ifelse(uganda$FLOOR ==203|uganda$FLOOR ==204|uganda$FLOOR ==207|uganda$FLOOR ==208,1,0)
table(uganda$ROOF)
uganda$roof_b <- ifelse(uganda$ROOF==14|uganda$ROOF==12|uganda$ROOF==11|uganda$ROOF==28,1,0)

uganda$OCCISCO_b <- ifelse(uganda$OCCISCO ==1|uganda$OCCISCO ==2,1,0)
uganda$OCCISCO_b <- ifelse(uganda$OCCISCO ==3|uganda$OCCISCO ==4|uganda$OCCISCO ==5,2,uganda$OCCISCO_b)
uganda$OCCISCO_b <- ifelse(uganda$OCCISCO ==6|uganda$OCCISCO ==7|uganda$OCCISCO ==8|uganda$OCCISCO ==9,3,uganda$OCCISCO_b)
uganda$OCCISCO_low <- ifelse(uganda$OCCISCO_b==3,1,0)

table(uganda$OCCISCO_b)
table(uganda$OCCISCO_low)
table(uganda$HHWT)
names(uganda)
gc()

#checking if the variables are available in both years
table(uganda$owner_b,uganda$YEAR)
table(uganda$eletric_b,uganda$YEAR)
table(uganda$water_b,uganda$YEAR)
table(uganda$trash_b,uganda$YEAR)
table(uganda$autos_b,uganda$YEAR)######
table(uganda$tv_b,uganda$YEAR)
table(uganda$toilet_b,uganda$YEAR)
table(uganda$floor_b,uganda$YEAR)
table(uganda$roof_b,uganda$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
uganda$PUBl_ASSET <- ifelse(is.na(uganda$eletric_b), 0, uganda$eletric_b) +
  ifelse(is.na(uganda$water_b), 0, uganda$water_b) +
  ifelse(is.na(uganda$trash_b), 0, uganda$trash_b)
uganda %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

uganda$PRIV_ASSET <- ifelse(is.na(uganda$owner_b), 0, uganda$owner_b) +
  ifelse(is.na(uganda$autos_b), 0, uganda$autos_b) +
  ifelse(is.na(uganda$tv_b), 0, uganda$tv_b) +
  ifelse(is.na(uganda$toilet_b), 0, uganda$toilet_b) +
  ifelse(is.na(uganda$floor_b), 0, uganda$floor_b) +
  ifelse(is.na(uganda$roof_b), 0, uganda$roof_b)
uganda %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

uganda$TOTAL_ASSET <- ifelse(is.na(uganda$eletric_b), 0, uganda$eletric_b) +
  ifelse(is.na(uganda$water_b), 0, uganda$water_b) +
  ifelse(is.na(uganda$trash_b), 0, uganda$trash_b) +
  ifelse(is.na(uganda$owner_b), 0, uganda$owner_b) +
  ifelse(is.na(uganda$autos_b), 0, uganda$autos_b) +
  ifelse(is.na(uganda$tv_b), 0, uganda$tv_b) +
  ifelse(is.na(uganda$toilet_b), 0, uganda$toilet_b) +
  ifelse(is.na(uganda$floor_b), 0, uganda$floor_b) +
  ifelse(is.na(uganda$roof_b), 0, uganda$roof_b)
assets<-uganda %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

uganda$TOTAL_ASSET_gini <- ifelse(is.na(uganda$eletric_b), 0, uganda$eletric_b) +
  ifelse(is.na(uganda$water_b), 0, uganda$water_b) +
  ifelse(is.na(uganda$owner_b), 0, uganda$owner_b) +
  ifelse(is.na(uganda$toilet_b), 0, uganda$toilet_b) +
  ifelse(is.na(uganda$floor_b), 0, uganda$floor_b) +
  ifelse(is.na(uganda$roof_b), 0, uganda$roof_b)
assets<-uganda %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

uganda$IPUM1991 <- as.integer(uganda$GEO2_UG1991)
uganda$IPUM2002 <- as.integer(uganda$GEO2_UG2002)
uganda$IPUM2014 <- as.integer(uganda$GEO2_UG2014)
kampala_91$IPUM1991 <- as.integer(kampala_91$IPUM1991)
kampala_02$IPUM2002 <- as.integer(kampala_02$IPUM2002)
kampala_14$IPUM2014 <- as.integer(kampala_14$IPUM2014)
##Joining by year

kampala_91 <- uganda %>% inner_join(kampala_91, by="IPUM1991")
kampala_02 <- uganda %>% inner_join(kampala_02, by="IPUM2002")
kampala_14 <- uganda %>% inner_join(kampala_14, by="IPUM2014")

names(kampala_91)
names(kampala_02)
names(kampala_14)

kampala_91 <- select(kampala_91, -c(CNTY1991))
kampala_02 <- select(kampala_02, -c(CNTY2002))
kampala_14 <- select(kampala_14, -c(COUN2014))

##Merging all years into one table
uganda_full <- rbind(kampala_91,kampala_02,kampala_14)
names(uganda_full)

##Excluding specific columns for the unifeied dataset
uganda_full<- select(uganda_full, -c(GEO2_UG1991,GEO2_UG2002,IPUM1991,IPUM2002,geometry))
table(uganda_full$CITY)
kampala_full <- uganda_full %>% filter(CITY=="kampala")

#save(uganda_full,file="C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/0_integration/uganda_full.rda")


table(uganda_full$CITY)

table(uganda_full$PERWT)

names(uganda_full)

table(uganda_full$YEAR)

uganda_full <- uganda_full %>%  filter (uganda_full$YRSCHOOL < 90)

uganda_full <- uganda_full %>%  filter (uganda_full$AGE >15)

uganda_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(uganda_full$YRSCHOOL)
summary(uganda_full$YRSCHOOL)
table(uganda_full$CITY)

####kampala
kampala_full <- uganda_full %>%  filter (CITY=="kampala")

summary(kampala_full$YRSCHOOL)
summary(kampala_full$AGE)

kampala_fu91 <- kampala_full %>%  filter (YEAR==1991)
kampala_fu02 <- kampala_full %>%  filter (YEAR==2002)
kampala_fu14 <- kampala_full %>%  filter (YEAR==2014)

Gini(kampala_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(kampala_fu02 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(kampala_fu14 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_coch92 <- Lc(kampala_fu91$YRSCHOOL, n = rep(1,length(kampala_fu91$YRSCHOOL)), plot = TRUE)
Lc_coch01 <- Lc(kampala_fu02$YRSCHOOL, n = rep(1,length(kampala_fu02$YRSCHOOL)), plot = TRUE)


plot(Lc_coch92,col='blue', main = "Lorenz Curve - kampala")
lines(Lc_coch01, col='red')

table(uganda_full$OCCISCO_b)
table(uganda_full$OCCISCO_low)

uganda_full_OCCISCO_b <- uganda_full %>% select(YEAR,OCCISCO_b,PERWT,OCCISCO_low)
uganda_full_OCCISCO_b <- uganda_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(uganda_full_OCCISCO_b$OCCISCO_b)

kampala_full <- uganda_full %>%  filter (CITY=="kampala")

OCCISCO_b_total <- uganda_full_OCCISCO_b %>%
  group_by(YEAR) %>%
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- uganda_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>%
  group_by(YEAR) %>%
  summarise(OCCISCO_b_TOP = sum(PERWT))
OCCISCO_b_MIDDLE <- uganda_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>%
  group_by(YEAR) %>%   summarise(OCCISCO_b_MIDDLE = sum(PERWT))
OCCISCO_b_BOTTOM <- uganda_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>%
  group_by(YEAR) %>%
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

kampala_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
kampala_OCCISCO_b<- kampala_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
kampala_OCCISCO_b<- kampala_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

kampala_OCCISCO_b

names(kampala_full)

# for PUBl_ASSET
kampala_PUBl_ASSET<-kampala_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kampala_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kampala_PUBl_ASSET <- kampala_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kampala_PUBl_ASSET <- kampala_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kampala_PUBl_ASSET$CITY<-"kampala"
# for PRIV_ASSET
kampala_PRIV_ASSET<-kampala_full %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kampala_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kampala_PRIV_ASSET <- kampala_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kampala_PRIV_ASSET <- kampala_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kampala_PRIV_ASSET$CITY<-"kampala"
# for TOTAL_ASSET
kampala_TOTAL_ASSET<-kampala_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kampala_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kampala_TOTAL_ASSET <- kampala_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kampala_TOTAL_ASSET <- kampala_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kampala_TOTAL_ASSET$CITY<-"kampala"
write.csv(kampala_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kampala_PUBl_ASSET.csv", row.names = TRUE)
write.csv(kampala_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kampala_PRIV_ASSET.csv", row.names = TRUE)
write.csv(kampala_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kampala_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

kampala_WATER <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)

table(kampala_WATER$HHWT)
table(kampala_WATER$WATSUP)
table(kampala_WATER$OCCISCO_low)
kampala_WATER <- kampala_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99) %>% filter(OCCISCO_b!=0)


table(kampala_WATER$WATSUP)
table(kampala_WATER$water_b)

kampala_WATER$water_b <- factor(kampala_WATER$water_b)

kampala_WATER$YEAR <- factor(kampala_WATER$YEAR)

kampala_WATER_01 <- kampala_WATER %>% filter(kampala_WATER$YEAR==1991)
kampala_WATER_12 <- kampala_WATER %>% filter(kampala_WATER$YEAR==2002)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = kampala_WATER_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = kampala_WATER_12, family = "binomial",weights=HHWT)
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

kampala_OWN <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,OCCISCO_b,OCCISCO_low,owner_b,YRSCHOOL,CITY)
table(kampala_OWN$OWNERSHIP)
kampala_OWN <- kampala_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)%>% filter(OCCISCO_b!=0)

table(kampala_OWN$OWNERSHIP)
table(kampala_OWN$owner_b)

kampala_OWN$owner_b <- factor(kampala_OWN$owner_b)

kampala_OWN$YEAR <- factor(kampala_OWN$YEAR)
kampala_OWN_01 <- kampala_OWN %>% filter(kampala_OWN$YEAR==1991)
kampala_OWN_12 <- kampala_OWN %>% filter(kampala_OWN$YEAR==2002)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = kampala_OWN_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = kampala_OWN_12, family = "binomial",weights=HHWT)
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

#################################### FOR TOILET ##################################

kampala_TOILET <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,OCCISCO_b,OCCISCO_low,YRSCHOOL,CITY)
table(kampala_TOILET$TOILET)
kampala_TOILET <- kampala_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)%>% filter(OCCISCO_b!=0)

table(kampala_TOILET$TOILET)
table(kampala_TOILET$toilet_b)

kampala_TOILET$toilet_b <- factor(kampala_TOILET$toilet_b)

kampala_TOILET$YEAR <- factor(kampala_TOILET$YEAR)
kampala_TOILET_01 <- kampala_TOILET %>% filter(kampala_TOILET$YEAR==1991)
kampala_TOILET_12 <- kampala_TOILET %>% filter(kampala_TOILET$YEAR==2002)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = kampala_TOILET_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = kampala_TOILET_12, family = "binomial",weights=HHWT)
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

kampala_ELECTRIC <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,OCCISCO_b,OCCISCO_low,YRSCHOOL,CITY)
table(kampala_ELECTRIC$ELECTRIC)
kampala_ELECTRIC <- kampala_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)%>% filter(OCCISCO_b!=0)

table(kampala_ELECTRIC$ELECTRIC)
table(kampala_ELECTRIC$eletric_b)

kampala_ELECTRIC$eletric_b <- factor(kampala_ELECTRIC$eletric_b)

kampala_ELECTRIC$YEAR <- factor(kampala_ELECTRIC$YEAR)
kampala_ELECTRIC_01 <- kampala_ELECTRIC %>% filter(kampala_ELECTRIC$YEAR==1991)
kampala_ELECTRIC_12 <- kampala_ELECTRIC %>% filter(kampala_ELECTRIC$YEAR==2002)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = kampala_ELECTRIC_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = kampala_ELECTRIC_12, family = "binomial",weights=HHWT)
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

kampala_TRASH <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(kampala_TRASH$TRASH)
kampala_TRASH <- kampala_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)%>% filter(OCCISCO_b!=0)

table(kampala_TRASH$TRASH)
table(kampala_TRASH$trash_b)

kampala_TRASH$trash_b <- factor(kampala_TRASH$trash_b)

kampala_TRASH$YEAR <- factor(kampala_TRASH$YEAR)
kampala_TRASH_12 <- kampala_TRASH %>% filter(kampala_TRASH$YEAR==2002)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = kampala_TRASH_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}


# #################################### FOR AUTOS ##################################
# 
# kampala_AUTOS <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
# table(kampala_AUTOS$AUTOS)
# kampala_AUTOS <- kampala_AUTOS %>% filter(AUTOS!=99)%>% filter(OCCISCO_b!=0)
# 
# table(kampala_full$AUTOS)
# table(kampala_full$autos_b)
# 
# kampala_AUTOS$autos_b <- factor(kampala_AUTOS$autos_b)
# 
# kampala_AUTOS$YEAR <- factor(kampala_AUTOS$YEAR)
# kampala_AUTOS_01 <- kampala_AUTOS %>% filter(kampala_AUTOS$YEAR==1991)
# kampala_AUTOS_12 <- kampala_AUTOS %>% filter(kampala_AUTOS$YEAR==2002)
# 
# mylogit_01 <- glm(autos_b ~ OCCISCO_low, data = kampala_AUTOS_01, family = "binomial",weights=HHWT)
# summary(mylogit_01)
# exp(coef(mylogit_01))
# 
# mylogit_12 <- glm(autos_b ~ OCCISCO_low, data = kampala_AUTOS_12, family = "binomial",weights=HHWT)
# summary(mylogit_12)
# exp(coef(mylogit_12))
# 
# AUTOS_T2 <- summary(mylogit_01)$coefficients
# AUTOS_T2_z <- AUTOS_T2[2,1:4]
# AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
# AUTOS_T3 <- summary(mylogit_12)$coefficients
# AUTOS_T3_z <- AUTOS_T3[2,1:4]
# AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)

#################################### FOR FLOORS ##################################

kampala_FLOOR <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(kampala_FLOOR$FLOOR)
kampala_FLOOR <- kampala_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)%>% filter(OCCISCO_b!=0)

table(kampala_FLOOR$FLOOR)
table(kampala_FLOOR$floor_b)

kampala_FLOOR$floor_b <- factor(kampala_FLOOR$floor_b)

kampala_FLOOR$YEAR <- factor(kampala_FLOOR$YEAR)
kampala_FLOOR_01 <- kampala_FLOOR %>% filter(kampala_FLOOR$YEAR==1991)
kampala_FLOOR_12 <- kampala_FLOOR %>% filter(kampala_FLOOR$YEAR==2002)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = kampala_FLOOR_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = kampala_FLOOR_12, family = "binomial",weights=HHWT)
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

kampala_ROOF <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(kampala_ROOF$ROOF)
kampala_ROOF <- kampala_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)%>% filter(OCCISCO_b!=0)

table(kampala_ROOF$ROOF)
table(kampala_ROOF$roof_b)

kampala_ROOF$roof_b <- factor(kampala_ROOF$roof_b)

kampala_ROOF$YEAR <- factor(kampala_ROOF$YEAR)
kampala_ROOF_01 <- kampala_ROOF %>% filter(kampala_ROOF$YEAR==1991)
kampala_ROOF_12 <- kampala_ROOF %>% filter(kampala_ROOF$YEAR==2002)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = kampala_ROOF_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = kampala_ROOF_12, family = "binomial",weights=HHWT)
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

kampala_TV <- kampala_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(kampala_TV$TV)
kampala_TV <- kampala_TV %>% filter(TV!=0) %>% filter(TV!=99)%>% filter(OCCISCO_b!=0)

table(kampala_TV$TV)
table(kampala_TV$tv_b)

kampala_TV$tv_b <- factor(kampala_TV$tv_b)

kampala_TV$YEAR <- factor(kampala_TV$YEAR)
kampala_TV_01 <- kampala_TV %>% filter(kampala_TV$YEAR==1991)
kampala_TV_12 <- kampala_TV %>% filter(kampala_TV$YEAR==2002)

# mylogit_01 <- glm(tv_b ~ OCCISCO_low, data = kampala_TV_01, family = "binomial",weights=HHWT)
# summary(mylogit_01)
# exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = kampala_TV_12, family = "binomial",weights=HHWT)
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


kampala__OCC_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z,FLOOR_T2_z,FLOOR_T3_z) 
kampala__OCC_logit$CITY <- "Kampala"
kampala__OCC_logit <- rbind(kampala__OCC_logit,sep = ".") 
write.csv(kampala__OCC_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/kampala_logit.csv"))
