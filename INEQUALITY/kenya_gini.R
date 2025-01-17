library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/kenya")


geo2_ke89 <- read_sf("geo2_ke1989.shp")
geo2_ke99 <- read_sf("geo2_ke1999.shp")
geo2_ke09 <- read_sf("geo2_ke2009.shp")
centroid_ke89 <- st_centroid(geo2_ke89)
centroid_ke99 <- st_centroid(geo2_ke99)
centroid_ke09 <- st_point_on_surface(geo2_ke09)
AUE_nakuru <- read_sf("Nakuru_studyArea.shp")
AUE_nakuru <- st_transform(AUE_nakuru, 4326)

sf::sf_use_s2(FALSE)

nakuru_1989 <- geo2_ke89[st_nearest_feature(AUE_nakuru,centroid_ke89),]
nakuru_1989['CITY']='nakuru'
nakuru_1999 <- geo2_ke99[st_nearest_feature(AUE_nakuru,centroid_ke99),]
nakuru_1999['CITY']='nakuru'
nakuru_2009 <- geo2_ke09[st_intersection(nakuru_1999,centroid_ke09),]
nakuru_2009['CITY']='nakuru'

plot(st_geometry(nakuru_2009), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ke09[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00235.xml")
ddi <-read_ipums_ddi("ipumsi_00254.xml")
kenya <- read_ipums_micro(ddi)

names(kenya)

######################creating binary variables for logit regressions

names(kenya)

# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "GEO2_KE1989" "GEO2_KE1999"
# [8] "GEO2_KE2009" "OWNERSHIP"   "OWNERSHIPD"  "ELECTRIC"    "WATSUP"      "SEWAGE"      "AUTOS"      
# [15] "REFRIG"      "TV"          "TOILET"      "FLOOR"       "ROOF"        "PERNUM"      "PERWT"      
# [22] "AGE"         "SEX"         "SCHOOL"      "LIT"         "EDATTAIN"    "EDATTAIND"   "YRSCHOOL"   
# [29] "EMPSTAT"     "EMPSTATD"    "LABFORCE"    "OCCISCO"     "OCC"       


table(kenya$OWNERSHIP)
kenya$owner_b <- ifelse(kenya$OWNERSHIP ==1,1,0)
table(kenya$ELECTRIC)
kenya$eletric_b <- ifelse(kenya$ELECTRIC ==1,1,0)
table(kenya$WATSUP)
kenya$water_b <- ifelse(kenya$WATSUP ==11|kenya$WATSUP ==10,1,0)
table(kenya$SEWAGE)
kenya$sewage_b <- ifelse(kenya$SEWAGE ==11|kenya$SEWAGE ==12,1,0)
table(kenya$AUTOS)
kenya$autos_b <- ifelse(kenya$AUTOS ==7,1,0)
table(kenya$REFRIG)
kenya$refrig_b <- ifelse(kenya$REFRIG ==2,1,0)
table(kenya$TV)
kenya$tv_b <- ifelse(kenya$TV ==20,1,0)
table(kenya$TOILET)
kenya$toilet_b <- ifelse(kenya$TOILET==21|kenya$TOILET==22|kenya$TOILET==23,1,0)
table(kenya$FLOOR)
kenya$floor_b <- ifelse(kenya$FLOOR==202|kenya$FLOOR==213|kenya$FLOOR==222|kenya$FLOOR==236,1,0)
table(kenya$ROOF)
kenya$roof_b <- ifelse(kenya$ROOF==14|kenya$ROOF==11|kenya$ROOF==28,1,0)

names(kenya)
gc()

#checking if the variables are available in both years
table(kenya$owner_b,kenya$YEAR)
table(kenya$eletric_b,kenya$YEAR)
table(kenya$water_b,kenya$YEAR)
table(kenya$sewage_b,kenya$YEAR)
table(kenya$autos_b,kenya$YEAR)###
table(kenya$refrig_b,kenya$YEAR)###
table(kenya$tv_b,kenya$YEAR)###
table(kenya$toilet_b,kenya$YEAR)###
table(kenya$floor_b,kenya$YEAR)
table(kenya$roof_b,kenya$YEAR)


##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
kenya$PUBl_ASSET <- ifelse(is.na(kenya$eletric_b), 0,kenya$eletric_b) +
  ifelse(is.na(kenya$water_b), 0,kenya$water_b)+
  ifelse(is.na(kenya$sewage_b), 0,kenya$sewage_b)
kenya %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

kenya$PRIV_ASSET <- ifelse(is.na(kenya$autos_b), 0,kenya$autos_b) +
  ifelse(is.na(kenya$refrig_b), 0,kenya$refrig_b)+
  ifelse(is.na(kenya$tv_b), 0,kenya$tv_b)+
  ifelse(is.na(kenya$autos_b), 0,kenya$autos_b) +
  ifelse(is.na(kenya$toilet_b), 0,kenya$toilet_b)+
  ifelse(is.na(kenya$floor_b), 0,kenya$floor_b)+
  ifelse(is.na(kenya$roof_b), 0,kenya$roof_b)
kenya %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

kenya$TOTAL_ASSET <- ifelse(is.na(kenya$eletric_b), 0,kenya$eletric_b) +
  ifelse(is.na(kenya$water_b), 0,kenya$water_b)+
  ifelse(is.na(kenya$sewage_b), 0,kenya$sewage_b)+
  ifelse(is.na(kenya$autos_b), 0,kenya$autos_b) +
  ifelse(is.na(kenya$refrig_b), 0,kenya$refrig_b)+
  ifelse(is.na(kenya$tv_b), 0,kenya$tv_b)+
  ifelse(is.na(kenya$autos_b), 0,kenya$autos_b) +
  ifelse(is.na(kenya$toilet_b), 0,kenya$toilet_b)+
  ifelse(is.na(kenya$floor_b), 0,kenya$floor_b)+
  ifelse(is.na(kenya$roof_b), 0,kenya$roof_b)
kenya %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

kenya$TOTAL_ASSET_gini <- ifelse(is.na(kenya$eletric_b), 0,kenya$eletric_b) +
  ifelse(is.na(kenya$water_b), 0,kenya$water_b)+
  ifelse(is.na(kenya$owner_b), 0,kenya$owner_b)+
  ifelse(is.na(kenya$sewage_b), 0,kenya$sewage_b)+
  ifelse(is.na(kenya$floor_b), 0,kenya$floor_b)+
  ifelse(is.na(kenya$roof_b), 0,kenya$roof_b)
assets<-kenya %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

kenya$IPUM1989 <- as.integer(kenya$GEO2_KE1989)
kenya$IPUM1999 <- as.integer(kenya$GEO2_KE1999)
kenya$IPUM2009 <- as.integer(kenya$GEO2_KE2009)
nakuru_1989$IPUM1989 <- as.integer(nakuru_1989$IPUM1989)
nakuru_1999$IPUM1999 <- as.integer(nakuru_1999$IPUM1999)
nakuru_2009$IPUM2009 <- as.integer(nakuru_2009$IPUM2009)

##Joining by year

nakuru_1989 <- kenya %>% inner_join(nakuru_1989, by="IPUM1989")
nakuru_1999 <- kenya %>% inner_join(nakuru_1999, by="IPUM1999")
nakuru_2009 <- kenya %>% inner_join(nakuru_2009, by="IPUM2009")

names(nakuru_1989)
names(nakuru_1999)
names(nakuru_2009)

nakuru_1989 <- select(nakuru_1989, -c(DIST1989))
nakuru_1999 <- select(nakuru_1999, -c(DIST1999))
nakuru_2009 <- select(nakuru_2009, -c(DIST2009))

##Merging all years into one table
nakuru_full <- rbind(nakuru_1989,nakuru_1999,nakuru_2009)
names(nakuru_full)

names(nakuru_full)

table(nakuru_full$YEAR)

# nakuru_full <- nakuru_full %>%  filter (nakuru_full$YRSCHOOL < 90)
# 
# nakuru_full <- nakuru_full %>%  filter (nakuru_full$AGE >15)
# 
# nakuru_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(nakuru_full$YRSCHOOL)
summary(nakuru_full$YRSCHOOL)

nakuru_fu89 <- nakuru_full %>%  filter (YEAR==1989)
nakuru_fu99 <- nakuru_full %>%  filter (YEAR==1999)
nakuru_fu09 <- nakuru_full %>%  filter (YEAR==2009)
Gini(nakuru_fu89 $YRSCHOOL,na.rm = TRUE)
Gini(nakuru_fu99 $YRSCHOOL,na.rm = TRUE)
Gini(nakuru_fu09 $YRSCHOOL,na.rm = TRUE)
Gini(nakuru_fu89 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(nakuru_fu99 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(nakuru_fu09 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves

## compute the Lorenz curves
Lc_naku89 <- Lc(nakuru_fu89$YRSCHOOL, n = rep(1,length(nakuru_fu89$YRSCHOOL)), plot = TRUE)
Lc_naku99 <- Lc(nakuru_fu99$YRSCHOOL, n = rep(1,length(nakuru_fu99$YRSCHOOL)), plot = TRUE)
Lc_naku09 <- Lc(nakuru_fu09$YRSCHOOL, n = rep(1,length(nakuru_fu09$YRSCHOOL)), plot = TRUE)

plot(Lc_naku89,col='blue', main = "Lorenz Curve - Nakuru")
lines(Lc_naku99, col='red')
lines(Lc_naku09, col='green')


table(nakuru_full$OCCISCO)


nakuru_full$OCCISCO_b <- ifelse(nakuru_full$OCCISCO ==1|nakuru_full$OCCISCO ==2,1,0)
nakuru_full$OCCISCO_b <- ifelse(nakuru_full$OCCISCO ==3|nakuru_full$OCCISCO ==4|nakuru_full$OCCISCO ==5,2,nakuru_full$OCCISCO_b)
nakuru_full$OCCISCO_b <- ifelse(nakuru_full$OCCISCO ==6|nakuru_full$OCCISCO ==7|nakuru_full$OCCISCO ==8|nakuru_full$OCCISCO ==9,3,nakuru_full$OCCISCO_b)
table(nakuru_full$OCCISCO_b)

nakuru_full_OCCISCO_b <- nakuru_full %>% select(YEAR,OCCISCO_b,PERWT)
nakuru_full_OCCISCO_b <- nakuru_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(nakuru_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- nakuru_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- nakuru_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- nakuru_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- nakuru_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

nakuru_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
nakuru_OCCISCO_b<- nakuru_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
nakuru_OCCISCO_b<- nakuru_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

nakuru_OCCISCO_b

# for PUBl_ASSET
nakuru_PUBl_ASSET<-nakuru_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- nakuru_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
nakuru_PUBl_ASSET <- nakuru_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
nakuru_PUBl_ASSET <- nakuru_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
nakuru_PUBl_ASSET$CITY<-"nakuru"
# for PRIV_ASSET
nakuru_PRIV_ASSET<-nakuru_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- nakuru_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
nakuru_PRIV_ASSET <- nakuru_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
nakuru_PRIV_ASSET <- nakuru_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
nakuru_PRIV_ASSET$CITY<-"nakuru"
# for TOTAL_ASSET
nakuru_TOTAL_ASSET<-nakuru_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- nakuru_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
nakuru_TOTAL_ASSET <- nakuru_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
nakuru_TOTAL_ASSET <- nakuru_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
nakuru_TOTAL_ASSET$CITY<-"nakuru"
write.csv(nakuru_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/nakuru_PUBl_ASSET.csv", row.names = TRUE)
write.csv(nakuru_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/nakuru_PRIV_ASSET.csv", row.names = TRUE)
write.csv(nakuru_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/nakuru_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

nakuru_WATER <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(nakuru_WATER$WATSUP)
table(nakuru_WATER$HHWT)
nakuru_WATER <- nakuru_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(nakuru_WATER$WATSUP)
table(nakuru_WATER$water_b)

nakuru_WATER$water_b <- factor(nakuru_WATER$water_b)

nakuru_WATER$YEAR <- factor(nakuru_WATER$YEAR)
nakuru_WATER_99 <- nakuru_WATER %>% filter(nakuru_WATER$YEAR==1999)
nakuru_WATER_09 <- nakuru_WATER %>% filter(nakuru_WATER$YEAR==2009)

mylogit_99 <- glm(water_b ~ YRSCHOOL, data = nakuru_WATER_99, family = "binomial")
summary(mylogit_99)
exp(coef(mylogit_99))

mylogit_09 <- glm(water_b ~ YRSCHOOL, data = nakuru_WATER_09, family = "binomial")
summary(mylogit_09)
exp(coef(mylogit_09))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_99)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_09)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

nakuru_OWN <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(nakuru_OWN$OWNERSHIP)
nakuru_OWN <- nakuru_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(nakuru_OWN$OWNERSHIP)
table(nakuru_OWN$owner_b)

nakuru_OWN$owner_b <- factor(nakuru_OWN$owner_b)

nakuru_OWN$YEAR <- factor(nakuru_OWN$YEAR)
nakuru_OWN_01 <- nakuru_OWN %>% filter(nakuru_OWN$YEAR==1999)
nakuru_OWN_12 <- nakuru_OWN %>% filter(nakuru_OWN$YEAR==2009)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = nakuru_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = nakuru_OWN_12, family = "binomial")
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

nakuru_SEWG <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(nakuru_SEWG$SEWAGE)
nakuru_SEWG <- nakuru_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(nakuru_SEWG$SEWAGE)
table(nakuru_SEWG$sewage_b)

nakuru_SEWG$sewage_b <- factor(nakuru_SEWG$sewage_b)

nakuru_SEWG$YEAR <- factor(nakuru_SEWG$YEAR)
nakuru_SEWG_01 <- nakuru_SEWG %>% filter(nakuru_SEWG$YEAR==1999)
nakuru_SEWG_12 <- nakuru_SEWG %>% filter(nakuru_SEWG$YEAR==2009)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = nakuru_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = nakuru_SEWG_12, family = "binomial")
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

nakuru_REFRIG <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(nakuru_REFRIG$REFRIG)
nakuru_REFRIG <- nakuru_REFRIG %>% filter(REFRIG!=0)

table(nakuru_REFRIG$REFRIG)
table(nakuru_REFRIG$refrig_b)

nakuru_REFRIG$refrig_b <- factor(nakuru_REFRIG$refrig_b)

nakuru_REFRIG$YEAR <- factor(nakuru_REFRIG$YEAR)
nakuru_REFRIG_01 <- nakuru_REFRIG %>% filter(nakuru_REFRIG$YEAR==1999)
nakuru_REFRIG_12 <- nakuru_REFRIG %>% filter(nakuru_REFRIG$YEAR==2009)

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = nakuru_REFRIG_12, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}


#################################### FOR TOILET ##################################

nakuru_TOILET <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(nakuru_TOILET$TOILET)
nakuru_TOILET <- nakuru_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(nakuru_TOILET$TOILET)
table(nakuru_TOILET$toilet_b)

nakuru_TOILET$toilet_b <- factor(nakuru_TOILET$toilet_b)

nakuru_TOILET$YEAR <- factor(nakuru_TOILET$YEAR)
nakuru_TOILET_01 <- nakuru_TOILET %>% filter(nakuru_TOILET$YEAR==1999)

mylogit <- glm(toilet_b ~ YRSCHOOL + YEAR, data = nakuru_TOILET, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = nakuru_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

nakuru_ELECTRIC <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(nakuru_ELECTRIC$ELECTRIC)
nakuru_ELECTRIC <- nakuru_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(nakuru_ELECTRIC$ELECTRIC)
table(nakuru_ELECTRIC$eletric_b)

nakuru_ELECTRIC$eletric_b <- factor(nakuru_ELECTRIC$eletric_b)

nakuru_ELECTRIC$YEAR <- factor(nakuru_ELECTRIC$YEAR)
nakuru_ELECTRIC_01 <- nakuru_ELECTRIC %>% filter(nakuru_ELECTRIC$YEAR==1999)
nakuru_ELECTRIC_12 <- nakuru_ELECTRIC %>% filter(nakuru_ELECTRIC$YEAR==2009)

mylogit <- glm(eletric_b ~ YRSCHOOL + YEAR, data = nakuru_ELECTRIC, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = nakuru_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = nakuru_ELECTRIC_12, family = "binomial")
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

#################################### FOR AUTOS ##################################

nakuru_AUTOS <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(nakuru_AUTOS$AUTOS)
nakuru_AUTOS <- nakuru_AUTOS %>% filter(AUTOS!=99)

table(nakuru_AUTOS$AUTOS)
table(nakuru_AUTOS$autos_b)

nakuru_AUTOS$autos_b <- factor(nakuru_AUTOS$autos_b)

nakuru_AUTOS$YEAR <- factor(nakuru_AUTOS$YEAR)
nakuru_AUTOS_12 <- nakuru_AUTOS %>% filter(nakuru_AUTOS$YEAR==2009)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = nakuru_AUTOS_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}


#################################### FOR FLOORS ##################################

nakuru_FLOOR <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(nakuru_FLOOR$FLOOR)
nakuru_FLOOR <- nakuru_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(nakuru_FLOOR$FLOOR)
table(nakuru_FLOOR$floor_b)

nakuru_FLOOR$floor_b <- factor(nakuru_FLOOR$floor_b)

nakuru_FLOOR$YEAR <- factor(nakuru_FLOOR$YEAR)
nakuru_FLOOR_01 <- nakuru_FLOOR %>% filter(nakuru_FLOOR$YEAR==1999)
nakuru_FLOOR_12 <- nakuru_FLOOR %>% filter(nakuru_FLOOR$YEAR==2009)


mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = nakuru_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = nakuru_FLOOR_12, family = "binomial")
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

nakuru_ROOF <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(nakuru_ROOF$ROOF)
nakuru_ROOF <- nakuru_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(nakuru_ROOF$ROOF)
table(nakuru_ROOF$roof_b)

nakuru_ROOF$roof_b <- factor(nakuru_ROOF$roof_b)

nakuru_ROOF$YEAR <- factor(nakuru_ROOF$YEAR)
nakuru_ROOF_01 <- nakuru_ROOF %>% filter(nakuru_ROOF$YEAR==1999)
nakuru_ROOF_12 <- nakuru_ROOF %>% filter(nakuru_ROOF$YEAR==2009)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = nakuru_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = nakuru_ROOF_12, family = "binomial")
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

nakuru_TV <- nakuru_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(nakuru_TV$TV)
nakuru_TV <- nakuru_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(nakuru_TV$TV)
table(nakuru_TV$tv_b)

nakuru_TV$tv_b <- factor(nakuru_TV$tv_b)

nakuru_TV$YEAR <- factor(nakuru_TV$YEAR)
nakuru_TV_12 <- nakuru_TV %>% filter(nakuru_TV$YEAR==2009)

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = nakuru_TV_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}


nakuru_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T3_z,TOILET_T2_z,ELECTRIC_T2_z,ELECTRIC_T3_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T3_z) 
nakuru_logit$CITY <- "Nakuru"


#################################################Saving an unique table results join

KENYA_logit <- rbind(nakuru_logit,sep = ".") 
write.csv(KENYA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/KENYA_logit.csv"))


