library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/moroco")

geo2_ma94 <- read_sf("geo2_ma1994.shp")
geo2_ma04 <- read_sf("geo2_ma2004.shp")
geo2_ma14 <- read_sf("geo2_ma2014.shp")

centroid_ma94 <- st_centroid(geo2_ma94)
centroid_ma04 <- st_centroid(geo2_ma04)
centroid_ma14 <- st_centroid(geo2_ma14)
AUE_marrakesh <- read_sf("Marrakesh_studyArea.shp")
AUE_marrakesh  <- st_transform(AUE_marrakesh , 4326)

sf::sf_use_s2(FALSE)

marrakesh_94 <- geo2_ma94[st_intersection(AUE_marrakesh,centroid_ma94),]
marrakesh_94['CITY']='marrakesh'
marrakesh_04 <- geo2_ma04[st_intersection(AUE_marrakesh,centroid_ma04),]
marrakesh_04['CITY']='marrakesh'
marrakesh_14 <- geo2_ma14[st_intersection(AUE_marrakesh,centroid_ma14),]
marrakesh_14['CITY']='marrakesh'

plot(st_geometry(marrakesh_14), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ma04[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00234.xml")
ddi <-read_ipums_ddi("ipumsi_00257.xml")
moroco <- read_ipums_micro(ddi)

names(moroco)

######################creating binary variables for logit regressions

names(moroco)


table(moroco$OWNERSHIP)
moroco$owner_b <- ifelse(moroco$OWNERSHIP ==1,1,0)
table(moroco$ELECTRIC)
moroco$eletric_b <- ifelse(moroco$ELECTRIC ==1,1,0)
table(moroco$WATSUP)
moroco$water_b <- ifelse(moroco$WATSUP ==12,1,0)
table(moroco$SEWAGE)
moroco$sewage_b <- ifelse(moroco$SEWAGE ==11|moroco$SEWAGE ==12,1,0)
table(moroco$TRASH)
moroco$trash_b <- ifelse(moroco$TRASH ==10|moroco$TRASH ==12,1,0)
table(moroco$AUTOS)
moroco$autos_b <- ifelse(moroco$AUTOS ==1|moroco$AUTOS==2,1,0)
table(moroco$REFRIG)
moroco$refrig_b <- ifelse(moroco$REFRIG ==2,1,0)
table(moroco$TV)
moroco$tv_b <- ifelse(moroco$TV ==20,1,0)
table(moroco$TOILET)
moroco$toilet_b <- ifelse(moroco$TOILET==20|moroco$TOILET==21|moroco$TOILET==22,1,0)
table(moroco$FLOOR)
moroco$floor_b <- ifelse(moroco$FLOOR ==203|moroco$FLOOR ==213|moroco$FLOOR ==219|moroco$FLOOR ==236,1,0)
table(moroco$ROOF)
moroco$roof_b <- ifelse(moroco$ROOF==11|moroco$ROOF==30|moroco$ROOF==34,1,0)

names(moroco)
gc()

#checking if the variables are available in both years
table(moroco$owner_b,moroco$YEAR)
table(moroco$eletric_b,moroco$YEAR)
table(moroco$water_b,moroco$YEAR)
table(moroco$sewage_b,moroco$YEAR)
table(moroco$trash_b,moroco$YEAR)###########
table(moroco$autos_b,moroco$YEAR)###########
table(moroco$refrig_b,moroco$YEAR)#########
table(moroco$tv_b,moroco$YEAR)
table(moroco$toilet_b,moroco$YEAR)
table(moroco$floor_b,moroco$YEAR)##############
table(moroco$roof_b,moroco$YEAR)################


##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
moroco$PUBl_ASSET <- ifelse(is.na(moroco$eletric_b), 0, moroco$eletric_b) +
  ifelse(is.na(moroco$water_b), 0, moroco$water_b) +
  ifelse(is.na(moroco$sewage_b), 0, moroco$sewage_b) +
  ifelse(is.na(moroco$trash_b), 0, moroco$trash_b)
moroco %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

moroco$PRIV_ASSET <- ifelse(is.na(moroco$owner_b), 0, moroco$owner_b) +
  ifelse(is.na(moroco$autos_b), 0, moroco$autos_b) +
  ifelse(is.na(moroco$refrig_b), 0, moroco$refrig_b) +
  ifelse(is.na(moroco$tv_b), 0, moroco$tv_b)+
  ifelse(is.na(moroco$toilet_b), 0, moroco$toilet_b) +
  ifelse(is.na(moroco$floor_b), 0, moroco$floor_b) +
  ifelse(is.na(moroco$roof_b), 0, moroco$roof_b)
moroco %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

moroco$TOTAL_ASSET <- ifelse(is.na(moroco$owner_b), 0, moroco$owner_b) +
  ifelse(is.na(moroco$autos_b), 0, moroco$autos_b) +
  ifelse(is.na(moroco$refrig_b), 0, moroco$refrig_b) +
  ifelse(is.na(moroco$tv_b), 0, moroco$tv_b)+
  ifelse(is.na(moroco$toilet_b), 0, moroco$toilet_b) +
  ifelse(is.na(moroco$floor_b), 0, moroco$floor_b) +
  ifelse(is.na(moroco$roof_b), 0, moroco$roof_b) +
  ifelse(is.na(moroco$eletric_b), 0, moroco$eletric_b) +
  ifelse(is.na(moroco$water_b), 0, moroco$water_b) +
  ifelse(is.na(moroco$sewage_b), 0, moroco$sewage_b) +
  ifelse(is.na(moroco$trash_b), 0, moroco$trash_b)
moroco %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

moroco$TOTAL_ASSET_gini <- ifelse(is.na(moroco$owner_b), 0, moroco$owner_b) +
  ifelse(is.na(moroco$toilet_b), 0, moroco$toilet_b) +
  ifelse(is.na(moroco$eletric_b), 0, moroco$eletric_b) +
  ifelse(is.na(moroco$water_b), 0, moroco$water_b) 
moroco %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join


moroco$IPUM1994 <- as.integer(moroco$GEO2_MA1994)
moroco$IPUM2004 <- as.integer(moroco$GEO2_MA2004)
moroco$IPUM2014 <- as.integer(moroco$GEO2_MA2014)

marrakesh_94$IPUM1994 <- as.integer(marrakesh_94$IPUM1994)
marrakesh_04$IPUM2004 <- as.integer(marrakesh_04$IPUM2004)
marrakesh_14$IPUM2014 <- as.integer(marrakesh_14$IPUM2014)

##Joining by year

marrakesh_94 <- moroco %>% inner_join(marrakesh_94, by="IPUM1994")
marrakesh_04 <- moroco %>% inner_join(marrakesh_04, by="IPUM2004")
marrakesh_14 <- moroco %>% inner_join(marrakesh_14, by="IPUM2014")

names(marrakesh_94)
names(marrakesh_04)
names(marrakesh_14)

marrakesh_94 <- select(marrakesh_94, -c(PROV1994))
marrakesh_04 <- select(marrakesh_04, -c(PROV2004))
marrakesh_14 <- select(marrakesh_14, -c(PROV2014))

##Merging all years into one table
marrakesh_full <- rbind(marrakesh_94,marrakesh_04,marrakesh_14)
names(marrakesh_full)

gc()

##Excluding specific columns for the unifeied dataset
marrakesh_full<- select(marrakesh_full, -c(GEO2_MA1994,GEO2_MA2004,IPUM1994,IPUM2004,geometry))
table(marrakesh_full$CITY)

table(marrakesh_full$YEAR)

# marrakesh_full <- marrakesh_full %>%  filter (marrakesh_full$YRSCHOOL < 90)
# 
# marrakesh_full <- marrakesh_full %>%  filter (marrakesh_full$AGE >15)
# 
# marrakesh_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(marrakesh_full$YRSCHOOL)
summary(marrakesh_full$YRSCHOOL)

marrakesh_fu94 <- marrakesh_full %>%  filter (YEAR==1994)
marrakesh_fu04 <- marrakesh_full %>%  filter (YEAR==2004)
marrakesh_fu14 <- marrakesh_full %>%  filter (YEAR==2014)
Gini(marrakesh_fu94 $YRSCHOOL,na.rm = TRUE)
Gini(marrakesh_fu04 $YRSCHOOL,na.rm = TRUE)
Gini(marrakesh_fu14 $YRSCHOOL,na.rm = TRUE)
Gini(marrakesh_fu94 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(marrakesh_fu04 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(marrakesh_fu14 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_mark94 <- Lc(marrakesh_fu94$YRSCHOOL, n = rep(1,length(marrakesh_fu94$YRSCHOOL)), plot = TRUE)
Lc_mark04 <- Lc(marrakesh_fu04$YRSCHOOL, n = rep(1,length(marrakesh_fu04$YRSCHOOL)), plot = TRUE)
Lc_mark14 <- Lc(marrakesh_fu14$YRSCHOOL, n = rep(1,length(marrakesh_fu14$YRSCHOOL)), plot = TRUE)

plot(Lc_mark94,col='blue', main = "Lorenz Curve - Marrakesh")
lines(Lc_mark04, col='red')
lines(Lc_mark14, col='green')

table(marrakesh_full$OCCISCO)


marrakesh_full$OCCISCO_b <- ifelse(marrakesh_full$OCCISCO ==1|marrakesh_full$OCCISCO ==2,1,0)
marrakesh_full$OCCISCO_b <- ifelse(marrakesh_full$OCCISCO ==3|marrakesh_full$OCCISCO ==4|marrakesh_full$OCCISCO ==5,2,marrakesh_full$OCCISCO_b)
marrakesh_full$OCCISCO_b <- ifelse(marrakesh_full$OCCISCO ==6|marrakesh_full$OCCISCO ==7|marrakesh_full$OCCISCO ==8|marrakesh_full$OCCISCO ==9,3,marrakesh_full$OCCISCO_b)
table(marrakesh_full$OCCISCO_b)

marrakesh_full_OCCISCO_b <- marrakesh_full %>% select(YEAR,OCCISCO_b,PERWT)
marrakesh_full_OCCISCO_b <- marrakesh_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(marrakesh_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- marrakesh_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- marrakesh_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- marrakesh_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- marrakesh_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

marrakesh_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
marrakesh_OCCISCO_b<- marrakesh_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
marrakesh_OCCISCO_b<- marrakesh_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

marrakesh_OCCISCO_b

# for PUBl_ASSET
marrakesh_PUBl_ASSET<-marrakesh_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- marrakesh_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
marrakesh_PUBl_ASSET <- marrakesh_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
marrakesh_PUBl_ASSET <- marrakesh_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
marrakesh_PUBl_ASSET$CITY<-"marrakesh"
# for PRIV_ASSET
marrakesh_PRIV_ASSET<-marrakesh_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- marrakesh_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
marrakesh_PRIV_ASSET <- marrakesh_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
marrakesh_PRIV_ASSET <- marrakesh_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
marrakesh_PRIV_ASSET$CITY<-"marrakesh"
# for TOTAL_ASSET
marrakesh_TOTAL_ASSET<-marrakesh_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- marrakesh_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
marrakesh_TOTAL_ASSET <- marrakesh_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
marrakesh_TOTAL_ASSET <- marrakesh_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
marrakesh_TOTAL_ASSET$CITY<-"marrakesh"
write.csv(marrakesh_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/marrakesh_PUBl_ASSET.csv", row.names = TRUE)
write.csv(marrakesh_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/marrakesh_PRIV_ASSET.csv", row.names = TRUE)
write.csv(marrakesh_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/marrakesh_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

marrakesh_WATER <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(marrakesh_WATER$WATSUP)
table(marrakesh_WATER_12$HHWT)
marrakesh_WATER <- marrakesh_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(marrakesh_WATER$WATSUP)
table(marrakesh_WATER$water_b)

marrakesh_WATER$water_b <- factor(marrakesh_WATER$water_b)

marrakesh_WATER$YEAR <- factor(marrakesh_WATER$YEAR)
marrakesh_WATER_01 <- marrakesh_WATER %>% filter(marrakesh_WATER$YEAR==2004)
marrakesh_WATER_12 <- marrakesh_WATER %>% filter(marrakesh_WATER$YEAR==2014)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = marrakesh_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = marrakesh_WATER_12, family = "binomial")
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

marrakesh_OWN <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(marrakesh_OWN$OWNERSHIP)
marrakesh_OWN <- marrakesh_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(marrakesh_OWN$OWNERSHIP)
table(marrakesh_OWN$owner_b)

marrakesh_OWN$owner_b <- factor(marrakesh_OWN$owner_b)

marrakesh_OWN$YEAR <- factor(marrakesh_OWN$YEAR)
marrakesh_OWN_01 <- marrakesh_OWN %>% filter(marrakesh_OWN$YEAR==2004)
marrakesh_OWN_12 <- marrakesh_OWN %>% filter(marrakesh_OWN$YEAR==2014)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = marrakesh_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = marrakesh_OWN_12, family = "binomial")
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

marrakesh_SEWG <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(marrakesh_SEWG$SEWAGE)
marrakesh_SEWG <- marrakesh_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(marrakesh_SEWG$SEWAGE)
table(marrakesh_SEWG$sewage_b)

marrakesh_SEWG$sewage_b <- factor(marrakesh_SEWG$sewage_b)

marrakesh_SEWG$YEAR <- factor(marrakesh_SEWG$YEAR)
marrakesh_SEWG_01 <- marrakesh_SEWG %>% filter(marrakesh_SEWG$YEAR==2004)
marrakesh_SEWG_12 <- marrakesh_SEWG %>% filter(marrakesh_SEWG$YEAR==2014)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = marrakesh_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(sewage_b ~ YRSCHOOL, data = marrakesh_SEWG_12, family = "binomial")
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

marrakesh_REFRIG <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(marrakesh_REFRIG$REFRIG)
marrakesh_REFRIG <- marrakesh_REFRIG %>% filter(REFRIG!=0)

table(marrakesh_REFRIG$REFRIG)
table(marrakesh_REFRIG$refrig_b)

marrakesh_REFRIG$refrig_b <- factor(marrakesh_REFRIG$refrig_b)

marrakesh_REFRIG$YEAR <- factor(marrakesh_REFRIG$YEAR)
marrakesh_REFRIG_01 <- marrakesh_REFRIG %>% filter(marrakesh_REFRIG$YEAR==2004)
marrakesh_REFRIG_12 <- marrakesh_REFRIG %>% filter(marrakesh_REFRIG$YEAR==2014)

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = marrakesh_REFRIG_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

marrakesh_TOILET <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(marrakesh_TOILET$TOILET)
marrakesh_TOILET <- marrakesh_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(marrakesh_TOILET$TOILET)
table(marrakesh_TOILET$toilet_b)

marrakesh_TOILET$toilet_b <- factor(marrakesh_TOILET$toilet_b)

marrakesh_TOILET$YEAR <- factor(marrakesh_TOILET$YEAR)
marrakesh_TOILET_01 <- marrakesh_TOILET %>% filter(marrakesh_TOILET$YEAR==2004)
marrakesh_TOILET_12 <- marrakesh_TOILET %>% filter(marrakesh_TOILET$YEAR==2014)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = marrakesh_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = marrakesh_TOILET_12, family = "binomial")
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

marrakesh_ELECTRIC <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(marrakesh_ELECTRIC$ELECTRIC)
marrakesh_ELECTRIC <- marrakesh_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(marrakesh_ELECTRIC$ELECTRIC)
table(marrakesh_ELECTRIC$eletric_b)

marrakesh_ELECTRIC$eletric_b <- factor(marrakesh_ELECTRIC$eletric_b)

marrakesh_ELECTRIC$YEAR <- factor(marrakesh_ELECTRIC$YEAR)
marrakesh_ELECTRIC_01 <- marrakesh_ELECTRIC %>% filter(marrakesh_ELECTRIC$YEAR==2004)
marrakesh_ELECTRIC_12 <- marrakesh_ELECTRIC %>% filter(marrakesh_ELECTRIC$YEAR==2014)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = marrakesh_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = marrakesh_ELECTRIC_12, family = "binomial")
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

marrakesh_TRASH <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(marrakesh_TRASH$TRASH)
marrakesh_TRASH <- marrakesh_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(marrakesh_TRASH$TRASH)
table(marrakesh_TRASH$trash_b)

marrakesh_TRASH$trash_b <- factor(marrakesh_TRASH$trash_b)

marrakesh_TRASH$YEAR <- factor(marrakesh_TRASH$YEAR)
marrakesh_TRASH_12 <- marrakesh_TRASH %>% filter(marrakesh_TRASH$YEAR==2014)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = marrakesh_TRASH_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

marrakesh_AUTOS <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(marrakesh_AUTOS$AUTOS)
marrakesh_AUTOS <- marrakesh_AUTOS %>% filter(AUTOS!=99)

table(marrakesh_AUTOS$AUTOS)
table(marrakesh_AUTOS$autos_b)

marrakesh_AUTOS$autos_b <- factor(marrakesh_AUTOS$autos_b)

marrakesh_AUTOS$YEAR <- factor(marrakesh_AUTOS$YEAR)
marrakesh_AUTOS_12 <- marrakesh_AUTOS %>% filter(marrakesh_AUTOS$YEAR==2014)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = marrakesh_AUTOS_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

marrakesh_FLOOR <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(marrakesh_FLOOR$FLOOR)
marrakesh_FLOOR <- marrakesh_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(marrakesh_FLOOR$FLOOR)
table(marrakesh_FLOOR$floor_b)

marrakesh_FLOOR$floor_b <- factor(marrakesh_FLOOR$floor_b)

marrakesh_FLOOR$YEAR <- factor(marrakesh_FLOOR$YEAR)
marrakesh_FLOOR_12 <- marrakesh_FLOOR %>% filter(marrakesh_FLOOR$YEAR==2014)

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = marrakesh_FLOOR_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T3 <- summary(mylogit_12)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

marrakesh_ROOF <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(marrakesh_ROOF$ROOF)
marrakesh_ROOF <- marrakesh_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(marrakesh_ROOF$ROOF)
table(marrakesh_ROOF$roof_b)

marrakesh_ROOF$roof_b <- factor(marrakesh_ROOF$roof_b)

marrakesh_ROOF$YEAR <- factor(marrakesh_ROOF$YEAR)
marrakesh_ROOF_12 <- marrakesh_ROOF %>% filter(marrakesh_ROOF$YEAR==2014)

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = marrakesh_ROOF_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

marrakesh_TV <- marrakesh_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(marrakesh_TV$TV)
marrakesh_TV <- marrakesh_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(marrakesh_TV$TV)
table(marrakesh_TV$tv_b)

marrakesh_TV$tv_b <- factor(marrakesh_TV$tv_b)

marrakesh_TV$YEAR <- factor(marrakesh_TV$YEAR)
marrakesh_TV_01 <- marrakesh_TV %>% filter(marrakesh_TV$YEAR==2004)
marrakesh_TV_12 <- marrakesh_TV %>% filter(marrakesh_TV$YEAR==2014)

mylogit <- glm(tv_b ~ YRSCHOOL + YEAR, data = marrakesh_TV, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = marrakesh_TV_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = marrakesh_TV_12, family = "binomial")
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

marrakesh_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T3_z,FLOOR_T3_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
marrakesh_logit$CITY <- "Marrakesh"


#################################################Saving an unique table results join

MOROCO_logit <- rbind(marrakesh_logit,sep = ".") 
write.csv(MOROCO_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/MOROCO_logit.csv"))


