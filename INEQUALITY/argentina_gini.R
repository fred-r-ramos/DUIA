library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
library(survey)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/argentina")

###loading data and correcting projection

geo2_91 <- read_sf("geo2_ar1991.shp")
geo2_01 <- read_sf("geo2_ar2001.shp")
geo2_10 <- read_sf("geo2_ar2010.shp")
AUE_ba <- read_sf("Buenos_Aires_studyArea.shp")
AUE_co <- read_sf("Cordoba_studyArea.shp")
AUE_b <- st_transform(AUE_ba, 4326)
AUE_c <- st_transform(AUE_co, 4326)

sf::sf_use_s2(FALSE)

### selecting by centroids
centroid_ar91 <- st_centroid(geo2_91)
centroid_ar01 <- st_centroid(geo2_01)
centroid_ar10 <- st_centroid(geo2_10)

buenos_aires_91 <- geo2_91[st_intersection(AUE_b, centroid_ar91),]
buenos_aires_91['CITY']='buenos aires'
buenos_aires_01 <- geo2_01[st_intersection(AUE_b, centroid_ar01),]
buenos_aires_01['CITY']='buenos aires'
buenos_aires_10 <- geo2_10[st_intersection(AUE_b, centroid_ar10),]
buenos_aires_10['CITY']='buenos aires'

cordoba_91 <- geo2_91[st_intersection(AUE_c, centroid_ar91),]
cordoba_91['CITY']='cordoba'
cordoba_01 <- geo2_01[st_intersection(AUE_c, centroid_ar01),]
cordoba_01['CITY']='cordoba'
cordoba_10 <- geo2_10[st_intersection(AUE_c, centroid_ar10),]
cordoba_10['CITY']='cordoba'

##### joining and plotting geometry

geo_argentina_91 <- rbind(buenos_aires_91,cordoba_91)
geo_argentina_01 <- rbind(buenos_aires_01,cordoba_01)
geo_argentina_10 <- rbind(buenos_aires_10,cordoba_10)

plot(st_geometry(geo_argentina_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00216.xml")
ddi <-read_ipums_ddi("ipumsi_00242.xml") #this file contains the variables for logit regressions
argentina <- read_ipums_micro(ddi)

names(argentina)
table(argentina$YEAR)

######################creating binary variables for logit regressions

table(argentina$OWNERSHIP)
argentina$owner_b <- ifelse(argentina$OWNERSHIP ==1,1,0)
table(argentina$WATSUP)
argentina$water_b <- ifelse(argentina$WATSUP ==11|argentina$WATSUP ==10,1,0)
table(argentina$SEWAGE)
argentina$sewage_b <- ifelse(argentina$SEWAGE ==11|argentina$SEWAGE ==12,1,0)
table(argentina$TOILET)
argentina$toilet_b <- ifelse(argentina$TOILET ==21,1,0)
table(argentina$ROOF)
argentina$roof_b <- ifelse(argentina$ROOF ==14|argentina$ROOF ==15|argentina$ROOF ==19|argentina$ROOF ==21|argentina$ROOF ==23|argentina$ROOF ==26,1,0)
table(argentina$FLOOR)
argentina$floors_b <- ifelse(argentina$FLOOR==210|argentina$FLOOR==236,1,0)
table(argentina$REFRIG)
argentina$refrig_b <- ifelse(argentina$REFRIG == 2,1,0)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL

argentina$PUBl_ASSET <- (argentina$water_b)+(argentina$sewage_b)
argentina %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

argentina$PRIV_ASSET <- ifelse(is.na(argentina$owner_b), 0, argentina$owner_b) +
  ifelse(is.na(argentina$toilet_b), 0, argentina$toilet_b) +
  ifelse(is.na(argentina$roof_b), 0, argentina$roof_b) +
  ifelse(is.na(argentina$floors_b), 0, argentina$floors_b) +
  ifelse(is.na(argentina$refrig_b), 0, argentina$refrig_b)

argentina %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

argentina$TOTAL_ASSET <- rowSums(cbind(
  ifelse(is.na(argentina$owner_b), 0, argentina$owner_b),
  ifelse(is.na(argentina$toilet_b), 0, argentina$toilet_b),
  ifelse(is.na(argentina$roof_b), 0, argentina$roof_b),
  ifelse(is.na(argentina$floors_b), 0, argentina$floors_b),
  ifelse(is.na(argentina$refrig_b), 0, argentina$refrig_b),
  ifelse(is.na(argentina$water_b), 0, argentina$water_b),
  ifelse(is.na(argentina$sewage_b), 0, argentina$sewage_b)
))
assets<-argentina %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

argentina$TOTAL_ASSET_gini <- rowSums(cbind(
  ifelse(is.na(argentina$owner_b), 0, argentina$owner_b),
  ifelse(is.na(argentina$toilet_b), 0, argentina$toilet_b),
  ifelse(is.na(argentina$roof_b), 0, argentina$roof_b),
  ifelse(is.na(argentina$floors_b), 0, argentina$floors_b),
  ifelse(is.na(argentina$water_b), 0, argentina$water_b),
  ifelse(is.na(argentina$sewage_b), 0, argentina$sewage_b)
))
assets<-argentina %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

argentina$IPUM1991 <- as.integer(argentina$GEO2_AR1991)
argentina$IPUM2001 <- as.integer(argentina$GEO2_AR2001)
argentina$IPUM2010 <- as.integer(argentina$GEO2_AR2010)
geo_argentina_91$IPUM1991 <- as.integer(geo_argentina_91$IPUM1991)
geo_argentina_01$IPUM2001 <- as.integer(geo_argentina_01$IPUM2001)
geo_argentina_10$IPUM2010 <- as.integer(geo_argentina_10$IPUM2010)

##Joining by year

argentina_91 <- argentina %>% inner_join(geo_argentina_91, by="IPUM1991")
argentina_01 <- argentina %>% inner_join(geo_argentina_01, by="IPUM2001")
argentina_10 <- argentina %>% inner_join(geo_argentina_10, by="IPUM2010")

names(argentina_91)
names(argentina_01)
names(argentina_10)

argentina_91 <- select(argentina_91, -c(DEPT1991))
argentina_01 <- select(argentina_01, -c(DEPT2001))
argentina_10 <- select(argentina_10, -c(DEPT2010))

##Merging all years into one table

argentina_full <- rbind(argentina_91,argentina_01,argentina_10)
names(argentina_full)
table(argentina_full$YEAR)

##Excluding specific columns for the unified dataset
#argentina_full<- select(argentina_full, -c(GEO2_AR1991,GEO2_AR2001,GEO2_AR2010,IPUM1991,IPUM2001,IPUM2010))
#table(argentina_full$CITY)

names(argentina_full)

table(argentina_full$YEAR)

argentina_full <- argentina_full %>%  filter (argentina_full$YRSCHOOL < 90)

argentina_full <- argentina_full %>%  filter (argentina_full$AGE >15)

argentina_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

table(argentina_full$YEAR)

mean(argentina_full$YRSCHOOL)
summary(argentina_full$YRSCHOOL)

argentina_fu91 <- argentina_full %>%  filter (YEAR==1991)
argentina_fu01 <- argentina_full %>%  filter (YEAR==2001)
argentina_fu10 <- argentina_full %>%  filter (YEAR==2010)
Gini(argentina_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(argentina_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(argentina_fu10 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves

Lc_arg91 <- Lc(argentina_fu91$YRSCHOOL, n = rep(1,length(argentina_fu91$YRSCHOOL)), plot = TRUE)
Lc_arg01 <- Lc(argentina_fu01$YRSCHOOL, n = rep(1,length(argentina_fu01$YRSCHOOL)), plot = TRUE)
Lc_arg10 <- Lc(argentina_fu10$YRSCHOOL, n = rep(1,length(argentina_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_arg91,col='blue', main = "Lorenz Curve - Argentina")
lines(Lc_arg01, col='red')
lines(Lc_arg10, col='green')

buenos_aires_full <- argentina_full %>%  filter (CITY=="buenos aires")

summary(buenos_aires_full$YRSCHOOL)
summary(buenos_aires_full$AGE)

buenos_aires_fu91 <- buenos_aires_full %>%  filter (YEAR==1991)
buenos_aires_fu01 <- buenos_aires_full %>%  filter (YEAR==2001)
buenos_aires_fu10 <- buenos_aires_full %>%  filter (YEAR==2010)
Gini(buenos_aires_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(buenos_aires_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(buenos_aires_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(buenos_aires_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(buenos_aires_fu01 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(buenos_aires_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_bua91 <- Lc(buenos_aires_fu91$YRSCHOOL, n = rep(1,length(buenos_aires_fu91$YRSCHOOL)), plot = TRUE)
Lc_arg01 <- Lc(buenos_aires_fu01$YRSCHOOL, n = rep(1,length(buenos_aires_fu01$YRSCHOOL)), plot = TRUE)
Lc_arg10 <- Lc(buenos_aires_fu10$YRSCHOOL, n = rep(1,length(buenos_aires_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_arg91,col='blue', main = "Lorenz Curve - Buenos Aires")
lines(Lc_arg01, col='red')
lines(Lc_arg10, col='green')

cordoba_full <- argentina_full %>%  filter (CITY=="cordoba")

summary(cordoba_full$YRSCHOOL)
summary(cordoba_full$AGE)

cordoba_fu91 <- cordoba_full %>%  filter (YEAR==1991)
cordoba_fu01 <- cordoba_full %>%  filter (YEAR==2001)
cordoba_fu10 <- cordoba_full %>%  filter (YEAR==2010)
Gini(cordoba_fu91 $YRSCHOOL,na.rm = TRUE)
Gini(cordoba_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(cordoba_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(cordoba_fu91 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(cordoba_fu01 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(cordoba_fu10 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_cor91 <- Lc(cordoba_fu91$YRSCHOOL, n = rep(1,length(cordoba_fu91$YRSCHOOL)), plot = TRUE)
Lc_cor01 <- Lc(cordoba_fu01$YRSCHOOL, n = rep(1,length(cordoba_fu01$YRSCHOOL)), plot = TRUE)
Lc_cor10 <- Lc(cordoba_fu10$YRSCHOOL, n = rep(1,length(cordoba_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_cor91,col='blue', main = "Lorenz Curve - Cordoba")
lines(Lc_cor01, col='red')
lines(Lc_cor10, col='green')


################################analyzing the evolution in amenities access

#checking if the variables are available in both years
table(argentina$owner_b,argentina$YEAR)
table(argentina$water_b,argentina$YEAR)
table(argentina$sewage_b,argentina$YEAR)
table(argentina$toilet_b,argentina$YEAR)
table(argentina$roof_b,argentina$YEAR)
table(argentina$floors_b,argentina$YEAR)
table(argentina$refrig_b,argentina$YEAR)

table(cordoba_full$PUBl_ASSET,cordoba_full$YEAR)
table(buenos_aires_full$PUBl_ASSET,buenos_aires_full$YEAR)
table(cordoba_full$PRIV_ASSET,cordoba_full$YEAR)
table(buenos_aires_full$PRIV_ASSET,buenos_aires_full$YEAR)

# for PUBl_ASSET
cordoba_PUBl_ASSET<-cordoba_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cordoba_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cordoba_PUBl_ASSET <- cordoba_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cordoba_PUBl_ASSET <- cordoba_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cordoba_PUBl_ASSET$CITY<-"Cordoba"
# for PRIV_ASSET
cordoba_PRIV_ASSET<-cordoba_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cordoba_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cordoba_PRIV_ASSET <- cordoba_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cordoba_PRIV_ASSET <- cordoba_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cordoba_PRIV_ASSET$CITY<-"Cordoba"
# for TOTAL_ASSET
cordoba_TOTAL_ASSET<-cordoba_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cordoba_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cordoba_TOTAL_ASSET <- cordoba_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cordoba_TOTAL_ASSET <- cordoba_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cordoba_TOTAL_ASSET$CITY<-"Cordoba"

write.csv(cordoba_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cordoba_PUBl_ASSET.csv", row.names = TRUE)
write.csv(cordoba_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cordoba_PRIV_ASSET.csv", row.names = TRUE)
write.csv(cordoba_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cordoba_TOTAL_ASSET.csv", row.names = TRUE)


# for PUBl_ASSET
buenos_aires_PUBl_ASSET<-buenos_aires_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- buenos_aires_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
buenos_aires_PUBl_ASSET <- buenos_aires_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
buenos_aires_PUBl_ASSET <- buenos_aires_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
buenos_aires_PUBl_ASSET$CITY<-"Buenos Aires"
# for PRIV_ASSET
buenos_aires_PRIV_ASSET<-buenos_aires_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- buenos_aires_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
buenos_aires_PRIV_ASSET <- buenos_aires_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
buenos_aires_PRIV_ASSET <- buenos_aires_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
buenos_aires_PRIV_ASSET$CITY<-"Buenos Aires"
# for TOTAL_ASSET
buenos_aires_TOTAL_ASSET<-buenos_aires_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- buenos_aires_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
buenos_aires_TOTAL_ASSET <- buenos_aires_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
buenos_aires_TOTAL_ASSET <- buenos_aires_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
buenos_aires_TOTAL_ASSET$CITY<-"Buenos Aires"


table(cordoba_full$PUBl_ASSET,cordoba_full$YEAR)
table(buenos_aires_full$PUBl_ASSET,buenos_aires_full$YEAR)
table(cordoba_full$PRIV_ASSET,cordoba_full$YEAR)
table(buenos_aires_full$PRIV_ASSET,buenos_aires_full$YEAR)


write.csv(buenos_aires_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/buenos_aires_PUBl_ASSET.csv", row.names = TRUE)
write.csv(buenos_aires_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/buenos_aires_PRIV_ASSET.csv", row.names = TRUE)
write.csv(buenos_aires_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/buenos_aires_TOTAL_ASSET.csv", row.names = TRUE)

######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

buenos_aires_WATER <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(buenos_aires_WATER$WATSUP)
buenos_aires_WATER <- buenos_aires_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(buenos_aires_WATER$WATSUP)
table(buenos_aires_WATER$water_b)

buenos_aires_WATER$water_b <- factor(buenos_aires_WATER$water_b)

buenos_aires_WATER$YEAR <- factor(buenos_aires_WATER$YEAR)
buenos_aires_WATER_01 <- buenos_aires_WATER %>% filter(buenos_aires_WATER$YEAR==2001)
buenos_aires_WATER_10 <- buenos_aires_WATER %>% filter(buenos_aires_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = buenos_aires_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(water_b ~ YRSCHOOL, data = buenos_aires_WATER_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_10)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

buenos_aires_OWN <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(buenos_aires_OWN$OWNERSHIP)
buenos_aires_OWN <- buenos_aires_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(buenos_aires_OWN$OWNERSHIP)
table(buenos_aires_OWN$owner_b)

buenos_aires_OWN$owner_b <- factor(buenos_aires_OWN$owner_b)

buenos_aires_OWN$YEAR <- factor(buenos_aires_OWN$YEAR)
buenos_aires_OWN_01 <- buenos_aires_OWN %>% filter(buenos_aires_OWN$YEAR==2001)
buenos_aires_OWN_10 <- buenos_aires_OWN %>% filter(buenos_aires_OWN$YEAR==2010)

mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = buenos_aires_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(owner_b ~ YRSCHOOL, data = buenos_aires_OWN_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_10)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}


#################################### FOR SEWAGE ##################################

buenos_aires_SEWG <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(buenos_aires_SEWG$SEWAGE)
buenos_aires_SEWG <- buenos_aires_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(buenos_aires_SEWG$SEWAGE)
table(buenos_aires_SEWG$sewage_b)

buenos_aires_SEWG$sewage_b <- factor(buenos_aires_SEWG$sewage_b)

buenos_aires_SEWG$YEAR <- factor(buenos_aires_SEWG$YEAR)
buenos_aires_SEWG_01 <- buenos_aires_SEWG %>% filter(buenos_aires_SEWG$YEAR==2001)
buenos_aires_SEWG_10 <- buenos_aires_SEWG %>% filter(buenos_aires_SEWG$YEAR==2010)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = buenos_aires_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = buenos_aires_SEWG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_10)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

buenos_aires_REFRIG <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(buenos_aires_REFRIG$REFRIG)
buenos_aires_REFRIG <- buenos_aires_REFRIG %>% filter(REFRIG!=0)

table(buenos_aires_REFRIG$REFRIG)
table(buenos_aires_REFRIG$refrig_b)

buenos_aires_REFRIG$refrig_b <- factor(buenos_aires_REFRIG$refrig_b)

buenos_aires_REFRIG$YEAR <- factor(buenos_aires_REFRIG$YEAR)
buenos_aires_REFRIG_01 <- buenos_aires_REFRIG %>% filter(buenos_aires_REFRIG$YEAR==2001)
buenos_aires_REFRIG_10 <- buenos_aires_REFRIG %>% filter(buenos_aires_REFRIG$YEAR==2010)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = buenos_aires_REFRIG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(refrig_b ~ YRSCHOOL, data = buenos_aires_REFRIG_10, family = "binomial")
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

buenos_aires_TOILET <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(buenos_aires_TOILET$TOILET)
buenos_aires_TOILET <- buenos_aires_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(buenos_aires_TOILET$TOILET)
table(buenos_aires_TOILET$toilet_b)

buenos_aires_TOILET$toilet_b <- factor(buenos_aires_TOILET$toilet_b)

buenos_aires_TOILET$YEAR <- factor(buenos_aires_TOILET$YEAR)
buenos_aires_TOILET_01 <- buenos_aires_TOILET %>% filter(buenos_aires_TOILET$YEAR==2001)
buenos_aires_TOILET_10 <- buenos_aires_TOILET %>% filter(buenos_aires_TOILET$YEAR==2010)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = buenos_aires_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(toilet_b ~ YRSCHOOL, data = buenos_aires_TOILET_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

buenos_aires_ROOF <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(buenos_aires_ROOF$ROOF)
buenos_aires_ROOF <- buenos_aires_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(buenos_aires_ROOF$ROOF)
table(buenos_aires_ROOF$roof_b)

buenos_aires_ROOF$roof_b <- factor(buenos_aires_ROOF$roof_b)

buenos_aires_ROOF$YEAR <- factor(buenos_aires_ROOF$YEAR)
buenos_aires_ROOF_01 <- buenos_aires_ROOF %>% filter(buenos_aires_ROOF$YEAR==2001)
buenos_aires_ROOF_10 <- buenos_aires_ROOF %>% filter(buenos_aires_ROOF$YEAR==2010)

mylogit <- glm(roof_b ~ YRSCHOOL + YEAR, data = buenos_aires_ROOF, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = buenos_aires_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(roof_b ~ YRSCHOOL, data = buenos_aires_ROOF_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR FLOOR ##################################

buenos_aires_FLOOR <- buenos_aires_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floors_b,YRSCHOOL,CITY)
table(buenos_aires_FLOOR$FLOOR)
buenos_aires_FLOOR <- buenos_aires_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=999)

table(buenos_aires_FLOOR$FLOOR)
table(buenos_aires_FLOOR$floors_b)

buenos_aires_FLOOR$floors_b <- factor(buenos_aires_FLOOR$floors_b)

buenos_aires_FLOOR$YEAR <- factor(buenos_aires_FLOOR$YEAR)
buenos_aires_FLOOR_01 <- buenos_aires_FLOOR %>% filter(buenos_aires_FLOOR$YEAR==2001)
buenos_aires_FLOOR_10 <- buenos_aires_FLOOR %>% filter(buenos_aires_FLOOR$YEAR==2010)

mylogit <- glm(floors_b ~ YRSCHOOL + YEAR, data = buenos_aires_FLOOR, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(floors_b ~ YRSCHOOL, data = buenos_aires_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(floors_b ~ YRSCHOOL, data = buenos_aires_FLOOR_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}


BUENOS_AIRES_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ROOF_T2_z,ROOF_T3_z,FLOOR_T2_z,FLOOR_T3_z) 
BUENOS_AIRES_logit$CITY <- "Buenos Aires"

######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

cordoba_WATER <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(cordoba_WATER$WATSUP)
cordoba_WATER <- cordoba_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(cordoba_WATER$WATSUP)
table(cordoba_WATER$water_b)

cordoba_WATER$water_b <- factor(cordoba_WATER$water_b)

cordoba_WATER$YEAR <- factor(cordoba_WATER$YEAR)
cordoba_WATER_01 <- cordoba_WATER %>% filter(cordoba_WATER$YEAR==2001)
cordoba_WATER_10 <- cordoba_WATER %>% filter(cordoba_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = cordoba_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(water_b ~ YRSCHOOL, data = cordoba_WATER_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_10)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

cordoba_OWN <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(cordoba_OWN$OWNERSHIP)
cordoba_OWN <- cordoba_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(cordoba_OWN$OWNERSHIP)
table(cordoba_OWN$owner_b)

cordoba_OWN$owner_b <- factor(cordoba_OWN$owner_b)

cordoba_OWN$YEAR <- factor(cordoba_OWN$YEAR)
cordoba_OWN_01 <- cordoba_OWN %>% filter(cordoba_OWN$YEAR==2001)
cordoba_OWN_10 <- cordoba_OWN %>% filter(cordoba_OWN$YEAR==2010)

mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = cordoba_OWN, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = cordoba_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(owner_b ~ YRSCHOOL, data = cordoba_OWN_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_10)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

cordoba_SEWG <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(cordoba_SEWG$SEWAGE)
cordoba_SEWG <- cordoba_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(cordoba_SEWG$SEWAGE)
table(cordoba_SEWG$sewage_b)

cordoba_SEWG$sewage_b <- factor(cordoba_SEWG$sewage_b)

cordoba_SEWG$YEAR <- factor(cordoba_SEWG$YEAR)
cordoba_SEWG_01 <- cordoba_SEWG %>% filter(cordoba_SEWG$YEAR==2001)
cordoba_SEWG_10 <- cordoba_SEWG %>% filter(cordoba_SEWG$YEAR==2010)

mylogit <- glm(sewage_b ~ YRSCHOOL + YEAR, data = cordoba_SEWG, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = cordoba_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = cordoba_SEWG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_10)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

cordoba_REFRIG <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(cordoba_REFRIG$REFRIG)
cordoba_REFRIG <- cordoba_REFRIG %>% filter(REFRIG!=0)

table(cordoba_REFRIG$REFRIG)
table(cordoba_REFRIG$refrig_b)

cordoba_REFRIG$refrig_b <- factor(cordoba_REFRIG$refrig_b)

cordoba_REFRIG$YEAR <- factor(cordoba_REFRIG$YEAR)
cordoba_REFRIG_01 <- cordoba_REFRIG %>% filter(cordoba_REFRIG$YEAR==2001)
cordoba_REFRIG_10 <- cordoba_REFRIG %>% filter(cordoba_REFRIG$YEAR==2010)

mylogit <- glm(refrig_b ~ YRSCHOOL + YEAR, data = cordoba_REFRIG, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = cordoba_REFRIG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(refrig_b ~ YRSCHOOL, data = cordoba_REFRIG_10, family = "binomial")
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

cordoba_TOILET <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(cordoba_TOILET$TOILET)
cordoba_TOILET <- cordoba_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(cordoba_TOILET$TOILET)
table(cordoba_TOILET$toilet_b)

cordoba_TOILET$toilet_b <- factor(cordoba_TOILET$toilet_b)

cordoba_TOILET$YEAR <- factor(cordoba_TOILET$YEAR)
cordoba_TOILET_01 <- cordoba_TOILET %>% filter(cordoba_TOILET$YEAR==2001)
cordoba_TOILET_10 <- cordoba_TOILET %>% filter(cordoba_TOILET$YEAR==2010)

mylogit <- glm(toilet_b ~ YRSCHOOL + YEAR, data = cordoba_TOILET, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = cordoba_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(toilet_b ~ YRSCHOOL, data = cordoba_TOILET_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

cordoba_ROOF <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(cordoba_ROOF$ROOF)
cordoba_ROOF <- cordoba_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(cordoba_ROOF$ROOF)
table(cordoba_ROOF$roof_b)

cordoba_ROOF$roof_b <- factor(cordoba_ROOF$roof_b)

cordoba_ROOF$YEAR <- factor(cordoba_ROOF$YEAR)
cordoba_ROOF_01 <- cordoba_ROOF %>% filter(cordoba_ROOF$YEAR==2001)
cordoba_ROOF_10 <- cordoba_ROOF %>% filter(cordoba_ROOF$YEAR==2010)

mylogit <- glm(roof_b ~ YRSCHOOL + YEAR, data = cordoba_ROOF, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = cordoba_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(roof_b ~ YRSCHOOL, data = cordoba_ROOF_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}


#################################### FOR FLOOR ##################################

cordoba_FLOOR <- cordoba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floors_b,YRSCHOOL,CITY)
table(cordoba_FLOOR$FLOOR)
cordoba_FLOOR <- cordoba_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=999)

table(cordoba_FLOOR$FLOOR)
table(cordoba_FLOOR$floors_b)

cordoba_FLOOR$floors_b <- factor(cordoba_FLOOR$floors_b)

cordoba_FLOOR$YEAR <- factor(cordoba_FLOOR$YEAR)
cordoba_FLOOR_01 <- cordoba_FLOOR %>% filter(cordoba_FLOOR$YEAR==2001)
cordoba_FLOOR_10 <- cordoba_FLOOR %>% filter(cordoba_FLOOR$YEAR==2010)

mylogit <- glm(floors_b ~ YRSCHOOL + YEAR, data = cordoba_FLOOR, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(floors_b ~ YRSCHOOL, data = cordoba_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(floors_b ~ YRSCHOOL, data = cordoba_FLOOR_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}


CORDOBA_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ROOF_T2_z,ROOF_T3_z,FLOOR_T2_z,FLOOR_T3_z) 
CORDOBA_logit$CITY <- "Cordoba"
#################################################Saving an unique table results join

ARGENTINA_logit <- rbind(BUENOS_AIRES_logit, CORDOBA_logit,sep = ".") 
write.csv(ARGENTINA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/ARGENTINA_logit.csv"))

ARGE<-read.csv("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/ARGENTINA_logit.csv")

