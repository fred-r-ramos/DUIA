library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/thailand")

geo2_th90 <- read_sf("geo2_th1990.shp")
geo2_th00 <- read_sf("geo2_th2000.shp")
geo2_th90 <- st_transform(geo2_th90 , 4326)
geo2_th00 <- st_transform(geo2_th00 , 4326)

sf::sf_use_s2(FALSE)

centroid_th90 <- st_centroid(geo2_th90)
centroid_th00 <- st_centroid(geo2_th00)
AUE_bangkok <- read_sf("Bangkok_studyArea.shp")
AUE_bangkok  <- st_transform(AUE_bangkok , 4326)

bangkok_90 <- geo2_th90[st_intersection(AUE_bangkok,centroid_th90),]
bangkok_90['CITY']='bangkok'
bangkok_00 <- geo2_th00[st_intersection(AUE_bangkok,centroid_th00),]
bangkok_00['CITY']='bangkok'

plot(st_geometry(bangkok_00), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_th00[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00223.xml")
ddi <-read_ipums_ddi("ipumsi_00263.xml")
thailand <- read_ipums_micro(ddi)

names(thailand)

######################creating binary variables for logit regressions

names(thailand)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(thailand$OWNERSHIP)
thailand$owner_b <- ifelse(thailand$OWNERSHIP ==1,1,0)
table(thailand$ELECTRIC)
thailand$eletric_b <- ifelse(thailand$ELECTRIC ==1,1,0)
table(thailand$WATSUP)
thailand$water_b <- ifelse(thailand$WATSUP ==11,1,0)
table(thailand$AUTOS)
thailand$autos_b <- ifelse(thailand$AUTOS ==1|thailand$AUTOS ==2|thailand$AUTOS ==3|thailand$AUTOS ==4|thailand$AUTOS ==5|thailand$AUTOS ==6,1,0)
table(thailand$REFRIG)
thailand$refrig_b <- ifelse(thailand$REFRIG ==2,1,0)
table(thailand$TV)
thailand$tv_b <- ifelse(thailand$TV>10 & thailand$TV<99,1,0)
table(thailand$TOILET)
thailand$toilet_b <- ifelse(thailand$TOILET >10 & thailand$TOILET <25,1,0)

names(thailand)
gc()

#checking if the variables are available in both years
table(thailand$owner_b,thailand$YEAR)
table(thailand$eletric_b,thailand$YEAR)################
table(thailand$water_b,thailand$YEAR)
table(thailand$autos_b,thailand$YEAR)
table(thailand$refrig_b,thailand$YEAR)
table(thailand$tv_b,thailand$YEAR)
table(thailand$toilet_b,thailand$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
thailand$PUBl_ASSET <- ifelse(is.na(thailand$eletric_b), 0, thailand$eletric_b) +
  ifelse(is.na(thailand$water_b), 0, thailand$water_b)
thailand %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

thailand$PRIV_ASSET <- ifelse(is.na(thailand$owner_b), 0, thailand$owner_b) +
  ifelse(is.na(thailand$autos_b), 0, thailand$autos_b) +
  ifelse(is.na(thailand$refrig_b), 0, thailand$refrig_b) +
  ifelse(is.na(thailand$tv_b), 0, thailand$tv_b) +
  ifelse(is.na(thailand$toilet_b), 0, thailand$toilet_b) 
thailand %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

thailand$TOTAL_ASSET <- ifelse(is.na(thailand$owner_b), 0, thailand$owner_b) +
  ifelse(is.na(thailand$autos_b), 0, thailand$autos_b) +
  ifelse(is.na(thailand$refrig_b), 0, thailand$refrig_b) +
  ifelse(is.na(thailand$tv_b), 0, thailand$tv_b) +
  ifelse(is.na(thailand$toilet_b), 0, thailand$toilet_b) +
  ifelse(is.na(thailand$eletric_b), 0, thailand$eletric_b) +
  ifelse(is.na(thailand$water_b), 0, thailand$water_b)
thailand %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

thailand$TOTAL_ASSET_gini <- ifelse(is.na(thailand$owner_b), 0, thailand$owner_b) +
  ifelse(is.na(thailand$autos_b), 0, thailand$autos_b) +
  ifelse(is.na(thailand$refrig_b), 0, thailand$refrig_b) +
  ifelse(is.na(thailand$tv_b), 0, thailand$tv_b) +
  ifelse(is.na(thailand$toilet_b), 0, thailand$toilet_b) +
  ifelse(is.na(thailand$water_b), 0, thailand$water_b)
thailand %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

thailand$IPUM1990 <- as.integer(thailand$GEO2_TH1990)
thailand$IPUM2000 <- as.integer(thailand$GEO2_TH2000)
bangkok_90$IPUM1990 <- as.integer(bangkok_90$IPUM1990)
bangkok_00$IPUM2000 <- as.integer(bangkok_00$IPUM2000)

##Joining by year

bangkok_90 <- thailand %>% inner_join(bangkok_90, by="IPUM1990")
bangkok_00 <- thailand %>% inner_join(bangkok_00, by="IPUM2000")

names(bangkok_90)
names(bangkok_00)

bangkok_90 <- select(bangkok_90, -c(DIST1990))
bangkok_00 <- select(bangkok_00, -c(DIST2000))

##Merging all years into one table
bangkok_full <- rbind(bangkok_90,bangkok_00)
names(bangkok_full)

names(bangkok_full)

table(bangkok_full$YEAR)

bangkok_full <- bangkok_full %>%  filter (bangkok_full$YRSCHOOL < 90)
# 
bangkok_full <- bangkok_full %>%  filter (bangkok_full$AGE >15)
# 
bangkok_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(bangkok_full$YRSCHOOL)
summary(bangkok_full$YRSCHOOL)

bangkok_fu90 <- bangkok_full %>%  filter (YEAR==1990)
bangkok_fu00 <- bangkok_full %>%  filter (YEAR==2000)

Gini(bangkok_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(bangkok_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(bangkok_fu90 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(bangkok_fu00 $TOTAL_ASSET_gini,na.rm = TRUE)
## compute the Lorenz curves
Lc_bag90 <- Lc(bangkok_fu90$YRSCHOOL, n = rep(1,length(bangkok_fu90$YRSCHOOL)), plot = TRUE)
Lc_bag00 <- Lc(bangkok_fu00$YRSCHOOL, n = rep(1,length(bangkok_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_bag90,col='blue', main = "Lorenz Curve - Bangkok")
lines(Lc_bag00, col='red')

##Merging all years into one table
thailand_full <- thailand
names(thailand_full)

names(thailand_full)

table(thailand_full$YEAR)

thailand_full <- thailand_full %>%  filter (thailand_full$YRSCHOOL < 90)

thailand_full <- thailand_full %>%  filter (thailand_full$AGE >15)

thailand_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(thailand_full$YRSCHOOL)
summary(thailand_full$YRSCHOOL)

thailand_fu90 <- thailand_full %>%  filter (YEAR==1990)
thailand_fu00 <- thailand_full %>%  filter (YEAR==2000)

Gini(thailand_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(thailand_fu00 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves
Lc_bag90 <- Lc(thailand_fu90$YRSCHOOL, n = rep(1,length(thailand_fu90$YRSCHOOL)), plot = TRUE)
Lc_bag00 <- Lc(thailand_fu00$YRSCHOOL, n = rep(1,length(thailand_fu00$YRSCHOOL)), plot = TRUE)

plot(Lc_bag90,col='blue', main = "Lorenz Curve - thailand")
lines(Lc_bag00, col='red')


table(bangkok_full$OCCISCO)


bangkok_full$OCCISCO_b <- ifelse(bangkok_full$OCCISCO ==1|bangkok_full$OCCISCO ==2,1,0)
bangkok_full$OCCISCO_b <- ifelse(bangkok_full$OCCISCO ==3|bangkok_full$OCCISCO ==4|bangkok_full$OCCISCO ==5,2,bangkok_full$OCCISCO_b)
bangkok_full$OCCISCO_b <- ifelse(bangkok_full$OCCISCO ==6|bangkok_full$OCCISCO ==7|bangkok_full$OCCISCO ==8|bangkok_full$OCCISCO ==9,3,bangkok_full$OCCISCO_b)
table(bangkok_full$OCCISCO_b)

bangkok_full_OCCISCO_b <- bangkok_full %>% select(YEAR,OCCISCO_b,PERWT)
bangkok_full_OCCISCO_b <- bangkok_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(bangkok_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- bangkok_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- bangkok_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- bangkok_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- bangkok_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

bangkok_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
bangkok_OCCISCO_b<- bangkok_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
bangkok_OCCISCO_b<- bangkok_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

bangkok_OCCISCO_b

# for PUBl_ASSET
bangkok_PUBl_ASSET<-bangkok_full %>%  
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bangkok_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bangkok_PUBl_ASSET <- bangkok_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bangkok_PUBl_ASSET <- bangkok_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bangkok_PUBl_ASSET$CITY<-"bangkok"
# for PRIV_ASSET
bangkok_PRIV_ASSET<-bangkok_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bangkok_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bangkok_PRIV_ASSET <- bangkok_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bangkok_PRIV_ASSET <- bangkok_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bangkok_PRIV_ASSET$CITY<-"bangkok"
# for TOTAL_ASSET
bangkok_TOTAL_ASSET<-bangkok_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bangkok_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bangkok_TOTAL_ASSET <- bangkok_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bangkok_TOTAL_ASSET <- bangkok_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bangkok_TOTAL_ASSET$CITY<-"bangkok"
write.csv(bangkok_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bangkok_PUBl_ASSET.csv", row.names = TRUE)
write.csv(bangkok_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bangkok_PRIV_ASSET.csv", row.names = TRUE)
write.csv(bangkok_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bangkok_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

bangkok_WATER <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(bangkok_WATER$WATSUP)
bangkok_WATER <- bangkok_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(bangkok_WATER$WATSUP)
table(bangkok_WATER$water_b)

bangkok_WATER$water_b <- factor(bangkok_WATER$water_b)

bangkok_WATER$YEAR <- factor(bangkok_WATER$YEAR)
bangkok_WATER_01 <- bangkok_WATER %>% filter(bangkok_WATER$YEAR==1990)
bangkok_WATER_12 <- bangkok_WATER %>% filter(bangkok_WATER$YEAR==2000)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = bangkok_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = bangkok_WATER_12, family = "binomial")
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

bangkok_OWN <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(bangkok_OWN$OWNERSHIP)
bangkok_OWN <- bangkok_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(bangkok_OWN$OWNERSHIP)
table(bangkok_OWN$owner_b)

bangkok_OWN$owner_b <- factor(bangkok_OWN$owner_b)

bangkok_OWN$YEAR <- factor(bangkok_OWN$YEAR)
bangkok_OWN_01 <- bangkok_OWN %>% filter(bangkok_OWN$YEAR==1990)
bangkok_OWN_12 <- bangkok_OWN %>% filter(bangkok_OWN$YEAR==2000)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = bangkok_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = bangkok_OWN_12, family = "binomial")
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

bangkok_REFRIG <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(bangkok_REFRIG$REFRIG)
bangkok_REFRIG <- bangkok_REFRIG %>% filter(REFRIG!=0)

table(bangkok_REFRIG$REFRIG)
table(bangkok_REFRIG$refrig_b)

bangkok_REFRIG$refrig_b <- factor(bangkok_REFRIG$refrig_b)

bangkok_REFRIG$YEAR <- factor(bangkok_REFRIG$YEAR)
bangkok_REFRIG_01 <- bangkok_REFRIG %>% filter(bangkok_REFRIG$YEAR==1990)
bangkok_REFRIG_12 <- bangkok_REFRIG %>% filter(bangkok_REFRIG$YEAR==2000)

mylogit <- glm(refrig_b ~ YRSCHOOL + YEAR, data = bangkok_REFRIG, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = bangkok_REFRIG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = bangkok_REFRIG_12, family = "binomial")
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

bangkok_TOILET <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(bangkok_TOILET$TOILET)
bangkok_TOILET <- bangkok_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(bangkok_TOILET$TOILET)
table(bangkok_TOILET$toilet_b)

bangkok_TOILET$toilet_b <- factor(bangkok_TOILET$toilet_b)

bangkok_TOILET$YEAR <- factor(bangkok_TOILET$YEAR)
bangkok_TOILET_01 <- bangkok_TOILET %>% filter(bangkok_TOILET$YEAR==1990)
bangkok_TOILET_12 <- bangkok_TOILET %>% filter(bangkok_TOILET$YEAR==2000)

mylogit <- glm(toilet_b ~ YRSCHOOL + YEAR, data = bangkok_TOILET, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = bangkok_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = bangkok_TOILET_12, family = "binomial")
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

bangkok_ELECTRIC <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(bangkok_ELECTRIC$ELECTRIC)
bangkok_ELECTRIC <- bangkok_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(bangkok_ELECTRIC$ELECTRIC)
table(bangkok_ELECTRIC$eletric_b)

bangkok_ELECTRIC$eletric_b <- factor(bangkok_ELECTRIC$eletric_b)

bangkok_ELECTRIC$YEAR <- factor(bangkok_ELECTRIC$YEAR)
bangkok_ELECTRIC_01 <- bangkok_ELECTRIC %>% filter(bangkok_ELECTRIC$YEAR==1990)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = bangkok_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))


ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

bangkok_AUTOS <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(bangkok_AUTOS$AUTOS)
bangkok_AUTOS <- bangkok_AUTOS %>% filter(AUTOS!=0) %>% filter(AUTOS!=99)

table(bangkok_AUTOS$AUTOS)
table(bangkok_AUTOS$autos_b)

bangkok_AUTOS$autos_b <- factor(bangkok_AUTOS$autos_b)

bangkok_AUTOS$YEAR <- factor(bangkok_AUTOS$YEAR)
bangkok_AUTOS_01 <- bangkok_AUTOS %>% filter(bangkok_AUTOS$YEAR==1990)
bangkok_AUTOS_12 <- bangkok_AUTOS %>% filter(bangkok_AUTOS$YEAR==2000)

mylogit <- glm(autos_b ~ YRSCHOOL + YEAR, data = bangkok_AUTOS, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = bangkok_AUTOS_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = bangkok_AUTOS_12, family = "binomial")
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

#################################### FOR TV ##################################

bangkok_TV <- bangkok_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(bangkok_TV$TV)
bangkok_TV <- bangkok_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(bangkok_TV$TV)
table(bangkok_TV$tv_b)

bangkok_TV$tv_b <- factor(bangkok_TV$tv_b)

bangkok_TV$YEAR <- factor(bangkok_TV$YEAR)
bangkok_TV_01 <- bangkok_TV %>% filter(bangkok_TV$YEAR==1990)
bangkok_TV_12 <- bangkok_TV %>% filter(bangkok_TV$YEAR==2000)

mylogit <- glm(tv_b ~ YRSCHOOL + YEAR, data = bangkok_TV, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = bangkok_TV_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = bangkok_TV_12, family = "binomial")
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

bangkok_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,AUTOS_T2_z,AUTOS_T3_z,TV_T2_z,TV_T3_z) 
bangkok_logit$CITY <- "Bangkok"

THAILAND_logit <- rbind(bangkok_logit,sep = ".") 
write.csv(THAILAND_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/THAILAND_logit.csv"))



