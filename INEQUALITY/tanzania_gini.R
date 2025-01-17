library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/tanzania")

sf::sf_use_s2(FALSE)

geo2_tz88 <- read_sf("geo2_tz1988.shp")
geo2_tz02 <- read_sf("geo2_tz2002.shp")
geo2_tz12 <- read_sf("geo2_tz2012.shp")
centroid_tz88 <- st_centroid(geo2_tz88)
centroid_tz02 <- st_centroid(geo2_tz02)
centroid_tz12 <- st_centroid(geo2_tz12)
AUE_arusha <- read_sf("Arusha_studyArea.shp")
AUE_arusha  <- st_transform(AUE_arusha , 4326)

arusha_88 <- geo2_tz88[st_intersection(AUE_arusha,centroid_tz88),]
arusha_88['CITY']='arusha'
arusha_02 <- geo2_tz02[st_intersection(AUE_arusha,centroid_tz02),]
arusha_02['CITY']='arusha'
arusha_12 <- geo2_tz12[st_intersection(AUE_arusha,centroid_tz12),]
arusha_12['CITY']='arusha'

plot(st_geometry(arusha_12), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_tz12[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00220.xml")
ddi <-read_ipums_ddi("ipumsi_00262.xml")
tanzania <- read_ipums_micro(ddi)

names(tanzania)

######################creating binary variables for logit regressions

names(tanzania)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(tanzania$OWNERSHIP)
tanzania$owner_b <- ifelse(tanzania$OWNERSHIP ==1,1,0)
table(tanzania$ELECTRIC)
tanzania$eletric_b <- ifelse(tanzania$ELECTRIC ==1,1,0)
table(tanzania$WATSUP)
tanzania$water_b <- ifelse(tanzania$WATSUP ==10|tanzania$WATSUP ==11,1,0)
table(tanzania$TRASH)
tanzania$trash_b <- ifelse(tanzania$TRASH ==10,1,0)
table(tanzania$AUTOS)
tanzania$autos_b <- ifelse(tanzania$AUTOS ==7,1,0)
table(tanzania$REFRIG)
tanzania$refrig_b <- ifelse(tanzania$REFRIG ==2,1,0)
table(tanzania$TV)
tanzania$tv_b <- ifelse(tanzania$TV ==20,1,0)
table(tanzania$TOILET)
tanzania$toilet_b <- ifelse(tanzania$TOILET>19,1,0)
table(tanzania$FLOOR)
tanzania$floor_b <- ifelse(tanzania$FLOOR >120,1,0)
table(tanzania$ROOF)
tanzania$roof_b <- ifelse(tanzania$ROOF==11|tanzania$ROOF==14|tanzania$ROOF==34|tanzania$ROOF==28,1,0)

names(tanzania)
gc()

#checking if the variables are available in both years
table(tanzania$owner_b,tanzania$YEAR)###########
table(tanzania$eletric_b,tanzania$YEAR)
table(tanzania$water_b,tanzania$YEAR)
table(tanzania$trash_b,tanzania$YEAR)#########
table(tanzania$autos_b,tanzania$YEAR)##############
table(tanzania$refrig_b,tanzania$YEAR)##########
table(tanzania$tv_b,tanzania$YEAR)#########
table(tanzania$toilet_b,tanzania$YEAR)
table(tanzania$floor_b,tanzania$YEAR)
table(tanzania$roof_b,tanzania$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
tanzania$PUBl_ASSET <- ifelse(is.na(tanzania$eletric_b), 0, tanzania$eletric_b) +
  ifelse(is.na(tanzania$water_b), 0, tanzania$water_b)
tanzania %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

tanzania$PRIV_ASSET <- ifelse(is.na(tanzania$floor_b), 0, tanzania$floor_b) +
  ifelse(is.na(tanzania$roof_b), 0, tanzania$roof_b) +
ifelse(is.na(tanzania$toilet_b), 0, tanzania$toilet_b)
tanzania %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

tanzania$TOTAL_ASSET <-  ifelse(is.na(tanzania$floor_b), 0, tanzania$floor_b) +
  ifelse(is.na(tanzania$roof_b), 0, tanzania$roof_b) +
  ifelse(is.na(tanzania$toilet_b), 0, tanzania$toilet_b) +
  ifelse(is.na(tanzania$eletric_b), 0, tanzania$eletric_b) +
  ifelse(is.na(tanzania$water_b), 0, tanzania$water_b)
assets<-tanzania %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

tanzania$IPUM1988 <- as.integer(tanzania$GEO2_TZ1988)
tanzania$IPUM2002 <- as.integer(tanzania$GEO2_TZ2002)
tanzania$IPUM2012 <- as.integer(tanzania$GEO2_TZ2012)
arusha_88$IPUM1988 <- as.integer(arusha_88$IPUM1988)
arusha_02$IPUM2002 <- as.integer(arusha_02$IPUM2002)
arusha_12$IPUM2012 <- as.integer(arusha_12$IPUM2012)

##Joining by year

arusha_88 <- tanzania %>% inner_join(arusha_88, by="IPUM1988")
arusha_02 <- tanzania %>% inner_join(arusha_02, by="IPUM2002")
arusha_12 <- tanzania %>% inner_join(arusha_12, by="IPUM2012")

names(arusha_88)
names(arusha_02)
names(arusha_12)

arusha_88 <- select(arusha_88, -c(DIST1988))
arusha_02 <- select(arusha_02, -c(DIST2002))
arusha_12 <- select(arusha_12, -c(DIST2012))

##Merging all years into one table

# tanzania_full <- tanzania %>%  filter (tanzania$YRSCHOOL < 90)
# 
# tanzania_full <- tanzania_full %>%  filter (tanzania_full$AGE >15)
# 
# tanzania_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(tanzania_full$YRSCHOOL)
summary(tanzania_full$YRSCHOOL)

tanzania_fu88 <- tanzania_full %>%  filter (YEAR==1988)
tanzania_fu02 <- tanzania_full %>%  filter (YEAR==2002)
tanzania_fu12 <- tanzania_full %>%  filter (YEAR==2012)
Gini(tanzania_fu88 $YRSCHOOL,na.rm = TRUE)
Gini(tanzania_fu02 $YRSCHOOL,na.rm = TRUE)
Gini(tanzania_fu12 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves
Lc_taz88 <- Lc(tanzania_fu88$YRSCHOOL, n = rep(1,length(tanzania_fu88$YRSCHOOL)), plot = TRUE)
Lc_taz02 <- Lc(tanzania_fu02$YRSCHOOL, n = rep(1,length(tanzania_fu02$YRSCHOOL)), plot = TRUE)
Lc_taz12 <- Lc(tanzania_fu12$YRSCHOOL, n = rep(1,length(tanzania_fu12$YRSCHOOL)), plot = TRUE)

plot(Lc_taz88,col='blue', main = "Lorenz Curve - Tanzania")
lines(Lc_taz02, col='red')
lines(Lc_taz12, col='green')


arusha_full <- rbind(arusha_88,arusha_02,arusha_12)
names(arusha_full)
arusha_full <- arusha_full %>% filter(YEAR > 1990)
table(arusha_full$YEAR)

arusha_full <- arusha_full %>%  filter (arusha_full$YRSCHOOL < 90)
# 
arusha_full <- arusha_full %>%  filter (arusha_full$AGE >15)
# 
arusha_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(arusha_full$YRSCHOOL)
summary(arusha_full$YRSCHOOL)

arusha_fu88 <- arusha_full %>%  filter (YEAR==1988)
arusha_fu02 <- arusha_full %>%  filter (YEAR==2002)
arusha_fu12 <- arusha_full %>%  filter (YEAR==2012)
Gini(arusha_fu88 $YRSCHOOL,na.rm = TRUE)
Gini(arusha_fu02 $YRSCHOOL,na.rm = TRUE)
Gini(arusha_fu12 $YRSCHOOL,na.rm = TRUE)
Gini(arusha_fu02 $TOTAL_ASSET,na.rm = TRUE)
Gini(arusha_fu12 $TOTAL_ASSET,na.rm = TRUE)


## compute the Lorenz curves
Lc_aru88 <- Lc(arusha_fu88$YRSCHOOL, n = rep(1,length(arusha_fu88$YRSCHOOL)), plot = TRUE)
Lc_aru02 <- Lc(arusha_fu02$YRSCHOOL, n = rep(1,length(arusha_fu02$YRSCHOOL)), plot = TRUE)
Lc_aru12 <- Lc(arusha_fu12$YRSCHOOL, n = rep(1,length(arusha_fu12$YRSCHOOL)), plot = TRUE)
plot(Lc_aru88,col='blue', main = "Lorenz Curve - Arusha")
lines(Lc_aru02, col='red')
lines(Lc_aru12, col='green')


table(tanzania_full$OCCISCO)


tanzania_full$OCCISCO_b <- ifelse(tanzania_full$OCCISCO ==1|tanzania_full$OCCISCO ==2,1,0)
tanzania_full$OCCISCO_b <- ifelse(tanzania_full$OCCISCO ==3|tanzania_full$OCCISCO ==4|tanzania_full$OCCISCO ==5,2,tanzania_full$OCCISCO_b)
tanzania_full$OCCISCO_b <- ifelse(tanzania_full$OCCISCO ==6|tanzania_full$OCCISCO ==7|tanzania_full$OCCISCO ==8|tanzania_full$OCCISCO ==9,3,tanzania_full$OCCISCO_b)
table(tanzania_full$OCCISCO_b)

tanzania_full_OCCISCO_b <- tanzania_full %>% select(YEAR,OCCISCO_b,PERWT)
tanzania_full_OCCISCO_b <- tanzania_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(tanzania_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- tanzania_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- tanzania_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- tanzania_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- tanzania_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

arusha_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
arusha_OCCISCO_b<- arusha_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
arusha_OCCISCO_b<- arusha_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

arusha_OCCISCO_b

# for PUBl_ASSET
arusha_PUBl_ASSET<-arusha_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- arusha_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
arusha_PUBl_ASSET <- arusha_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
arusha_PUBl_ASSET <- arusha_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
arusha_PUBl_ASSET$CITY<-"arusha"
# for PRIV_ASSET
arusha_PRIV_ASSET<-arusha_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- arusha_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
arusha_PRIV_ASSET <- arusha_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
arusha_PRIV_ASSET <- arusha_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
arusha_PRIV_ASSET$CITY<-"arusha"
# for TOTAL_ASSET
arusha_TOTAL_ASSET<-arusha_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- arusha_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
arusha_TOTAL_ASSET <- arusha_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
arusha_TOTAL_ASSET <- arusha_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
arusha_TOTAL_ASSET$CITY<-"arusha"
write.csv(arusha_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/arusha_PUBl_ASSET.csv", row.names = TRUE)
write.csv(arusha_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/arusha_PRIV_ASSET.csv", row.names = TRUE)
write.csv(arusha_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/arusha_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

arusha_WATER <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(arusha_WATER$WATSUP)
table(arusha_WATER$HHWT)
arusha_WATER <- arusha_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(arusha_WATER$WATSUP)
table(arusha_WATER$water_b)

arusha_WATER$water_b <- factor(arusha_WATER$water_b)

arusha_WATER$YEAR <- factor(arusha_WATER$YEAR)
arusha_WATER_01 <- arusha_WATER %>% filter(arusha_WATER$YEAR==2002)
arusha_WATER_12 <- arusha_WATER %>% filter(arusha_WATER$YEAR==2012)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = arusha_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = arusha_WATER_12, family = "binomial",weights = HHWT)
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

arusha_OWN <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(arusha_OWN$OWNERSHIP)
arusha_OWN <- arusha_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(arusha_OWN$OWNERSHIP)
table(arusha_OWN$owner_b)

arusha_OWN$owner_b <- factor(arusha_OWN$owner_b)

arusha_OWN$YEAR <- factor(arusha_OWN$YEAR)
arusha_OWN_12 <- arusha_OWN %>% filter(arusha_OWN$YEAR==2012)

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = arusha_OWN_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

arusha_REFRIG <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(arusha_REFRIG$REFRIG)
arusha_REFRIG <- arusha_REFRIG %>% filter(REFRIG!=0)

table(arusha_REFRIG$REFRIG)
table(arusha_REFRIG$refrig_b)

arusha_REFRIG$refrig_b <- factor(arusha_REFRIG$refrig_b)

arusha_REFRIG$YEAR <- factor(arusha_REFRIG$YEAR)
arusha_REFRIG_12 <- arusha_REFRIG %>% filter(arusha_REFRIG$YEAR==2012)

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = arusha_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

arusha_TOILET <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(arusha_TOILET$TOILET)
arusha_TOILET <- arusha_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(arusha_TOILET$TOILET)
table(arusha_TOILET$toilet_b)

arusha_TOILET$toilet_b <- factor(arusha_TOILET$toilet_b)

arusha_TOILET$YEAR <- factor(arusha_TOILET$YEAR)
arusha_TOILET_01 <- arusha_TOILET %>% filter(arusha_TOILET$YEAR==2002)
arusha_TOILET_12 <- arusha_TOILET %>% filter(arusha_TOILET$YEAR==2012)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = arusha_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = arusha_TOILET_12, family = "binomial",weights = HHWT)
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

arusha_ELECTRIC <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(arusha_ELECTRIC$ELECTRIC)
arusha_ELECTRIC <- arusha_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(arusha_ELECTRIC$ELECTRIC)
table(arusha_ELECTRIC$eletric_b)

arusha_ELECTRIC$eletric_b <- factor(arusha_ELECTRIC$eletric_b)

arusha_ELECTRIC$YEAR <- factor(arusha_ELECTRIC$YEAR)
arusha_ELECTRIC_01 <- arusha_ELECTRIC %>% filter(arusha_ELECTRIC$YEAR==2002)
arusha_ELECTRIC_12 <- arusha_ELECTRIC %>% filter(arusha_ELECTRIC$YEAR==2012)

mylogit <- glm(eletric_b ~ YRSCHOOL + YEAR, data = arusha_ELECTRIC, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = arusha_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = arusha_ELECTRIC_12, family = "binomial")
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

arusha_TRASH <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(arusha_TRASH$TRASH)
arusha_TRASH <- arusha_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(arusha_TRASH$TRASH)
table(arusha_TRASH$trash_b)

arusha_TRASH$trash_b <- factor(arusha_TRASH$trash_b)

arusha_TRASH$YEAR <- factor(arusha_TRASH$YEAR)
arusha_TRASH_12 <- arusha_TRASH %>% filter(arusha_TRASH$YEAR==2012)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = arusha_TRASH_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

arusha_AUTOS <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(arusha_AUTOS$AUTOS)
arusha_AUTOS <- arusha_AUTOS %>% filter(AUTOS!=99)

table(arusha_AUTOS$AUTOS)
table(arusha_AUTOS$autos_b)

arusha_AUTOS$autos_b <- factor(arusha_AUTOS$autos_b)

arusha_AUTOS$YEAR <- factor(arusha_AUTOS$YEAR)
arusha_AUTOS_12 <- arusha_AUTOS %>% filter(arusha_AUTOS$YEAR==2012)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = arusha_AUTOS_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

arusha_FLOOR <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(arusha_FLOOR$FLOOR)
arusha_FLOOR <- arusha_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(arusha_FLOOR$FLOOR)
table(arusha_FLOOR$floor_b)

arusha_FLOOR$floor_b <- factor(arusha_FLOOR$floor_b)

arusha_FLOOR$YEAR <- factor(arusha_FLOOR$YEAR)
arusha_FLOOR_01 <- arusha_FLOOR %>% filter(arusha_FLOOR$YEAR==2002)
arusha_FLOOR_12 <- arusha_FLOOR %>% filter(arusha_FLOOR$YEAR==2012)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = arusha_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = arusha_FLOOR_12, family = "binomial",weights = HHWT)
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

arusha_ROOF <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(arusha_ROOF$ROOF)
arusha_ROOF <- arusha_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(arusha_ROOF$ROOF)
table(arusha_ROOF$roof_b)

arusha_ROOF$roof_b <- factor(arusha_ROOF$roof_b)

arusha_ROOF$YEAR <- factor(arusha_ROOF$YEAR)
arusha_ROOF_01 <- arusha_ROOF %>% filter(arusha_ROOF$YEAR==2002)
arusha_ROOF_12 <- arusha_ROOF %>% filter(arusha_ROOF$YEAR==2012)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = arusha_ROOF_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = arusha_ROOF_12, family = "binomial",weights = HHWT)
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

arusha_TV <- arusha_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(arusha_TV$TV)
arusha_TV <- arusha_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(arusha_TV$TV)
table(arusha_TV$tv_b)

arusha_TV$tv_b <- factor(arusha_TV$tv_b)

arusha_TV$YEAR <- factor(arusha_TV$YEAR)
arusha_TV_12 <- arusha_TV %>% filter(arusha_TV$YEAR==2012)

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = arusha_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

arusha_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T3_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T3_z) 
arusha_logit$CITY <- "Arusha"

TANZANIA_logit <- rbind(arusha_logit,sep = ".") 
write.csv(TANZANIA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/TANZANIA_logit.csv"))




