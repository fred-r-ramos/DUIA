library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
library(splitstackshape)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/venezuela")

geo2_ve90 <- read_sf("geo2_ve1990.shp")
geo2_ve01 <- read_sf("geo2_ve2001.shp")
centroid_ve90 <- st_centroid(geo2_ve90)
centroid_ve01 <- st_centroid(geo2_ve01)
AUE_cabimas <- read_sf("Cabimas_studyArea.shp")
AUE_cabimas  <- st_transform(AUE_cabimas , 4326)

sf::sf_use_s2(FALSE)

cabimas_90 <- geo2_ve90[st_intersection(AUE_cabimas,centroid_ve90),]
cabimas_90['CITY']='cabimas'
cabimas_01 <- geo2_ve01[st_intersection(AUE_cabimas,centroid_ve01),]
cabimas_01['CITY']='cabimas'

plot(st_geometry(cabimas_01), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ve01[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00227.xml")
ddi <-read_ipums_ddi("ipumsi_00264.xml")
venezuela <- read_ipums_micro(ddi)

names(venezuela)

######################creating binary variables for logit regressions

names(venezuela)

table(venezuela$OWNERSHIP)
venezuela$owner_b <- ifelse(venezuela$OWNERSHIP ==1,1,0)
table(venezuela$ELECTRIC)
venezuela$eletric_b <- ifelse(venezuela$ELECTRIC ==1,1,0)
table(venezuela$WATSUP)
venezuela$water_b <- ifelse(venezuela$WATSUP==11|venezuela$WATSUP ==10,1,0)
table(venezuela$SEWAGE)
venezuela$sewage_b <- ifelse(venezuela$SEWAGE ==11|venezuela$SEWAGE ==12,1,0)
table(venezuela$TRASH)
venezuela$trash_b <- ifelse(venezuela$TRASH ==11|venezuela$TRASH ==12,1,0)
table(venezuela$AUTOS)
venezuela$autos_b <- ifelse(venezuela$AUTOS ==7,1,0)
table(venezuela$REFRIG)
venezuela$refrig_b <- ifelse(venezuela$REFRIG ==2,1,0)
table(venezuela$TV)
venezuela$tv_b <- ifelse(venezuela$TV ==20,1,0)
table(venezuela$TOILET)
venezuela$toilet_b <- ifelse(venezuela$TOILET >19 & venezuela$TOILET < 24 ,1,0)
table(venezuela$FLOOR)
venezuela$floor_b <- ifelse(venezuela$FLOOR>100 & venezuela$FLOOR<999,1,0)
table(venezuela$ROOF)
venezuela$roof_b <- ifelse(venezuela$ROOF>10 & venezuela$ROOF <30,1,0)

names(venezuela)
gc()

#checking if the variables are available in both years
table(venezuela$owner_b,venezuela$YEAR)
table(venezuela$eletric_b,venezuela$YEAR)
table(venezuela$water_b,venezuela$YEAR)
table(venezuela$sewage_b,venezuela$YEAR)
table(venezuela$trash_b,venezuela$YEAR)######
table(venezuela$autos_b,venezuela$YEAR)
table(venezuela$refrig_b,venezuela$YEAR)
table(venezuela$tv_b,venezuela$YEAR)
table(venezuela$toilet_b,venezuela$YEAR)
table(venezuela$floor_b,venezuela$YEAR)
table(venezuela$roof_b,venezuela$YEAR)##########

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
venezuela$PUBl_ASSET <- venezuela$eletric_b+venezuela$water_b+venezuela$sewage_b
venezuela %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

venezuela$PRIV_ASSET <- venezuela$owner_b+venezuela$autos_b+venezuela$refrig_b+venezuela$tv_b+venezuela$toilet_b+venezuela$floor_b
venezuela %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

venezuela$TOTAL_ASSET <- venezuela$PRIV_ASSET+venezuela$PUBl_ASSET
assets<-venezuela %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

venezuela$IPUM1990 <- as.integer(venezuela$GEO2_VE1990)
venezuela$IPUM2001 <- as.integer(venezuela$GEO2_VE2001)
cabimas_90$IPUM1990 <- as.integer(cabimas_90$IPUM1990)
cabimas_01$IPUM2001 <- as.integer(cabimas_01$IPUM2001)

##Joining by year

cabimas_90 <- venezuela %>% inner_join(cabimas_90, by="IPUM1990")
cabimas_01 <- venezuela %>% inner_join(cabimas_01, by="IPUM2001")

names(cabimas_90)
names(cabimas_01)

cabimas_90 <- select(cabimas_90, -c(MUNI1990))
cabimas_01 <- select(cabimas_01, -c(MUNI2001))

##Merging all years into one table
venezuela_full <- rbind(cabimas_90,cabimas_01)
names(venezuela_full)

##Excluding specific columns for the unifeied dataset
venezuela_full<- select(venezuela_full, -c(GEO2_VE1990,GEO2_VE2001,IPUM1990,IPUM2001,geometry))
table(venezuela_full$CITY)

names(venezuela_full)

table(venezuela_full$YEAR)

venezuela_full <- venezuela_full %>%  filter (venezuela_full$YRSCHOOL < 90)

venezuela_full <- venezuela_full %>%  filter (venezuela_full$AGE >15)

venezuela_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(venezuela_full$YRSCHOOL)
summary(venezuela_full$YRSCHOOL)
table(venezuela_full$CITY)

####cabimas 
cabimas_full <- venezuela_full %>%  filter (CITY=="cabimas")

summary(cabimas_full$YRSCHOOL)
summary(cabimas_full$AGE)

cabimas_fu90 <- cabimas_full %>%  filter (YEAR==1990)
cabimas_fu01 <- cabimas_full %>%  filter (YEAR==2001)

Gini(cabimas_fu90 $YRSCHOOL,na.rm = TRUE)
Gini(cabimas_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(cabimas_fu90 $TOTAL_ASSET,na.rm = TRUE)
Gini(cabimas_fu01 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_cab85 <- Lc(cabimas_fu90$YRSCHOOL, n = rep(1,length(cabimas_fu90$YRSCHOOL)), plot = TRUE)
Lc_cab01 <- Lc(cabimas_fu01$YRSCHOOL, n = rep(1,length(cabimas_fu01$YRSCHOOL)), plot = TRUE)

plot(Lc_cab85,col='blue', main = "Lorenz Curve - Cabimas")
lines(Lc_cab01, col='red')

table(venezuela_full$OCCISCO)


venezuela_full$OCCISCO_b <- ifelse(venezuela_full$OCCISCO ==1|venezuela_full$OCCISCO ==2,1,0)
venezuela_full$OCCISCO_b <- ifelse(venezuela_full$OCCISCO ==3|venezuela_full$OCCISCO ==4|venezuela_full$OCCISCO ==5,2,venezuela_full$OCCISCO_b)
venezuela_full$OCCISCO_b <- ifelse(venezuela_full$OCCISCO ==6|venezuela_full$OCCISCO ==7|venezuela_full$OCCISCO ==8|venezuela_full$OCCISCO ==9,3,venezuela_full$OCCISCO_b)
table(venezuela_full$OCCISCO_b)

venezuela_full_OCCISCO_b <- venezuela_full %>% select(YEAR,OCCISCO_b,PERWT)
venezuela_full_OCCISCO_b <- venezuela_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(venezuela_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- venezuela_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- venezuela_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- venezuela_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- venezuela_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

cabimas_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
cabimas_OCCISCO_b<- cabimas_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
cabimas_OCCISCO_b<- cabimas_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

cabimas_OCCISCO_b

# for PUBl_ASSET
cabimas_PUBl_ASSET<-cabimas_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cabimas_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cabimas_PUBl_ASSET <- cabimas_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cabimas_PUBl_ASSET <- cabimas_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cabimas_PUBl_ASSET$CITY<-"cabimas"
# for PRIV_ASSET
cabimas_PRIV_ASSET<-cabimas_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cabimas_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cabimas_PRIV_ASSET <- cabimas_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cabimas_PRIV_ASSET <- cabimas_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cabimas_PRIV_ASSET$CITY<-"cabimas"
# for TOTAL_ASSET
cabimas_TOTAL_ASSET<-cabimas_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cabimas_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cabimas_TOTAL_ASSET <- cabimas_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cabimas_TOTAL_ASSET <- cabimas_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cabimas_TOTAL_ASSET$CITY<-"cabimas"
write.csv(cabimas_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cabimas_PUBl_ASSET.csv", row.names = TRUE)
write.csv(cabimas_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cabimas_PRIV_ASSET.csv", row.names = TRUE)
write.csv(cabimas_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cabimas_TOTAL_ASSET.csv", row.names = TRUE)

################################### FOR WATSUP ##################################

cabimas_WATER <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(cabimas_WATER$WATSUP)
table(cabimas_WATER$HHWT)
cabimas_WATER <- cabimas_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(cabimas_WATER$WATSUP)
table(cabimas_WATER$water_b)

cabimas_WATER$water_b <- factor(cabimas_WATER$water_b)

cabimas_WATER$YEAR <- factor(cabimas_WATER$YEAR)
cabimas_WATER_01 <- cabimas_WATER %>% filter(cabimas_WATER$YEAR==1990)
cabimas_WATER_12 <- cabimas_WATER %>% filter(cabimas_WATER$YEAR==2001)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = cabimas_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = cabimas_WATER_12, family = "binomial",weights = HHWT)
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

cabimas_OWN <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(cabimas_OWN$OWNERSHIP)
cabimas_OWN <- cabimas_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(cabimas_OWN$OWNERSHIP)
table(cabimas_OWN$owner_b)

cabimas_OWN$owner_b <- factor(cabimas_OWN$owner_b)

cabimas_OWN$YEAR <- factor(cabimas_OWN$YEAR)
cabimas_OWN_01 <- cabimas_OWN %>% filter(cabimas_OWN$YEAR==1990)
cabimas_OWN_12 <- cabimas_OWN %>% filter(cabimas_OWN$YEAR==2001)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = cabimas_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = cabimas_OWN_12, family = "binomial",weights = HHWT)
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

cabimas_SEWG <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(cabimas_SEWG$SEWAGE)
cabimas_SEWG <- cabimas_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(cabimas_SEWG$SEWAGE)
table(cabimas_SEWG$sewage_b)

cabimas_SEWG$sewage_b <- factor(cabimas_SEWG$sewage_b)

cabimas_SEWG$YEAR <- factor(cabimas_SEWG$YEAR)
cabimas_SEWG_01 <- cabimas_SEWG %>% filter(cabimas_SEWG$YEAR==1990)
cabimas_SEWG_12 <- cabimas_SEWG %>% filter(cabimas_SEWG$YEAR==2001)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = cabimas_SEWG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(sewage_b ~ YRSCHOOL, data = cabimas_SEWG_12, family = "binomial",weights = HHWT)
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

cabimas_REFRIG <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(cabimas_REFRIG$REFRIG)
cabimas_REFRIG <- cabimas_REFRIG %>% filter(REFRIG!=0) %>% filter(REFRIG!=9)

table(cabimas_REFRIG$REFRIG)
table(cabimas_REFRIG$refrig_b)

cabimas_REFRIG$refrig_b <- factor(cabimas_REFRIG$refrig_b)

cabimas_REFRIG$YEAR <- factor(cabimas_REFRIG$YEAR)
cabimas_REFRIG_01 <- cabimas_REFRIG %>% filter(cabimas_REFRIG$YEAR==1990)
cabimas_REFRIG_12 <- cabimas_REFRIG %>% filter(cabimas_REFRIG$YEAR==2001)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = cabimas_REFRIG_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = cabimas_REFRIG_12, family = "binomial",weights = HHWT)
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

cabimas_TOILET <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(cabimas_TOILET$TOILET)
cabimas_TOILET <- cabimas_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(cabimas_TOILET$TOILET)
table(cabimas_TOILET$toilet_b)

cabimas_TOILET$toilet_b <- factor(cabimas_TOILET$toilet_b)

cabimas_TOILET$YEAR <- factor(cabimas_TOILET$YEAR)
cabimas_TOILET_01 <- cabimas_TOILET %>% filter(cabimas_TOILET$YEAR==1990)
cabimas_TOILET_12 <- cabimas_TOILET %>% filter(cabimas_TOILET$YEAR==2001)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = cabimas_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = cabimas_TOILET_12, family = "binomial",weights = HHWT)
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

cabimas_ELECTRIC <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(cabimas_ELECTRIC$ELECTRIC)
cabimas_ELECTRIC <- cabimas_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(cabimas_ELECTRIC$ELECTRIC)
table(cabimas_ELECTRIC$eletric_b)

cabimas_ELECTRIC$eletric_b <- factor(cabimas_ELECTRIC$eletric_b)

cabimas_ELECTRIC$YEAR <- factor(cabimas_ELECTRIC$YEAR)
cabimas_ELECTRIC_01 <- cabimas_ELECTRIC %>% filter(cabimas_ELECTRIC$YEAR==1990)
cabimas_ELECTRIC_12 <- cabimas_ELECTRIC %>% filter(cabimas_ELECTRIC$YEAR==2001)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = cabimas_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = cabimas_ELECTRIC_12, family = "binomial",weights = HHWT)
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

cabimas_TRASH <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(cabimas_TRASH$TRASH)
cabimas_TRASH <- cabimas_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(cabimas_TRASH$TRASH)
table(cabimas_TRASH$trash_b)

cabimas_TRASH$trash_b <- factor(cabimas_TRASH$trash_b)

cabimas_TRASH$YEAR <- factor(cabimas_TRASH$YEAR)
cabimas_TRASH_12 <- cabimas_TRASH %>% filter(cabimas_TRASH$YEAR==2001)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = cabimas_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

cabimas_AUTOS <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(cabimas_AUTOS$AUTOS)
cabimas_AUTOS <- cabimas_AUTOS %>% filter(AUTOS!=9)

table(cabimas_AUTOS$AUTOS)
table(cabimas_AUTOS$autos_b)

cabimas_AUTOS$autos_b <- factor(cabimas_AUTOS$autos_b)

cabimas_AUTOS$YEAR <- factor(cabimas_AUTOS$YEAR)
cabimas_AUTOS_01 <- cabimas_AUTOS %>% filter(cabimas_AUTOS$YEAR==1990)
cabimas_AUTOS_12 <- cabimas_AUTOS %>% filter(cabimas_AUTOS$YEAR==2001)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = cabimas_AUTOS_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = cabimas_AUTOS_12, family = "binomial",weights = HHWT)
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

cabimas_FLOOR <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(cabimas_FLOOR$FLOOR)
cabimas_FLOOR <- cabimas_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=999)

table(cabimas_FLOOR$FLOOR)
table(cabimas_FLOOR$floor_b)

cabimas_FLOOR$floor_b <- factor(cabimas_FLOOR$floor_b)

cabimas_FLOOR$YEAR <- factor(cabimas_FLOOR$YEAR)
cabimas_FLOOR_01 <- cabimas_FLOOR %>% filter(cabimas_FLOOR$YEAR==1990)
cabimas_FLOOR_12 <- cabimas_FLOOR %>% filter(cabimas_FLOOR$YEAR==2001)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = cabimas_FLOOR_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = cabimas_FLOOR_12, family = "binomial",weights = HHWT)
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

cabimas_ROOF <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(cabimas_ROOF$ROOF)
cabimas_ROOF <- cabimas_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(cabimas_ROOF$ROOF)
table(cabimas_ROOF$roof_b)

cabimas_ROOF$roof_b <- factor(cabimas_ROOF$roof_b)

cabimas_ROOF$YEAR <- factor(cabimas_ROOF$YEAR)
cabimas_ROOF_12 <- cabimas_ROOF %>% filter(cabimas_ROOF$YEAR==2001)

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = cabimas_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

cabimas_TV <- cabimas_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(cabimas_TV$TV)
cabimas_TV <- cabimas_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(cabimas_TV$TV)
table(cabimas_TV$tv_b)

cabimas_TV$tv_b <- factor(cabimas_TV$tv_b)

cabimas_TV$YEAR <- factor(cabimas_TV$YEAR)
cabimas_TV_01 <- cabimas_TV %>% filter(cabimas_TV$YEAR==1990)
cabimas_TV_12 <- cabimas_TV %>% filter(cabimas_TV$YEAR==2001)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = cabimas_TV_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = cabimas_TV_12, family = "binomial",weights = HHWT)
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

cabimas_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
cabimas_logit$CITY <- "Cabimas"

VENEZUELA_logit <- rbind(cabimas_logit,sep = ".") 
write.csv(VENEZUELA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/VENEZUELA_logit.csv"))

gc()
