library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/rwanda")

sf::sf_use_s2(FALSE)

geo2_rw91 <- read_sf("geo2_rw1991.shp")
geo2_rw02 <- read_sf("geo2_rw2002.shp")
geo2_rw12 <- read_sf("geo2_rw2012.shp")

geo2_rw12$IPUM2012 <-geo2_rw12$IPUM2002
geo2_rw12$PROV2012 <-geo2_rw12$PROV2002
geo2_rw12 <- select(geo2_rw12,-IPUM2002)
geo2_rw12 <- select(geo2_rw12,-PROV2002)

centroid_rw91 <- st_centroid(geo2_rw91)
centroid_rw02 <- st_centroid(geo2_rw02)
centroid_rw12 <- st_centroid(geo2_rw12)
AUE_kigali <- read_sf("Kigali_studyArea.shp")
AUE_kigali  <- st_transform(AUE_kigali , 4326)


kigali_91 <- geo2_rw91[st_intersection(AUE_kigali,centroid_rw91),]
kigali_91['CITY']='kigali'
kigali_02 <- geo2_rw02[st_intersection(AUE_kigali,centroid_rw02),]
kigali_02['CITY']='kigali'
kigali_12 <- geo2_rw12[st_intersection(AUE_kigali,centroid_rw12),]
kigali_12['CITY']='kigali'

plot(st_geometry(kigali_12), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_rw12[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00233.xml")
ddi<- read_ipums_ddi("ipumsi_00260.xml")
rwanda <- read_ipums_micro(ddi)

names(rwanda)

######################creating binary variables for logit regressions

names(rwanda)

table(rwanda$OWNERSHIP)
rwanda$owner_b <- ifelse(rwanda$OWNERSHIP ==1,1,0)
table(rwanda$ELECTRIC)
rwanda$eletric_b <- ifelse(rwanda$ELECTRIC ==1,1,0)
table(rwanda$WATSUP)
rwanda$water_b <- ifelse(rwanda$WATSUP ==11,1,0)
table(rwanda$SEWAGE)
rwanda$sewage_b <- ifelse(rwanda$SEWAGE ==11|rwanda$SEWAGE ==12,1,0)
table(rwanda$TRASH)
rwanda$trash_b <- ifelse(rwanda$TRASH ==11|rwanda$TRASH ==12,1,0)
table(rwanda$AUTOS)
rwanda$autos_b <- ifelse(rwanda$AUTOS ==1|rwanda$AUTOS ==2|rwanda$AUTOS ==3|rwanda$AUTOS ==4,1,0)
table(rwanda$REFRIG)
rwanda$refrig_b <- ifelse(rwanda$REFRIG ==2,1,0)
table(rwanda$TV)
rwanda$tv_b <- ifelse(rwanda$TV ==20|rwanda$TV ==21|rwanda$TV ==22|rwanda$TV ==23,1,0)
table(rwanda$TOILET)
rwanda$toilet_b <- ifelse(rwanda$TOILET==21|rwanda$TOILET==22|rwanda$TOILET==23,1,0)
table(rwanda$FLOOR)
rwanda$floor_b <- ifelse(rwanda$FLOOR==202|rwanda$FLOOR==205|rwanda$FLOOR==207|rwanda$FLOOR==208|rwanda$FLOOR==209|rwanda$FLOOR==213,1,0)
table(rwanda$ROOF)
rwanda$roof_b <- ifelse(rwanda$ROOF==11|rwanda$ROOF==14|rwanda$ROOF==17|rwanda$ROOF==18|rwanda$ROOF==28,1,0)

names(rwanda)
gc()

#checking if the variables are available in both years
table(rwanda$owner_b,rwanda$YEAR)
table(rwanda$eletric_b,rwanda$YEAR)
table(rwanda$water_b,rwanda$YEAR)
table(rwanda$sewage_b,rwanda$YEAR)########
table(rwanda$trash_b,rwanda$YEAR)
table(rwanda$refrig_b,rwanda$YEAR)##########
table(rwanda$tv_b,rwanda$YEAR)
table(rwanda$toilet_b,rwanda$YEAR)
table(rwanda$floor_b,rwanda$YEAR)
table(rwanda$roof_b,rwanda$YEAR)
table(rwanda$autos_b,rwanda$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
rwanda$PUBl_ASSET <- ifelse(is.na(rwanda$eletric_b), 0, rwanda$eletric_b) +
  ifelse(is.na(rwanda$water_b), 0, rwanda$water_b)+
  ifelse(is.na(rwanda$trash_b), 0, rwanda$trash_b)
rwanda %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

rwanda$PRIV_ASSET <- ifelse(is.na(rwanda$owner_b), 0, rwanda$owner_b) +
  ifelse(is.na(rwanda$tv_b), 0, rwanda$tv_b)+
  ifelse(is.na(rwanda$toilet_b), 0, rwanda$toilet_b)+
  ifelse(is.na(rwanda$floor_b), 0, rwanda$floor_b)+
  ifelse(is.na(rwanda$roof_b), 0, rwanda$roof_b)+
  ifelse(is.na(rwanda$autos_b), 0, rwanda$autos_b)
rwanda %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

rwanda$TOTAL_ASSET <- ifelse(is.na(rwanda$eletric_b), 0, rwanda$eletric_b) +
  ifelse(is.na(rwanda$water_b), 0, rwanda$water_b)+
  ifelse(is.na(rwanda$trash_b), 0, rwanda$trash_b)+
  ifelse(is.na(rwanda$owner_b), 0, rwanda$owner_b) +
  ifelse(is.na(rwanda$tv_b), 0, rwanda$tv_b)+
  ifelse(is.na(rwanda$toilet_b), 0, rwanda$toilet_b)+
  ifelse(is.na(rwanda$floor_b), 0, rwanda$floor_b)+
  ifelse(is.na(rwanda$roof_b), 0, rwanda$roof_b)+
  ifelse(is.na(rwanda$autos_b), 0, rwanda$autos_b)
assets<-rwanda %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

rwanda$TOTAL_ASSET_gini <- ifelse(is.na(rwanda$eletric_b), 0, rwanda$eletric_b) +
  ifelse(is.na(rwanda$water_b), 0, rwanda$water_b)+
  ifelse(is.na(rwanda$owner_b), 0, rwanda$owner_b) +
  ifelse(is.na(rwanda$toilet_b), 0, rwanda$toilet_b)+
  ifelse(is.na(rwanda$floor_b), 0, rwanda$floor_b)+
  ifelse(is.na(rwanda$roof_b), 0, rwanda$roof_b)
assets<-rwanda %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))


##Creating field for join

rwanda$IPUM1991 <- as.integer(rwanda$GEO2_RW1991)
rwanda$IPUM2002 <- as.integer(rwanda$GEO2_RW2002)
rwanda$IPUM2012 <- as.integer(rwanda$GEO2_RW2012)
kigali_91$IPUM1991 <- as.integer(kigali_91$IPUM1991)
kigali_02$IPUM2002 <- as.integer(kigali_02$IPUM2002)
kigali_12$IPUM2012 <- as.integer(kigali_12$IPUM2012)


table(rwanda$IPUM2012)


####fixing the error in geo2level data in ipums
kigali_12[1, 6] = 1012
kigali_12[2, 6] = 1011
kigali_12[3, 6] = 1013

##Joining by year

kigali_91 <- rwanda %>% inner_join(kigali_91, by="IPUM1991")
kigali_02 <- rwanda %>% inner_join(kigali_02, by="IPUM2002")
kigali_12 <- rwanda %>% inner_join(kigali_12, by="IPUM2012")
names(kigali_91)
names(kigali_02)
names(kigali_12)

kigali_91 <- select(kigali_91, -c(COMM1991))
kigali_02 <- select(kigali_02, -c(DIST2002))
kigali_12 <- select(kigali_12, -c(PROV2012))

##Merging all years into one table
rwanda_full <- rbind(kigali_91,kigali_02,kigali_12)
rwanda_full <- select(rwanda_full, -c(GEO2_RW1991,GEO2_RW2002,GEO2_RW2012,geometry))
names(rwanda_full)

####kigali
kigali_full <- rwanda_full %>%  filter (CITY=="kigali")

summary(kigali_full$YRSCHOOL)
summary(kigali_full$AGE)

# kigali_full <- kigali_full %>%  filter (kigali_full$YRSCHOOL < 90)
# 
# kigali_full <- kigali_full %>%  filter (kigali_full$AGE >15)
# 
# kigali_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))


kigali_fu02 <- kigali_full %>%  filter (YEAR==2002)
kigali_fu12 <- kigali_full %>%  filter (YEAR==2012)
kigali_fu91 <- kigali_full %>%  filter (YEAR==1991)

Gini(kigali_fu02$YRSCHOOL,na.rm = TRUE)
Gini(kigali_fu12$YRSCHOOL,na.rm = TRUE)
Gini(kigali_fu02$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(kigali_fu12$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(kigali_fu91$TOTAL_ASSET_gini,na.rm = TRUE)



## compute the Lorenz curves
Lc_kig02 <- Lc(kigali_fu02$YRSCHOOL, n = rep(1,length(kigali_fu02$YRSCHOOL)), plot = TRUE)
Lc_kig12 <- Lc(kigali_fu12$YRSCHOOL, n = rep(1,length(kigali_fu12$YRSCHOOL)), plot = TRUE)

plot(Lc_kig02,col='blue', main = "Lorenz Curve - Kigali")
lines(Lc_kig12, col='red')

table(kigali_full$OCCISCO)


kigali_full$OCCISCO_b <- ifelse(kigali_full$OCCISCO ==1|kigali_full$OCCISCO ==2,1,0)
kigali_full$OCCISCO_b <- ifelse(kigali_full$OCCISCO ==3|kigali_full$OCCISCO ==4|kigali_full$OCCISCO ==5,2,kigali_full$OCCISCO_b)
kigali_full$OCCISCO_b <- ifelse(kigali_full$OCCISCO ==6|kigali_full$OCCISCO ==7|kigali_full$OCCISCO ==8|kigali_full$OCCISCO ==9,3,kigali_full$OCCISCO_b)
table(kigali_full$OCCISCO_b)

kigali_full_OCCISCO_b <- kigali_full %>% select(YEAR,OCCISCO_b,PERWT)
kigali_full_OCCISCO_b <- kigali_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(kigali_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- kigali_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- kigali_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- kigali_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- kigali_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

KIGALI_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
KIGALI_OCCISCO_b<- KIGALI_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
KIGALI_OCCISCO_b<- KIGALI_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

KIGALI_OCCISCO_b

# for PUBl_ASSET
kigali_PUBl_ASSET<-kigali_full %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kigali_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kigali_PUBl_ASSET <- kigali_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kigali_PUBl_ASSET <- kigali_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kigali_PUBl_ASSET$CITY<-"kigali"
# for PRIV_ASSET
kigali_PRIV_ASSET<-kigali_full %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kigali_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kigali_PRIV_ASSET <- kigali_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kigali_PRIV_ASSET <- kigali_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kigali_PRIV_ASSET$CITY<-"kigali"
# for TOTAL_ASSET
kigali_TOTAL_ASSET<-kigali_full %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- kigali_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
kigali_TOTAL_ASSET <- kigali_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
kigali_TOTAL_ASSET <- kigali_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
kigali_TOTAL_ASSET$CITY<-"kigali"
write.csv(kigali_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kigali_PUBl_ASSET.csv", row.names = TRUE)
write.csv(kigali_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kigali_PRIV_ASSET.csv", row.names = TRUE)
write.csv(kigali_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/kigali_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

kigali_WATER <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(kigali_WATER$WATSUP)
table(kigali_WATER$HHWT)
kigali_WATER <- kigali_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(kigali_WATER$WATSUP)
table(kigali_WATER$water_b)

kigali_WATER$water_b <- factor(kigali_WATER$water_b)

kigali_WATER$YEAR <- factor(kigali_WATER$YEAR)
kigali_WATER_01 <- kigali_WATER %>% filter(kigali_WATER$YEAR==2002)
kigali_WATER_12 <- kigali_WATER %>% filter(kigali_WATER$YEAR==2012)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = kigali_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = kigali_WATER_12, family = "binomial")
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

kigali_OWN <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(kigali_OWN$OWNERSHIP)
kigali_OWN <- kigali_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(kigali_OWN$OWNERSHIP)
table(kigali_OWN$owner_b)

kigali_OWN$owner_b <- factor(kigali_OWN$owner_b)

kigali_OWN$YEAR <- factor(kigali_OWN$YEAR)
kigali_OWN_01 <- kigali_OWN %>% filter(kigali_OWN$YEAR==2002)
kigali_OWN_12 <- kigali_OWN %>% filter(kigali_OWN$YEAR==2012)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = kigali_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = kigali_OWN_12, family = "binomial")
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

kigali_SEWG <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(kigali_SEWG$SEWAGE)
kigali_SEWG <- kigali_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(kigali_SEWG$SEWAGE)
table(kigali_SEWG$sewage_b)

kigali_SEWG$sewage_b <- factor(kigali_SEWG$sewage_b)

kigali_SEWG$YEAR <- factor(kigali_SEWG$YEAR)
kigali_SEWG_12 <- kigali_SEWG %>% filter(kigali_SEWG$YEAR==2012)

mylogit_12 <- glm(sewage_b ~ YRSCHOOL, data = kigali_SEWG_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

kigali_REFRIG <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(kigali_REFRIG$REFRIG)
kigali_REFRIG <- kigali_REFRIG %>% filter(REFRIG!=0)

table(kigali_REFRIG$REFRIG)
table(kigali_REFRIG$refrig_b)

kigali_REFRIG$refrig_b <- factor(kigali_REFRIG$refrig_b)

kigali_REFRIG$YEAR <- factor(kigali_REFRIG$YEAR)
kigali_REFRIG_12 <- kigali_REFRIG %>% filter(kigali_REFRIG$YEAR==2012)

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = kigali_REFRIG_12, family = "binomial")
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

kigali_TOILET <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(kigali_TOILET$TOILET)
kigali_TOILET <- kigali_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(kigali_TOILET$TOILET)
table(kigali_TOILET$toilet_b)

kigali_TOILET$toilet_b <- factor(kigali_TOILET$toilet_b)

kigali_TOILET$YEAR <- factor(kigali_TOILET$YEAR)
kigali_TOILET_01 <- kigali_TOILET %>% filter(kigali_TOILET$YEAR==2002)
kigali_TOILET_12 <- kigali_TOILET %>% filter(kigali_TOILET$YEAR==2012)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = kigali_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = kigali_TOILET_12, family = "binomial")
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

kigali_ELECTRIC <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(kigali_ELECTRIC$ELECTRIC)
kigali_ELECTRIC <- kigali_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(kigali_ELECTRIC$ELECTRIC)
table(kigali_ELECTRIC$eletric_b)

kigali_ELECTRIC$eletric_b <- factor(kigali_ELECTRIC$eletric_b)

kigali_ELECTRIC$YEAR <- factor(kigali_ELECTRIC$YEAR)
kigali_ELECTRIC_01 <- kigali_ELECTRIC %>% filter(kigali_ELECTRIC$YEAR==2002)
kigali_ELECTRIC_12 <- kigali_ELECTRIC %>% filter(kigali_ELECTRIC$YEAR==2012)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = kigali_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = kigali_ELECTRIC_12, family = "binomial")
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

kigali_TRASH <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(kigali_TRASH$TRASH)
kigali_TRASH <- kigali_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(kigali_TRASH$TRASH)
table(kigali_TRASH$trash_b)

kigali_TRASH$trash_b <- factor(kigali_TRASH$trash_b)

kigali_TRASH$YEAR <- factor(kigali_TRASH$YEAR)
kigali_TRASH_01 <- kigali_TRASH %>% filter(kigali_TRASH$YEAR==2002)
kigali_TRASH_12 <- kigali_TRASH %>% filter(kigali_TRASH$YEAR==2012)

mylogit_01 <- glm(trash_b ~ YRSCHOOL, data = kigali_TRASH_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = kigali_TRASH_12, family = "binomial")
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

kigali_AUTOS <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(kigali_AUTOS$AUTOS)
kigali_AUTOS <- kigali_AUTOS %>% filter(AUTOS!=99)

table(kigali_AUTOS$AUTOS)
table(kigali_AUTOS$autos_b)

kigali_AUTOS$autos_b <- factor(kigali_AUTOS$autos_b)

kigali_AUTOS$YEAR <- factor(kigali_AUTOS$YEAR)
kigali_AUTOS_01 <- kigali_AUTOS %>% filter(kigali_AUTOS$YEAR==2002)
kigali_AUTOS_12 <- kigali_AUTOS %>% filter(kigali_AUTOS$YEAR==2012)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = kigali_AUTOS_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = kigali_AUTOS_12, family = "binomial")
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

kigali_FLOOR <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(kigali_FLOOR$FLOOR)
kigali_FLOOR <- kigali_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(kigali_FLOOR$FLOOR)
table(kigali_FLOOR$floor_b)

kigali_FLOOR$floor_b <- factor(kigali_FLOOR$floor_b)

kigali_FLOOR$YEAR <- factor(kigali_FLOOR$YEAR)
kigali_FLOOR_01 <- kigali_FLOOR %>% filter(kigali_FLOOR$YEAR==2002)
kigali_FLOOR_12 <- kigali_FLOOR %>% filter(kigali_FLOOR$YEAR==2012)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = kigali_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = kigali_FLOOR_12, family = "binomial")
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

kigali_ROOF <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(kigali_ROOF$ROOF)
kigali_ROOF <- kigali_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(kigali_ROOF$ROOF)
table(kigali_ROOF$roof_b)

kigali_ROOF$roof_b <- factor(kigali_ROOF$roof_b)

kigali_ROOF$YEAR <- factor(kigali_ROOF$YEAR)
kigali_ROOF_01 <- kigali_ROOF %>% filter(kigali_ROOF$YEAR==2002)
kigali_ROOF_12 <- kigali_ROOF %>% filter(kigali_ROOF$YEAR==2012)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = kigali_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = kigali_ROOF_12, family = "binomial")
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

kigali_TV <- kigali_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(kigali_TV$TV)
kigali_TV <- kigali_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(kigali_TV$TV)
table(kigali_TV$tv_b)

kigali_TV$tv_b <- factor(kigali_TV$tv_b)

kigali_TV$YEAR <- factor(kigali_TV$YEAR)
kigali_TV_01 <- kigali_TV %>% filter(kigali_TV$YEAR==2002)
kigali_TV_12 <- kigali_TV %>% filter(kigali_TV$YEAR==2012)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = kigali_TV_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = kigali_TV_12, family = "binomial")
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

kigali_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T3_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
kigali_logit$CITY <- "Kigali"

RWANDA_logit <- rbind(kigali_logit,sep = ".") 
write.csv(RWANDA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/RWANDA_logit.csv"))




