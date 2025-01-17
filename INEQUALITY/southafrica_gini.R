library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/south africa")

sf::sf_use_s2(FALSE)

geo2_za96 <- read_sf("geo2_za1996.shp")
geo2_za01 <- read_sf("geo2_za2001.shp")
geo2_za16 <- read_sf("geo2_za2016.shp")
geo2_za07 <- read_sf("geo2_za2007.shp")
geo2_za11 <- read_sf("geo2_za2011.shp")
centroid_za96 <- st_centroid(geo2_za96)
centroid_za01 <- st_centroid(geo2_za01)
centroid_za16 <- st_centroid(geo2_za16)
centroid_za07 <- st_centroid(geo2_za07)
centroid_za11 <- st_centroid(geo2_za11)
AUE_johannesburg <- read_sf("Johannesburg_studyArea.shp")
AUE_johannesburg  <- st_transform(AUE_johannesburg , 4326)
gc()

jburg_96 <- geo2_za96[st_intersection(AUE_johannesburg,centroid_za96),]
jburg_96['CITY']='johannesburg'
jburg_01 <- geo2_za01[st_intersection(AUE_johannesburg,centroid_za01),]
jburg_01['CITY']='johannesburg'
jburg_16 <- geo2_za16[st_intersection(AUE_johannesburg,centroid_za16),]
jburg_16['CITY']='johannesburg'
jburg_07 <- geo2_za07[st_intersection(AUE_johannesburg,centroid_za07),]
jburg_07['CITY']='johannesburg'
jburg_11 <- geo2_za11[st_intersection(AUE_johannesburg,centroid_za11),]
jburg_11['CITY']='johannesburg'

plot(st_geometry(jburg_16), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_za11[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00177.xml")
ddi <-read_ipums_ddi("ipumsi_00261.xml")
southafrica <- read_ipums_micro(ddi)

names(southafrica)
table(southafrica$YEAR)

######################creating binary variables for logit regressions

names(southafrica)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(southafrica$OWNERSHIP)
southafrica$owner_b <- ifelse(southafrica$OWNERSHIP ==1,1,0)
table(southafrica$ELECTRIC)
southafrica$eletric_b <- ifelse(southafrica$ELECTRIC ==1,1,0)
table(southafrica$WATSUP)
southafrica$water_b <- ifelse(southafrica$WATSUP ==11,1,0)
table(southafrica$SEWAGE)
southafrica$sewage_b <- ifelse(southafrica$SEWAGE ==11|southafrica$SEWAGE ==12,1,0)
table(southafrica$TRASH)
southafrica$trash_b <- ifelse(southafrica$TRASH ==10|southafrica$TRASH ==12,1,0)
table(southafrica$AUTOS)
southafrica$autos_b <- ifelse(southafrica$AUTOS ==7,1,0)
table(southafrica$REFRIG)
southafrica$refrig_b <- ifelse(southafrica$REFRIG ==2,1,0)
table(southafrica$TV)
southafrica$tv_b <- ifelse(southafrica$TV ==20,1,0)
table(southafrica$TOILET)
southafrica$toilet_b <- ifelse(southafrica$TOILET==21|southafrica$TOILET==22,1,0)
table(southafrica$ROOF)
southafrica$roof_b <- ifelse(southafrica$ROOF==11|southafrica$ROOF==19|southafrica$ROOF==28,1,0)

names(southafrica)
gc()

#checking if the variables are available in both years
table(southafrica$owner_b,southafrica$YEAR)
table(southafrica$eletric_b,southafrica$YEAR)
table(southafrica$water_b,southafrica$YEAR)
table(southafrica$sewage_b,southafrica$YEAR)#############
table(southafrica$trash_b,southafrica$YEAR)
table(southafrica$autos_b,southafrica$YEAR)############
table(southafrica$refrig_b,southafrica$YEAR)
table(southafrica$tv_b,southafrica$YEAR)
table(southafrica$toilet_b,southafrica$YEAR)
table(southafrica$roof_b,southafrica$YEAR)#########


##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
southafrica$PUBl_ASSET <- ifelse(is.na(southafrica$eletric_b), 0, southafrica$eletric_b) +
  ifelse(is.na(southafrica$water_b), 0, southafrica$water_b) +
  ifelse(is.na(southafrica$sewage_b), 0, southafrica$sewage_b) +
  ifelse(is.na(southafrica$trash_b), 0, southafrica$trash_b)
southafrica %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

southafrica$PRIV_ASSET <- ifelse(is.na(southafrica$owner_b), 0, southafrica$owner_b) +
  ifelse(is.na(southafrica$autos_b), 0, southafrica$autos_b) +
  ifelse(is.na(southafrica$refrig_b), 0, southafrica$refrig_b) +
  ifelse(is.na(southafrica$tv_b), 0, southafrica$tv_b)+
  ifelse(is.na(southafrica$toilet_b), 0, southafrica$toilet_b)+
  ifelse(is.na(southafrica$roof_b), 0, southafrica$roof_b)
southafrica %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

southafrica$TOTAL_ASSET <- ifelse(is.na(southafrica$owner_b), 0, southafrica$owner_b) +
  ifelse(is.na(southafrica$autos_b), 0, southafrica$autos_b) +
  ifelse(is.na(southafrica$refrig_b), 0, southafrica$refrig_b) +
  ifelse(is.na(southafrica$tv_b), 0, southafrica$tv_b)+
  ifelse(is.na(southafrica$toilet_b), 0, southafrica$toilet_b)+
  ifelse(is.na(southafrica$roof_b), 0, southafrica$roof_b)+
  ifelse(is.na(southafrica$eletric_b), 0, southafrica$eletric_b) +
  ifelse(is.na(southafrica$water_b), 0, southafrica$water_b) +
  ifelse(is.na(southafrica$sewage_b), 0, southafrica$sewage_b) +
  ifelse(is.na(southafrica$trash_b), 0, southafrica$trash_b)
assets<-southafrica %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

southafrica$TOTAL_ASSET_gini <- ifelse(is.na(southafrica$owner_b), 0, southafrica$owner_b) +
  ifelse(is.na(southafrica$refrig_b), 0, southafrica$refrig_b) +
  ifelse(is.na(southafrica$tv_b), 0, southafrica$tv_b)+
  ifelse(is.na(southafrica$toilet_b), 0, southafrica$toilet_b)+
  ifelse(is.na(southafrica$roof_b), 0, southafrica$roof_b)+
  ifelse(is.na(southafrica$eletric_b), 0, southafrica$eletric_b) +
  ifelse(is.na(southafrica$water_b), 0, southafrica$water_b) +
  ifelse(is.na(southafrica$trash_b), 0, southafrica$trash_b)
assets<-southafrica %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

southafrica$IPUM1996 <- as.integer(southafrica$GEO2_ZA1996)
southafrica$IPUM2001 <- as.integer(southafrica$GEO2_ZA2001)
southafrica$IPUM2007 <- as.integer(southafrica$GEO2_ZA2007)
southafrica$IPUM2011 <- as.integer(southafrica$GEO2_ZA2011)
southafrica$IPUM2016 <- as.integer(southafrica$GEO2_ZA2016)


jburg_96$IPUM1996 <- as.integer(jburg_96$IPUM1996)
jburg_01$IPUM2001 <- as.integer(jburg_01$IPUM2001)
jburg_07$IPUM2007 <- as.integer(jburg_07$IPUM2007)
jburg_11$IPUM2011 <- as.integer(jburg_11$IPUM2011)
jburg_16$IPUM2016 <- as.integer(jburg_16$IPUM2016)


##Joining by year

jburg_96 <- southafrica %>% inner_join(jburg_96, by="IPUM1996")
jburg_01 <- southafrica %>% inner_join(jburg_01, by="IPUM2001")
jburg_16 <- southafrica %>% inner_join(jburg_16, by="IPUM2016")
jburg_07 <- southafrica %>% inner_join(jburg_07, by="IPUM2007")
jburg_11 <- southafrica %>% inner_join(jburg_11, by="IPUM2011")

names(jburg_96)
names(jburg_01)
names(jburg_16)
names(jburg_07)
names(jburg_11)

jburg_96 <- select(jburg_96, -c(DIST1996))
jburg_01 <- select(jburg_01, -c(DIST2001))
jburg_16 <- select(jburg_16, -c(DIST2016))
jburg_07 <- select(jburg_07, -c(DIST2007))
jburg_11 <- select(jburg_11, -c(DIST2011))

rm(southafrica)
gc()

##Merging all years into one table
southafrica_full <- rbind(jburg_96,jburg_16,jburg_01, jburg_07, jburg_11)
names(southafrica_full)

##Excluding specific columns for the unifeied dataset
southafrica_full<- select(southafrica_full, -c(GEO2_ZA1996,GEO2_ZA2016,GEO2_ZA2001,GEO2_ZA2011,GEO2_ZA2007,IPUM1996,IPUM2011,geometry))

table(southafrica_full$CITY)

####johannesburg
johannesburg_full <- southafrica_full %>%  filter (CITY=="johannesburg")

table(johannesburg_full$YEAR)
# 
# johannesburg_full <- johannesburg_full %>%  filter (johannesburg_full$YRSCHOOL < 90)
# 
# johannesburg_full <- johannesburg_full %>%  filter (johannesburg_full$AGE >15)
# 
# johannesburg_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

summary(johannesburg_full$YRSCHOOL)
summary(johannesburg_full$AGE)

johannesburg_fu96 <- johannesburg_full %>%  filter (YEAR==1996)
johannesburg_fu01 <- johannesburg_full %>%  filter (YEAR==2001)
johannesburg_fu07 <- johannesburg_full %>%  filter (YEAR==2007)
johannesburg_fu11 <- johannesburg_full %>%  filter (YEAR==2011)
johannesburg_fu16 <- johannesburg_full %>%  filter (YEAR==2016)

Gini(johannesburg_fu96$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu07$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu11$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu16$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu07$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu11$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu16$YRSCHOOL,na.rm = TRUE)
Gini(johannesburg_fu01$TOTAL_ASSET_gini,na.rm = TRUE)
Gini(johannesburg_fu11$TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_joh96 <- Lc(johannesburg_fu96$YRSCHOOL, n = rep(1,length(johannesburg_fu96$YRSCHOOL)), plot = TRUE)
Lc_joh07 <- Lc(johannesburg_fu07$YRSCHOOL, n = rep(1,length(johannesburg_fu07$YRSCHOOL)), plot = TRUE)
Lc_joh11 <- Lc(johannesburg_fu11$YRSCHOOL, n = rep(1,length(johannesburg_fu11$YRSCHOOL)), plot = TRUE)
Lc_joh16 <- Lc(johannesburg_fu16$YRSCHOOL, n = rep(1,length(johannesburg_fu16$YRSCHOOL)), plot = TRUE)

plot(Lc_joh96,col='blue', main = "Lorenz Curve - Belo Horizonte")
lines(Lc_joh07, col='red')
lines(Lc_joh11, col='green')
lines(Lc_joh16, col='yellow')

# for PUBl_ASSET
johannesburg_PUBl_ASSET<-johannesburg_full %>% filter(YEAR==2007|YEAR==2016) %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- johannesburg_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
johannesburg_PUBl_ASSET <- johannesburg_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
johannesburg_PUBl_ASSET <- johannesburg_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
johannesburg_PUBl_ASSET$CITY<-"johannesburg"
# for PRIV_ASSET
johannesburg_PRIV_ASSET<-johannesburg_full %>% filter(YEAR==2007|YEAR==2016) %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- johannesburg_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
johannesburg_PRIV_ASSET <- johannesburg_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
johannesburg_PRIV_ASSET <- johannesburg_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
johannesburg_PRIV_ASSET$CITY<-"johannesburg"
# for TOTAL_ASSET
johannesburg_TOTAL_ASSET<-johannesburg_full %>% filter(YEAR==2007|YEAR==2016) %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- johannesburg_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
johannesburg_TOTAL_ASSET <- johannesburg_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
johannesburg_TOTAL_ASSET <- johannesburg_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
johannesburg_TOTAL_ASSET$CITY<-"johannesburg"
write.csv(johannesburg_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/johannesburg_PUBl_ASSET.csv", row.names = TRUE)
write.csv(johannesburg_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/johannesburg_PRIV_ASSET.csv", row.names = TRUE)
write.csv(johannesburg_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/johannesburg_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

johannesburg_WATER <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(johannesburg_WATER$WATSUP)
table(johannesburg_WATER$HHWT)
johannesburg_WATER <- johannesburg_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(johannesburg_WATER$WATSUP)
table(johannesburg_WATER$water_b)

johannesburg_WATER$water_b <- factor(johannesburg_WATER$water_b)

johannesburg_WATER$YEAR <- factor(johannesburg_WATER$YEAR)
johannesburg_WATER_01 <- johannesburg_WATER %>% filter(johannesburg_WATER$YEAR==1996)
johannesburg_WATER_12 <- johannesburg_WATER %>% filter(johannesburg_WATER$YEAR==2011)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = johannesburg_WATER_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = johannesburg_WATER_12, family = "binomial",weights = HHWT)
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

johannesburg_OWN <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(johannesburg_OWN$OWNERSHIP)
johannesburg_OWN <- johannesburg_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(johannesburg_OWN$OWNERSHIP)
table(johannesburg_OWN$owner_b)

johannesburg_OWN$owner_b <- factor(johannesburg_OWN$owner_b)

johannesburg_OWN$YEAR <- factor(johannesburg_OWN$YEAR)
johannesburg_OWN_01 <- johannesburg_OWN %>% filter(johannesburg_OWN$YEAR==1996)
johannesburg_OWN_12 <- johannesburg_OWN %>% filter(johannesburg_OWN$YEAR==2011)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = johannesburg_OWN_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = johannesburg_OWN_12, family = "binomial",weights = HHWT)
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

johannesburg_REFRIG <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(johannesburg_REFRIG$REFRIG)
johannesburg_REFRIG <- johannesburg_REFRIG %>% filter(REFRIG!=0)

table(johannesburg_REFRIG$REFRIG)
table(johannesburg_REFRIG$refrig_b)

johannesburg_REFRIG$refrig_b <- factor(johannesburg_REFRIG$refrig_b)

johannesburg_REFRIG$YEAR <- factor(johannesburg_REFRIG$YEAR)
johannesburg_REFRIG_12 <- johannesburg_REFRIG %>% filter(johannesburg_REFRIG$YEAR==2011)

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = johannesburg_REFRIG_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

johannesburg_TOILET <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(johannesburg_TOILET$TOILET)
johannesburg_TOILET <- johannesburg_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(johannesburg_TOILET$TOILET)
table(johannesburg_TOILET$toilet_b)

johannesburg_TOILET$toilet_b <- factor(johannesburg_TOILET$toilet_b)

johannesburg_TOILET$YEAR <- factor(johannesburg_TOILET$YEAR)
johannesburg_TOILET_01 <- johannesburg_TOILET %>% filter(johannesburg_TOILET$YEAR==1996)
johannesburg_TOILET_12 <- johannesburg_TOILET %>% filter(johannesburg_TOILET$YEAR==2011)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = johannesburg_TOILET_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = johannesburg_TOILET_12, family = "binomial",weights = HHWT)
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

johannesburg_ELECTRIC <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(johannesburg_ELECTRIC$ELECTRIC)
johannesburg_ELECTRIC <- johannesburg_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)

table(johannesburg_ELECTRIC$ELECTRIC)
table(johannesburg_ELECTRIC$eletric_b)

johannesburg_ELECTRIC$eletric_b <- factor(johannesburg_ELECTRIC$eletric_b)

johannesburg_ELECTRIC$YEAR <- factor(johannesburg_ELECTRIC$YEAR)
johannesburg_ELECTRIC_01 <- johannesburg_ELECTRIC %>% filter(johannesburg_ELECTRIC$YEAR==1996)
johannesburg_ELECTRIC_12 <- johannesburg_ELECTRIC %>% filter(johannesburg_ELECTRIC$YEAR==2011)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = johannesburg_ELECTRIC_01, family = "binomial",weights = HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = johannesburg_ELECTRIC_12, family = "binomial",weights = HHWT)
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

johannesburg_TRASH <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(johannesburg_TRASH$TRASH)
johannesburg_TRASH <- johannesburg_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(johannesburg_TRASH$TRASH)
table(johannesburg_TRASH$trash_b)

johannesburg_TRASH$trash_b <- factor(johannesburg_TRASH$trash_b)

johannesburg_TRASH$YEAR <- factor(johannesburg_TRASH$YEAR)
johannesburg_TRASH_12 <- johannesburg_TRASH %>% filter(johannesburg_TRASH$YEAR==2011)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = johannesburg_TRASH_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))


TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

johannesburg_AUTOS <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(johannesburg_AUTOS$AUTOS)
johannesburg_AUTOS <- johannesburg_AUTOS %>% filter(AUTOS!=9) %>% filter(AUTOS!=8)

table(johannesburg_AUTOS$AUTOS)
table(johannesburg_AUTOS$autos_b)

johannesburg_AUTOS$autos_b <- factor(johannesburg_AUTOS$autos_b)

johannesburg_AUTOS$YEAR <- factor(johannesburg_AUTOS$YEAR)
johannesburg_AUTOS_12 <- johannesburg_AUTOS %>% filter(johannesburg_AUTOS$YEAR==2011)

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = johannesburg_AUTOS_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

johannesburg_ROOF <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(johannesburg_ROOF$ROOF)
johannesburg_ROOF <- johannesburg_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(johannesburg_ROOF$ROOF)
table(johannesburg_ROOF$roof_b)

johannesburg_ROOF$roof_b <- factor(johannesburg_ROOF$roof_b)

johannesburg_ROOF$YEAR <- factor(johannesburg_ROOF$YEAR)
johannesburg_ROOF_12 <- johannesburg_ROOF %>% filter(johannesburg_ROOF$YEAR==2011)

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = johannesburg_ROOF_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

johannesburg_TV <- johannesburg_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(johannesburg_TV$TV)
johannesburg_TV <- johannesburg_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(johannesburg_TV$TV)
table(johannesburg_TV$tv_b)

johannesburg_TV$tv_b <- factor(johannesburg_TV$tv_b)

johannesburg_TV$YEAR <- factor(johannesburg_TV$YEAR)
johannesburg_TV_12 <- johannesburg_TV %>% filter(johannesburg_TV$YEAR==2011)

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = johannesburg_TV_12, family = "binomial",weights = HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

johannesburg_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T3_z,ROOF_T3_z,TV_T3_z) 
johannesburg_logit$CITY <- "Johannesburg"

SAFRICA_logit <- rbind(johannesburg_logit,sep = ".") 
write.csv(SAFRICA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/SAFRICA_logit.csv"))




