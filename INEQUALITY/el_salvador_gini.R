library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/elsalvador")

geo2_sv92 <- read_sf("geo2_sv1992.shp")
geo2_sv07 <- read_sf("geo2_sv2007.shp")

AUE_sansalvador <- read_sf("San_Salvador_studyArea.shp")
AUE_sansalvador  <- st_transform(AUE_sansalvador , 4326)

sf::sf_use_s2(FALSE)

centroid_sv92 <- st_centroid(geo2_sv92)
centroid_sv07 <- st_centroid(geo2_sv07)

san_salvador_92 <- geo2_sv92[st_intersection(AUE_sansalvador,centroid_sv92),]
san_salvador_92['CITY']='san salvador'
san_salvador_07 <- geo2_sv07[st_intersection(AUE_sansalvador,centroid_sv07),]
san_salvador_07['CITY']='san salvador'


plot(st_geometry(san_salvador_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_sv07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 
#ddi <-read_ipums_ddi("ipumsi_00238.xml")
ddi <-read_ipums_ddi("ipumsi_00250.xml")
el_salvador <- read_ipums_micro(ddi)

names(el_salvador)
table(el_salvador$PERWT)

######################creating binary variables for logit regressions

names(el_salvador)

# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "GEO2_SV1992" "GEO2_SV2007" "OWNERSHIP"  
# [9] "OWNERSHIPD"  "ELECTRIC"    "WATSUP"      "SEWAGE"      "TRASH"       "AUTOS"       "REFRIG"      "TV"         
# [17] "TOILET"      "FLOOR"       "ROOF"        "PERNUM"      "PERWT"       "AGE"         "SEX"         "SCHOOL"     
# [25] "LIT"         "EDATTAIN"    "EDATTAIND"   "YRSCHOOL"    "EMPSTAT"     "EMPSTATD"    "LABFORCE"    "OCCISCO"    
# [33] "OCC"         "INDGEN"     


table(el_salvador$OWNERSHIP)
el_salvador$owner_b <- ifelse(el_salvador$OWNERSHIP ==1,1,0)
table(el_salvador$ELECTRIC)
el_salvador$eletric_b <- ifelse(el_salvador$ELECTRIC ==1,1,0)
table(el_salvador$WATSUP)
el_salvador$water_b <- ifelse(el_salvador$WATSUP ==11,1,0)
table(el_salvador$SEWAGE)
el_salvador$sewage_b <- ifelse(el_salvador$SEWAGE ==11|el_salvador$SEWAGE ==12,1,0)
table(el_salvador$TRASH)
el_salvador$trash_b <- ifelse(el_salvador$TRASH ==11|el_salvador$TRASH ==12,1,0)
table(el_salvador$AUTOS)
el_salvador$autos_b <- ifelse(el_salvador$AUTOS ==7,1,0)
table(el_salvador$REFRIG)
el_salvador$refrig_b <- ifelse(el_salvador$REFRIG ==2,1,0)
table(el_salvador$TV)
el_salvador$tv_b <- ifelse(el_salvador$TV ==20,1,0)
table(el_salvador$TOILET)
el_salvador$toilet_b <- ifelse(el_salvador$TOILET >19,1,0)
table(el_salvador$FLOOR)
el_salvador$floor_b <- ifelse(el_salvador$FLOOR >100,1,0)
table(el_salvador$ROOF)
el_salvador$roof_b <- ifelse(el_salvador$ROOF==14|el_salvador$ROOF==12|el_salvador$ROOF==21|el_salvador$ROOF==28,1,0)

names(el_salvador)
gc()

#checking if the variables are available in both years
table(el_salvador$owner_b,el_salvador$YEAR)
table(el_salvador$eletric_b,el_salvador$YEAR)
table(el_salvador$water_b,el_salvador$YEAR)
table(el_salvador$sewage_b,el_salvador$YEAR)
table(el_salvador$trash_b,el_salvador$YEAR)
table(el_salvador$autos_b,el_salvador$YEAR)
table(el_salvador$refrig_b,el_salvador$YEAR)
table(el_salvador$tv_b,el_salvador$YEAR)
table(el_salvador$toilet_b,el_salvador$YEAR)
table(el_salvador$floor_b,el_salvador$YEAR)
table(el_salvador$roof_b,el_salvador$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
el_salvador$PUBl_ASSET <- el_salvador$eletric_b+el_salvador$water_b+el_salvador$sewage_b+el_salvador$trash_b
el_salvador %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

el_salvador$PRIV_ASSET <- el_salvador$owner_b+el_salvador$autos_b+el_salvador$refrig_b+el_salvador$tv_b+el_salvador$toilet_b+el_salvador$floor_b+el_salvador$roof_b
el_salvador %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

el_salvador$TOTAL_ASSET <- el_salvador$eletric_b+el_salvador$water_b+el_salvador$sewage_b+el_salvador$trash_b+el_salvador$owner_b+el_salvador$autos_b+el_salvador$refrig_b+el_salvador$tv_b+el_salvador$toilet_b+el_salvador$floor_b+el_salvador$roof_b
assets<-el_salvador %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))


##Creating field for join

el_salvador$IPUM1992 <- as.integer(el_salvador$GEO2_SV1992)
el_salvador$IPUM2007 <- as.integer(el_salvador$GEO2_SV2007)
san_salvador_92$IPUM1992 <- as.integer(san_salvador_92$IPUM1992)
san_salvador_07$IPUM2007 <- as.integer(san_salvador_07$IPUM2007)

##Joining by year

san_salvador_92 <- el_salvador %>% inner_join(san_salvador_92, by="IPUM1992")
san_salvador_07 <- el_salvador %>% inner_join(san_salvador_07, by="IPUM2007")

names(san_salvador_92)
names(san_salvador_07)

san_salvador_92 <- select(san_salvador_92, -c(MUNI1992))
san_salvador_07 <- select(san_salvador_07, -c(MUNI2007))

##Merging all years into one table
el_salvador_full <- rbind(san_salvador_92,san_salvador_07)
names(el_salvador_full)

##Excluding specific columns for the unifeied dataset
el_salvador_full<- select(el_salvador_full, -c(GEO2_SV1992,GEO2_SV2007,IPUM1992,IPUM2007,geometry))
table(el_salvador_full$CITY)

names(el_salvador_full)

table(el_salvador_full$YEAR)

el_salvador_full <- el_salvador_full %>%  filter (el_salvador_full$YRSCHOOL < 90)

el_salvador_full <- el_salvador_full %>%  filter (el_salvador_full$AGE >15)

el_salvador_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(el_salvador_full$YRSCHOOL)
summary(el_salvador_full$YRSCHOOL)

el_salvador_fu92 <- el_salvador_full %>%  filter (YEAR==1992)
el_salvador_fu07 <- el_salvador_full %>%  filter (YEAR==2007)

Gini(el_salvador_fu92 $YRSCHOOL,na.rm = TRUE)
Gini(el_salvador_fu07 $YRSCHOOL,na.rm = TRUE)
Gini(el_salvador_fu92 $TOTAL_ASSET,na.rm = TRUE)
Gini(el_salvador_fu07 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_sansalv92 <- Lc(el_salvador_fu92$YRSCHOOL, n = rep(1,length(el_salvador_fu92$YRSCHOOL)), plot = TRUE)
Lc_sansalv07 <- Lc(el_salvador_fu07$YRSCHOOL, n = rep(1,length(el_salvador_fu07$YRSCHOOL)), plot = TRUE)

plot(Lc_sansalv92,col='blue', main = "Lorenz Curve - San Salvador")
lines(Lc_sansalv07, col='red')

table(el_salvador_full$OCCISCO)


el_salvador_full$OCCISCO_b <- ifelse(el_salvador_full$OCCISCO ==1|el_salvador_full$OCCISCO ==2,1,0)
el_salvador_full$OCCISCO_b <- ifelse(el_salvador_full$OCCISCO ==3|el_salvador_full$OCCISCO ==4|el_salvador_full$OCCISCO ==5,2,el_salvador_full$OCCISCO_b)
el_salvador_full$OCCISCO_b <- ifelse(el_salvador_full$OCCISCO ==6|el_salvador_full$OCCISCO ==7|el_salvador_full$OCCISCO ==8|el_salvador_full$OCCISCO ==9,3,el_salvador_full$OCCISCO_b)
table(el_salvador_full$OCCISCO_b)

el_salvador_full_OCCISCO_b <- el_salvador_full %>% select(YEAR,OCCISCO_b,PERWT)
el_salvador_full_OCCISCO_b <- el_salvador_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(el_salvador_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- el_salvador_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- el_salvador_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- el_salvador_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- el_salvador_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

el_salvador_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
el_salvador_OCCISCO_b<- el_salvador_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
el_salvador_OCCISCO_b<- el_salvador_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

el_salvador_OCCISCO_b

# for PUBl_ASSET
el_salvador_PUBl_ASSET<-el_salvador_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- el_salvador_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
el_salvador_PUBl_ASSET <- el_salvador_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
el_salvador_PUBl_ASSET <- el_salvador_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
el_salvador_PUBl_ASSET$CITY<-"san_salvador"
# for PRIV_ASSET
el_salvador_PRIV_ASSET<-el_salvador_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- el_salvador_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
el_salvador_PRIV_ASSET <- el_salvador_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
el_salvador_PRIV_ASSET <- el_salvador_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
el_salvador_PRIV_ASSET$CITY<-"san_salvador"
# for TOTAL_ASSET
el_salvador_TOTAL_ASSET<-el_salvador_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- el_salvador_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
el_salvador_TOTAL_ASSET <- el_salvador_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
el_salvador_TOTAL_ASSET <- el_salvador_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
el_salvador_TOTAL_ASSET$CITY<-"san_salvador"
write.csv(el_salvador_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/el_salvador_PUBl_ASSET.csv", row.names = TRUE)
write.csv(el_salvador_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/el_salvador_PRIV_ASSET.csv", row.names = TRUE)
write.csv(el_salvador_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/el_salvador_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

el_salvador_WATER <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(el_salvador_WATER$WATSUP)
el_salvador_WATER <- el_salvador_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(el_salvador_WATER$WATSUP)
table(el_salvador_WATER$water_b)

el_salvador_WATER$water_b <- factor(el_salvador_WATER$water_b)

el_salvador_WATER$YEAR <- factor(el_salvador_WATER$YEAR)
el_salvador_WATER_92 <- el_salvador_WATER %>% filter(el_salvador_WATER$YEAR==1992)
el_salvador_WATER_07 <- el_salvador_WATER %>% filter(el_salvador_WATER$YEAR==2007)

mylogit_92 <- glm(water_b ~ YRSCHOOL, data = el_salvador_WATER_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(water_b ~ YRSCHOOL, data = el_salvador_WATER_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_92)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_07)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

el_salvador_OWN <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(el_salvador_OWN$OWNERSHIP)
el_salvador_OWN <- el_salvador_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(el_salvador_OWN$OWNERSHIP)
table(el_salvador_OWN$owner_b)

el_salvador_OWN$owner_b <- factor(el_salvador_OWN$owner_b)

el_salvador_OWN$YEAR <- factor(el_salvador_OWN$YEAR)
el_salvador_OWN_92 <- el_salvador_OWN %>% filter(el_salvador_OWN$YEAR==1992)
el_salvador_OWN_07 <- el_salvador_OWN %>% filter(el_salvador_OWN$YEAR==2007)

mylogit_92 <- glm(owner_b ~ YRSCHOOL, data = el_salvador_OWN_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(owner_b ~ YRSCHOOL, data = el_salvador_OWN_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

OWN_T2 <- summary(mylogit_92)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_07)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

el_salvador_SEWG <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(el_salvador_SEWG$SEWAGE)
el_salvador_SEWG <- el_salvador_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(el_salvador_SEWG$SEWAGE)
table(el_salvador_SEWG$sewage_b)

el_salvador_SEWG$sewage_b <- factor(el_salvador_SEWG$sewage_b)

el_salvador_SEWG$YEAR <- factor(el_salvador_SEWG$YEAR)
el_salvador_SEWG_92 <- el_salvador_SEWG %>% filter(el_salvador_SEWG$YEAR==1992)
el_salvador_SEWG_07 <- el_salvador_SEWG %>% filter(el_salvador_SEWG$YEAR==2007)

mylogit_92 <- glm(sewage_b ~ YRSCHOOL, data = el_salvador_SEWG_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = el_salvador_SEWG_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

SEWG_T2 <- summary(mylogit_92)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_07)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

el_salvador_REFRIG <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(el_salvador_REFRIG$REFRIG)
el_salvador_REFRIG <- el_salvador_REFRIG %>% filter(REFRIG!=0)

table(el_salvador_REFRIG$REFRIG)
table(el_salvador_REFRIG$refrig_b)

el_salvador_REFRIG$refrig_b <- factor(el_salvador_REFRIG$refrig_b)

el_salvador_REFRIG$YEAR <- factor(el_salvador_REFRIG$YEAR)
el_salvador_REFRIG_92 <- el_salvador_REFRIG %>% filter(el_salvador_REFRIG$YEAR==1992)
el_salvador_REFRIG_07 <- el_salvador_REFRIG %>% filter(el_salvador_REFRIG$YEAR==2007)

mylogit_92 <- glm(refrig_b ~ YRSCHOOL, data = el_salvador_REFRIG_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(refrig_b ~ YRSCHOOL, data = el_salvador_REFRIG_07, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_92)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

el_salvador_TOILET <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(el_salvador_TOILET$TOILET)
el_salvador_TOILET <- el_salvador_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(el_salvador_TOILET$TOILET)
table(el_salvador_TOILET$toilet_b)

el_salvador_TOILET$toilet_b <- factor(el_salvador_TOILET$toilet_b)

el_salvador_TOILET$YEAR <- factor(el_salvador_TOILET$YEAR)
el_salvador_TOILET_92 <- el_salvador_TOILET %>% filter(el_salvador_TOILET$YEAR==1992)
el_salvador_TOILET_07 <- el_salvador_TOILET %>% filter(el_salvador_TOILET$YEAR==2007)

mylogit_92 <- glm(toilet_b ~ YRSCHOOL, data = el_salvador_TOILET_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(toilet_b ~ YRSCHOOL, data = el_salvador_TOILET_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

TOILET_T2 <- summary(mylogit_92)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

el_salvador_ELECTRIC <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(el_salvador_ELECTRIC$ELECTRIC)
el_salvador_ELECTRIC <- el_salvador_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(el_salvador_ELECTRIC$ELECTRIC)
table(el_salvador_ELECTRIC$eletric_b)

el_salvador_ELECTRIC$eletric_b <- factor(el_salvador_ELECTRIC$eletric_b)

el_salvador_ELECTRIC$YEAR <- factor(el_salvador_ELECTRIC$YEAR)
el_salvador_ELECTRIC_92 <- el_salvador_ELECTRIC %>% filter(el_salvador_ELECTRIC$YEAR==1992)
el_salvador_ELECTRIC_07 <- el_salvador_ELECTRIC %>% filter(el_salvador_ELECTRIC$YEAR==2007)

mylogit_92 <- glm(eletric_b ~ YRSCHOOL, data = el_salvador_ELECTRIC_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(eletric_b ~ YRSCHOOL, data = el_salvador_ELECTRIC_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

ELECTRIC_T2 <- summary(mylogit_92)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

el_salvador_TRASH <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(el_salvador_TRASH$TRASH)
el_salvador_TRASH <- el_salvador_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(el_salvador_TRASH$TRASH)
table(el_salvador_TRASH$trash_b)

el_salvador_TRASH$trash_b <- factor(el_salvador_TRASH$trash_b)

el_salvador_TRASH$YEAR <- factor(el_salvador_TRASH$YEAR)
el_salvador_TRASH_92 <- el_salvador_TRASH %>% filter(el_salvador_TRASH$YEAR==1992)
el_salvador_TRASH_07 <- el_salvador_TRASH %>% filter(el_salvador_TRASH$YEAR==2007)

mylogit_92 <- glm(trash_b ~ YRSCHOOL, data = el_salvador_TRASH_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(trash_b ~ YRSCHOOL, data = el_salvador_TRASH_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

TRASH_T2 <- summary(mylogit_92)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

el_salvador_AUTOS <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(el_salvador_AUTOS$AUTOS)
el_salvador_AUTOS <- el_salvador_AUTOS %>% filter(AUTOS!=0) %>% filter(AUTOS!=99)

table(el_salvador_AUTOS$AUTOS)
table(el_salvador_AUTOS$autos_b)

el_salvador_AUTOS$autos_b <- factor(el_salvador_AUTOS$autos_b)

el_salvador_AUTOS$YEAR <- factor(el_salvador_AUTOS$YEAR)
el_salvador_AUTOS_92 <- el_salvador_AUTOS %>% filter(el_salvador_AUTOS$YEAR==1992)
el_salvador_AUTOS_07 <- el_salvador_AUTOS %>% filter(el_salvador_AUTOS$YEAR==2007)

mylogit_92 <- glm(autos_b ~ YRSCHOOL, data = el_salvador_AUTOS_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(autos_b ~ YRSCHOOL, data = el_salvador_AUTOS_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

AUTOS_T2 <- summary(mylogit_92)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)

if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}
#################################### FOR FLOORS ##################################

el_salvador_FLOOR <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(el_salvador_FLOOR$FLOOR)
el_salvador_FLOOR <- el_salvador_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(el_salvador_FLOOR$FLOOR)
table(el_salvador_FLOOR$floor_b)

el_salvador_FLOOR$floor_b <- factor(el_salvador_FLOOR$floor_b)

el_salvador_FLOOR$YEAR <- factor(el_salvador_FLOOR$YEAR)
el_salvador_FLOOR_92 <- el_salvador_FLOOR %>% filter(el_salvador_FLOOR$YEAR==1992)
el_salvador_FLOOR_07 <- el_salvador_FLOOR %>% filter(el_salvador_FLOOR$YEAR==2007)

mylogit_92 <- glm(floor_b ~ YRSCHOOL, data = el_salvador_FLOOR_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(floor_b ~ YRSCHOOL, data = el_salvador_FLOOR_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

FLOOR_T2 <- summary(mylogit_92)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_10)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

el_salvador_ROOF <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(el_salvador_ROOF$ROOF)
el_salvador_ROOF <- el_salvador_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(el_salvador_ROOF$ROOF)
table(el_salvador_ROOF$roof_b)

el_salvador_ROOF$roof_b <- factor(el_salvador_ROOF$roof_b)

el_salvador_ROOF$YEAR <- factor(el_salvador_ROOF$YEAR)
el_salvador_ROOF_92 <- el_salvador_ROOF %>% filter(el_salvador_ROOF$YEAR==1992)
el_salvador_ROOF_07 <- el_salvador_ROOF %>% filter(el_salvador_ROOF$YEAR==2007)

mylogit_92 <- glm(roof_b ~ YRSCHOOL, data = el_salvador_ROOF_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(roof_b ~ YRSCHOOL, data = el_salvador_ROOF_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

ROOF_T2 <- summary(mylogit_92)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}


#################################### FOR TV ##################################

el_salvador_TV <- el_salvador_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(el_salvador_TV$TV)
el_salvador_TV <- el_salvador_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(el_salvador_TV$TV)
table(el_salvador_TV$tv_b)

el_salvador_TV$tv_b <- factor(el_salvador_TV$tv_b)

el_salvador_TV$YEAR <- factor(el_salvador_TV$YEAR)
el_salvador_TV_92 <- el_salvador_TV %>% filter(el_salvador_TV$YEAR==1992)
el_salvador_TV_07 <- el_salvador_TV %>% filter(el_salvador_TV$YEAR==2007)

mylogit_92 <- glm(tv_b ~ YRSCHOOL, data = el_salvador_TV_92, family = "binomial")
summary(mylogit_92)
exp(coef(mylogit_92))

mylogit_07 <- glm(tv_b ~ YRSCHOOL, data = el_salvador_TV_07, family = "binomial")
summary(mylogit_07)
exp(coef(mylogit_07))

TV_T2 <- summary(mylogit_92)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_10)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

el_salvador_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
el_salvador_logit$CITY <- "El Salvador"


#################################################Saving an unique table results join

el_salvador_logit <- rbind(el_salvador_logit,sep = ".") 
write.csv(el_salvador_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/el_salvador_logit.csv"))


