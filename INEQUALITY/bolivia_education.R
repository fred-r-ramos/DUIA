library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/bolivia")

geo2_bo1992 <- read_sf("geo2_bo1992.shp")
geo2_bo2001 <- read_sf("geo2_bo2001.shp")
geo2_bo2012 <- read_sf("geo2_bo2012.shp")
AUE_cb <- read_sf("cochabamba_studyArea.shp")
AUE_cochabamba <- st_transform(AUE_cb, 4326)


sf::sf_use_s2(FALSE)

centroid_bo1992 <- st_centroid(geo2_bo1992)
centroid_bo2001 <- st_centroid(geo2_bo2001)
centroid_bo2012 <- st_centroid(geo2_bo2012)


cochabamba_92 <- geo2_bo1992[st_intersection(AUE_cochabamba,centroid_bo1992),]
cochabamba_92['CITY']='cochabamba'
cochabamba_01 <- geo2_bo2001[st_intersection(AUE_cochabamba,centroid_bo2001),]
cochabamba_01['CITY']='cochabamba'
cochabamba_12 <- geo2_bo2012[st_intersection(AUE_cochabamba,centroid_bo2012),]
cochabamba_12['CITY']='cochabamba'


plot(st_geometry(cochabamba_12), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_bo2001[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00229.xml")
ddi <-read_ipums_ddi("ipumsi_00266.xml")

bolivia <- read_ipums_micro(ddi)

names(bolivia)

######################creating binary variables for logit regressions

names(bolivia)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(bolivia$OWNERSHIP)
bolivia$owner_b <- ifelse(bolivia$OWNERSHIP ==1,1,0)
table(bolivia$ELECTRIC)
bolivia$eletric_b <- ifelse(bolivia$ELECTRIC ==1,1,0)
table(bolivia$WATSUP)
bolivia$water_b <- ifelse(bolivia$WATSUP ==11,1,0)
table(bolivia$SEWAGE)
bolivia$sewage_b <- ifelse(bolivia$SEWAGE ==11|bolivia$SEWAGE ==12,1,0)
table(bolivia$TRASH)
bolivia$trash_b <- ifelse(bolivia$TRASH ==11|bolivia$TRASH ==12,1,0)
table(bolivia$AUTOS)
bolivia$autos_b <- ifelse(bolivia$AUTOS ==7,1,0)
table(bolivia$REFRIG)
bolivia$refrig_b <- ifelse(bolivia$REFRIG ==2,1,0)
table(bolivia$TV)
bolivia$tv_b <- ifelse(bolivia$TV ==20,1,0)
table(bolivia$TOILET)
bolivia$toilet_b <- ifelse(bolivia$TOILET >19,1,0)
table(bolivia$FLOOR)
bolivia$floor_b <- ifelse(bolivia$FLOOR >100,1,0)
table(bolivia$ROOF)
bolivia$roof_b <- ifelse(bolivia$ROOF==14|bolivia$ROOF==12,1,0)

bolivia$OCCISCO_b <- ifelse(bolivia$OCCISCO ==1|bolivia$OCCISCO ==2,1,0)
bolivia$OCCISCO_b <- ifelse(bolivia$OCCISCO ==3|bolivia$OCCISCO ==4|bolivia$OCCISCO ==5,2,bolivia$OCCISCO_b)
bolivia$OCCISCO_b <- ifelse(bolivia$OCCISCO ==6|bolivia$OCCISCO ==7|bolivia$OCCISCO ==8|bolivia$OCCISCO ==9,3,bolivia$OCCISCO_b)
bolivia$OCCISCO_low <- ifelse(bolivia$OCCISCO_b==3,1,0)

table(bolivia$OCCISCO_b)
table(bolivia$OCCISCO_low)

names(bolivia)
gc()

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL

#checking if the variables are available in both years
table(bolivia$owner_b,bolivia$YEAR)
table(bolivia$eletric_b,bolivia$YEAR)
table(bolivia$sewage_b,bolivia$YEAR)
table(bolivia$trash_b,bolivia$YEAR)#not available for all years
table(bolivia$autos_b,bolivia$YEAR)
table(bolivia$refrig_b,bolivia$YEAR)#not available for all years
table(bolivia$tv_b,bolivia$YEAR)
table(bolivia$toilet_b,bolivia$YEAR)
table(bolivia$floor_b,bolivia$YEAR)
table(bolivia$roof_b,bolivia$YEAR)

bolivia$PUBl_ASSET <- ifelse(is.na(bolivia$water_b), 0, bolivia$water_b) +
  ifelse(is.na(bolivia$sewage_b), 0, bolivia$sewage_b) +
  ifelse(is.na(bolivia$eletric_b), 0, bolivia$eletric_b) 
bolivia %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

bolivia$PRIV_ASSET <- ifelse(is.na(bolivia$owner_b), 0, bolivia$owner_b) +
  ifelse(is.na(bolivia$autos_b), 0, bolivia$autos_b) +
  ifelse(is.na(bolivia$tv_b), 0, bolivia$tv_b) +
  ifelse(is.na(bolivia$toilet_b), 0, bolivia$toilet_b) +
  ifelse(is.na(bolivia$floor_b), 0, bolivia$floor_b) +
  ifelse(is.na(bolivia$roof_b), 0, bolivia$roof_b)
bolivia %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

bolivia$TOTAL_ASSET <- ifelse(is.na(bolivia$water_b), 0, bolivia$water_b) +
  ifelse(is.na(bolivia$sewage_b), 0, bolivia$sewage_b) +
  ifelse(is.na(bolivia$eletric_b), 0, bolivia$eletric_b) +
  ifelse(is.na(bolivia$owner_b), 0, bolivia$owner_b) +
  ifelse(is.na(bolivia$autos_b), 0, bolivia$autos_b) +
  ifelse(is.na(bolivia$tv_b), 0, bolivia$tv_b) +
  ifelse(is.na(bolivia$toilet_b), 0, bolivia$toilet_b) +
  ifelse(is.na(bolivia$floor_b), 0, bolivia$floor_b) +
  ifelse(is.na(bolivia$roof_b), 0, bolivia$roof_b)
bolivia %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

bolivia$TOTAL_ASSET_gini <- ifelse(is.na(bolivia$water_b), 0, bolivia$water_b) +
  ifelse(is.na(bolivia$sewage_b), 0, bolivia$sewage_b) +
  ifelse(is.na(bolivia$eletric_b), 0, bolivia$eletric_b) +
  ifelse(is.na(bolivia$owner_b), 0, bolivia$owner_b) +
  ifelse(is.na(bolivia$toilet_b), 0, bolivia$toilet_b) +
  ifelse(is.na(bolivia$floor_b), 0, bolivia$floor_b) +
  ifelse(is.na(bolivia$roof_b), 0, bolivia$roof_b)
bolivia %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))



##Creating field for join
bolivia$IPUM1992 <- as.integer(bolivia$GEO2_BO1992)
bolivia$IPUM2001 <- as.integer(bolivia$GEO2_BO2001)
bolivia$IPUM2012 <- as.integer(bolivia$GEO2_BO2012)
cochabamba_92$IPUM1992 <- as.integer(cochabamba_92$IPUM1992)
cochabamba_01$IPUM2001 <- as.integer(cochabamba_01$IPUM2001)
cochabamba_12$IPUM2012 <- as.integer(cochabamba_12$IPUM2012)

##Joining by year

cochabamba_92 <- bolivia %>% inner_join(cochabamba_92, by="IPUM1992")
cochabamba_01 <- bolivia %>% inner_join(cochabamba_01, by="IPUM2001")
cochabamba_12 <- bolivia %>% inner_join(cochabamba_12, by="IPUM2012")
names(cochabamba_92)
names(cochabamba_01)
names(cochabamba_12)

cochabamba_92 <- select(cochabamba_92, -c(PROV1992))
cochabamba_01 <- select(cochabamba_01, -c(PROV2001))
cochabamba_12 <- select(cochabamba_12, -c(PROV2012))

##Merging all years into one table
bolivia_full <- rbind(cochabamba_92,cochabamba_01,cochabamba_12)
names(bolivia_full)

##Excluding specific columns for the unifeied dataset
bolivia_full<- select(bolivia_full, -c(GEO2_BO1992,GEO2_BO2001,IPUM1992,IPUM2001,GEO2_BO2012,IPUM2012,geometry))

table(bolivia_full$CITY)

table(bolivia_full$PERWT)

names(bolivia_full)

table(bolivia_full$YEAR)

bolivia_full <- bolivia_full %>%  filter (bolivia_full$YRSCHOOL < 90)

bolivia_full <- bolivia_full %>%  filter (bolivia_full$AGE >15)

bolivia_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(bolivia_full$YRSCHOOL)
summary(bolivia_full$YRSCHOOL)
table(bolivia_full$CITY)

####cochabamba
cochabamba_full <- bolivia_full %>%  filter (CITY=="cochabamba")

summary(cochabamba_full$YRSCHOOL)
summary(cochabamba_full$AGE)

cochabamba_fu92 <- cochabamba_full %>%  filter (YEAR==1992)
cochabamba_fu01 <- cochabamba_full %>%  filter (YEAR==2001)
cochabamba_fu12 <- cochabamba_full %>%  filter (YEAR==2012)

Gini(cochabamba_fu92 $YRSCHOOL,na.rm = TRUE)
Gini(cochabamba_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(cochabamba_fu12 $YRSCHOOL,na.rm = TRUE)

Gini(cochabamba_fu92 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(cochabamba_fu01 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(cochabamba_fu12 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_coch91 <- Lc(cochabamba_fu92$YRSCHOOL, n = rep(1,length(cochabamba_fu92$YRSCHOOL)), plot = TRUE)
Lc_coch01 <- Lc(cochabamba_fu01$YRSCHOOL, n = rep(1,length(cochabamba_fu01$YRSCHOOL)), plot = TRUE)
Lc_coch12 <- Lc(cochabamba_fu12$YRSCHOOL, n = rep(1,length(cochabamba_fu12$YRSCHOOL)), plot = TRUE)

plot(Lc_coch91,col='blue', main = "Lorenz Curve - cochabamba")
lines(Lc_coch01, col='red')
lines(Lc_coch12, col='green')

table(bolivia_full$OCCISCO_b)
table(bolivia_full$OCCISCO_low)
 
bolivia_full_OCCISCO_b <- bolivia_full %>% select(YEAR,OCCISCO_b,PERWT,OCCISCO_low)
bolivia_full_OCCISCO_b <- bolivia_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(bolivia_full_OCCISCO_b$OCCISCO_b)
 
cochabamba_full <- bolivia_full %>%  filter (CITY=="cochabamba")

OCCISCO_b_total <- bolivia_full_OCCISCO_b %>%
  group_by(YEAR) %>%
   summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- bolivia_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>%
  group_by(YEAR) %>%
  summarise(OCCISCO_b_TOP = sum(PERWT))
OCCISCO_b_MIDDLE <- bolivia_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>%
  group_by(YEAR) %>%   summarise(OCCISCO_b_MIDDLE = sum(PERWT))
OCCISCO_b_BOTTOM <- bolivia_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>%
   group_by(YEAR) %>%
   summarise(OCCISCO_b_BOTTOM = sum(PERWT))

cochabamba_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
cochabamba_OCCISCO_b<- cochabamba_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
cochabamba_OCCISCO_b<- cochabamba_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

cochabamba_OCCISCO_b

names(cochabamba_full)

################################analyzing the evolution in amenities access

# for PUBl_ASSET
cochabamba_PUBl_ASSET<-cochabamba_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cochabamba_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cochabamba_PUBl_ASSET <- cochabamba_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cochabamba_PUBl_ASSET <- cochabamba_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cochabamba_PUBl_ASSET$CITY<-"cochabamba"
# for PRIV_ASSET
cochabamba_PRIV_ASSET<-cochabamba_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cochabamba_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cochabamba_PRIV_ASSET <- cochabamba_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cochabamba_PRIV_ASSET <- cochabamba_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cochabamba_PRIV_ASSET$CITY<-"cochabamba"
# for TOTAL_ASSET
cochabamba_TOTAL_ASSET<-cochabamba_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cochabamba_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cochabamba_TOTAL_ASSET <- cochabamba_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cochabamba_TOTAL_ASSET <- cochabamba_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cochabamba_TOTAL_ASSET$CITY<-"Cochabamba"
write.csv(cochabamba_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cochabamba_PUBl_ASSET.csv", row.names = TRUE)
write.csv(cochabamba_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cochabamba_PRIV_ASSET.csv", row.names = TRUE)
write.csv(cochabamba_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cochabamba_TOTAL_ASSET.csv", row.names = TRUE)


######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

cochabamba_WATER <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)

table(cochabamba_WATER$HHWT)
table(cochabamba_WATER$WATSUP)
table(cochabamba_WATER$OCCISCO_low)
cochabamba_WATER <- cochabamba_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)


table(cochabamba_WATER$WATSUP)
table(cochabamba_WATER$water_b)

cochabamba_WATER$water_b <- factor(cochabamba_WATER$water_b)

cochabamba_WATER$YEAR <- factor(cochabamba_WATER$YEAR)

cochabamba_WATER_01 <- cochabamba_WATER %>% filter(cochabamba_WATER$YEAR==2001)
cochabamba_WATER_12 <- cochabamba_WATER %>% filter(cochabamba_WATER$YEAR==2012)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = cochabamba_WATER_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ YRSCHOOL, data = cochabamba_WATER_12, family = "binomial",weights=HHWT)
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

cochabamba_WATER <- cochabamba_WATER %>% filter(OCCISCO_b!=0)
table(cochabamba_WATER$OCCISCO_low)

mylogit_01 <- glm(water_b ~ OCCISCO_b, data = cochabamba_WATER_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ OCCISCO_b, data = cochabamba_WATER_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

#################################### FOR OWNERSHIP ##################################

cochabamba_OWN <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,OCCISCO_b,OCCISCO_low,owner_b,YRSCHOOL,CITY)
table(cochabamba_OWN$OWNERSHIP)
cochabamba_OWN <- cochabamba_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(cochabamba_OWN$OWNERSHIP)
table(cochabamba_OWN$owner_b)

cochabamba_OWN$owner_b <- factor(cochabamba_OWN$owner_b)

cochabamba_OWN$YEAR <- factor(cochabamba_OWN$YEAR)
cochabamba_OWN_01 <- cochabamba_OWN %>% filter(cochabamba_OWN$YEAR==2001)
cochabamba_OWN_12 <- cochabamba_OWN %>% filter(cochabamba_OWN$YEAR==2012)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = cochabamba_OWN_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ YRSCHOOL, data = cochabamba_OWN_12, family = "binomial",weights=HHWT)
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

cochabamba_SEWG <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,OCCISCO_b,OCCISCO_low,sewage_b,YRSCHOOL,CITY)
table(cochabamba_SEWG$SEWAGE)
cochabamba_SEWG <- cochabamba_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(cochabamba_SEWG$SEWAGE)
table(cochabamba_SEWG$sewage_b)

cochabamba_SEWG$sewage_b <- factor(cochabamba_SEWG$sewage_b)

cochabamba_SEWG$YEAR <- factor(cochabamba_SEWG$YEAR)
cochabamba_SEWG_01 <- cochabamba_SEWG %>% filter(cochabamba_SEWG$YEAR==2001)
cochabamba_SEWG_12 <- cochabamba_SEWG %>% filter(cochabamba_SEWG$YEAR==2012)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = cochabamba_SEWG_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(sewage_b ~ YRSCHOOL, data = cochabamba_SEWG_12, family = "binomial",weights=HHWT)
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

cochabamba_REFRIG <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,OCCISCO_b,OCCISCO_low,YRSCHOOL,CITY)
table(cochabamba_REFRIG$REFRIG)
cochabamba_REFRIG <- cochabamba_REFRIG %>% filter(REFRIG!=0)

table(cochabamba_REFRIG$REFRIG)
table(cochabamba_REFRIG$refrig_b)

cochabamba_REFRIG$refrig_b <- factor(cochabamba_REFRIG$refrig_b)

cochabamba_REFRIG$YEAR <- factor(cochabamba_REFRIG$YEAR)
cochabamba_REFRIG_01 <- cochabamba_REFRIG %>% filter(cochabamba_REFRIG$YEAR==2001)
cochabamba_REFRIG_12 <- cochabamba_REFRIG %>% filter(cochabamba_REFRIG$YEAR==2012)

mylogit_01 <- glm(refrig_b ~ YRSCHOOL, data = cochabamba_REFRIG_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ YRSCHOOL, data = cochabamba_REFRIG_12, family = "binomial",weights=HHWT)
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

cochabamba_TOILET <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,OCCISCO_b,OCCISCO_low,YRSCHOOL,CITY)
table(cochabamba_TOILET$TOILET)
cochabamba_TOILET <- cochabamba_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(cochabamba_TOILET$TOILET)
table(cochabamba_TOILET$toilet_b)

cochabamba_TOILET$toilet_b <- factor(cochabamba_TOILET$toilet_b)

cochabamba_TOILET$YEAR <- factor(cochabamba_TOILET$YEAR)
cochabamba_TOILET_01 <- cochabamba_TOILET %>% filter(cochabamba_TOILET$YEAR==2001)
cochabamba_TOILET_12 <- cochabamba_TOILET %>% filter(cochabamba_TOILET$YEAR==2012)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = cochabamba_TOILET_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ YRSCHOOL, data = cochabamba_TOILET_12, family = "binomial",weights=HHWT)
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

cochabamba_ELECTRIC <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,OCCISCO_b,OCCISCO_low,YRSCHOOL,CITY)
table(cochabamba_ELECTRIC$ELECTRIC)
cochabamba_ELECTRIC <- cochabamba_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(cochabamba_ELECTRIC$ELECTRIC)
table(cochabamba_ELECTRIC$eletric_b)

cochabamba_ELECTRIC$eletric_b <- factor(cochabamba_ELECTRIC$eletric_b)

cochabamba_ELECTRIC$YEAR <- factor(cochabamba_ELECTRIC$YEAR)
cochabamba_ELECTRIC_01 <- cochabamba_ELECTRIC %>% filter(cochabamba_ELECTRIC$YEAR==2001)
cochabamba_ELECTRIC_12 <- cochabamba_ELECTRIC %>% filter(cochabamba_ELECTRIC$YEAR==2012)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = cochabamba_ELECTRIC_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ YRSCHOOL, data = cochabamba_ELECTRIC_12, family = "binomial",weights=HHWT)
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

cochabamba_TRASH <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(cochabamba_TRASH$TRASH)
cochabamba_TRASH <- cochabamba_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(cochabamba_TRASH$TRASH)
table(cochabamba_TRASH$trash_b)

cochabamba_TRASH$trash_b <- factor(cochabamba_TRASH$trash_b)

cochabamba_TRASH$YEAR <- factor(cochabamba_TRASH$YEAR)
cochabamba_TRASH_12 <- cochabamba_TRASH %>% filter(cochabamba_TRASH$YEAR==2012)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = cochabamba_TRASH_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

cochabamba_AUTOS <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(cochabamba_AUTOS$AUTOS)
cochabamba_AUTOS <- cochabamba_AUTOS %>% filter(AUTOS!=99)

table(cochabamba_AUTOS$AUTOS)
table(cochabamba_AUTOS$autos_b)

cochabamba_AUTOS$autos_b <- factor(cochabamba_AUTOS$autos_b)

cochabamba_AUTOS$YEAR <- factor(cochabamba_AUTOS$YEAR)
cochabamba_AUTOS_01 <- cochabamba_AUTOS %>% filter(cochabamba_AUTOS$YEAR==2001)
cochabamba_AUTOS_12 <- cochabamba_AUTOS %>% filter(cochabamba_AUTOS$YEAR==2012)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = cochabamba_AUTOS_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = cochabamba_AUTOS_12, family = "binomial",weights=HHWT)
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

cochabamba_FLOOR <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(cochabamba_FLOOR$FLOOR)
cochabamba_FLOOR <- cochabamba_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(cochabamba_FLOOR$FLOOR)
table(cochabamba_FLOOR$floor_b)

cochabamba_FLOOR$floor_b <- factor(cochabamba_FLOOR$floor_b)

cochabamba_FLOOR$YEAR <- factor(cochabamba_FLOOR$YEAR)
cochabamba_FLOOR_01 <- cochabamba_FLOOR %>% filter(cochabamba_FLOOR$YEAR==2001)
cochabamba_FLOOR_12 <- cochabamba_FLOOR %>% filter(cochabamba_FLOOR$YEAR==2012)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = cochabamba_FLOOR_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = cochabamba_FLOOR_12, family = "binomial",weights=HHWT)
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

cochabamba_ROOF <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(cochabamba_ROOF$ROOF)
cochabamba_ROOF <- cochabamba_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(cochabamba_ROOF$ROOF)
table(cochabamba_ROOF$roof_b)

cochabamba_ROOF$roof_b <- factor(cochabamba_ROOF$roof_b)

cochabamba_ROOF$YEAR <- factor(cochabamba_ROOF$YEAR)
cochabamba_ROOF_01 <- cochabamba_ROOF %>% filter(cochabamba_ROOF$YEAR==2001)
cochabamba_ROOF_12 <- cochabamba_ROOF %>% filter(cochabamba_ROOF$YEAR==2012)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = cochabamba_ROOF_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = cochabamba_ROOF_12, family = "binomial",weights=HHWT)
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

cochabamba_TV <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,OCCISCO_b,OCCISCO_low,CITY)
table(cochabamba_TV$TV)
cochabamba_TV <- cochabamba_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(cochabamba_TV$TV)
table(cochabamba_TV$tv_b)

cochabamba_TV$tv_b <- factor(cochabamba_TV$tv_b)

cochabamba_TV$YEAR <- factor(cochabamba_TV$YEAR)
cochabamba_TV_01 <- cochabamba_TV %>% filter(cochabamba_TV$YEAR==2001)
cochabamba_TV_12 <- cochabamba_TV %>% filter(cochabamba_TV$YEAR==2012)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = cochabamba_TV_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = cochabamba_TV_12, family = "binomial",weights=HHWT)
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


cochabamba__ysc_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z,FLOOR_T2_z,FLOOR_T3_z) 
cochabamba__ysc_logit$CITY <- "cochabamba"

#################################################Saving an unique table results join

BOLIVIA_logit <- rbind(cochabamba_logit,sep = ".") 
write.csv(BOLIVIA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/BOLIVIA_logit.csv"))

cochabamba_WATER <- cochabamba_WATER %>% filter(OCCISCO_b!=0)
table(cochabamba_WATER$OCCISCO_low)

###############################################################################################################
###############################################   FOR OCCUPATION   ############################################
###############################################################################################################


cochabamba_WATER <- cochabamba_WATER %>% filter(OCCISCO_b!=0)

mylogit_01 <- glm(water_b ~ OCCISCO_low, data = cochabamba_WATER_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ OCCISCO_low, data = cochabamba_WATER_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)

gc()


#################################### FOR OWNERSHIP ##################################

cochabamba_OWN <- cochabamba_OWN %>% filter(OCCISCO_b!=0)

table(cochabamba_OWN$OWNERSHIP)
table(cochabamba_OWN$owner_b)

cochabamba_OWN$owner_b <- factor(cochabamba_OWN$owner_b)

cochabamba_OWN$YEAR <- factor(cochabamba_OWN$YEAR)
cochabamba_OWN_01 <- cochabamba_OWN %>% filter(cochabamba_OWN$YEAR==2001)
cochabamba_OWN_12 <- cochabamba_OWN %>% filter(cochabamba_OWN$YEAR==2012)


mylogit_01 <- glm(owner_b ~ OCCISCO_low, data = cochabamba_OWN_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ OCCISCO_low, data = cochabamba_OWN_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)

#################################### FOR SEWAGE ##################################

cochabamba_SEWG_01 <- cochabamba_SEWG_01 %>% filter(OCCISCO_b!=0)
cochabamba_SEWG_12 <- cochabamba_SEWG_12 %>% filter(OCCISCO_b!=0)

mylogit_01 <- glm(sewage_b ~ OCCISCO_low, data = cochabamba_SEWG_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(sewage_b ~ OCCISCO_low, data = cochabamba_SEWG_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

SEWG_T2 <- summary(mylogit_01)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
SEWG_T3 <- summary(mylogit_12)$coefficients
SEWG_T3_z <- SEWG_T3[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)

#################################### FOR REFRIG ##################################

cochabamba_REFRIG_01 <- cochabamba_REFRIG_01 %>% filter(OCCISCO_b!=0)
cochabamba_REFRIG_12 <- cochabamba_REFRIG_12 %>% filter(OCCISCO_b!=0)

mylogit_01 <- glm(refrig_b ~ OCCISCO_low, data = cochabamba_REFRIG_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(refrig_b ~ OCCISCO_low, data = cochabamba_REFRIG_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

REFRIG_T2 <- summary(mylogit_01)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
REFRIG_T3 <- summary(mylogit_12)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)

#################################### FOR TOILET ##################################

cochabamba_TOILET_01 <- cochabamba_TOILET_01 %>% filter(OCCISCO_b!=0)
cochabamba_TOILET_12 <- cochabamba_TOILET_12 %>% filter(OCCISCO_b!=0)

mylogit_01 <- glm(toilet_b ~ OCCISCO_low, data = cochabamba_TOILET_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ OCCISCO_low, data = cochabamba_TOILET_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_12)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)

#################################### FOR ELECTRIC ##################################

cochabamba_ELECTRIC_01 <- cochabamba_ELECTRIC_01 %>% filter(OCCISCO_b!=0)
cochabamba_ELECTRIC_12 <- cochabamba_ELECTRIC_12 %>% filter(OCCISCO_b!=0)

mylogit_01 <- glm(eletric_b ~ OCCISCO_low, data = cochabamba_ELECTRIC_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ OCCISCO_low, data = cochabamba_ELECTRIC_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_12)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)

#################################### FOR TRASH ##################################

cochabamba_TRASH <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(cochabamba_TRASH$TRASH)
cochabamba_TRASH <- cochabamba_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(cochabamba_TRASH$TRASH)
table(cochabamba_TRASH$trash_b)

cochabamba_TRASH$trash_b <- factor(cochabamba_TRASH$trash_b)

cochabamba_TRASH$YEAR <- factor(cochabamba_TRASH$YEAR)
cochabamba_TRASH_12 <- cochabamba_TRASH %>% filter(cochabamba_TRASH$YEAR==2012)

mylogit_12 <- glm(trash_b ~ YRSCHOOL, data = cochabamba_TRASH_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TRASH_T3 <- summary(mylogit_12)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)

#################################### FOR AUTOS ##################################

cochabamba_AUTOS <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(cochabamba_AUTOS$AUTOS)
cochabamba_AUTOS <- cochabamba_AUTOS %>% filter(AUTOS!=99)

table(cochabamba_AUTOS$AUTOS)
table(cochabamba_AUTOS$autos_b)

cochabamba_AUTOS$autos_b <- factor(cochabamba_AUTOS$autos_b)

cochabamba_AUTOS$YEAR <- factor(cochabamba_AUTOS$YEAR)
cochabamba_AUTOS_01 <- cochabamba_AUTOS %>% filter(cochabamba_AUTOS$YEAR==2001)
cochabamba_AUTOS_12 <- cochabamba_AUTOS %>% filter(cochabamba_AUTOS$YEAR==2012)

mylogit_01 <- glm(autos_b ~ YRSCHOOL, data = cochabamba_AUTOS_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ YRSCHOOL, data = cochabamba_AUTOS_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

AUTOS_T2 <- summary(mylogit_01)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
AUTOS_T3 <- summary(mylogit_12)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)

#################################### FOR FLOORS ##################################

cochabamba_FLOOR <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(cochabamba_FLOOR$FLOOR)
cochabamba_FLOOR <- cochabamba_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(cochabamba_FLOOR$FLOOR)
table(cochabamba_FLOOR$floor_b)

cochabamba_FLOOR$floor_b <- factor(cochabamba_FLOOR$floor_b)

cochabamba_FLOOR$YEAR <- factor(cochabamba_FLOOR$YEAR)
cochabamba_FLOOR_01 <- cochabamba_FLOOR %>% filter(cochabamba_FLOOR$YEAR==2001)
cochabamba_FLOOR_12 <- cochabamba_FLOOR %>% filter(cochabamba_FLOOR$YEAR==2012)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = cochabamba_FLOOR_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = cochabamba_FLOOR_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

FLOOR_T2 <- summary(mylogit_01)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_12)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)

#################################### FOR ROOF ##################################

cochabamba_ROOF <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(cochabamba_ROOF$ROOF)
cochabamba_ROOF <- cochabamba_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(cochabamba_ROOF$ROOF)
table(cochabamba_ROOF$roof_b)

cochabamba_ROOF$roof_b <- factor(cochabamba_ROOF$roof_b)

cochabamba_ROOF$YEAR <- factor(cochabamba_ROOF$YEAR)
cochabamba_ROOF_01 <- cochabamba_ROOF %>% filter(cochabamba_ROOF$YEAR==2001)
cochabamba_ROOF_12 <- cochabamba_ROOF %>% filter(cochabamba_ROOF$YEAR==2012)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = cochabamba_ROOF_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = cochabamba_ROOF_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

ROOF_T2 <- summary(mylogit_01)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_12)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)

#################################### FOR TV ##################################

cochabamba_TV <- cochabamba_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(cochabamba_TV$TV)
cochabamba_TV <- cochabamba_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(cochabamba_TV$TV)
table(cochabamba_TV$tv_b)

cochabamba_TV$tv_b <- factor(cochabamba_TV$tv_b)

cochabamba_TV$YEAR <- factor(cochabamba_TV$YEAR)
cochabamba_TV_01 <- cochabamba_TV %>% filter(cochabamba_TV$YEAR==2001)
cochabamba_TV_12 <- cochabamba_TV %>% filter(cochabamba_TV$YEAR==2012)

mylogit_01 <- glm(tv_b ~ YRSCHOOL, data = cochabamba_TV_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(tv_b ~ YRSCHOOL, data = cochabamba_TV_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T2 <- summary(mylogit_01)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)

cochabamba_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,REFRIG_T2_z,REFRIG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,AUTOS_T2_z,AUTOS_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z,FLOOR_T2_z,FLOOR_T3_z) 
cochabamba_logit$CITY <- "cochabamba"