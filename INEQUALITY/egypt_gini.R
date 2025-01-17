library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/egypt")

geo2_eg86 <- read_sf("geo2_eg1986.shp")
geo2_eg96 <- read_sf("geo2_eg1996.shp")
geo2_eg06 <- read_sf("geo2_eg2006.shp")

AUE_alexandria <- read_sf("Alexandria_studyArea.shp")
AUE_cairo <- read_sf("Cairo_studyArea.shp")
AUE_cairo <- st_transform(AUE_cairo, 4326)
AUE_alexandria <- st_transform(AUE_alexandria, 4326)

centroid_eg86 <- st_centroid(geo2_eg86)
centroid_eg96 <- st_centroid(geo2_eg96)
centroid_eg06 <- st_centroid(geo2_eg06)

cairo_86 <- geo2_eg86[st_intersection(AUE_cairo,centroid_eg86),]
cairo_86['CITY']='cairo'
cairo_96 <- geo2_eg96[st_intersection(AUE_cairo,centroid_eg96),]
cairo_96['CITY']='cairo'
cairo_06 <- geo2_eg06[st_intersection(AUE_cairo,centroid_eg06),]
cairo_06['CITY']='cairo'

alexandria_86 <- geo2_eg86[st_intersection(AUE_alexandria,centroid_eg86),]
alexandria_86['CITY']='alexandria'
alexandria_96 <- geo2_eg96[st_intersection(AUE_alexandria,centroid_eg96),]
alexandria_96['CITY']='alexandria'
alexandria_06 <- geo2_eg06[st_intersection(AUE_alexandria,centroid_eg06),]
alexandria_06['CITY']='alexandria'

geo_egypt_86 <- rbind(cairo_86,alexandria_86)
geo_egypt_96 <- rbind(cairo_96,alexandria_96)
geo_egypt_06 <- rbind(cairo_06,alexandria_06)

plot(st_geometry(geo_egypt_06), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_eg06[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00249.xml")
egypt <- read_ipums_micro(ddi)

names(egypt)

table(egypt$OWNERSHIP)
egypt$owner_b <- ifelse(egypt$OWNERSHIP ==1,1,0)
table(egypt$ELECTRIC)
egypt$eletric_b <- ifelse(egypt$ELECTRIC ==1,1,0)
table(egypt$WATSUP)
egypt$water_b <- ifelse(egypt$WATSUP ==11,1,0)
table(egypt$SEWAGE)
egypt$sewage_b <- ifelse(egypt$SEWAGE ==11|egypt$SEWAGE ==12,1,0)
table(egypt$AUTOS)
egypt$autos_b <- ifelse(egypt$AUTOS>0&egypt$AUTOS<8,1,0)
table(egypt$REFRIG)
egypt$refrig_b <- ifelse(egypt$REFRIG ==2,1,0)
table(egypt$TV)
egypt$tv_b <- ifelse(egypt$TV>10&egypt$TV<99,1,0)
table(egypt$TOILET)
egypt$toilet_b <- ifelse(egypt$TOILET==20,1,0)

egypt$OCCISCO_b <- ifelse(egypt$OCCISCO ==1|egypt$OCCISCO ==2,1,0)
egypt$OCCISCO_b <- ifelse(egypt$OCCISCO ==3|egypt$OCCISCO ==4|egypt$OCCISCO ==5,2,egypt$OCCISCO_b)
egypt$OCCISCO_b <- ifelse(egypt$OCCISCO ==6|egypt$OCCISCO ==7|egypt$OCCISCO ==8|egypt$OCCISCO ==9,3,egypt$OCCISCO_b)
egypt$OCCISCO_low <- ifelse(egypt$OCCISCO_b==3,1,0)

table(egypt$OCCISCO_b)
table(egypt$OCCISCO_low)
table(egypt$HHWT)
names(egypt)
gc()

#checking if the variables are available in both years
table(egypt$owner_b,egypt$YEAR)
table(egypt$eletric_b,egypt$YEAR)
table(egypt$water_b,egypt$YEAR)
table(egypt$sewage_b,egypt$YEAR)
table(egypt$autos_b,egypt$YEAR)
table(egypt$refrig_b,egypt$YEAR)
table(egypt$tv_b,egypt$YEAR)
table(egypt$toilet_b,egypt$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
egypt$PUBl_ASSET <- ifelse(is.na(egypt$eletric_b), 0, egypt$eletric_b)+
  ifelse(is.na(egypt$water_b), 0, egypt$water_b)
  #ifelse(is.na(egypt$sewage_b), 0, egypt$sewage_b)
egypt %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

egypt$PRIV_ASSET <- ifelse(is.na(egypt$owner_b), 0, egypt$owner_b)+
  ifelse(is.na(egypt$autos_b), 0, egypt$autos_b)+
  ifelse(is.na(egypt$refrig_b), 0, egypt$refrig_b)+
  ifelse(is.na(egypt$toilet_b), 0, egypt$toilet_b)
egypt %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

egypt$TOTAL_ASSET <- ifelse(is.na(egypt$eletric_b), 0, egypt$eletric_b)+
  ifelse(is.na(egypt$water_b), 0, egypt$water_b)+
  ifelse(is.na(egypt$owner_b), 0, egypt$owner_b)+
  ifelse(is.na(egypt$autos_b), 0, egypt$autos_b)+
  ifelse(is.na(egypt$refrig_b), 0, egypt$refrig_b)+
  ifelse(is.na(egypt$toilet_b), 0, egypt$toilet_b)
  #ifelse(is.na(egypt$sewage_b), 0, egypt$sewage_b)
assets<-egypt %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))


##Creating field for join

egypt$IPUM1986 <- as.integer(egypt$GEO2_EG1986)
egypt$IPUM1996 <- as.integer(egypt$GEO2_EG1996)
egypt$IPUM2006 <- as.integer(egypt$GEO2_EG2006)
geo_egypt_86$IPUM1986 <- as.integer(geo_egypt_86$IPUM1986)
geo_egypt_96$IPUM1996 <- as.integer(geo_egypt_96$IPUM1996)
geo_egypt_06$IPUM2006 <- as.integer(geo_egypt_06$IPUM2006)

##Joining by year

geo_egypt_86 <- egypt %>% inner_join(geo_egypt_86, by="IPUM1986")
geo_egypt_96 <- egypt %>% inner_join(geo_egypt_96, by="IPUM1996")
geo_egypt_06 <- egypt %>% inner_join(geo_egypt_06, by="IPUM2006")

names(geo_egypt_86)
names(geo_egypt_96)
names(geo_egypt_06)

geo_egypt_86 <- select(geo_egypt_86, -c(DIST1986))
geo_egypt_96 <- select(geo_egypt_96, -c(DIST1996))
geo_egypt_06 <- select(geo_egypt_06, -c(DIST2006))

##Merging all years into one table
egypt_full <- rbind(geo_egypt_86,geo_egypt_96,geo_egypt_06)
names(egypt_full)

##Excluding specific columns for the unifeied dataset
egypt_full<- select(egypt_full, -c(GEO2_EG1986,GEO2_EG2006,GEO2_EG1996,IPUM1986,IPUM1996,IPUM2006,geometry))
table(egypt_full$CITY)

names(egypt_full)

table(egypt_full$OCCISCO)


egypt_full$OCCISCO_b <- ifelse(egypt_full$OCCISCO ==1|egypt_full$OCCISCO ==2,1,0)
egypt_full$OCCISCO_b <- ifelse(egypt_full$OCCISCO ==3|egypt_full$OCCISCO ==4|egypt_full$OCCISCO ==5,2,egypt_full$OCCISCO_b)
egypt_full$OCCISCO_b <- ifelse(egypt_full$OCCISCO ==6|egypt_full$OCCISCO ==7|egypt_full$OCCISCO ==8|egypt_full$OCCISCO ==9,3,egypt_full$OCCISCO_b)
table(egypt_full$OCCISCO_b)

egypt_full_OCCISCO_b <- egypt_full %>% select(YEAR,CITY,OCCISCO_b,PERWT)
egypt_full_OCCISCO_b <- egypt_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(egypt_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- egypt_full_OCCISCO_b %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- egypt_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- egypt_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- egypt_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

egypt_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR","CITY"))
egypt_OCCISCO_b<- egypt_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR","CITY"))
egypt_OCCISCO_b<- egypt_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR","CITY"))

egypt_OCCISCO_b

alexandria_full<- egypt_full%>% filter(CITY=="alexandria")
cairo_full<- egypt_full%>% filter(CITY=="cairo")

# for PUBl_ASSET
alexandria_PUBl_ASSET<-alexandria_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- alexandria_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
alexandria_PUBl_ASSET <- alexandria_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
alexandria_PUBl_ASSET <- alexandria_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
alexandria_PUBl_ASSET$CITY<-"alexandria"
# for PRIV_ASSET
alexandria_PRIV_ASSET<-alexandria_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- alexandria_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
alexandria_PRIV_ASSET <- alexandria_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
alexandria_PRIV_ASSET <- alexandria_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
alexandria_PRIV_ASSET$CITY<-"alexandria"
# for TOTAL_ASSET
alexandria_TOTAL_ASSET<-alexandria_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- alexandria_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
alexandria_TOTAL_ASSET <- alexandria_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
alexandria_TOTAL_ASSET <- alexandria_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
alexandria_TOTAL_ASSET$CITY<-"alexandria"


alexandria_fu86 <- alexandria_full %>%  filter (YEAR==1986)
alexandria_fu96 <- alexandria_full %>%  filter (YEAR==1996)
alexandria_fu06 <- alexandria_full %>%  filter (YEAR==2006)
Gini(alexandria_fu86 $TOTAL_ASSET,na.rm = TRUE)
Gini(alexandria_fu96 $TOTAL_ASSET,na.rm = TRUE)
Gini(alexandria_fu06 $TOTAL_ASSET,na.rm = TRUE)


write.csv(alexandria_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/alexandria_PUBl_ASSET.csv", row.names = TRUE)
write.csv(alexandria_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/alexandria_PRIV_ASSET.csv", row.names = TRUE)
write.csv(alexandria_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/alexandria_TOTAL_ASSET.csv", row.names = TRUE)

# for PUBl_ASSET
cairo_PUBl_ASSET<-cairo_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cairo_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cairo_PUBl_ASSET <- cairo_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cairo_PUBl_ASSET <- cairo_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cairo_PUBl_ASSET$CITY<-"cairo"
# for PRIV_ASSET
cairo_PRIV_ASSET<-cairo_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cairo_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cairo_PRIV_ASSET <- cairo_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cairo_PRIV_ASSET <- cairo_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cairo_PRIV_ASSET$CITY<-"cairo"
# for TOTAL_ASSET
cairo_TOTAL_ASSET<-cairo_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- cairo_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
cairo_TOTAL_ASSET <- cairo_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
cairo_TOTAL_ASSET <- cairo_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
cairo_TOTAL_ASSET$CITY<-"cairo"

cairo_fu86 <- cairo_full %>%  filter (YEAR==1986)
cairo_fu96 <- cairo_full %>%  filter (YEAR==1996)
cairo_fu06 <- cairo_full %>%  filter (YEAR==2006)
Gini(cairo_fu86 $TOTAL_ASSET,na.rm = TRUE)
Gini(cairo_fu96 $TOTAL_ASSET,na.rm = TRUE)
Gini(cairo_fu06 $TOTAL_ASSET,na.rm = TRUE)


write.csv(cairo_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cairo_PUBl_ASSET.csv", row.names = TRUE)
write.csv(cairo_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cairo_PRIV_ASSET.csv", row.names = TRUE)
write.csv(cairo_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/cairo_TOTAL_ASSET.csv", row.names = TRUE)

######################Logistic regressions for selected variables############################
############################################################################### FOR WATSUP BH##################################

alexandria_WATER <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,OCCISCO_b,OCCISCO_low,water_b,CITY)
table(alexandria_WATER$OCCISCO_b)
table(alexandria_WATER$YEAR)
alexandria_WATER <- alexandria_WATER %>% filter(OCCISCO_b!=0)%>% filter(WATSUP!=99)


alexandria_WATER_00 <- alexandria_WATER %>% filter(alexandria_WATER$YEAR==1996)
alexandria_WATER_10 <- alexandria_WATER %>% filter(alexandria_WATER$YEAR==2006)

mylogit_00 <- glm(water_b ~ OCCISCO_low, data = alexandria_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(water_b ~ OCCISCO_low, data = alexandria_WATER_10, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}

WATER_T3 <- summary(mylogit_10)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


#################################### FOR OWNERSHIP ##################################

alexandria_OWN <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,OCCISCO_b,OCCISCO_low,owner_b,CITY)
table(alexandria_OWN$OWNERSHIP)
alexandria_OWN <- alexandria_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9) %>% filter(OCCISCO_b!=0)

table(alexandria_OWN$OWNERSHIP)
table(alexandria_OWN$owner_b)

alexandria_OWN$owner_b <- factor(alexandria_OWN$owner_b)

alexandria_OWN$YEAR <- factor(alexandria_OWN$YEAR)
alexandria_OWN_00 <- alexandria_OWN %>% filter(alexandria_OWN$YEAR==1996)
alexandria_OWN_10 <- alexandria_OWN %>% filter(alexandria_OWN$YEAR==2006)

mylogit_00 <- glm(owner_b ~ OCCISCO_low, data = alexandria_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(owner_b ~ OCCISCO_low, data = alexandria_OWN_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}

OWN_T3 <- summary(mylogit_00)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}


#################################### FOR ELECTRIC ##################################

alexandria_ELECTRIC <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,OCCISCO_b,OCCISCO_low,eletric_b,CITY)
table(alexandria_ELECTRIC$ELECTRIC)
alexandria_ELECTRIC <- alexandria_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)%>% filter(OCCISCO_b!=0)

table(alexandria_ELECTRIC$ELECTRIC)
table(alexandria_ELECTRIC$eletric_b)

alexandria_ELECTRIC$eletric_b <- factor(alexandria_ELECTRIC$eletric_b)

alexandria_ELECTRIC$YEAR <- factor(alexandria_ELECTRIC$YEAR)
alexandria_ELECTRIC_00 <- alexandria_ELECTRIC %>% filter(alexandria_ELECTRIC$YEAR==1996)
alexandria_ELECTRIC_10 <- alexandria_ELECTRIC %>% filter(alexandria_ELECTRIC$YEAR==2006)

mylogit_00 <- glm(eletric_b ~ OCCISCO_low, data = alexandria_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(eletric_b ~ OCCISCO_low, data = alexandria_ELECTRIC_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}

ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

alexandria_SEWG <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,OCCISCO_b,OCCISCO_low,CITY)
table(alexandria_SEWG$SEWAGE)
alexandria_SEWG <- alexandria_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)%>% filter(OCCISCO_b!=0)

table(alexandria_SEWG$SEWAGE)
table(alexandria_SEWG$sewage_b)

alexandria_SEWG$sewage_b <- factor(alexandria_SEWG$sewage_b)

alexandria_SEWG$YEAR <- factor(alexandria_SEWG$YEAR)
alexandria_SEWG_00 <- alexandria_SEWG %>% filter(alexandria_SEWG$YEAR==1996)
alexandria_SEWG_10 <- alexandria_SEWG %>% filter(alexandria_SEWG$YEAR==2006)

mylogit_00 <- glm(sewage_b ~ OCCISCO_low, data = alexandria_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(sewage_b ~ OCCISCO_low, data = alexandria_SEWG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

SEWG_T3 <- summary(mylogit_10)$coefficients
SEWG_T3_z <- SEWG_T2[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

alexandria_AUTOS <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OCCISCO_b,OCCISCO_low,AUTOS,autos_b,CITY)
table(alexandria_AUTOS$AUTOS)
alexandria_AUTOS <- alexandria_AUTOS %>% filter(AUTOS!=9)%>% filter(OCCISCO_b!=0)

table(alexandria_AUTOS$AUTOS)
table(alexandria_AUTOS$autos_b)

alexandria_AUTOS$autos_b <- factor(alexandria_AUTOS$autos_b)

alexandria_AUTOS$YEAR <- factor(alexandria_AUTOS$YEAR)
alexandria_AUTOS_00 <- alexandria_AUTOS %>% filter(alexandria_AUTOS$YEAR==1996)
alexandria_AUTOS_10 <- alexandria_AUTOS %>% filter(alexandria_AUTOS$YEAR==2006)

mylogit_00 <- glm(autos_b ~ OCCISCO_low, data = alexandria_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(autos_b ~ OCCISCO_low, data = alexandria_AUTOS_10, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T2[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

alexandria_REFRIG <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OCCISCO_b,OCCISCO_low,REFRIG,refrig_b,CITY)
table(alexandria_REFRIG$REFRIG)
alexandria_REFRIG <- alexandria_REFRIG %>% filter(REFRIG!=0)%>% filter(OCCISCO_b!=0)

table(alexandria_REFRIG$REFRIG)
table(alexandria_REFRIG$refrig_b)

alexandria_REFRIG$refrig_b <- factor(alexandria_REFRIG$refrig_b)

alexandria_REFRIG$YEAR <- factor(alexandria_REFRIG$YEAR)
alexandria_REFRIG_00 <- alexandria_REFRIG %>% filter(alexandria_REFRIG$YEAR==1996)
alexandria_REFRIG_10 <- alexandria_REFRIG %>% filter(alexandria_REFRIG$YEAR==2006)

mylogit_00 <- glm(refrig_b ~ OCCISCO_low, data = alexandria_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(refrig_b ~ OCCISCO_low, data = alexandria_REFRIG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

alexandria_TV <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,OCCISCO_b,OCCISCO_low,SERIAL,HHWT,TV,tv_b,CITY)
table(alexandria_TV$TV)
alexandria_TV <- alexandria_TV %>% filter(TV!=0) %>% filter(TV!=99)%>% filter(OCCISCO_b!=0)

table(alexandria_TV$TV)
table(alexandria_TV$tv_b)

alexandria_TV$tv_b <- factor(alexandria_TV$tv_b)

alexandria_TV$YEAR <- factor(alexandria_TV$YEAR)
alexandria_TV_00 <- alexandria_TV %>% filter(alexandria_TV$YEAR==1996)
alexandria_TV_10 <- alexandria_TV %>% filter(alexandria_TV$YEAR==2006)


mylogit_00 <- glm(tv_b ~ OCCISCO_low, data = alexandria_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(tv_b ~ OCCISCO_low, data = alexandria_TV_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
TV_T3 <- summary(mylogit_00)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

alexandria_TOILET <- alexandria_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,OCCISCO_b,OCCISCO_low,SERIAL,HHWT,TOILET,toilet_b,CITY)
table(alexandria_TOILET$TOILET)
alexandria_TOILET <- alexandria_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)%>% filter(OCCISCO_b!=0)

table(alexandria_TOILET$TOILET)
table(alexandria_TOILET$toilet_b)

alexandria_TOILET$toilet_b <- factor(alexandria_TOILET$toilet_b)

alexandria_TOILET$YEAR <- factor(alexandria_TOILET$YEAR)
alexandria_TOILET_00 <- alexandria_TOILET %>% filter(alexandria_TOILET$YEAR==1996)
alexandria_TOILET_10 <- alexandria_TOILET %>% filter(alexandria_TOILET$YEAR==2006)

mylogit_00 <- glm(toilet_b ~ OCCISCO_low, data = alexandria_TOILET_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(toilet_b ~ OCCISCO_low, data = alexandria_TOILET_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TOILET_T2 <- summary(mylogit_00)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### consolidating data for alexandria ##################################

alexandria_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,SEWG_T2_z,SEWG_T3_z,AUTOS_T2_z,AUTOS_T3_z,REFRIG_T2_z,REFRIG_T3_z,TV_T2_z,TV_T3_z,TOILET_T2_z,TOILET_T3_z)
alexandria_logit$CITY <- "Alexandria"

gc()

######################Logistic regressions for selected variables############################
############################################################################### FOR WATSUP BH##################################

cairo_WATER <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,OCCISCO_b,OCCISCO_low,water_b,CITY)
table(cairo_WATER$OCCISCO_b)
table(cairo_WATER$YEAR)
cairo_WATER <- cairo_WATER %>% filter(OCCISCO_b!=0)%>% filter(WATSUP!=99)


cairo_WATER_00 <- cairo_WATER %>% filter(cairo_WATER$YEAR==1996)
cairo_WATER_10 <- cairo_WATER %>% filter(cairo_WATER$YEAR==2006)

mylogit_00 <- glm(water_b ~ OCCISCO_low, data = cairo_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(water_b ~ OCCISCO_low, data = cairo_WATER_10, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
WATER_T3 <- summary(mylogit_10)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

cairo_OWN <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,OCCISCO_b,OCCISCO_low,owner_b,CITY)
table(cairo_OWN$OWNERSHIP)
cairo_OWN <- cairo_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9) %>% filter(OCCISCO_b!=0)

table(cairo_OWN$OWNERSHIP)
table(cairo_OWN$owner_b)

cairo_OWN$owner_b <- factor(cairo_OWN$owner_b)

cairo_OWN$YEAR <- factor(cairo_OWN$YEAR)
cairo_OWN_00 <- cairo_OWN %>% filter(cairo_OWN$YEAR==1996)
cairo_OWN_10 <- cairo_OWN %>% filter(cairo_OWN$YEAR==2006)

mylogit_00 <- glm(owner_b ~ OCCISCO_low, data = cairo_OWN_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(owner_b ~ OCCISCO_low, data = cairo_OWN_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

OWN_T2 <- summary(mylogit_00)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
OWN_T3 <- summary(mylogit_00)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

cairo_ELECTRIC <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,OCCISCO_b,OCCISCO_low,eletric_b,CITY)
table(cairo_ELECTRIC$ELECTRIC)
cairo_ELECTRIC <- cairo_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=9)%>% filter(OCCISCO_b!=0)

table(cairo_ELECTRIC$ELECTRIC)
table(cairo_ELECTRIC$eletric_b)

cairo_ELECTRIC$eletric_b <- factor(cairo_ELECTRIC$eletric_b)

cairo_ELECTRIC$YEAR <- factor(cairo_ELECTRIC$YEAR)
cairo_ELECTRIC_00 <- cairo_ELECTRIC %>% filter(cairo_ELECTRIC$YEAR==1996)
cairo_ELECTRIC_10 <- cairo_ELECTRIC %>% filter(cairo_ELECTRIC$YEAR==2006)

mylogit_00 <- glm(eletric_b ~ OCCISCO_low, data = cairo_ELECTRIC_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(eletric_b ~ OCCISCO_low, data = cairo_ELECTRIC_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ELECTRIC_T2 <- summary(mylogit_00)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

cairo_SEWG <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,OCCISCO_b,OCCISCO_low,CITY)
table(cairo_SEWG$SEWAGE)
cairo_SEWG <- cairo_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)%>% filter(OCCISCO_b!=0)

table(cairo_SEWG$SEWAGE)
table(cairo_SEWG$sewage_b)

cairo_SEWG$sewage_b <- factor(cairo_SEWG$sewage_b)

cairo_SEWG$YEAR <- factor(cairo_SEWG$YEAR)
cairo_SEWG_00 <- cairo_SEWG %>% filter(cairo_SEWG$YEAR==1996)
cairo_SEWG_10 <- cairo_SEWG %>% filter(cairo_SEWG$YEAR==2006)

mylogit_00 <- glm(sewage_b ~ OCCISCO_low, data = cairo_SEWG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(sewage_b ~ OCCISCO_low, data = cairo_SEWG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

SEWG_T2 <- summary(mylogit_00)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}
SEWG_T3 <- summary(mylogit_10)$coefficients
SEWG_T3_z <- SEWG_T2[2,1:4]
SEWG_T3_z<-as.data.frame(SEWG_T3_z)
if (SEWG_T3_z[4,1] > 0.05) {SEWG_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

cairo_AUTOS <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OCCISCO_b,OCCISCO_low,AUTOS,autos_b,CITY)
table(cairo_AUTOS$AUTOS)
cairo_AUTOS <- cairo_AUTOS %>% filter(AUTOS!=9)%>% filter(OCCISCO_b!=0)

table(cairo_AUTOS$AUTOS)
table(cairo_AUTOS$autos_b)

cairo_AUTOS$autos_b <- factor(cairo_AUTOS$autos_b)

cairo_AUTOS$YEAR <- factor(cairo_AUTOS$YEAR)
cairo_AUTOS_00 <- cairo_AUTOS %>% filter(cairo_AUTOS$YEAR==1996)
cairo_AUTOS_10 <- cairo_AUTOS %>% filter(cairo_AUTOS$YEAR==2006)

mylogit_00 <- glm(autos_b ~ OCCISCO_low, data = cairo_AUTOS_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(autos_b ~ OCCISCO_low, data = cairo_AUTOS_10, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

AUTOS_T2 <- summary(mylogit_00)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}
AUTOS_T3 <- summary(mylogit_10)$coefficients
AUTOS_T3_z <- AUTOS_T2[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

cairo_REFRIG <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OCCISCO_b,OCCISCO_low,REFRIG,refrig_b,CITY)
table(cairo_REFRIG$REFRIG)
cairo_REFRIG <- cairo_REFRIG %>% filter(REFRIG!=0)%>% filter(OCCISCO_b!=0)

table(cairo_REFRIG$REFRIG)
table(cairo_REFRIG$refrig_b)

cairo_REFRIG$refrig_b <- factor(cairo_REFRIG$refrig_b)

cairo_REFRIG$YEAR <- factor(cairo_REFRIG$YEAR)
cairo_REFRIG_00 <- cairo_REFRIG %>% filter(cairo_REFRIG$YEAR==1996)
cairo_REFRIG_10 <- cairo_REFRIG %>% filter(cairo_REFRIG$YEAR==2006)

mylogit_00 <- glm(refrig_b ~ OCCISCO_low, data = cairo_REFRIG_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(refrig_b ~ OCCISCO_low, data = cairo_REFRIG_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

REFRIG_T2 <- summary(mylogit_00)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}
REFRIG_T3 <- summary(mylogit_10)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

cairo_TV <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,OCCISCO_b,OCCISCO_low,SERIAL,HHWT,TV,tv_b,CITY)
table(cairo_TV$TV)
cairo_TV <- cairo_TV %>% filter(TV!=0) %>% filter(TV!=99)%>% filter(OCCISCO_b!=0)

table(cairo_TV$TV)
table(cairo_TV$tv_b)

cairo_TV$tv_b <- factor(cairo_TV$tv_b)

cairo_TV$YEAR <- factor(cairo_TV$YEAR)
cairo_TV_00 <- cairo_TV %>% filter(cairo_TV$YEAR==1996)
cairo_TV_10 <- cairo_TV %>% filter(cairo_TV$YEAR==2006)


mylogit_00 <- glm(tv_b ~ OCCISCO_low, data = cairo_TV_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(tv_b ~ OCCISCO_low, data = cairo_TV_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TV_T2 <- summary(mylogit_00)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
TV_T3 <- summary(mylogit_00)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

cairo_TOILET <- cairo_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,OCCISCO_b,OCCISCO_low,SERIAL,HHWT,TOILET,toilet_b,CITY)
table(cairo_TOILET$TOILET)
cairo_TOILET <- cairo_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)%>% filter(OCCISCO_b!=0)

table(cairo_TOILET$TOILET)
table(cairo_TOILET$toilet_b)

cairo_TOILET$toilet_b <- factor(cairo_TOILET$toilet_b)

cairo_TOILET$YEAR <- factor(cairo_TOILET$YEAR)
cairo_TOILET_00 <- cairo_TOILET %>% filter(cairo_TOILET$YEAR==1996)
cairo_TOILET_10 <- cairo_TOILET %>% filter(cairo_TOILET$YEAR==2006)

mylogit_00 <- glm(toilet_b ~ OCCISCO_low, data = cairo_TOILET_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(toilet_b ~ OCCISCO_low, data = cairo_TOILET_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TOILET_T2 <- summary(mylogit_00)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### consolidating data for cairo ##################################

cairo_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,SEWG_T2_z,SEWG_T3_z,AUTOS_T2_z,AUTOS_T3_z,REFRIG_T2_z,REFRIG_T3_z,TV_T2_z,TV_T3_z,TOILET_T2_z,TOILET_T3_z)
cairo_logit$CITY <- "Cairo"

gc()

EGYPT_logit <- rbind(cairo_logit,alexandria_logit,sep = ".") 
write.csv(EGYPT_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/EGYPT_logit_OCC.csv"))

