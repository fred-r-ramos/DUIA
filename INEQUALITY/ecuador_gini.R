library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/ecuador")

geo2_ec90 <- read_sf("geo2_ec1990.shp")
geo2_ec01 <- read_sf("geo2_ec2001.shp")
geo2_ec10 <- read_sf("geo2_ec2010.shp")
AUE_quito <- read_sf("quito_studyArea.shp")
AUE_quito <- st_transform(AUE_quito, 4326)
geo2_ec90 <- st_transform(geo2_ec90, 4326)

st_crs(geo2_ec01)
print(crs)

sf::sf_use_s2(FALSE)
centroid_ec90 <- st_centroid(geo2_ec90)
centroid_ec01 <- st_centroid(geo2_ec01)
centroid_ec10 <- st_centroid(geo2_ec10)

quito_90 <- geo2_ec90[st_intersection(AUE_quito,centroid_ec90),]
quito_90['CITY']='quito'
quito_01 <- geo2_ec01[st_intersection(AUE_quito,centroid_ec01),]
quito_01['CITY']='quito'
quito_10 <- geo2_ec10[st_intersection(AUE_quito,centroid_ec10),]
quito_10['CITY']='quito'


plot(st_geometry(geo2_ec90), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_ec10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00237.xml")
ddi <-read_ipums_ddi("ipumsi_00248.xml")
ecuador <- read_ipums_micro(ddi)

names(ecuador)
table(ecuador$YEAR)
######################creating binary variables for logit regressions

names(ecuador)

# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "GEO2_EC1990" "GEO2_EC2001" "GEO2_EC2010"
# [9] "OWNERSHIP"   "OWNERSHIPD"  "ELECTRIC"    "WATSUP"      "SEWAGE"      "TRASH"       "TOILET"      "FLOOR"      
# [17] "ROOF"        "PERNUM"      "PERWT"       "AGE"         "SEX"         "SCHOOL"      "LIT"         "EDATTAIN"   
# [25] "EDATTAIND"   "YRSCHOOL"    "EMPSTAT"     "EMPSTATD"    "LABFORCE"    "OCCISCO"     "OCC"         "INDGEN"     


table(ecuador$OWNERSHIP)
ecuador$owner_b <- ifelse(ecuador$OWNERSHIP ==1,1,0)
table(ecuador$ELECTRIC)
ecuador$eletric_b <- ifelse(ecuador$ELECTRIC ==1,1,0)
table(ecuador$WATSUP)
ecuador$water_b <- ifelse(ecuador$WATSUP ==11,1,0)
table(ecuador$SEWAGE)
ecuador$sewage_b <- ifelse(ecuador$SEWAGE ==11|ecuador$SEWAGE ==12,1,0)
table(ecuador$TRASH)
ecuador$trash_b <- ifelse(ecuador$TRASH ==11|ecuador$TRASH ==12,1,0)#
table(ecuador$TOILET)
ecuador$toilet_b <- ifelse(ecuador$TOILET >19,1,0)
table(ecuador$FLOOR)
ecuador$floor_b <- ifelse(ecuador$FLOOR !=100,1,0)
table(ecuador$ROOF)
ecuador$roof_b <- ifelse(ecuador$ROOF==15|ecuador$ROOF==12|ecuador$ROOF==28,1,0)

names(ecuador)
gc()

#checking if the variables are available in both years
table(ecuador$owner_b,ecuador$YEAR)
table(ecuador$eletric_b,ecuador$YEAR)
table(ecuador$water_b,ecuador$YEAR)
table(ecuador$sewage_b,ecuador$YEAR)
table(ecuador$trash_b,ecuador$YEAR)#
table(ecuador$toilet_b,ecuador$YEAR)
table(ecuador$floor_b,ecuador$YEAR)
table(ecuador$roof_b,ecuador$YEAR)#

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
ecuador$PUBl_ASSET<- ifelse(is.na(ecuador$eletric_b), 0, ecuador$eletric_b) +
  ifelse(is.na(ecuador$water_b), 0, ecuador$water_b) +
  ifelse(is.na(ecuador$sewage_b), 0, ecuador$sewage_b)
ecuador %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ecuador$PRIV_ASSET <- ifelse(is.na(ecuador$owner_b), 0, ecuador$owner_b) +
  ifelse(is.na(ecuador$toilet_b), 0, ecuador$toilet_b) +
  ifelse(is.na(ecuador$floor_b), 0, ecuador$floor_b)
ecuador %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ecuador$TOTAL_ASSET <- ifelse(is.na(ecuador$eletric_b), 0, ecuador$eletric_b) +
  ifelse(is.na(ecuador$water_b), 0, ecuador$water_b) +
  ifelse(is.na(ecuador$sewage_b), 0, ecuador$sewage_b) +
  ifelse(is.na(ecuador$owner_b), 0, ecuador$owner_b) +
  ifelse(is.na(ecuador$toilet_b), 0, ecuador$toilet_b) +
  ifelse(is.na(ecuador$floor_b), 0, ecuador$floor_b)
assets<-ecuador %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
table(ecuador$YEAR)

##Creating field for join

ecuador$IPUM1990 <- as.integer(ecuador$GEO2_EC1990)
ecuador$IPUM2001 <- as.integer(ecuador$GEO2_EC2001)
ecuador$IPUM2010 <- as.integer(ecuador$GEO2_EC2010)
quito_90$IPUM1990 <- as.integer(quito_90$IPUM1990)
quito_01$IPUM2001 <- as.integer(quito_01$IPUM2001)
quito_10$IPUM2010 <- as.integer(quito_10$IPUM2010)

##Joining by year

quito_90 <- ecuador %>% inner_join(quito_90, by="IPUM1990")
quito_01 <- ecuador %>% inner_join(quito_01, by="IPUM2001")
quito_10 <- ecuador %>% inner_join(quito_10, by="IPUM2010")

names(quito_90)
names(quito_01)
names(quito_10)

quito_90 <- select(quito_90, -c(CANT1990))
quito_01 <- select(quito_01, -c(CANT2001))
quito_10 <- select(quito_10, -c(CANT2010))

##Merging all years into one table
quito_full <- rbind(quito_90,quito_01,quito_10)
names(quito_full)

table(quito_full$PERWT)

##Excluding specific columns for the unifeied dataset
quito_full<- select(quito_full, -c(GEO2_EC1990,GEO2_EC2001,GEO2_EC2010,IPUM1990,IPUM2001,IPUM2010,geometry))

table(quito_full$CITY)

names(quito_full)

table(quito_full$YEAR)

quito_full <- quito_full %>%  filter (quito_full$YRSCHOOL < 90)

quito_full <- quito_full %>%  filter (quito_full$AGE >15)

quito_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(quito_full$YRSCHOOL)
summary(quito_full$YRSCHOOL)

quito_fu01 <- quito_full %>%  filter (YEAR==2001)
quito_fu90 <- quito_full %>%  filter (YEAR==1990)
quito_fu10 <- quito_full %>%  filter (YEAR==2010)

Gini(quito_fu01 $YRSCHOOL,na.rm = TRUE)
Gini(quito_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(quito_fu01 $TOTAL_ASSET,na.rm = TRUE)
Gini(quito_fu10 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_quit01 <- Lc(quito_fu01$YRSCHOOL, n = rep(1,length(quito_fu01$YRSCHOOL)), plot = TRUE)
Lc_quit10 <- Lc(quito_fu10$YRSCHOOL, n = rep(1,length(quito_fu10$YRSCHOOL)), plot = TRUE)


plot(Lc_quit01,col='blue', main = "Lorenz Curve - Quito")
lines(Lc_quit10, col='red')

table(quito_full$OCCISCO)


quito_full$OCCISCO_b <- ifelse(quito_full$OCCISCO ==1|quito_full$OCCISCO ==2,1,0)
quito_full$OCCISCO_b <- ifelse(quito_full$OCCISCO ==3|quito_full$OCCISCO ==4|quito_full$OCCISCO ==5,2,quito_full$OCCISCO_b)
quito_full$OCCISCO_b <- ifelse(quito_full$OCCISCO ==6|quito_full$OCCISCO ==7|quito_full$OCCISCO ==8|quito_full$OCCISCO ==9,3,quito_full$OCCISCO_b)
table(quito_full$OCCISCO_b)

quito_full_OCCISCO_b <- quito_full %>% select(YEAR,OCCISCO_b,PERWT)
quito_full_OCCISCO_b <- quito_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(quito_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- quito_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- quito_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- quito_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- quito_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

ecuador_ababa_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
ecuador_ababa_OCCISCO_b<- ecuador_ababa_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
ecuador_ababa_OCCISCO_b<- ecuador_ababa_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

ecuador_ababa_OCCISCO_b


# for PUBl_ASSET
quito_PUBl_ASSET<-quito_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- quito_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
quito_PUBl_ASSET <- quito_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
quito_PUBl_ASSET <- quito_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
quito_PUBl_ASSET$CITY<-"quito"
# for PRIV_ASSET
quito_PRIV_ASSET<-quito_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- quito_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
quito_PRIV_ASSET <- quito_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
quito_PRIV_ASSET <- quito_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
quito_PRIV_ASSET$CITY<-"quito"
# for TOTAL_ASSET
quito_TOTAL_ASSET<-quito_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- quito_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
quito_TOTAL_ASSET <- quito_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
quito_TOTAL_ASSET <- quito_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
quito_TOTAL_ASSET$CITY<-"quito"
write.csv(quito_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/quito_PUBl_ASSET.csv", row.names = TRUE)
write.csv(quito_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/quito_PRIV_ASSET.csv", row.names = TRUE)
write.csv(quito_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/quito_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

quito_WATER <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(quito_WATER$WATSUP)
quito_WATER <- quito_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(quito_WATER$WATSUP)
table(quito_WATER$water_b)

quito_WATER$water_b <- factor(quito_WATER$water_b)

quito_WATER$YEAR <- factor(quito_WATER$YEAR)
quito_WATER_01 <- quito_WATER %>% filter(quito_WATER$YEAR==2001)
quito_WATER_10 <- quito_WATER %>% filter(quito_WATER$YEAR==2010)

mylogit_01 <- glm(water_b ~ YRSCHOOL, data = quito_WATER_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(water_b ~ YRSCHOOL, data = quito_WATER_10, family = "binomial")
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

gc()

#################################### FOR OWNERSHIP ##################################

quito_OWN <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(quito_OWN$OWNERSHIP)
quito_OWN <- quito_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(quito_OWN$OWNERSHIP)
table(quito_OWN$owner_b)

quito_OWN$owner_b <- factor(quito_OWN$owner_b)

quito_OWN$YEAR <- factor(quito_OWN$YEAR)
quito_OWN_01 <- quito_OWN %>% filter(quito_OWN$YEAR==2001)
quito_OWN_10 <- quito_OWN %>% filter(quito_OWN$YEAR==2010)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = quito_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(owner_b ~ YRSCHOOL, data = quito_OWN_10, family = "binomial")
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

quito_SEWG <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(quito_SEWG$SEWAGE)
quito_SEWG <- quito_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(quito_SEWG$SEWAGE)
table(quito_SEWG$sewage_b)

quito_SEWG$sewage_b <- factor(quito_SEWG$sewage_b)

quito_SEWG$YEAR <- factor(quito_SEWG$YEAR)
quito_SEWG_01 <- quito_SEWG %>% filter(quito_SEWG$YEAR==2001)
quito_SEWG_10 <- quito_SEWG %>% filter(quito_SEWG$YEAR==2010)

mylogit_01 <- glm(sewage_b ~ YRSCHOOL, data = quito_SEWG_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(sewage_b ~ YRSCHOOL, data = quito_SEWG_10, family = "binomial")
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


#################################### FOR TOILET ##################################

quito_TOILET <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(quito_TOILET$TOILET)
quito_TOILET <- quito_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(quito_TOILET$TOILET)
table(quito_TOILET$toilet_b)

quito_TOILET$toilet_b <- factor(quito_TOILET$toilet_b)

quito_TOILET$YEAR <- factor(quito_TOILET$YEAR)
quito_TOILET_01 <- quito_TOILET %>% filter(quito_TOILET$YEAR==2001)
quito_TOILET_10 <- quito_TOILET %>% filter(quito_TOILET$YEAR==2010)

mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = quito_TOILET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(toilet_b ~ YRSCHOOL, data = quito_TOILET_10, family = "binomial")
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

#################################### FOR ELECTRIC ##################################

quito_ELECTRIC <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(quito_ELECTRIC$ELECTRIC)
quito_ELECTRIC <- quito_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(quito_ELECTRIC$ELECTRIC)
table(quito_ELECTRIC$eletric_b)

quito_ELECTRIC$eletric_b <- factor(quito_ELECTRIC$eletric_b)

quito_ELECTRIC$YEAR <- factor(quito_ELECTRIC$YEAR)
quito_ELECTRIC_01 <- quito_ELECTRIC %>% filter(quito_ELECTRIC$YEAR==2001)
quito_ELECTRIC_10 <- quito_ELECTRIC %>% filter(quito_ELECTRIC$YEAR==2010)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = quito_ELECTRIC_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(eletric_b ~ YRSCHOOL, data = quito_ELECTRIC_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ELECTRIC_T2 <- summary(mylogit_01)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_10)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

quito_TRASH <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(quito_TRASH$TRASH)
quito_TRASH <- quito_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(quito_TRASH$TRASH)
table(quito_TRASH$trash_b)

quito_TRASH$trash_b <- factor(quito_TRASH$trash_b)

quito_TRASH$YEAR <- factor(quito_TRASH$YEAR)
quito_TRASH_10 <- quito_TRASH %>% filter(quito_TRASH$YEAR==2010)

mylogit_10 <- glm(trash_b ~ YRSCHOOL, data = quito_TRASH_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

TRASH_T3 <- summary(mylogit_10)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

quito_FLOOR <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(quito_FLOOR$FLOOR)
quito_FLOOR <- quito_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(quito_FLOOR$FLOOR)
table(quito_FLOOR$floor_b)

quito_FLOOR$floor_b <- factor(quito_FLOOR$floor_b)

quito_FLOOR$YEAR <- factor(quito_FLOOR$YEAR)
quito_FLOOR_01 <- quito_FLOOR %>% filter(quito_FLOOR$YEAR==2001)
quito_FLOOR_10 <- quito_FLOOR %>% filter(quito_FLOOR$YEAR==2010)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = quito_FLOOR_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(floor_b ~ YRSCHOOL, data = quito_FLOOR_10, family = "binomial")
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

#################################### FOR ROOF ##################################

quito_ROOF <- quito_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(quito_ROOF$ROOF)
quito_ROOF <- quito_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(quito_ROOF$ROOF)
table(quito_ROOF$roof_b)

quito_ROOF$roof_b <- factor(quito_ROOF$roof_b)

quito_ROOF$YEAR <- factor(quito_ROOF$YEAR)
quito_ROOF_10 <- quito_ROOF %>% filter(quito_ROOF$YEAR==2010)

mylogit_10 <- glm(roof_b ~ YRSCHOOL, data = quito_ROOF_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

ROOF_T3 <- summary(mylogit_10)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}


quito_logit <- cbind(WATER_T2_z,OWN_T2_z,OWN_T3_z,SEWG_T2_z,SEWG_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T3_z) 
quito_logit$CITY <- "Quito"


#################################################Saving an unique table results join

ECUADOR_logit <- rbind(quito_logit,sep = ".") 
write.csv(ECUADOR_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/ECUADOR_logit.csv"))



