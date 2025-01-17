library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/ethiopia")

sf::sf_use_s2(FALSE)

geo2_et84 <- read_sf("geo2_et1984.shp")
geo2_et94 <- read_sf("geo2_et1994.shp")
geo2_et07 <- read_sf("geo2_et2007.shp")
AUE_addisababa <- read_sf("Addis_Ababa_studyArea.shp")
AUE_addisababa <- st_transform(AUE_addisababa, 4326)

centroid_et84 <- st_centroid(geo2_et84)
centroid_et94 <- st_centroid(geo2_et94)
centroid_et07 <- st_centroid(geo2_et07)

addis_ababa_84 <- geo2_et84[st_intersection(AUE_addisababa,centroid_et84),]
addis_ababa_84['CITY']='addis ababa'
addis_ababa_94 <-  geo2_et94[st_intersection(AUE_addisababa,centroid_et94),]
addis_ababa_94['CITY']='addis ababa'
addis_ababa_07 <-  geo2_et07[st_intersection(AUE_addisababa,centroid_et07),]
addis_ababa_07['CITY']='addis ababa'


plot(st_geometry(addis_ababa_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_et07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 
#ddi <-read_ipums_ddi("ipumsi_00218.xml")
ddi <-read_ipums_ddi("ipumsi_00251.xml")
ethiopia <- read_ipums_micro(ddi)

names(ethiopia)

######################creating binary variables for logit regressions

names(ethiopia)


# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "FORMTYPE"    "GEO2_ET1984"
# [8] "GEO2_ET1994" "GEO2_ET2007" "OWNERSHIP"   "OWNERSHIPD"  "ELECTRIC"    "WATSUP"      "TRASH"      
# [15] "TV"          "TOILET"      "FLOOR"       "ROOF"        "PERNUM"      "PERWT"       "RESIDENT"   
# [22] "AGE"         "SEX"         "SCHOOL"      "LIT"         "EDATTAIN"    "EDATTAIND"   "YRSCHOOL"   
# [29] "EMPSTAT"     "EMPSTATD"    "LABFORCE"    "OCCISCO"     "OCC"         "INDGEN"     



table(ethiopia$OWNERSHIP)
ethiopia$owner_b <- ifelse(ethiopia$OWNERSHIP ==1,1,0)
table(ethiopia$ELECTRIC)
ethiopia$eletric_b <- ifelse(ethiopia$ELECTRIC ==1,1,0)
table(ethiopia$WATSUP)
ethiopia$water_b <- ifelse(ethiopia$WATSUP ==11,1,0)
table(ethiopia$TRASH)
ethiopia$trash_b <- ifelse(ethiopia$TRASH ==10|ethiopia$TRASH ==12,1,0)
table(ethiopia$TV)
ethiopia$tv_b <- ifelse(ethiopia$TV ==20,1,0)
table(ethiopia$TOILET)
ethiopia$toilet_b <- ifelse(ethiopia$TOILET ==20|ethiopia$TOILET ==21|ethiopia$TOILET ==22,1,0)
table(ethiopia$FLOOR)
ethiopia$floor_b <- ifelse(ethiopia$FLOOR !=100|ethiopia$FLOOR !=999,1,0)
table(ethiopia$ROOF)
ethiopia$roof_b <- ifelse(ethiopia$ROOF==11|ethiopia$ROOF==28|ethiopia$ROOF==34,1,0)

names(ethiopia)
gc()

#checking if the variables are available in both years
table(ethiopia$owner_b,ethiopia$YEAR)
table(ethiopia$eletric_b,ethiopia$YEAR)
table(ethiopia$water_b,ethiopia$YEAR)
table(ethiopia$trash_b,ethiopia$YEAR)#
table(ethiopia$tv_b,ethiopia$YEAR)
table(ethiopia$toilet_b,ethiopia$YEAR)
table(ethiopia$floor_b,ethiopia$YEAR)#
table(ethiopia$roof_b,ethiopia$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
ethiopia$PUBl_ASSET <-  ifelse(is.na(ethiopia$eletric_b), 0, ethiopia$eletric_b)+
  ifelse(is.na(ethiopia$water_b), 0, ethiopia$water_b)
ethiopia %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ethiopia$PRIV_ASSET <- ifelse(is.na(ethiopia$owner_b), 0, ethiopia$owner_b)+
  ifelse(is.na(ethiopia$tv_b), 0, ethiopia$tv_b)+
  ifelse(is.na(ethiopia$roof_b), 0, ethiopia$roof_b) +
  ifelse(is.na(ethiopia$toilet_b), 0, ethiopia$toilet_b)+
  ifelse(is.na(ethiopia$roof_b), 0, ethiopia$roof_b)
ethiopia %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ethiopia$TOTAL_ASSET <- ifelse(is.na(ethiopia$eletric_b), 0, ethiopia$eletric_b)+
  ifelse(is.na(ethiopia$water_b), 0, ethiopia$water_b)+
  ifelse(is.na(ethiopia$owner_b), 0, ethiopia$owner_b)+
  ifelse(is.na(ethiopia$tv_b), 0, ethiopia$tv_b)+
  ifelse(is.na(ethiopia$roof_b), 0, ethiopia$roof_b) +
  ifelse(is.na(ethiopia$toilet_b), 0, ethiopia$toilet_b)+
  ifelse(is.na(ethiopia$roof_b), 0, ethiopia$roof_b)
assets<-ethiopia %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

ethiopia$IPUM1984 <- as.integer(ethiopia$GEO2_ET1984)
ethiopia$IPUM1994 <- as.integer(ethiopia$GEO2_ET1994)
ethiopia$IPUM2007 <- as.integer(ethiopia$GEO2_ET2007)
addis_ababa_84$IPUM1984 <- as.integer(addis_ababa_84$IPUM1984)
addis_ababa_94$IPUM1994 <- as.integer(addis_ababa_94$IPUM1994)
addis_ababa_07$IPUM2007 <- as.integer(addis_ababa_07$IPUM2007)


##Joining by year

addis_ababa_84 <- ethiopia %>% inner_join(addis_ababa_84, by="IPUM1984")
addis_ababa_94 <- ethiopia %>% inner_join(addis_ababa_94, by="IPUM1994")
addis_ababa_07 <- ethiopia %>% inner_join(addis_ababa_07, by="IPUM2007")

names(addis_ababa_84)
names(addis_ababa_94)
names(addis_ababa_07)

addis_ababa_84 <- select(addis_ababa_84, -c(AWRJ1984))
addis_ababa_94 <- select(addis_ababa_94, -c(ZONE1994))
addis_ababa_07 <- select(addis_ababa_07, -c(ZONE2007))

##Merging all years into one table
addis_ababa_full <- rbind(addis_ababa_84,addis_ababa_94,addis_ababa_07)
names(addis_ababa_full)

addis_ababa_full <- addis_ababa_full %>%  filter (addis_ababa_full$YRSCHOOL < 90)
# 
addis_ababa_full <- addis_ababa_full %>%  filter (addis_ababa_full$AGE >15)
# 
addis_ababa_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

table(addis_ababa_full$YEAR)
mean(addis_ababa_full$YRSCHOOL)
summary(addis_ababa_full$YRSCHOOL)

addis_ababa_fu84 <- addis_ababa_full %>%  filter (YEAR==1984)
addis_ababa_fu94 <- addis_ababa_full %>%  filter (YEAR==1994)
addis_ababa_fu07 <- addis_ababa_full %>%  filter (YEAR==2007)

Gini(addis_ababa_fu84 $YRSCHOOL,na.rm = TRUE)
Gini(addis_ababa_fu94 $YRSCHOOL,na.rm = TRUE)
Gini(addis_ababa_fu07 $YRSCHOOL,na.rm = TRUE)
Gini(addis_ababa_fu84 $TOTAL_ASSET,na.rm = TRUE)
Gini(addis_ababa_fu94 $TOTAL_ASSET,na.rm = TRUE)
Gini(addis_ababa_fu07 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_add84 <- Lc(addis_ababa_fu84$YRSCHOOL, n = rep(1,length(addis_ababa_fu84$YRSCHOOL)), plot = TRUE)
Lc_add94 <- Lc(addis_ababa_fu94$YRSCHOOL, n = rep(1,length(addis_ababa_fu94$YRSCHOOL)), plot = TRUE)
Lc_acc07 <- Lc(addis_ababa_fu07$YRSCHOOL, n = rep(1,length(addis_ababa_fu07$YRSCHOOL)), plot = TRUE)
plot(Lc_add84,col='blue', main = "Lorenz Curve - Addis Ababa")
lines(Lc_add94, col='red')
lines(Lc_acc07,col='green')



ethiopia_fu84 <- ethiopia %>%  filter (YEAR==1984)
ethiopia_fu94 <- ethiopia %>%  filter (YEAR==1994)
ethiopia_fu07 <- ethiopia %>%  filter (YEAR==2007)

Gini(ethiopia_fu84 $YRSCHOOL,na.rm = TRUE)
Gini(ethiopia_fu94 $YRSCHOOL,na.rm = TRUE)
Gini(ethiopia_fu07 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves

Lc_eth84 <- Lc(ethiopia_fu84$YRSCHOOL, n = rep(1,length(ethiopia_fu84$YRSCHOOL)), plot = TRUE)
Lc_eth94 <- Lc(ethiopia_fu94$YRSCHOOL, n = rep(1,length(ethiopia_fu94$YRSCHOOL)), plot = TRUE)
Lc_eth07 <- Lc(ethiopia_fu07$YRSCHOOL, n = rep(1,length(ethiopia_fu07$YRSCHOOL)), plot = TRUE)

plot(Lc_eth84,col='blue', main = "Lorenz Curve - Ethiopia")
lines(Lc_eth94, col='red')
lines(Lc_eth07,col='green')

table(addis_ababa_full$OCCISCO)


addis_ababa_full$OCCISCO_b <- ifelse(addis_ababa_full$OCCISCO ==1|addis_ababa_full$OCCISCO ==2,1,0)
addis_ababa_full$OCCISCO_b <- ifelse(addis_ababa_full$OCCISCO ==3|addis_ababa_full$OCCISCO ==4|addis_ababa_full$OCCISCO ==5,2,addis_ababa_full$OCCISCO_b)
addis_ababa_full$OCCISCO_b <- ifelse(addis_ababa_full$OCCISCO ==6|addis_ababa_full$OCCISCO ==7|addis_ababa_full$OCCISCO ==8|addis_ababa_full$OCCISCO ==9,3,addis_ababa_full$OCCISCO_b)
table(addis_ababa_full$OCCISCO_b)

addis_ababa_full_OCCISCO_b <- addis_ababa_full %>% select(YEAR,OCCISCO_b,PERWT)
addis_ababa_full_OCCISCO_b <- addis_ababa_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(addis_ababa_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- addis_ababa_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- addis_ababa_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- addis_ababa_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- addis_ababa_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

addis_ababa_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
addis_ababa_OCCISCO_b<- addis_ababa_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
addis_ababa_OCCISCO_b<- addis_ababa_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

addis_ababa_OCCISCO_b

# for PUBl_ASSET
addis_ababa_PUBl_ASSET<-addis_ababa_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- addis_ababa_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
addis_ababa_PUBl_ASSET <- addis_ababa_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
addis_ababa_PUBl_ASSET <- addis_ababa_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
addis_ababa_PUBl_ASSET$CITY<-"addis_ababa"
# for PRIV_ASSET
addis_ababa_PRIV_ASSET<-addis_ababa_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- addis_ababa_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
addis_ababa_PRIV_ASSET <- addis_ababa_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
addis_ababa_PRIV_ASSET <- addis_ababa_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
addis_ababa_PRIV_ASSET$CITY<-"addis_ababa"
# for TOTAL_ASSET
addis_ababa_TOTAL_ASSET<-addis_ababa_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- addis_ababa_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
addis_ababa_TOTAL_ASSET <- addis_ababa_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
addis_ababa_TOTAL_ASSET <- addis_ababa_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
addis_ababa_TOTAL_ASSET$CITY<-"addis_ababa"
write.csv(addis_ababa_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/addis_ababa_PUBl_ASSET.csv", row.names = TRUE)
write.csv(addis_ababa_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/addis_ababa_PRIV_ASSET.csv", row.names = TRUE)
write.csv(addis_ababa_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/addis_ababa_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

addis_ababa_WATER <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(addis_ababa_WATER$WATSUP)
table(addis_ababa_WATER$HHWT)
addis_ababa_WATER <- addis_ababa_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(addis_ababa_WATER$WATSUP)
table(addis_ababa_WATER$water_b)

addis_ababa_WATER$water_b <- factor(addis_ababa_WATER$water_b)

addis_ababa_WATER$YEAR <- factor(addis_ababa_WATER$YEAR)
addis_ababa_WATER_94 <- addis_ababa_WATER %>% filter(addis_ababa_WATER$YEAR==1994)
addis_ababa_WATER_07 <- addis_ababa_WATER %>% filter(addis_ababa_WATER$YEAR==2007)

mylogit_94 <- glm(water_b ~ YRSCHOOL, data = addis_ababa_WATER_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(water_b ~ YRSCHOOL, data = addis_ababa_WATER_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))
table(addis_ababa_WATER_07$water_b,addis_ababa_WATER_07$YRSCHOOL)

#####saving z-value and beta

WATER_T2 <- summary(mylogit_94)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_07)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


gc()

#################################### FOR OWNERSHIP ##################################

addis_ababa_OWN <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(addis_ababa_OWN$OWNERSHIP)
addis_ababa_OWN <- addis_ababa_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(addis_ababa_OWN$OWNERSHIP)
table(addis_ababa_OWN$owner_b)

addis_ababa_OWN$owner_b <- factor(addis_ababa_OWN$owner_b)

addis_ababa_OWN$YEAR <- factor(addis_ababa_OWN$YEAR)
addis_ababa_OWN_94 <- addis_ababa_OWN %>% filter(addis_ababa_OWN$YEAR==1994)
addis_ababa_OWN_07 <- addis_ababa_OWN %>% filter(addis_ababa_OWN$YEAR==2007)

# mylogit <- glm(owner_b ~ YRSCHOOL + YEAR, data = buenos_aires_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))

mylogit_94 <- glm(owner_b ~ YRSCHOOL, data = addis_ababa_OWN_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(owner_b ~ YRSCHOOL, data = addis_ababa_OWN_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

OWN_T2 <- summary(mylogit_94)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_07)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR TOILET ##################################

addis_ababa_TOILET <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(addis_ababa_TOILET$TOILET)
addis_ababa_TOILET <- addis_ababa_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(addis_ababa_TOILET$TOILET)
table(addis_ababa_TOILET$toilet_b)

addis_ababa_TOILET$toilet_b <- factor(addis_ababa_TOILET$toilet_b)

addis_ababa_TOILET$YEAR <- factor(addis_ababa_TOILET$YEAR)
addis_ababa_TOILET_94 <- addis_ababa_TOILET %>% filter(addis_ababa_TOILET$YEAR==1994)
addis_ababa_TOILET_07 <- addis_ababa_TOILET %>% filter(addis_ababa_TOILET$YEAR==2007)

mylogit_94 <- glm(toilet_b ~ YRSCHOOL, data = addis_ababa_TOILET_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(toilet_b ~ YRSCHOOL, data = addis_ababa_TOILET_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

TOILET_T2 <- summary(mylogit_94)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_07)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

addis_ababa_ELECTRIC <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(addis_ababa_ELECTRIC$ELECTRIC)
addis_ababa_ELECTRIC <- addis_ababa_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(addis_ababa_ELECTRIC$ELECTRIC)
table(addis_ababa_ELECTRIC$eletric_b)

addis_ababa_ELECTRIC$eletric_b <- factor(addis_ababa_ELECTRIC$eletric_b)

addis_ababa_ELECTRIC$YEAR <- factor(addis_ababa_ELECTRIC$YEAR)
addis_ababa_ELECTRIC_94 <- addis_ababa_ELECTRIC %>% filter(addis_ababa_ELECTRIC$YEAR==1994)
addis_ababa_ELECTRIC_07 <- addis_ababa_ELECTRIC %>% filter(addis_ababa_ELECTRIC$YEAR==2007)

mylogit_94 <- glm(eletric_b ~ YRSCHOOL, data = addis_ababa_ELECTRIC_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(eletric_b ~ YRSCHOOL, data = addis_ababa_ELECTRIC_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

ELECTRIC_T2 <- summary(mylogit_94)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_07)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

addis_ababa_TRASH <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TRASH,trash_b,YRSCHOOL,CITY)
table(addis_ababa_TRASH$TRASH)
addis_ababa_TRASH <- addis_ababa_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(addis_ababa_TRASH$TRASH)
table(addis_ababa_TRASH$trash_b)

addis_ababa_TRASH$trash_b <- factor(addis_ababa_TRASH$trash_b)

addis_ababa_TRASH$YEAR <- factor(addis_ababa_TRASH$YEAR)
addis_ababa_TRASH_07 <- addis_ababa_TRASH %>% filter(addis_ababa_TRASH$YEAR==2007)

mylogit_07 <- glm(trash_b ~ YRSCHOOL, data = addis_ababa_TRASH_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

TRASH_T3 <- summary(mylogit_07)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

addis_ababa_FLOOR <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(addis_ababa_FLOOR$FLOOR)
addis_ababa_FLOOR <- addis_ababa_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=999)

table(addis_ababa_FLOOR$FLOOR)
table(addis_ababa_FLOOR$floor_b)

addis_ababa_FLOOR$floor_b <- factor(addis_ababa_FLOOR$floor_b)

addis_ababa_FLOOR$YEAR <- factor(addis_ababa_FLOOR$YEAR)
addis_ababa_FLOOR_94 <- addis_ababa_FLOOR %>% filter(addis_ababa_FLOOR$YEAR==1994)
addis_ababa_FLOOR_07 <- addis_ababa_FLOOR %>% filter(addis_ababa_FLOOR$YEAR==2007)

mylogit_94 <- glm(floor_b ~ YRSCHOOL, data = addis_ababa_FLOOR_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(floor_b ~ YRSCHOOL, data = addis_ababa_FLOOR_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

FLOOR_T2 <- summary(mylogit_94)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_07)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

addis_ababa_ROOF <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(addis_ababa_ROOF$ROOF)
addis_ababa_ROOF <- addis_ababa_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(addis_ababa_ROOF$ROOF)
table(addis_ababa_ROOF$roof_b)

addis_ababa_ROOF$roof_b <- factor(addis_ababa_ROOF$roof_b)

addis_ababa_ROOF$YEAR <- factor(addis_ababa_ROOF$YEAR)
addis_ababa_ROOF_94 <- addis_ababa_ROOF %>% filter(addis_ababa_ROOF$YEAR==1994)
addis_ababa_ROOF_07 <- addis_ababa_ROOF %>% filter(addis_ababa_ROOF$YEAR==2007)

mylogit_94 <- glm(roof_b ~ YRSCHOOL, data = addis_ababa_ROOF_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(roof_b ~ YRSCHOOL, data = addis_ababa_ROOF_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

ROOF_T2 <- summary(mylogit_94)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_07)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

addis_ababa_TV <- addis_ababa_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(addis_ababa_TV$TV)
addis_ababa_TV <- addis_ababa_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(addis_ababa_TV$TV)
table(addis_ababa_TV$tv_b)

addis_ababa_TV$tv_b <- factor(addis_ababa_TV$tv_b)

addis_ababa_TV$YEAR <- factor(addis_ababa_TV$YEAR)
addis_ababa_TV_94 <- addis_ababa_TV %>% filter(addis_ababa_TV$YEAR==1994)
addis_ababa_TV_07 <- addis_ababa_TV %>% filter(addis_ababa_TV$YEAR==2007)

mylogit_94 <- glm(tv_b ~ YRSCHOOL, data = addis_ababa_TV_94, family = "binomial",weights=HHWT)
summary(mylogit_94)
exp(coef(mylogit_94))

mylogit_07 <- glm(tv_b ~ YRSCHOOL, data = addis_ababa_TV_07, family = "binomial",weights=HHWT)
summary(mylogit_07)
exp(coef(mylogit_07))

TV_T2 <- summary(mylogit_94)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
TV_T3 <- summary(mylogit_07)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}

addis_ababa_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z,TV_T3_z) 
addis_ababa_logit$CITY <- "Addis Ababa"


#################################################Saving an unique table results join

ETHIOPIA_logit <- rbind(addis_ababa_logit,sep = ".") 
write.csv(ETHIOPIA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/ETHIOPIA_logit.csv"))



