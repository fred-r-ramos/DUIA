library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/mozambique")

sf::sf_use_s2(FALSE)

geo2_mz97 <- read_sf("geo2_mz1997.shp")
geo2_mz07 <- read_sf("geo2_mz2007.shp")
centroid_mz97 <- st_centroid(geo2_mz97)
centroid_mz07 <- st_centroid(geo2_mz07)
AUE_beira <- read_sf("Beira_studyArea.shp")
AUE_beira  <- st_transform(AUE_beira , 4326)

beira_97 <- geo2_mz97[st_intersection(AUE_beira,centroid_mz97),]
beira_97['CITY']='beira'
beira_07 <-  geo2_mz07[st_intersection(AUE_beira,centroid_mz07),]
beira_07['CITY']='beira'

plot(st_geometry(beira_07), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_mz07[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 
#ddi <-read_ipums_ddi("ipumsi_00224.xml")
ddi <-read_ipums_ddi("ipumsi_00258.xml")
mozambique <- read_ipums_micro(ddi)

names(mozambique)

######################creating binary variables for logit regressions

names(mozambique)

# "COUNTRY"    "YEAR"       "SAMPLE"     "SERIAL"     "HHWT"       "OWNERSHIP"  "OWNERSHIPD" "ELECTRIC"   "WATSUP"    
# [10] "SEWAGE"     "TRASH"      "AUTOS"      "REFRIG"     "TV"         "TOILET"     "FLOOR"      "ROOF"       "PERNUM"    
# [19] "PERWT"      "AGE"        "SCHOOL"     "LIT"        "EDATTAIN"   "EDATTAIND"  "YRSCHOOL"   "CNTRY_NAME" "ADMIN_NAME"
# [28] "CNTRY_CODE" "PARENT"     "CITY"    


table(mozambique$OWNERSHIP)
mozambique$owner_b <- ifelse(mozambique$OWNERSHIP ==1,1,0)
table(mozambique$ELECTRIC)
mozambique$eletric_b <- ifelse(mozambique$ELECTRIC ==1,1,0)
table(mozambique$WATSUP)
mozambique$water_b <- ifelse(mozambique$WATSUP ==11,1,0)
table(mozambique$AUTOS)
mozambique$autos_b <- ifelse(mozambique$AUTOS ==7,1,0)
table(mozambique$TV)
mozambique$tv_b <- ifelse(mozambique$TV ==20,1,0)
table(mozambique$TOILET)
mozambique$toilet_b <- ifelse(mozambique$TOILET==20|mozambique$TOILET==21|mozambique$TOILET==22,1,0)
table(mozambique$FLOOR)
mozambique$floor_b <- ifelse(mozambique$FLOOR >100,1,0)
table(mozambique$ROOF)
mozambique$roof_b <- ifelse(mozambique$ROOF==14|mozambique$ROOF==12|mozambique$ROOF==28,1,0)

mozambique$OCCISCO_b <- ifelse(mozambique$OCCISCO ==1|mozambique$OCCISCO ==2,1,0)
mozambique$OCCISCO_b <- ifelse(mozambique$OCCISCO ==3|mozambique$OCCISCO ==4|mozambique$OCCISCO ==5,2,mozambique$OCCISCO_b)
mozambique$OCCISCO_b <- ifelse(mozambique$OCCISCO ==6|mozambique$OCCISCO ==7|mozambique$OCCISCO ==8|mozambique$OCCISCO ==9,3,mozambique$OCCISCO_b)
mozambique$OCCISCO_low <- ifelse(mozambique$OCCISCO_b==3,1,0)

table(mozambique$OCCISCO_b)
table(mozambique$OCCISCO_low)
table(mozambique$HHWT)
names(mozambique)
gc()
names(mozambique)
gc()

#checking if the variables are available in both years
table(mozambique$owner_b,mozambique$YEAR)#########
table(mozambique$eletric_b,mozambique$YEAR)
table(mozambique$water_b,mozambique$YEAR)
table(mozambique$autos_b,mozambique$YEAR)########
table(mozambique$tv_b,mozambique$YEAR)###########
table(mozambique$toilet_b,mozambique$YEAR)
table(mozambique$floor_b,mozambique$YEAR)############
table(mozambique$roof_b,mozambique$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
mozambique$PUBl_ASSET <- mozambique$eletric_b+mozambique$water_b
mozambique %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mozambique$PRIV_ASSET <- mozambique$toilet_b+mozambique$roof_b
mozambique %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

mozambique$TOTAL_ASSET <- mozambique$eletric_b+mozambique$water_b + mozambique$toilet_b+mozambique$roof_b
mozambique %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

mozambique$IPUM1997 <- as.integer(mozambique$GEO2_MZ1997)
mozambique$IPUM2007 <- as.integer(mozambique$GEO2_MZ2007)
beira_97$IPUM1997 <- as.integer(beira_97$IPUM1997)
beira_07$IPUM2007 <- as.integer(beira_07$IPUM2007)

##Joining by year

beira_97 <- mozambique %>% inner_join(beira_97, by="IPUM1997")
beira_07 <- mozambique %>% inner_join(beira_07, by="IPUM2007")

names(beira_97)
names(beira_07)

beira_97 <- select(beira_97, -c(DIST1997))
beira_07 <- select(beira_07, -c(DIST2007))

##Merging all years into one table
mozambique_full <- rbind(beira_97,beira_07)
names(mozambique_full)

##Excluding specific columns for the unifeied dataset
mozambique_full<- select(mozambique_full, -c(GEO2_MZ1997,GEO2_MZ2007,IPUM1997,IPUM2007,geometry))
table(mozambique_full$CITY)

table(mozambique_full$OCCISCO)


mozambique_full$OCCISCO_b <- ifelse(mozambique_full$OCCISCO ==1|mozambique_full$OCCISCO ==2,1,0)
mozambique_full$OCCISCO_b <- ifelse(mozambique_full$OCCISCO ==3|mozambique_full$OCCISCO ==4|mozambique_full$OCCISCO ==5,2,mozambique_full$OCCISCO_b)
mozambique_full$OCCISCO_b <- ifelse(mozambique_full$OCCISCO ==6|mozambique_full$OCCISCO ==7|mozambique_full$OCCISCO ==8|mozambique_full$OCCISCO ==9,3,mozambique_full$OCCISCO_b)
table(mozambique_full$OCCISCO_b)

mozambique_full_OCCISCO_b <- mozambique_full %>% select(YEAR,OCCISCO_b,PERWT)
mozambique_full_OCCISCO_b <- mozambique_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(mozambique_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- mozambique_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- mozambique_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- mozambique_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- mozambique_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

beira_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
beira_OCCISCO_b<- beira_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
beira_OCCISCO_b<- beira_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

beira_OCCISCO_b
beira_full <- mozambique_full %>% filter(CITY=="beira")

# for PUBl_ASSET
beira_PUBl_ASSET<-beira_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- beira_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
beira_PUBl_ASSET <- beira_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
beira_PUBl_ASSET <- beira_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
beira_PUBl_ASSET$CITY<-"beira"
# for PRIV_ASSET
beira_PRIV_ASSET<-beira_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- beira_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
beira_PRIV_ASSET <- beira_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
beira_PRIV_ASSET <- beira_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
beira_PRIV_ASSET$CITY<-"beira"
# for TOTAL_ASSET
beira_TOTAL_ASSET<-beira_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- beira_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
beira_TOTAL_ASSET <- beira_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
beira_TOTAL_ASSET <- beira_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
beira_TOTAL_ASSET$CITY<-"beira"

beira_97 <- beira_full %>%  filter (YEAR==1997)
beira_07 <- beira_full %>%  filter (YEAR==2007)
Gini(beira_97 $TOTAL_ASSET,na.rm = TRUE)
Gini(beira_07 $TOTAL_ASSET,na.rm = TRUE)

write.csv(beira_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/beira_PUBl_ASSET.csv", row.names = TRUE)
write.csv(beira_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/beira_PRIV_ASSET.csv", row.names = TRUE)
write.csv(beira_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/beira_TOTAL_ASSET.csv", row.names = TRUE)

#################################### FOR WATSUP ##################################

beira_WATER <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,OCCISCO_b,OCCISCO_low,CITY)

table(beira_WATER$HHWT)
table(beira_WATER$WATSUP)
table(beira_WATER$OCCISCO_low)
beira_WATER <- beira_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99) %>% filter(OCCISCO_b!=0)


table(beira_WATER$WATSUP)
table(beira_WATER$water_b)

beira_WATER$water_b <- factor(beira_WATER$water_b)

beira_WATER$YEAR <- factor(beira_WATER$YEAR)

beira_WATER_01 <- beira_WATER %>% filter(beira_WATER$YEAR==1997)
beira_WATER_12 <- beira_WATER %>% filter(beira_WATER$YEAR==2007)

mylogit_01 <- glm(water_b ~ OCCISCO_low, data = beira_WATER_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(water_b ~ OCCISCO_low, data = beira_WATER_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))


#####saving z-value and beta

WATER_T2 <- summary(mylogit_01)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}

WATER_T3 <- summary(mylogit_12)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


gc()

#################################### FOR OWNERSHIP ##################################

beira_OWN <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,OCCISCO_b,OCCISCO_low,owner_b,CITY)
table(beira_OWN$OWNERSHIP)
beira_OWN <- beira_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)%>% filter(OCCISCO_b!=0)

table(beira_OWN$OWNERSHIP)
table(beira_OWN$owner_b)

beira_OWN$owner_b <- factor(beira_OWN$owner_b)

beira_OWN$YEAR <- factor(beira_OWN$YEAR)
beira_OWN_01 <- beira_OWN %>% filter(beira_OWN$YEAR==1997)
beira_OWN_12 <- beira_OWN %>% filter(beira_OWN$YEAR==2007)

# mylogit_01 <- glm(owner_b ~ OCCISCO_low, data = beira_OWN_01, family = "binomial",weights=HHWT)
# summary(mylogit_01)
# exp(coef(mylogit_01))

mylogit_12 <- glm(owner_b ~ OCCISCO_low, data = beira_OWN_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

# OWN_T2 <- summary(mylogit_01)$coefficients
# OWN_T2_z <- OWN_T2[2,1:4]
# OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_12)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}



#################################### FOR TOILET ##################################

beira_TOILET <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_TOILET$TOILET)
beira_TOILET <- beira_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)%>% filter(OCCISCO_b!=0)

table(beira_TOILET$TOILET)
table(beira_TOILET$toilet_b)

beira_TOILET$toilet_b <- factor(beira_TOILET$toilet_b)

beira_TOILET$YEAR <- factor(beira_TOILET$YEAR)
beira_TOILET_01 <- beira_TOILET %>% filter(beira_TOILET$YEAR==1997)
beira_TOILET_12 <- beira_TOILET %>% filter(beira_TOILET$YEAR==2007)

mylogit_01 <- glm(toilet_b ~ OCCISCO_low, data = beira_TOILET_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(toilet_b ~ OCCISCO_low, data = beira_TOILET_12, family = "binomial",weights=HHWT)
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

beira_ELECTRIC <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_ELECTRIC$ELECTRIC)
beira_ELECTRIC <- beira_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)%>% filter(OCCISCO_b!=0)

table(beira_ELECTRIC$ELECTRIC)
table(beira_ELECTRIC$eletric_b)

beira_ELECTRIC$eletric_b <- factor(beira_ELECTRIC$eletric_b)

beira_ELECTRIC$YEAR <- factor(beira_ELECTRIC$YEAR)
beira_ELECTRIC_01 <- beira_ELECTRIC %>% filter(beira_ELECTRIC$YEAR==1997)
beira_ELECTRIC_12 <- beira_ELECTRIC %>% filter(beira_ELECTRIC$YEAR==2007)

mylogit_01 <- glm(eletric_b ~ OCCISCO_low, data = beira_ELECTRIC_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(eletric_b ~ OCCISCO_low, data = beira_ELECTRIC_12, family = "binomial",weights=HHWT)
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


#################################### FOR AUTOS ##################################

beira_AUTOS <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_AUTOS$AUTOS)
beira_AUTOS <- beira_AUTOS %>% filter(AUTOS!=99)%>% filter(OCCISCO_b!=0)

table(beira_AUTOS$AUTOS)
table(beira_AUTOS$autos_b)

beira_AUTOS$autos_b <- factor(beira_AUTOS$autos_b)

beira_AUTOS$YEAR <- factor(beira_AUTOS$YEAR)
beira_AUTOS_01 <- beira_AUTOS %>% filter(beira_AUTOS$YEAR==1997)
beira_AUTOS_12 <- beira_AUTOS %>% filter(beira_AUTOS$YEAR==2007)

mylogit_01 <- glm(autos_b ~ OCCISCO_low, data = beira_AUTOS_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(autos_b ~ OCCISCO_low, data = beira_AUTOS_12, family = "binomial",weights=HHWT)
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

beira_FLOOR <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_FLOOR$FLOOR)
beira_FLOOR <- beira_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)%>% filter(OCCISCO_b!=0)

table(beira_FLOOR$FLOOR)
table(beira_FLOOR$floor_b)

beira_FLOOR$floor_b <- factor(beira_FLOOR$floor_b)

beira_FLOOR$YEAR <- factor(beira_FLOOR$YEAR)
beira_FLOOR_01 <- beira_FLOOR %>% filter(beira_FLOOR$YEAR==1997)
beira_FLOOR_12 <- beira_FLOOR %>% filter(beira_FLOOR$YEAR==2007)

# mylogit_01 <- glm(floor_b ~ OCCISCO_low, data = beira_FLOOR_01, family = "binomial",weights=HHWT)
# summary(mylogit_01)
# exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ OCCISCO_low, data = beira_FLOOR_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

# FLOOR_T2 <- summary(mylogit_01)$coefficients
# FLOOR_T2_z <- FLOOR_T2[2,1:4]
# FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_12)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}

#################################### FOR ROOF ##################################

beira_ROOF <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_ROOF$ROOF)
beira_ROOF <- beira_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)%>% filter(OCCISCO_b!=0)

table(beira_ROOF$ROOF)
table(beira_ROOF$roof_b)

beira_ROOF$roof_b <- factor(beira_ROOF$roof_b)

beira_ROOF$YEAR <- factor(beira_ROOF$YEAR)
beira_ROOF_01 <- beira_ROOF %>% filter(beira_ROOF$YEAR==1997)
beira_ROOF_12 <- beira_ROOF %>% filter(beira_ROOF$YEAR==2007)

mylogit_01 <- glm(roof_b ~ OCCISCO_low, data = beira_ROOF_01, family = "binomial",weights=HHWT)
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ OCCISCO_low, data = beira_ROOF_12, family = "binomial",weights=HHWT)
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

beira_TV <- beira_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,OCCISCO_b,OCCISCO_low,CITY)
table(beira_TV$TV)
beira_TV <- beira_TV %>% filter(TV!=0) %>% filter(TV!=99)%>% filter(OCCISCO_b!=0)

table(beira_TV$TV)
table(beira_TV$tv_b)

beira_TV$tv_b <- factor(beira_TV$tv_b)

beira_TV$YEAR <- factor(beira_TV$YEAR)
beira_TV_01 <- beira_TV %>% filter(beira_TV$YEAR==1997)
beira_TV_12 <- beira_TV %>% filter(beira_TV$YEAR==2007)


mylogit_12 <- glm(tv_b ~ OCCISCO_low, data = beira_TV_12, family = "binomial",weights=HHWT)
summary(mylogit_12)
exp(coef(mylogit_12))

TV_T3 <- summary(mylogit_12)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}



beira__OCC_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T3_z,TOILET_T2_z,TOILET_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,AUTOS_T2_z,AUTOS_T3_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T3_z) 
beira__OCC_logit$CITY <- "Beira"

beira__OCC_logit <- rbind(beira__OCC_logit,sep = ".") 
write.csv(beira__OCC_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/BEIRA_logit_OCC.csv"))

