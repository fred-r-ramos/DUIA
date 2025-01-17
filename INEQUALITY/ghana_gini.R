library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(ineq)
library(aod)

setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/ghana")

sf::sf_use_s2(FALSE)

geo2_gh00 <- read_sf("geo2_gh2000.shp")
geo2_gh10 <- read_sf("geo2_gh2010.shp")
centroid_gh00 <- st_centroid(geo2_gh00)
centroid_gh10 <- st_centroid(geo2_gh10)
AUE_accra <- read_sf("Accra_studyArea.shp")
AUE_accra <- st_transform(AUE_accra, 4326)

accra_00 <- geo2_gh00[st_intersection(AUE_accra,centroid_gh00),]
accra_00['CITY']='accra'
accra_10 <- geo2_gh10[st_intersection(AUE_accra,centroid_gh10),]
accra_10['CITY']='accra'

plot(st_geometry(accra_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_gh10[0], add = TRUE)
plot(AUE_accra, col = "blue",alpha = 0.9, add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 
#ddi <-read_ipums_ddi("ipumsi_00217.xml")

ddi <-read_ipums_ddi("ipumsi_00241.xml")

ghana <- read_ipums_micro(ddi)

names(ghana)

##creating binary variables for the logistic regression######

table(ghana$OWNERSHIP)
ghana$owner_b <- ifelse(ghana$OWNERSHIP ==1,1,0)
table(ghana$TOILET)
ghana$toilet_b <- ifelse(ghana$TOILET ==21|ghana$TOILET ==22,1,0)
table(ghana$ELECTRIC)
ghana$eletric_b <- ifelse(ghana$ELECTRIC ==1,1,0)
table(ghana$WATSUP)
ghana$water_b <- ifelse(ghana$WATSUP ==11,1,0)
table(ghana$ROOF)
ghana$roof_b <- ifelse(ghana$ROOF==11|ghana$ROOF==14|ghana$ROOF==27,1,0)
table(ghana$FLOOR)
ghana$floor_b <- ifelse(ghana$FLOOR>100&ghana$FLOOR<999,1,0)
ghana$kitchen_b <- ifelse(ghana$KITCHEN==21|ghana$KITCHEN==22|ghana$KITCHEN==26,1,0)
ghana$OCCISCO_b <- ifelse(ghana$OCCISCO ==1|ghana$OCCISCO ==2,1,0)
ghana$OCCISCO_b <- ifelse(ghana$OCCISCO ==3|ghana$OCCISCO ==4|ghana$OCCISCO ==5,2,ghana$OCCISCO_b)
ghana$OCCISCO_b <- ifelse(ghana$OCCISCO ==6|ghana$OCCISCO ==7|ghana$OCCISCO ==8|ghana$OCCISCO ==9,3,ghana$OCCISCO_b)
table(ghana$OCCISCO_b)

#checking if the variables are available in both years
table(ghana$owner_b,ghana$YEAR)
table(ghana$toilet_b,ghana$YEAR)
table(ghana$eletric_b,ghana$YEAR)
table(ghana$water_b,ghana$YEAR)
table(ghana$roof_b,ghana$YEAR)
table(ghana$floor_b,ghana$YEAR)
table(ghana$kitchen_b,ghana$YEAR)


##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
ghana$PUBl_ASSET <- ghana$eletric_b+ghana$water_b
ghana %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ghana$PRIV_ASSET <- ghana$owner_b+ghana$toilet_b+ghana$roof_b+ghana$floor_b+ghana$kitchen_b
ghana %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

ghana$TOTAL_ASSET <- ghana$eletric_b+ghana$water_b+ghana$owner_b+ghana$toilet_b+ghana$roof_b+ghana$floor_b+ghana$kitchen_b
ghana %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join####################

ghana$IPUM2000 <- as.integer(ghana$GEO2_GH2000)
ghana$IPUM2010 <- as.integer(ghana$GEO2_GH2010)

names(accra_00)
accra_00$IPUM2000 <- as.integer(accra_00$IPUM2000)
names(accra_10)
accra_10$IPUM2010 <- as.integer(accra_10$IPUM2010)

ghana_full <- ghana
table(ghana_full$YEAR)

ghana_full <- ghana_full %>%  filter (ghana_full$YRSCHOOL < 90)

ghana_full <- ghana_full %>%  filter (ghana_full$AGE >15)

ghana_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(ghana_full$YRSCHOOL)
summary(ghana_full$YRSCHOOL)

##Joining by year#########

accra_00 <- ghana %>% inner_join(accra_00, by="IPUM2000")
accra_10 <- ghana %>% inner_join(accra_10, by="IPUM2010")

accra_00 <- select(accra_00, -c(DIST2000))
accra_10 <- select(accra_10, -c(DIST2010))

accra_full <- rbind(accra_00,accra_10)
names(accra_full)

accra_full <- accra_full %>%  filter (accra_full$YRSCHOOL < 90)

accra_full <- accra_full %>%  filter (accra_full$AGE >15)

accra_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(accra_full$YRSCHOOL)
summary(accra_full$YRSCHOOL)

ghana_fu00 <- ghana_full %>%  filter (YEAR==2000)
ghana_fu10 <- ghana_full %>%  filter (YEAR==2010)

Gini(ghana_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(ghana_fu10 $YRSCHOOL,na.rm = TRUE)

## compute the Lorenz curves for Ghana

Lc_gha00 <- Lc(ghana_fu00$YRSCHOOL, n = rep(1,length(ghana_fu00$YRSCHOOL)), plot = TRUE)
Lc_gha10 <- Lc(ghana_fu10$YRSCHOOL, n = rep(1,length(ghana_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_gha00,col='blue', main = "Lorenz Curve - Ghana")
lines(Lc_gha10, col='red')

## compute the Lorenz curves for Accra

summary(accra_full$YRSCHOOL)
summary(accra_full$AGE)

accra_fu00 <- accra_full %>%  filter (YEAR==2000)
accra_fu10 <- accra_full %>%  filter (YEAR==2010)
Gini(accra_fu00 $YRSCHOOL,na.rm = TRUE)
Gini(accra_fu10 $YRSCHOOL,na.rm = TRUE)
Gini(accra_fu00 $TOTAL_ASSET,na.rm = TRUE)
Gini(accra_fu10 $TOTAL_ASSET,na.rm = TRUE)

## compute the Lorenz curves
Lc_acc00 <- Lc(accra_fu00$YRSCHOOL, n = rep(1,length(accra_fu00$YRSCHOOL)), plot = TRUE)
Lc_acc10 <- Lc(accra_fu10$YRSCHOOL, n = rep(1,length(accra_fu10$YRSCHOOL)), plot = TRUE)

plot(Lc_acc00,col='blue', main = "Lorenz Curve - Accra")
lines(Lc_acc10, col='red')


# for PUBl_ASSET
accra_PUBl_ASSET<-accra_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- accra_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
accra_PUBl_ASSET <- accra_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
accra_PUBl_ASSET <- accra_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
accra_PUBl_ASSET$CITY<-"accra"
# for PRIV_ASSET
accra_PRIV_ASSET<-accra_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- accra_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
accra_PRIV_ASSET <- accra_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
accra_PRIV_ASSET <- accra_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
accra_PRIV_ASSET$CITY<-"accra"
# for TOTAL_ASSET
accra_TOTAL_ASSET<-accra_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- accra_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
accra_TOTAL_ASSET <- accra_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
accra_TOTAL_ASSET <- accra_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
accra_TOTAL_ASSET$CITY<-"accra"
write.csv(accra_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/accra_PUBl_ASSET.csv", row.names = TRUE)
write.csv(accra_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/accra_PRIV_ASSET.csv", row.names = TRUE)
write.csv(accra_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/accra_TOTAL_ASSET.csv", row.names = TRUE)

# ######################Logistic regressions for selected variables############################
# #################################### FOR WATSUP ##################################
# 
accra_WATER <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,OCCISCO_b,CITY)
table(accra_WATER$WATSUP)
table(accra_WATER$HHWT)
accra_WATER <- accra_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)
table(accra_WATER$WATSUP)
table(accra_WATER$water_b)

accra_WATER$water_b <- factor(accra_WATER$water_b)

accra_WATER$YEAR <- factor(accra_WATER$YEAR)
accra_WATER$OCCISCO_b <- factor(accra_WATER$OCCISCO_b)
accra_WATER_00 <- accra_WATER %>% filter(accra_WATER$YEAR==2000)
accra_WATER_10 <- accra_WATER %>% filter(accra_WATER$YEAR==2010)

mylogit_00 <- glm(water_b ~ YRSCHOOL, data = accra_WATER_00, family = "binomial")
summary(mylogit_00)
exp(coef(mylogit_00))

mylogit_10 <- glm(water_b ~ YRSCHOOL, data = accra_WATER_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

# mylogit <- glm(water_b ~ OCCISCO_b + YEAR, data = accra_WATER, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))
# 
# mylogit_00 <- glm(water_b ~ OCCISCO_b, data = accra_WATER_00, family = "binomial")
# summary(mylogit_00)
# exp(coef(mylogit_00))
# 
# mylogit_10 <- glm(water_b ~ OCCISCO_b, data = accra_WATER_10, family = "binomial")
# summary(mylogit_10)
# exp(coef(mylogit_10))

WATER_T2 <- summary(mylogit_00)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_10)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

#################################### FOR OWNERSHIP ##################################

accra_OWN <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,OCCISCO_b,YRSCHOOL,CITY)
table(accra_OWN$OWNERSHIP)
accra_OWN <- accra_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)
 
table(accra_OWN$OWNERSHIP)
table(accra_OWN$owner_b)

accra_OWN$owner_b <- factor(accra_OWN$owner_b)
accra_OWN$OCCISCO_b <- factor(accra_OWN$OCCISCO_b)

accra_OWN$YEAR <- factor(accra_OWN$YEAR)
accra_OWN_01 <- accra_OWN %>% filter(accra_OWN$YEAR==2000)
accra_OWN_10 <- accra_OWN %>% filter(accra_OWN$YEAR==2010)

mylogit_01 <- glm(owner_b ~ YRSCHOOL, data = accra_OWN_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))
 
mylogit_10 <- glm(owner_b ~ YRSCHOOL, data = accra_OWN_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

# mylogit <- glm(owner_b ~ OCCISCO_b + YEAR, data = accra_OWN, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))
# 
# mylogit_01 <- glm(owner_b ~ OCCISCO_b, data = accra_OWN_01, family = "binomial")
# summary(mylogit_01)
# exp(coef(mylogit_01))
# 
# mylogit_10 <- glm(owner_b ~ OCCISCO_b, data = accra_OWN_10, family = "binomial")
# summary(mylogit_10)
# exp(coef(mylogit_10))

OWN_T2 <- summary(mylogit_01)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_10)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

# #################################### FOR ELETRICITY ##################################

accra_ELET <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,OCCISCO_b,CITY)
table(accra_ELET$ELECTRIC)
accra_ELET <- accra_ELET %>% filter(ELECTRIC!=0)%>% filter(ELECTRIC!=9)
# 
table(accra_ELET$ELECTRIC)
table(accra_ELET$eletric_b)

accra_ELET$eletric_b <- factor(accra_ELET$eletric_b)

accra_ELET$YEAR <- factor(accra_ELET$YEAR)
accra_ELET$eletric_b <- factor(accra_ELET$OCCISCO_b)

accra_ELET_00 <- accra_ELET %>% filter(accra_ELET$YEAR==2000)
accra_ELET_10 <- accra_ELET %>% filter(accra_ELET$YEAR==2010)

mylogit_01 <- glm(eletric_b ~ YRSCHOOL, data = accra_ELET_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_10 <- glm(eletric_b ~ YRSCHOOL, data = accra_ELET_10, family = "binomial")
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


# #################################### FOR TOILET ##################################

accra_TOILET <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,OCCISCO_b,YRSCHOOL,CITY)
table(accra_TOILET$TOILET)
accra_TOILET <- accra_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)
 
table(accra_TOILET$TOILET)
table(accra_TOILET$toilet_b)
table(accra_TOILET$OCCISCO_b)

accra_TOILET$toilet_b <- factor(accra_TOILET$toilet_b)
accra_TOILET$YEAR <- factor(accra_TOILET$YEAR)
accra_TOILET$OCCISCO_b <- factor(accra_TOILET$OCCISCO_b)

accra_TOILET_00 <- accra_TOILET %>% filter(accra_TOILET$YEAR==2000)
accra_TOILET_10 <- accra_TOILET %>% filter(accra_TOILET$YEAR==2010)


mylogit_01 <- glm(toilet_b ~ YRSCHOOL, data = accra_TOILET_00, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))
 
mylogit_10 <- glm(toilet_b ~ YRSCHOOL, data = accra_TOILET_10, family = "binomial")
summary(mylogit_10)
exp(coef(mylogit_10))

# accra_TOILET <- accra_TOILET %>% filter(OCCISCO_b!=0)
# accra_TOILET_00 <- accra_TOILET %>% filter(accra_TOILET$YEAR==2000)
# accra_TOILET_10 <- accra_TOILET %>% filter(accra_TOILET$YEAR==2010)
# 
# mylogit <- glm(toilet_b ~ OCCISCO_b + YEAR, data = accra_TOILET, family = "binomial")
# summary(mylogit)
# exp(coef(mylogit))
# 
# mylogit_01 <- glm(toilet_b ~ OCCISCO_b, data = accra_TOILET_00, family = "binomial")
# summary(mylogit_01)
# exp(coef(mylogit_01))
# 
# mylogit_10 <- glm(toilet_b ~ OCCISCO_b, data = accra_TOILET_10, family = "binomial")
# summary(mylogit_10)
# exp(coef(mylogit_10))

TOILET_T2 <- summary(mylogit_01)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_10)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

accra_FLOOR <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(accra_FLOOR$FLOOR)
accra_FLOOR <- accra_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(accra_FLOOR$FLOOR)
table(accra_FLOOR$floor_b)

accra_FLOOR$floor_b <- factor(accra_FLOOR$floor_b)

accra_FLOOR$YEAR <- factor(accra_FLOOR$YEAR)
accra_FLOOR_00 <- accra_FLOOR %>% filter(accra_FLOOR$YEAR==2000)
accra_FLOOR_10 <- accra_FLOOR %>% filter(accra_FLOOR$YEAR==2010)

mylogit_01 <- glm(floor_b ~ YRSCHOOL, data = accra_FLOOR_00, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(floor_b ~ YRSCHOOL, data = accra_FLOOR_10, family = "binomial")
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

accra_ROOF <- accra_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(accra_ROOF$ROOF)
accra_ROOF <- accra_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(accra_ROOF$ROOF)
table(accra_ROOF$roof_b)

accra_ROOF$roof_b <- factor(accra_ROOF$roof_b)

accra_ROOF$YEAR <- factor(accra_ROOF$YEAR)
accra_ROOF_01 <- accra_ROOF %>% filter(accra_ROOF$YEAR==2000)
accra_ROOF_12 <- accra_ROOF %>% filter(accra_ROOF$YEAR==2010)

mylogit_01 <- glm(roof_b ~ YRSCHOOL, data = accra_ROOF_01, family = "binomial")
summary(mylogit_01)
exp(coef(mylogit_01))

mylogit_12 <- glm(roof_b ~ YRSCHOOL, data = accra_ROOF_12, family = "binomial")
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

ACCRA_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TOILET_T2_z,TOILET_T3_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z) 
ACCRA_logit$CITY <- "Accra"


#################################################Saving an unique table results join

GHANA_logit <- rbind(ACCRA_logit,sep = ".") 
write.csv(GHANA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/GHANA_logit.csv"))

