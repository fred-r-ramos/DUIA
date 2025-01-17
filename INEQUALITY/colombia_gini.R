library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)
library(splitstackshape)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/colombia")

geo2_co85 <- read_sf("geo2_co1985.shp")
geo2_co93 <- read_sf("geo2_co1993.shp")
geo2_co05 <- read_sf("geo2_co2005.shp")
AUE_bogota <- read_sf("Bogota_studyArea.shp")
AUE_valledupar <- read_sf("Valledupar_studyArea.shp")
AUE_bogota <- st_transform(AUE_bogota, 4326)
AUE_valledupar <- st_transform(AUE_valledupar, 4326)

sf::sf_use_s2(FALSE)

centroid_co85 <- st_centroid(geo2_co85)
centroid_co93 <- st_centroid(geo2_co93)
centroid_co05 <- st_centroid(geo2_co05)

bogota_85 <- geo2_co85[st_intersection(AUE_bogota,centroid_co85),]
bogota_85['CITY']='bogota'
bogota_93 <- geo2_co93[st_intersection(AUE_bogota,centroid_co93),]
bogota_93['CITY']='bogota'
bogota_05 <- geo2_co05[st_intersection(AUE_bogota,centroid_co05),]
bogota_05['CITY']='bogota'

valledupar_85 <- geo2_co85[st_nearest_feature(AUE_valledupar,centroid_co85),]
valledupar_85['CITY']='valledupar'
valledupar_93 <- geo2_co93[st_nearest_feature(AUE_valledupar,centroid_co93),]
valledupar_93['CITY']='valledupar'
valledupar_05 <- geo2_co05[st_nearest_feature(AUE_valledupar,centroid_co05),]
valledupar_05['CITY']='valledupar'

geo_colombia_85 <- rbind(bogota_85,valledupar_85)
geo_colombia_93 <- rbind(bogota_93,valledupar_93)
geo_colombia_05 <- rbind(bogota_05,valledupar_05)

plot(st_geometry(geo_colombia_05), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_co05[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00162.xml")
ddi <-read_ipums_ddi("ipumsi_00247.xml")
colombia <- read_ipums_micro(ddi)

names(colombia)

######################creating binary variables for logit regressions

names(colombia)

# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "GEO2_CO1985" "GEO2_CO1993" "GEO2_CO2005"
# [9] "OWNERSHIP"   "OWNERSHIPD"  "ELECTRIC"    "WATSUP"      "SEWAGE"      "TRASH"       "AUTOS"       "REFRIG"     
# [17] "TV"          "TOILET"      "FLOOR"       "PERNUM"      "PERWT"       "AGE"         "SEX"         "SCHOOL"     
# # [25] "LIT"         "EDATTAIN"    "EDATTAIND"   "YRSCHOOL"    "EMPSTAT"     "EMPSTATD"    "INDGEN"  


table(colombia$OWNERSHIP)
colombia$owner_b <- ifelse(colombia$OWNERSHIP ==1,1,0)
table(colombia$ELECTRIC)
colombia$eletric_b <- ifelse(colombia$ELECTRIC ==1,1,0)
table(colombia$WATSUP)
colombia$water_b <- ifelse(colombia$WATSUP ==10,1,0)
table(colombia$SEWAGE)
colombia$sewage_b <- ifelse(colombia$SEWAGE ==11|colombia$SEWAGE ==12,1,0)
table(colombia$TRASH)
colombia$trash_b <- ifelse(colombia$TRASH ==10,1,0)
table(colombia$AUTOS)
colombia$autos_b <- ifelse(colombia$AUTOS >0&colombia$AUTOS <8,1,0)
table(colombia$REFRIG)
colombia$refrig_b <- ifelse(colombia$REFRIG ==2,1,0)
table(colombia$TV)
colombia$tv_b <- ifelse(colombia$TV ==20,1,0)
table(colombia$TOILET)
colombia$toilet_b <- ifelse(colombia$TOILET ==21|colombia$TOILET ==22,1,0)
table(colombia$FLOOR)
colombia$floor_b <- ifelse(colombia$FLOOR !=100|colombia$FLOOR !=999,1,0)

names(colombia)
gc()

#checking if the variables are available in both years
table(colombia$owner_b,colombia$YEAR)
table(colombia$eletric_b,colombia$YEAR)
table(colombia$water_b,colombia$YEAR)
table(colombia$sewage_b,colombia$YEAR)#
table(colombia$trash_b,colombia$YEAR)
table(colombia$autos_b,colombia$YEAR)#
table(colombia$refrig_b,colombia$YEAR)#
table(colombia$tv_b,colombia$YEAR)#
table(colombia$toilet_b,colombia$YEAR)
table(colombia$floor_b,colombia$YEAR)#

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
colombia$PUBl_ASSET <- ifelse(is.na(colombia$eletric_b), 0, colombia$eletric_b) +
  ifelse(is.na(colombia$water_b), 0, colombia$water_b) +
  ifelse(is.na(colombia$trash_b), 0, colombia$trash_b)
colombia %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

colombia$PRIV_ASSET <- ifelse(is.na(colombia$owner_b), 0, colombia$owner_b) +
  ifelse(is.na(colombia$toilet_b), 0, colombia$toilet_b)
colombia %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

colombia$TOTAL_ASSET <- ifelse(is.na(colombia$eletric_b), 0, colombia$eletric_b) +
  ifelse(is.na(colombia$water_b), 0, colombia$water_b) +
  ifelse(is.na(colombia$trash_b), 0, colombia$trash_b) +
  ifelse(is.na(colombia$owner_b), 0, colombia$owner_b) +
  ifelse(is.na(colombia$toilet_b), 0, colombia$toilet_b)
colombia %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

colombia$TOTAL_ASSET_gini <- ifelse(is.na(colombia$eletric_b), 0, colombia$eletric_b) +
  ifelse(is.na(colombia$water_b), 0, colombia$water_b) +
  ifelse(is.na(colombia$owner_b), 0, colombia$owner_b) +
  ifelse(is.na(colombia$toilet_b), 0, colombia$toilet_b)
colombia %>%
  group_by(YEAR,TOTAL_ASSET_gini) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

colombia$IPUM1985 <- as.integer(colombia$GEO2_CO1985)
colombia$IPUM1993 <- as.integer(colombia$GEO2_CO1993)
colombia$IPUM2005 <- as.integer(colombia$GEO2_CO2005)
geo_colombia_85$IPUM1985 <- as.integer(geo_colombia_85$IPUM1985)
geo_colombia_93$IPUM1993 <- as.integer(geo_colombia_93$IPUM1993)
geo_colombia_05$IPUM2005 <- as.integer(geo_colombia_05$IPUM2005)

##Joining by year

geo_colombia_85 <- colombia %>% inner_join(geo_colombia_85, by="IPUM1985")
geo_colombia_93 <- colombia %>% inner_join(geo_colombia_93, by="IPUM1993")
geo_colombia_05 <- colombia %>% inner_join(geo_colombia_05, by="IPUM2005")

names(geo_colombia_85)
names(geo_colombia_93)
names(geo_colombia_05)

geo_colombia_85 <- select(geo_colombia_85, -c(MUNI1985))
geo_colombia_93 <- select(geo_colombia_93, -c(MUNI1993))
geo_colombia_05 <- select(geo_colombia_05, -c(MUNI2005))

##Merging all years into one table
colombia_full <- rbind(geo_colombia_85,geo_colombia_93,geo_colombia_05)
names(colombia_full)

#colombia_full <- expandRows(colombia_full, 'HHWT')

gc()

names(colombia_full)

table(colombia_full$YEAR)

colombia_full <- colombia_full %>%  filter (colombia_full$YRSCHOOL < 90)

colombia_full <- colombia_full %>%  filter (colombia_full$AGE >15)

colombia_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(colombia_full$YRSCHOOL)
summary(colombia_full$YRSCHOOL)
table(colombia_full$CITY)

####bogota 
bogota_full <- colombia_full %>%  filter (CITY=="bogota")

summary(bogota_full$YRSCHOOL)
summary(bogota_full$AGE)

bogota_fu85 <- bogota_full %>%  filter (YEAR==1985)
bogota_fu93 <- bogota_full %>%  filter (YEAR==1993)
bogota_fu05 <- bogota_full %>%  filter (YEAR==2005)

Gini(bogota_fu85 $YRSCHOOL,na.rm = TRUE)
Gini(bogota_fu93 $YRSCHOOL,na.rm = TRUE)
Gini(bogota_fu05 $YRSCHOOL,na.rm = TRUE)
Gini(bogota_fu85 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(bogota_fu93 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(bogota_fu05 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_bog85 <- Lc(bogota_fu85$YRSCHOOL, n = rep(1,length(bogota_fu85$YRSCHOOL)), plot = TRUE)
Lc_bog93 <- Lc(bogota_fu93$YRSCHOOL, n = rep(1,length(bogota_fu93$YRSCHOOL)), plot = TRUE)
Lc_bog05 <- Lc(bogota_fu05$YRSCHOOL, n = rep(1,length(bogota_fu05$YRSCHOOL)), plot = TRUE)

plot(Lc_bog85,col='blue', main = "Lorenz Curve - Bogota")
lines(Lc_bog93, col='red')
lines(Lc_bog05, col='green')

####valledupar 
####valledupar  
valledupar_full <- colombia_full %>%  filter (CITY=="valledupar")

summary(valledupar_full$YRSCHOOL)
summary(valledupar_full$AGE)

valledupar_fu85 <- valledupar_full %>%  filter (YEAR==1985)
valledupar_fu93 <- valledupar_full %>%  filter (YEAR==1993)
valledupar_fu05 <- valledupar_full %>%  filter (YEAR==2005)

Gini(valledupar_fu85 $YRSCHOOL,na.rm = TRUE)
Gini(valledupar_fu93 $YRSCHOOL,na.rm = TRUE)
Gini(valledupar_fu05 $YRSCHOOL,na.rm = TRUE)
Gini(valledupar_fu85 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(valledupar_fu93 $TOTAL_ASSET_gini,na.rm = TRUE)
Gini(valledupar_fu05 $TOTAL_ASSET_gini,na.rm = TRUE)

## compute the Lorenz curves
Lc_val85 <- Lc(valledupar_fu85$YRSCHOOL, n = rep(1,length(valledupar_fu85$YRSCHOOL)), plot = TRUE)
Lc_val93 <- Lc(valledupar_fu93$YRSCHOOL, n = rep(1,length(valledupar_fu93$YRSCHOOL)), plot = TRUE)
Lc_val05 <- Lc(valledupar_fu05$YRSCHOOL, n = rep(1,length(valledupar_fu05$YRSCHOOL)), plot = TRUE)

plot(Lc_val85,col='blue', main = "Lorenz Curve - valledupar ")
lines(Lc_val93, col='red')
lines(Lc_val05, col='green')

# for PUBl_ASSET
valledupar_PUBl_ASSET<-valledupar_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- valledupar_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
valledupar_PUBl_ASSET <- valledupar_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
valledupar_PUBl_ASSET <- valledupar_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
valledupar_PUBl_ASSET$CITY<-"valledupa"
# for PRIV_ASSET
valledupar_PRIV_ASSET<-valledupar_full %>%  
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- valledupar_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
valledupar_PRIV_ASSET <- valledupar_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
valledupar_PRIV_ASSET <- valledupar_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
valledupar_PRIV_ASSET$CITY<-"valledupa"
# for TOTAL_ASSET
valledupar_TOTAL_ASSET<-valledupar_full %>%  
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- valledupar_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
valledupar_TOTAL_ASSET <- valledupar_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
valledupar_TOTAL_ASSET <- valledupar_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
valledupar_TOTAL_ASSET$CITY<-"valledupa"
write.csv(valledupar_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/valledupar_PUBl_ASSET.csv", row.names = TRUE)
write.csv(valledupar_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/valledupar_PRIV_ASSET.csv", row.names = TRUE)
write.csv(valledupar_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/valledupar_TOTAL_ASSET.csv", row.names = TRUE)


# for PUBl_ASSET
bogota_PUBl_ASSET<-bogota_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bogota_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bogota_PUBl_ASSET <- bogota_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bogota_PUBl_ASSET <- bogota_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bogota_PUBl_ASSET$CITY<-"bogota"
# for PRIV_ASSET
bogota_PRIV_ASSET<-bogota_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bogota_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bogota_PRIV_ASSET <- bogota_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bogota_PRIV_ASSET <- bogota_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bogota_PRIV_ASSET$CITY<-"bogota"
# for TOTAL_ASSET
bogota_TOTAL_ASSET<-bogota_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- bogota_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
bogota_TOTAL_ASSET <- bogota_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
bogota_TOTAL_ASSET <- bogota_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
bogota_TOTAL_ASSET$CITY<-"bogota"
write.csv(bogota_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bogota_PUBl_ASSET.csv", row.names = TRUE)
write.csv(bogota_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bogota_PRIV_ASSET.csv", row.names = TRUE)
write.csv(bogota_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/bogota_TOTAL_ASSET.csv", row.names = TRUE)



######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

bogota_WATER <- bogota_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,WATSUP,water_b,YRSCHOOL,CITY)
table(bogota_WATER$WATSUP)
bogota_WATER <- bogota_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(bogota_WATER$WATSUP)
table(bogota_WATER$water_b)

bogota_WATER$water_b <- factor(bogota_WATER$water_b)

bogota_WATER$YEAR <- factor(bogota_WATER$YEAR)
bogota_WATER_93 <- bogota_WATER %>% filter(bogota_WATER$YEAR==1993)
bogota_WATER_05 <- bogota_WATER %>% filter(bogota_WATER$YEAR==2005)

mylogit_93 <- glm(water_b ~ YRSCHOOL, data = bogota_WATER_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(water_b ~ YRSCHOOL, data = bogota_WATER_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_93)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_05)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

bogota_OWN <- bogota_full %>% select(COUNTRY,YEAR,HHWT,AGE,SAMPLE,SERIAL,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(bogota_OWN$OWNERSHIP)
bogota_OWN <- bogota_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(bogota_OWN$OWNERSHIP)
table(bogota_OWN$owner_b)

bogota_OWN$owner_b <- factor(bogota_OWN$owner_b)

bogota_OWN$YEAR <- factor(bogota_OWN$YEAR)
bogota_OWN_93 <- bogota_OWN %>% filter(bogota_OWN$YEAR==1993)
bogota_OWN_05 <- bogota_OWN %>% filter(bogota_OWN$YEAR==2005)

mylogit_93 <- glm(owner_b ~ YRSCHOOL, data = bogota_OWN_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(owner_b ~ YRSCHOOL, data = bogota_OWN_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

OWN_T2 <- summary(mylogit_93)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_05)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

bogota_ELECTRIC <- bogota_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(bogota_ELECTRIC$ELECTRIC)
bogota_ELECTRIC <- bogota_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(bogota_ELECTRIC$ELECTRIC)
table(bogota_ELECTRIC$eletric_b)

bogota_ELECTRIC$eletric_b <- factor(bogota_ELECTRIC$eletric_b)

bogota_ELECTRIC$YEAR <- factor(bogota_ELECTRIC$YEAR)
bogota_ELECTRIC_93 <- bogota_ELECTRIC %>% filter(bogota_ELECTRIC$YEAR==1993)
bogota_ELECTRIC_05 <- bogota_ELECTRIC %>% filter(bogota_ELECTRIC$YEAR==2005)

mylogit_93 <- glm(eletric_b ~ YRSCHOOL, data = bogota_ELECTRIC_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(eletric_b ~ YRSCHOOL, data = bogota_ELECTRIC_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

ELECTRIC_T2 <- summary(mylogit_93)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_05)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}


#################################### FOR TRASH ##################################

bogota_TRASH <- bogota_full %>% select(COUNTRY,YEAR,HHWT,AGE,SAMPLE,SERIAL,TRASH,trash_b,YRSCHOOL,CITY)
table(bogota_TRASH$TRASH)
bogota_TRASH <- bogota_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(bogota_TRASH$TRASH)
table(bogota_TRASH$trash_b)

bogota_TRASH$trash_b <- factor(bogota_TRASH$trash_b)

bogota_TRASH$YEAR <- factor(bogota_TRASH$YEAR)
bogota_TRASH_93 <- bogota_TRASH %>% filter(bogota_TRASH$YEAR==1993)
bogota_TRASH_05 <- bogota_TRASH %>% filter(bogota_TRASH$YEAR==2005)

mylogit_93 <- glm(trash_b ~ YRSCHOOL, data = bogota_TRASH_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(trash_b ~ YRSCHOOL, data = bogota_TRASH_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TRASH_T2 <- summary(mylogit_93)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_05)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}


#################################### FOR AUTOS ##################################

bogota_AUTOS <- bogota_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,AUTOS,autos_b,YRSCHOOL,CITY)
table(bogota_AUTOS$AUTOS)
bogota_AUTOS <- bogota_AUTOS %>% filter(AUTOS!=8)

table(bogota_AUTOS$AUTOS)
table(bogota_AUTOS$autos_b)

bogota_AUTOS$autos_b <- factor(bogota_AUTOS$autos_b)

bogota_AUTOS$YEAR <- factor(bogota_AUTOS$YEAR)
bogota_AUTOS_05 <- bogota_AUTOS %>% filter(bogota_AUTOS$YEAR==2005)

mylogit_05 <- glm(autos_b ~ YRSCHOOL, data = bogota_AUTOS_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

AUTOS_T3 <- summary(mylogit_05)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}


#################################### FOR REFRIG ##################################

bogota_REFRIG <- bogota_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,REFRIG,refrig_b,YRSCHOOL,CITY)
table(bogota_REFRIG$REFRIG)
bogota_REFRIG <- bogota_REFRIG %>% filter(REFRIG!=0)

table(bogota_REFRIG$REFRIG)
table(bogota_REFRIG$refrig_b)

bogota_REFRIG$refrig_b <- factor(bogota_REFRIG$refrig_b)

bogota_REFRIG$YEAR <- factor(bogota_REFRIG$YEAR)
bogota_REFRIG_05 <- bogota_REFRIG %>% filter(bogota_REFRIG$YEAR==2005)

mylogit_05 <- glm(refrig_b ~ YRSCHOOL, data = bogota_REFRIG_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

REFRIG_T3 <- summary(mylogit_05)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}


#################################### FOR TV ##################################

bogota_TV <- bogota_full %>% select(COUNTRY,YEAR,HHWT,AGE,SAMPLE,SERIAL,TV,tv_b,YRSCHOOL,CITY)
table(bogota_TV$TV)
bogota_TV <- bogota_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(bogota_TV$TV)
table(bogota_TV$tv_b)

bogota_TV$tv_b <- factor(bogota_TV$tv_b)

bogota_TV$YEAR <- factor(bogota_TV$YEAR)
bogota_TV_05 <- bogota_TV %>% filter(bogota_TV$YEAR==2005)

mylogit_05 <- glm(tv_b ~ YRSCHOOL, data = bogota_TV_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TV_T3 <- summary(mylogit_05)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}


#################################### FOR TOILET ##################################

bogota_TOILET <- bogota_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,TOILET,toilet_b,YRSCHOOL,CITY)
table(bogota_TOILET$TOILET)
bogota_TOILET <- bogota_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(bogota_TOILET$TOILET)
table(bogota_TOILET$toilet_b)

bogota_TOILET$toilet_b <- factor(bogota_TOILET$toilet_b)

bogota_TOILET$YEAR <- factor(bogota_TOILET$YEAR)
bogota_TOILET_93 <- bogota_TOILET %>% filter(bogota_TOILET$YEAR==1993)
bogota_TOILET_05 <- bogota_TOILET %>% filter(bogota_TOILET$YEAR==2005)

mylogit_93 <- glm(toilet_b ~ YRSCHOOL, data = bogota_TOILET_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(toilet_b ~ YRSCHOOL, data = bogota_TOILET_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TOILET_T2 <- summary(mylogit_93)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_05)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}


BOGOTA_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T3_z,REFRIG_T3_z,TV_T3_z,TOILET_T2_z,TOILET_T3_z) 
BOGOTA_logit$CITY <- "Bogota"
#################################### FOR WATSUP ##################################

valledupar_WATER <- valledupar_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,HHWT,SERIAL,WATSUP,water_b,YRSCHOOL,CITY)
table(valledupar_WATER$WATSUP)
valledupar_WATER <- valledupar_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)

table(valledupar_WATER$WATSUP)
table(valledupar_WATER$water_b)

valledupar_WATER$water_b <- factor(valledupar_WATER$water_b)

valledupar_WATER$YEAR <- factor(valledupar_WATER$YEAR)
valledupar_WATER_93 <- valledupar_WATER %>% filter(valledupar_WATER$YEAR==1993)
valledupar_WATER_05 <- valledupar_WATER %>% filter(valledupar_WATER$YEAR==2005)

mylogit_93 <- glm(water_b ~ YRSCHOOL, data = valledupar_WATER_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(water_b ~ YRSCHOOL, data = valledupar_WATER_05, family = "binomial")
summary(mylogit_05)
exp(coef(mylogit_05))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_93)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_05)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}

gc()

#################################### FOR OWNERSHIP ##################################

valledupar_OWN <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(valledupar_OWN$OWNERSHIP)
valledupar_OWN <- valledupar_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(valledupar_OWN$OWNERSHIP)
table(valledupar_OWN$owner_b)

valledupar_OWN$owner_b <- factor(valledupar_OWN$owner_b)

valledupar_OWN$YEAR <- factor(valledupar_OWN$YEAR)
valledupar_OWN_93 <- valledupar_OWN %>% filter(valledupar_OWN$YEAR==1993)
valledupar_OWN_05 <- valledupar_OWN %>% filter(valledupar_OWN$YEAR==2005)

mylogit_93 <- glm(owner_b ~ YRSCHOOL, data = valledupar_OWN_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(owner_b ~ YRSCHOOL, data = valledupar_OWN_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

OWN_T2 <- summary(mylogit_93)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
OWN_T3 <- summary(mylogit_05)$coefficients
OWN_T3_z <- OWN_T3[2,1:4]
OWN_T3_z<-as.data.frame(OWN_T3_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}
if (OWN_T3_z[4,1] > 0.05) {OWN_T3_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

valledupar_ELECTRIC <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(valledupar_ELECTRIC$ELECTRIC)
valledupar_ELECTRIC <- valledupar_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(valledupar_ELECTRIC$ELECTRIC)
table(valledupar_ELECTRIC$eletric_b)

valledupar_ELECTRIC$eletric_b <- factor(valledupar_ELECTRIC$eletric_b)

valledupar_ELECTRIC$YEAR <- factor(valledupar_ELECTRIC$YEAR)
valledupar_ELECTRIC_93 <- valledupar_ELECTRIC %>% filter(valledupar_ELECTRIC$YEAR==1993)
valledupar_ELECTRIC_05 <- valledupar_ELECTRIC %>% filter(valledupar_ELECTRIC$YEAR==2005)

mylogit_93 <- glm(eletric_b ~ YRSCHOOL, data = valledupar_ELECTRIC_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(eletric_b ~ YRSCHOOL, data = valledupar_ELECTRIC_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

ELECTRIC_T2 <- summary(mylogit_93)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
ELECTRIC_T3 <- summary(mylogit_05)$coefficients
ELECTRIC_T3_z <- ELECTRIC_T3[2,1:4]
ELECTRIC_T3_z<-as.data.frame(ELECTRIC_T3_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}
if (ELECTRIC_T3_z[4,1] > 0.05) {ELECTRIC_T3_z[1,1] <- 0}

#################################### FOR TRASH ##################################

valledupar_TRASH <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,TRASH,trash_b,YRSCHOOL,CITY)
table(valledupar_TRASH$TRASH)
valledupar_TRASH <- valledupar_TRASH %>% filter(TRASH!=0) %>% filter(TRASH!=99)

table(valledupar_TRASH$TRASH)
table(valledupar_TRASH$trash_b)

valledupar_TRASH$trash_b <- factor(valledupar_TRASH$trash_b)

valledupar_TRASH$YEAR <- factor(valledupar_TRASH$YEAR)
valledupar_TRASH_93 <- valledupar_TRASH %>% filter(valledupar_TRASH$YEAR==1993)
valledupar_TRASH_05 <- valledupar_TRASH %>% filter(valledupar_TRASH$YEAR==2005)

mylogit_93 <- glm(trash_b ~ YRSCHOOL, data = valledupar_TRASH_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(trash_b ~ YRSCHOOL, data = valledupar_TRASH_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TRASH_T2 <- summary(mylogit_93)$coefficients
TRASH_T2_z <- TRASH_T2[2,1:4]
TRASH_T2_z<-as.data.frame(TRASH_T2_z)
TRASH_T3 <- summary(mylogit_05)$coefficients
TRASH_T3_z <- TRASH_T3[2,1:4]
TRASH_T3_z<-as.data.frame(TRASH_T3_z)
if (TRASH_T2_z[4,1] > 0.05) {TRASH_T2_z[1,1] <- 0}
if (TRASH_T3_z[4,1] > 0.05) {TRASH_T3_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

valledupar_AUTOS <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,AUTOS,autos_b,YRSCHOOL,CITY)
table(valledupar_AUTOS$AUTOS)
valledupar_AUTOS <- valledupar_AUTOS %>% filter(AUTOS!=8)

table(valledupar_AUTOS$AUTOS)
table(valledupar_AUTOS$autos_b)

valledupar_AUTOS$autos_b <- factor(valledupar_AUTOS$autos_b)

valledupar_AUTOS$YEAR <- factor(valledupar_AUTOS$YEAR)
valledupar_AUTOS_05 <- valledupar_AUTOS %>% filter(valledupar_AUTOS$YEAR==2005)

mylogit_05 <- glm(autos_b ~ YRSCHOOL, data = valledupar_AUTOS_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

AUTOS_T3 <- summary(mylogit_05)$coefficients
AUTOS_T3_z <- AUTOS_T3[2,1:4]
AUTOS_T3_z<-as.data.frame(AUTOS_T3_z)
if (AUTOS_T3_z[4,1] > 0.05) {AUTOS_T3_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

valledupar_REFRIG <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,REFRIG,refrig_b,YRSCHOOL,CITY)
table(valledupar_REFRIG$REFRIG)
valledupar_REFRIG <- valledupar_REFRIG %>% filter(REFRIG!=0)

table(valledupar_REFRIG$REFRIG)
table(valledupar_REFRIG$refrig_b)

valledupar_REFRIG$refrig_b <- factor(valledupar_REFRIG$refrig_b)

valledupar_REFRIG$YEAR <- factor(valledupar_REFRIG$YEAR)
valledupar_REFRIG_05 <- valledupar_REFRIG %>% filter(valledupar_REFRIG$YEAR==2005)

mylogit_05 <- glm(refrig_b ~ YRSCHOOL, data = valledupar_REFRIG_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

REFRIG_T3 <- summary(mylogit_05)$coefficients
REFRIG_T3_z <- REFRIG_T3[2,1:4]
REFRIG_T3_z<-as.data.frame(REFRIG_T3_z)
if (REFRIG_T3_z[4,1] > 0.05) {REFRIG_T3_z[1,1] <- 0}


#################################### FOR TV ##################################

valledupar_TV <- valledupar_full %>% select(COUNTRY,YEAR,HHWT,AGE,SAMPLE,SERIAL,TV,tv_b,YRSCHOOL,CITY)
table(valledupar_TV$TV)
valledupar_TV <- valledupar_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(valledupar_TV$TV)
table(valledupar_TV$tv_b)

valledupar_TV$tv_b <- factor(valledupar_TV$tv_b)

valledupar_TV$YEAR <- factor(valledupar_TV$YEAR)
valledupar_TV_05 <- valledupar_TV %>% filter(valledupar_TV$YEAR==2005)

mylogit_05 <- glm(tv_b ~ YRSCHOOL, data = valledupar_TV_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TV_T3 <- summary(mylogit_05)$coefficients
TV_T3_z <- TV_T3[2,1:4]
TV_T3_z<-as.data.frame(TV_T3_z)
if (TV_T3_z[4,1] > 0.05) {TV_T3_z[1,1] <- 0}


#################################### FOR TOILET ##################################

valledupar_TOILET <- valledupar_full %>% select(COUNTRY,YEAR,AGE,HHWT,SAMPLE,SERIAL,TOILET,toilet_b,YRSCHOOL,CITY)
table(valledupar_TOILET$TOILET)
valledupar_TOILET <- valledupar_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(valledupar_TOILET$TOILET)
table(valledupar_TOILET$toilet_b)

valledupar_TOILET$toilet_b <- factor(valledupar_TOILET$toilet_b)

valledupar_TOILET$YEAR <- factor(valledupar_TOILET$YEAR)
valledupar_TOILET_93 <- valledupar_TOILET %>% filter(valledupar_TOILET$YEAR==1993)
valledupar_TOILET_05 <- valledupar_TOILET %>% filter(valledupar_TOILET$YEAR==2005)

mylogit_93 <- glm(toilet_b ~ YRSCHOOL, data = valledupar_TOILET_93, family = "binomial",weights=HHWT)
summary(mylogit_93)
exp(coef(mylogit_93))

mylogit_05 <- glm(toilet_b ~ YRSCHOOL, data = valledupar_TOILET_05, family = "binomial",weights=HHWT)
summary(mylogit_05)
exp(coef(mylogit_05))

TOILET_T2 <- summary(mylogit_93)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
TOILET_T3 <- summary(mylogit_05)$coefficients
TOILET_T3_z <- TOILET_T3[2,1:4]
TOILET_T3_z<-as.data.frame(TOILET_T3_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}
if (TOILET_T3_z[4,1] > 0.05) {TOILET_T3_z[1,1] <- 0}

VALLEDUPAR_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,OWN_T3_z,ELECTRIC_T2_z,ELECTRIC_T3_z,TRASH_T2_z,TRASH_T3_z,AUTOS_T3_z,REFRIG_T3_z,TV_T3_z,TOILET_T2_z,TOILET_T3_z) 
VALLEDUPAR_logit$CITY <- "Valledupar"


COLOMBIA_logit <- rbind(BOGOTA_logit, VALLEDUPAR_logit,sep = ".") 
write.csv(COLOMBIA_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/COLOMBIA_logit.csv"))

















