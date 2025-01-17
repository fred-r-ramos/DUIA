library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ineq)
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/chile")

geo2_cl1992 <- read_sf("geo2_cl1992.shp")
geo2_cl2002 <- read_sf("geo2_cl2002.shp")
geo2_cl2017 <- read_sf("geo2_cl2017.shp")
AUE_santiago <- read_sf("Santiago_studyArea.shp")
AUE_santiago <- st_transform(AUE_santiago, 4326)


sf::sf_use_s2(FALSE)

centroid_cl1992 <- st_centroid(geo2_cl1992)
centroid_cl2002 <- st_centroid(geo2_cl2002)
centroid_cl2017 <- st_centroid(geo2_cl2017)

santiago_92 <- geo2_cl1992[st_intersection(AUE_santiago,centroid_cl1992),]
santiago_92['CITY']='santiago'
santiago_02 <- geo2_cl2002[st_intersection(AUE_santiago,centroid_cl2002),]
santiago_02['CITY']='santiago'
santiago_17 <- geo2_cl2017[st_intersection(AUE_santiago,centroid_cl2017),]
santiago_17['CITY']='santiago'

plot(st_geometry(santiago_17), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_cl2017[0], add = TRUE)


##Importing new extraction from IPUMS with the geographical variable 

#ddi <-read_ipums_ddi("ipumsi_00239.xml")
ddi <-read_ipums_ddi("ipumsi_00246.xml")
chile <- read_ipums_micro(ddi)

######################creating binary variables for logit regressions

names(chile)

# [1] "COUNTRY"     "YEAR"        "SAMPLE"      "SERIAL"      "HHWT"        "GEO2_CL1992"
# [7] "GEO2_CL2002" "GEO2_CL2017" "OWNERSHIP"   "OWNERSHIPD"  "ELECTRIC"    "WATSUP"     
# [13] "SEWAGE"      "AUTOS"       "REFRIG"      "TV"          "TOILET"      "FLOOR"      
# [19] "ROOF"        "PERNUM"      "PERWT"       "AGE"         "SEX"         "LIT"        
# [25] "EDATTAIN"    "EDATTAIND"   "YRSCHOOL"    "EMPSTAT"     "EMPSTATD"    "LABFORCE"   
# [31] "OCCISCO"     "OCC"         "INDGEN"     

table(chile$OWNERSHIP)
chile$owner_b <- ifelse(chile$OWNERSHIP ==1,1,0)
table(chile$ELECTRIC)
chile$eletric_b <- ifelse(chile$ELECTRIC ==1,1,0)
table(chile$WATSUP)
chile$water_b <- ifelse(chile$WATSUP ==11|chile$WATSUP ==10,1,0)
table(chile$SEWAGE)
chile$sewage_b <- ifelse(chile$SEWAGE ==11|chile$SEWAGE ==12|chile$SEWAGE ==10,1,0)
table(chile$AUTOS)
chile$autos_b <- ifelse(chile$AUTOS ==7,1,0)
table(chile$REFRIG)
chile$refrig_b <- ifelse(chile$REFRIG ==2,1,0)
table(chile$TV)
chile$tv_b <- ifelse(chile$TV >10,1,0)
table(chile$TOILET)
chile$toilet_b <- ifelse(chile$TOILET >19,1,0)
table(chile$FLOOR)
chile$floor_b <- ifelse(chile$FLOOR >100&chile$FLOOR<999,1,0)
table(chile$ROOF)
chile$roof_b <- ifelse(chile$ROOF>0&chile$ROOF<30,1,0)

#checking if the variables are available in both years
table(chile$owner_b,chile$YEAR)
table(chile$eletric_b,chile$YEAR)
table(chile$water_b,chile$YEAR)
table(chile$sewage_b,chile$YEAR)
table(chile$autos_b,chile$YEAR)
table(chile$refrig_b,chile$YEAR)
table(chile$tv_b,chile$YEAR)
table(chile$toilet_b,chile$YEAR)
table(chile$floor_b,chile$YEAR)
table(chile$roof_b,chile$YEAR)

##########calculating a variable of total assets PUBLIC, PRIVATE, TOTAL
chile$PUBl_ASSET <- chile$eletric_b+chile$water_b+chile$sewage_b
chile %>%
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

chile$PRIV_ASSET <- chile$owner_b+chile$autos_b+chile$refrig_b+chile$tv_b+chile$toilet_b+chile$floor_b+chile$roof_b
chile %>%
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

chile$TOTAL_ASSET <- chile$eletric_b+chile$water_b+chile$sewage_b+chile$owner_b+chile$autos_b+chile$refrig_b+chile$tv_b+chile$toilet_b+chile$floor_b+chile$roof_b
chile %>%
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))

##Creating field for join

chile$IPUM1992 <- as.integer(chile$GEO2_CL1992)
chile$IPUM2002 <- as.integer(chile$GEO2_CL2002)
chile$IPUM2017 <- as.integer(chile$GEO2_CL2017)
santiago_92$IPUM1992 <- as.integer(santiago_92$IPUM1992)
santiago_02$IPUM2002 <- as.integer(santiago_02$IPUM2002)
santiago_17$IPUM2017 <- as.integer(santiago_17$IPUM2017)

##Joining by year

chile_92 <- chile %>% inner_join(santiago_92, by="IPUM1992")
chile_02 <- chile %>% inner_join(santiago_02, by="IPUM2002")
chile_17 <- chile %>% inner_join(santiago_17, by="IPUM2017")


names(chile_92)
names(chile_02)
names(chile_17)

chile_92 <- select(chile_92, -c(MUNI1992))
chile_02 <- select(chile_02, -c(MUNI2002))
chile_17 <- select(chile_17, -c(COMM2017))


##Merging all years into one table
santiago_full <- rbind(chile_92,chile_02,chile_17)
names(santiago_full)

table(chile$PERWT)

##Excluding specific columns for the unified dataset
santiago_full<- select(santiago_full, -c(GEO2_CL1992,GEO2_CL2002,GEO2_CL2017,IPUM1992,IPUM2002,IPUM2017,geometry))
table(santiago_full$CITY)

names(santiago_full)

names(santiago_full)

table(santiago_full$YEAR)

santiago_full <- santiago_full %>%  filter (santiago_full$YRSCHOOL < 90)

santiago_full <- santiago_full %>%  filter (santiago_full$AGE >15)

santiago_full %>% group_by(YEAR) %>% summarise(avg=mean(YRSCHOOL),sd=sd(YRSCHOOL))

mean(santiago_full$YRSCHOOL)
summary(santiago_full$YRSCHOOL)

chile_fu92 <- santiago_full %>%  filter (YEAR==1992)
chile_fu02 <- santiago_full %>%  filter (YEAR==2002)
chile_fu17 <- santiago_full %>%  filter (YEAR==2017)

Gini(chile_fu92$YRSCHOOL,na.rm = TRUE)
Gini(chile_fu02$YRSCHOOL,na.rm = TRUE)
Gini(chile_fu92$TOTAL_ASSET,na.rm = TRUE)
Gini(chile_fu02$TOTAL_ASSET,na.rm = TRUE)



## compute the Lorenz curves
Lc_santi92 <- Lc(chile_fu92$YRSCHOOL, n = rep(1,length(chile_fu92$YRSCHOOL)), plot = TRUE)
Lc_santi02 <- Lc(chile_fu02$YRSCHOOL, n = rep(1,length(chile_fu02$YRSCHOOL)), plot = TRUE)
Lc_santi17 <- Lc(chile_fu17$YRSCHOOL, n = rep(1,length(chile_fu17$YRSCHOOL)), plot = TRUE)


plot(Lc_santi92,col='blue', main = "Lorenz Curve - Santiago")
lines(Lc_santi02, col='red')
lines(Lc_santi17, col='green')


table(santiago_full$OCCISCO)


santiago_full$OCCISCO_b <- ifelse(santiago_full$OCCISCO ==1|santiago_full$OCCISCO ==2,1,0)
santiago_full$OCCISCO_b <- ifelse(santiago_full$OCCISCO ==3|santiago_full$OCCISCO ==4|santiago_full$OCCISCO ==5,2,santiago_full$OCCISCO_b)
santiago_full$OCCISCO_b <- ifelse(santiago_full$OCCISCO ==6|santiago_full$OCCISCO ==7|santiago_full$OCCISCO ==8|santiago_full$OCCISCO ==9,3,santiago_full$OCCISCO_b)
table(santiago_full$OCCISCO_b)

santiago_full_OCCISCO_b <- santiago_full %>% select(YEAR,OCCISCO_b,PERWT)
santiago_full_OCCISCO_b <- santiago_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(santiago_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- santiago_full_OCCISCO_b %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- santiago_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- santiago_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- santiago_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

SANTIAGO_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR"))
SANTIAGO_OCCISCO_b<- SANTIAGO_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR"))
SANTIAGO_OCCISCO_b<- SANTIAGO_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR"))

SANTIAGO_OCCISCO_b



# for PUBl_ASSET
santiago_PUBl_ASSET<-santiago_full %>% 
  group_by(YEAR,PUBl_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- santiago_PUBl_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
santiago_PUBl_ASSET <- santiago_PUBl_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
santiago_PUBl_ASSET <- santiago_PUBl_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
santiago_PUBl_ASSET$CITY<-"santiago"
# for PRIV_ASSET
santiago_PRIV_ASSET<-santiago_full %>% 
  group_by(YEAR,PRIV_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- santiago_PRIV_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
santiago_PRIV_ASSET <- santiago_PRIV_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
santiago_PRIV_ASSET <- santiago_PRIV_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
santiago_PRIV_ASSET$CITY<-"santiago"
# for TOTAL_ASSET
santiago_TOTAL_ASSET<-santiago_full %>% 
  group_by(YEAR,TOTAL_ASSET) %>%
  summarise(weighted_sum = sum(HHWT))
# Calculate the totals by year
totals <- santiago_TOTAL_ASSET %>%
  group_by(YEAR) %>%
  summarise(total = sum(weighted_sum))
# Merge the totals back into the original data frame
santiago_TOTAL_ASSET <- santiago_TOTAL_ASSET %>%
  left_join(totals, by = "YEAR")
# Calculate the percentage over the total by year
santiago_TOTAL_ASSET <- santiago_TOTAL_ASSET %>%
  mutate(percentage = (weighted_sum / total) * 100)
santiago_TOTAL_ASSET$CITY<-"santiago"
write.csv(santiago_PUBl_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/santiago_PUBl_ASSET.csv", row.names = TRUE)
write.csv(santiago_PRIV_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/santiago_PRIV_ASSET.csv", row.names = TRUE)
write.csv(santiago_TOTAL_ASSET, file = "C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/assets_groups/santiago_TOTAL_ASSET.csv", row.names = TRUE)



######################Logistic regressions for selected variables############################
#################################### FOR WATSUP ##################################

santiago_WATER <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,WATSUP,water_b,YRSCHOOL,CITY)
table(santiago_WATER$WATSUP)
santiago_WATER <- santiago_WATER %>% filter(WATSUP!=0)%>% filter(WATSUP!=99)
table(santiago_WATER$HHWT)

table(santiago_WATER$WATSUP)
table(santiago_WATER$water_b)

santiago_WATER$water_b <- factor(santiago_WATER$water_b)

santiago_WATER$YEAR <- factor(santiago_WATER$YEAR)
santiago_WATER_02 <- santiago_WATER %>% filter(santiago_WATER$YEAR==2002)
santiago_WATER_17 <- santiago_WATER %>% filter(santiago_WATER$YEAR==2017)

mylogit_02 <- glm(water_b ~ YRSCHOOL, data = santiago_WATER_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

mylogit_17 <- glm(water_b ~ YRSCHOOL, data = santiago_WATER_17, family = "binomial")
summary(mylogit_17)
exp(coef(mylogit_17))

#####saving z-value and beta

WATER_T2 <- summary(mylogit_02)$coefficients
WATER_T2_z <- WATER_T2[2,1:4]
WATER_T2_z<-as.data.frame(WATER_T2_z)
WATER_T3 <- summary(mylogit_17)$coefficients
WATER_T3_z <- WATER_T3[2,1:4]
WATER_T3_z<-as.data.frame(WATER_T3_z)
if (WATER_T2_z[4,1] > 0.05) {WATER_T2_z[1,1] <- 0}
if (WATER_T3_z[4,1] > 0.05) {WATER_T3_z[1,1] <- 0}


gc()

#################################### FOR OWNERSHIP ##################################

santiago_OWN <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,OWNERSHIP,owner_b,YRSCHOOL,CITY)
table(santiago_OWN$OWNERSHIP)
santiago_OWN <- santiago_OWN %>% filter(OWNERSHIP!=0)%>% filter(OWNERSHIP!=9)

table(santiago_OWN$OWNERSHIP)
table(santiago_OWN$owner_b)

santiago_OWN$owner_b <- factor(santiago_OWN$owner_b)

santiago_OWN$YEAR <- factor(santiago_OWN$YEAR)
santiago_OWN_02 <- santiago_OWN %>% filter(santiago_OWN$YEAR==2002)
santiago_OWN_17 <- santiago_OWN %>% filter(santiago_OWN$YEAR==2017)

mylogit_02 <- glm(owner_b ~ YRSCHOOL, data = santiago_OWN_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

OWN_T2 <- summary(mylogit_02)$coefficients
OWN_T2_z <- OWN_T2[2,1:4]
OWN_T2_z<-as.data.frame(OWN_T2_z)
if (OWN_T2_z[4,1] > 0.05) {OWN_T2_z[1,1] <- 0}

#################################### FOR SEWAGE ##################################

santiago_SEWG <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,SEWAGE,sewage_b,YRSCHOOL,CITY)
table(santiago_SEWG$SEWAGE)
santiago_SEWG <- santiago_SEWG %>% filter(SEWAGE!=0)%>% filter(SEWAGE!=99)

table(santiago_SEWG$SEWAGE)
table(santiago_SEWG$sewage_b)

santiago_SEWG$sewage_b <- factor(santiago_SEWG$sewage_b)

santiago_SEWG$YEAR <- factor(santiago_SEWG$YEAR)
santiago_SEWG_02 <- santiago_SEWG %>% filter(santiago_SEWG$YEAR==2002)

mylogit_02 <- glm(sewage_b ~ YRSCHOOL, data = santiago_SEWG_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

SEWG_T2 <- summary(mylogit_02)$coefficients
SEWG_T2_z <- SEWG_T2[2,1:4]
SEWG_T2_z<-as.data.frame(SEWG_T2_z)
if (SEWG_T2_z[4,1] > 0.05) {SEWG_T2_z[1,1] <- 0}

#################################### FOR REFRIG ##################################

santiago_REFRIG <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,REFRIG,refrig_b,YRSCHOOL,CITY)
table(santiago_REFRIG$REFRIG)
santiago_REFRIG <- santiago_REFRIG %>% filter(REFRIG!=0)

table(santiago_REFRIG$REFRIG)
table(santiago_REFRIG$refrig_b)

santiago_REFRIG$refrig_b <- factor(santiago_REFRIG$refrig_b)

santiago_REFRIG$YEAR <- factor(santiago_REFRIG$YEAR)
santiago_REFRIG_02 <- santiago_REFRIG %>% filter(santiago_REFRIG$YEAR==2002)

mylogit_02 <- glm(refrig_b ~ YRSCHOOL, data = santiago_REFRIG_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

REFRIG_T2 <- summary(mylogit_02)$coefficients
REFRIG_T2_z <- REFRIG_T2[2,1:4]
REFRIG_T2_z<-as.data.frame(REFRIG_T2_z)
if (REFRIG_T2_z[4,1] > 0.05) {REFRIG_T2_z[1,1] <- 0}

#################################### FOR TOILET ##################################

santiago_TOILET <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TOILET,toilet_b,YRSCHOOL,CITY)
table(santiago_TOILET$TOILET)
santiago_TOILET <- santiago_TOILET %>% filter(TOILET!=0) %>% filter(TOILET!=99)

table(santiago_TOILET$TOILET)
table(santiago_TOILET$toilet_b)

santiago_TOILET$toilet_b <- factor(santiago_TOILET$toilet_b)

santiago_TOILET$YEAR <- factor(santiago_TOILET$YEAR)
santiago_TOILET_02 <- santiago_TOILET %>% filter(santiago_TOILET$YEAR==2002)

mylogit_02 <- glm(toilet_b ~ YRSCHOOL, data = santiago_TOILET_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

TOILET_T2 <- summary(mylogit_02)$coefficients
TOILET_T2_z <- TOILET_T2[2,1:4]
TOILET_T2_z<-as.data.frame(TOILET_T2_z)
if (TOILET_T2_z[4,1] > 0.05) {TOILET_T2_z[1,1] <- 0}

#################################### FOR ELECTRIC ##################################

santiago_ELECTRIC <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ELECTRIC,eletric_b,YRSCHOOL,CITY)
table(santiago_ELECTRIC$ELECTRIC)
santiago_ELECTRIC <- santiago_ELECTRIC %>% filter(ELECTRIC!=0) %>% filter(ELECTRIC!=99)

table(santiago_ELECTRIC$ELECTRIC)
table(santiago_ELECTRIC$eletric_b)

santiago_ELECTRIC$eletric_b <- factor(santiago_ELECTRIC$eletric_b)

santiago_ELECTRIC$YEAR <- factor(santiago_ELECTRIC$YEAR)
santiago_ELECTRIC_02 <- santiago_ELECTRIC %>% filter(santiago_ELECTRIC$YEAR==2002)

mylogit_02 <- glm(eletric_b ~ YRSCHOOL, data = santiago_ELECTRIC_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

ELECTRIC_T2 <- summary(mylogit_02)$coefficients
ELECTRIC_T2_z <- ELECTRIC_T2[2,1:4]
ELECTRIC_T2_z<-as.data.frame(ELECTRIC_T2_z)
if (ELECTRIC_T2_z[4,1] > 0.05) {ELECTRIC_T2_z[1,1] <- 0}

#################################### FOR AUTOS ##################################

santiago_AUTOS <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,AUTOS,autos_b,YRSCHOOL,CITY)
table(santiago_AUTOS$AUTOS)
santiago_AUTOS <- santiago_AUTOS %>% filter(AUTOS!=99)

table(santiago_AUTOS$AUTOS)
table(santiago_AUTOS$autos_b)

santiago_AUTOS$autos_b <- factor(santiago_AUTOS$autos_b)

santiago_AUTOS$YEAR <- factor(santiago_AUTOS$YEAR)
santiago_AUTOS_02 <- santiago_AUTOS %>% filter(santiago_AUTOS$YEAR==2002)

mylogit_02 <- glm(autos_b ~ YRSCHOOL, data = santiago_AUTOS_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

AUTOS_T2 <- summary(mylogit_02)$coefficients
AUTOS_T2_z <- AUTOS_T2[2,1:4]
AUTOS_T2_z<-as.data.frame(AUTOS_T2_z)
if (AUTOS_T2_z[4,1] > 0.05) {AUTOS_T2_z[1,1] <- 0}

#################################### FOR FLOORS ##################################

santiago_FLOOR <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,FLOOR,floor_b,YRSCHOOL,CITY)
table(santiago_FLOOR$FLOOR)
santiago_FLOOR <- santiago_FLOOR %>% filter(FLOOR!=0) %>% filter(FLOOR!=99)

table(santiago_FLOOR$FLOOR)
table(santiago_FLOOR$floor_b)

santiago_FLOOR$floor_b <- factor(santiago_FLOOR$floor_b)

santiago_FLOOR$YEAR <- factor(santiago_FLOOR$YEAR)
santiago_FLOOR_02 <- santiago_FLOOR %>% filter(santiago_FLOOR$YEAR==2002)
santiago_FLOOR_17 <- santiago_FLOOR %>% filter(santiago_FLOOR$YEAR==2017)

mylogit_02 <- glm(floor_b ~ YRSCHOOL, data = santiago_FLOOR_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

mylogit_17 <- glm(floor_b ~ YRSCHOOL, data = santiago_FLOOR_17, family = "binomial")
summary(mylogit_17)
exp(coef(mylogit_17))

FLOOR_T2 <- summary(mylogit_02)$coefficients
FLOOR_T2_z <- FLOOR_T2[2,1:4]
FLOOR_T2_z<-as.data.frame(FLOOR_T2_z)
FLOOR_T3 <- summary(mylogit_17)$coefficients
FLOOR_T3_z <- FLOOR_T3[2,1:4]
FLOOR_T3_z<-as.data.frame(FLOOR_T3_z)
if (FLOOR_T2_z[4,1] > 0.05) {FLOOR_T2_z[1,1] <- 0}
if (FLOOR_T3_z[4,1] > 0.05) {FLOOR_T3_z[1,1] <- 0}


#################################### FOR ROOF ##################################

santiago_ROOF <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,ROOF,roof_b,YRSCHOOL,CITY)
table(santiago_ROOF$ROOF)
santiago_ROOF <- santiago_ROOF %>% filter(ROOF!=0) %>% filter(ROOF!=99)

table(santiago_ROOF$ROOF)
table(santiago_ROOF$roof_b)

santiago_ROOF$roof_b <- factor(santiago_ROOF$roof_b)

santiago_ROOF$YEAR <- factor(santiago_ROOF$YEAR)
santiago_ROOF_02 <- santiago_ROOF %>% filter(santiago_ROOF$YEAR==2002)
santiago_ROOF_17 <- santiago_ROOF %>% filter(santiago_ROOF$YEAR==2017)

mylogit_02 <- glm(roof_b ~ YRSCHOOL, data = santiago_ROOF_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

mylogit_17 <- glm(roof_b ~ YRSCHOOL, data = santiago_ROOF_17, family = "binomial")
summary(mylogit_17)
exp(coef(mylogit_17))

ROOF_T2 <- summary(mylogit_02)$coefficients
ROOF_T2_z <- ROOF_T2[2,1:4]
ROOF_T2_z<-as.data.frame(ROOF_T2_z)
ROOF_T3 <- summary(mylogit_17)$coefficients
ROOF_T3_z <- ROOF_T3[2,1:4]
ROOF_T3_z<-as.data.frame(ROOF_T3_z)
if (ROOF_T2_z[4,1] > 0.05) {ROOF_T2_z[1,1] <- 0}
if (ROOF_T3_z[4,1] > 0.05) {ROOF_T3_z[1,1] <- 0}

#################################### FOR TV ##################################

santiago_TV <- santiago_full %>% select(COUNTRY,YEAR,AGE,SAMPLE,SERIAL,HHWT,TV,tv_b,YRSCHOOL,CITY)
table(santiago_TV$TV)
santiago_TV <- santiago_TV %>% filter(TV!=0) %>% filter(TV!=99)

table(santiago_TV$TV)
table(santiago_TV$tv_b)

santiago_TV$tv_b <- factor(santiago_TV$tv_b)

santiago_TV$YEAR <- factor(santiago_TV$YEAR)
santiago_TV_02 <- santiago_TV %>% filter(santiago_TV$YEAR==2002)

mylogit_02 <- glm(tv_b ~ YRSCHOOL, data = santiago_TV_02, family = "binomial")
summary(mylogit_02)
exp(coef(mylogit_02))

TV_T2 <- summary(mylogit_02)$coefficients
TV_T2_z <- TV_T2[2,1:4]
TV_T2_z<-as.data.frame(TV_T2_z)
if (TV_T2_z[4,1] > 0.05) {TV_T2_z[1,1] <- 0}

santiago_logit <- cbind(WATER_T2_z,WATER_T3_z,OWN_T2_z,SEWG_T2_z,REFRIG_T2_z,TOILET_T2_z,ELECTRIC_T2_z,AUTOS_T2_z,FLOOR_T2_z,FLOOR_T3_z,ROOF_T2_z,ROOF_T3_z,TV_T2_z) 
santiago_logit$CITY <- "Santiago"


#################################################Saving an unique table results join

CHILE_logit <- rbind(santiago_logit,sep = ".") 
write.csv(CHILE_logit,file.path("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/outputs_LOGITS/CHILE_logit.csv"))


