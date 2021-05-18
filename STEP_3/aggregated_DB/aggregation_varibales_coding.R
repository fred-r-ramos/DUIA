library(sf)
library(tidyverse)
library(dplyr)
library(ipumsr)
library(ggplot2)

###set the working directory where all the dataset are located (IPUMS, Second level Administrative Shapefile, AUE Study area)

setwd("C:/....")

###loading data and correcting projection

geo2_91 <- read_sf("geo2_ar1991.shp")
geo2_01 <- read_sf("geo2_ar2001.shp")
geo2_10 <- read_sf("geo2_ar2010.shp")
AUE_ba <- read_sf("CITY1_studyArea.shp")
AUE_co <- read_sf("CITY2_studyArea.shp")
AUE_b <- st_transform(AUE_ba, 4326)
AUE_c <- st_transform(AUE_co, 4326)

#### selecting by centroids

centroid_ar91 <- st_centroid(geo2_91)
centroid_ar01 <- st_centroid(geo2_01)
centroid_ar10 <- st_centroid(geo2_10)

CITY1_91 <- geo2_91[st_intersection(AUE_b, centroid_ar91),]
CITY1_91['CITY']='buenos aires'
CITY1_01 <- geo2_01[st_intersection(AUE_b, centroid_ar01),]
CITY1_01['CITY']='buenos aires'
CITY1_10 <- geo2_10[st_intersection(AUE_b, centroid_ar10),]
CITY1_10['CITY']='buenos aires'

CITY2_91 <- geo2_91[st_intersection(AUE_c, centroid_ar91),]
CITY2_91['CITY']='CITY2'
CITY2_01 <- geo2_01[st_intersection(AUE_c, centroid_ar01),]
CITY2_01['CITY']='CITY2'
CITY2_10 <- geo2_10[st_intersection(AUE_c, centroid_ar10),]
CITY2_10['CITY']='CITY2'

##### joining and plotting geometry

geo_NATION_91 <- rbind(CITY1_91,CITY2_91)
geo_NATION_01 <- rbind(CITY1_01,CITY2_01)
geo_NATION_10 <- rbind(CITY1_10,CITY2_10)

plot(st_geometry(geo_NATION_10), expandBB = c(1, 1, 1, 1), col = "red", lwd = 3)
plot(geo2_10[0], add = TRUE)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00000.xml")
NATION <- read_ipums_micro(ddi)

names(NATION)

##Creating field for join

NATION$IPUM1991 <- as.integer(NATION$GEO2_AR1991)
NATION$IPUM2001 <- as.integer(NATION$GEO2_AR2001)
NATION$IPUM2010 <- as.integer(NATION$GEO2_AR2010)
geo_NATION_91$IPUM1991 <- as.integer(geo_NATION_91$IPUM1991)
geo_NATION_01$IPUM2001 <- as.integer(geo_NATION_01$IPUM2001)
geo_NATION_10$IPUM2010 <- as.integer(geo_NATION_10$IPUM2010)

##Joining by year

NATION_91 <- NATION %>% inner_join(geo_NATION_91, by="IPUM1991")
NATION_01 <- NATION %>% inner_join(geo_NATION_01, by="IPUM2001")
NATION_10 <- NATION %>% inner_join(geo_NATION_10, by="IPUM2010")

names(NATION_91)
names(NATION_01)
names(NATION_10)

NATION_91 <- select(NATION_91, -c(DEPT1991))
NATION_01 <- select(NATION_01, -c(DEPT2001))
NATION_10 <- select(NATION_10, -c(DEPT2010))

##Merging all years into one table
NATION_full <- rbind(NATION_91,NATION_01,NATION_10)
names(NATION_full)

######Excluding specific columns for the unifeied dataset (as an example the case for Argentina GEO2_ARYEAR)

NATION_full<- select(NATION_full, -c(GEO2_AR1991,GEO2_AR2001,GEO2_AR2010,IPUM1991,IPUM2001,IPUM2010))
table(NATION_full$CITY)

NATION_cities <- NATION_full %>% filter(CITY=="buenos aires"|CITY=="CITY2")



###################Excluding observations not in the universe for specific variables and coding the variables#########################################################

##OWNERSHIP

NATION_full_ownership <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,OWNERSHIP,CITY)
NATION_full_ownership <- NATION_full_ownership %>% filter(OWNERSHIP !=0)

table(NATION_full_ownership$OWNERSHIP)

OWNERSHIP_total <- NATION_full_ownership %>% 
  group_by(CITY,YEAR) %>% 
  summarise(OWNERSHIP_total = sum(HHWT))

OWNERSHIP_owned <- NATION_full_ownership %>% filter (OWNERSHIP==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(OWNERSHIP_owned = sum(HHWT))

OWNERSHIP_n_owned <- NATION_full_ownership %>% filter (OWNERSHIP==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(OWNERSHIP_n_owned = sum(HHWT))

OWNERSHIP_owned_unkw <- NATION_full_ownership  %>% filter (OWNERSHIP==9) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(OWNERSHIP_owned_unkw = sum(HHWT))

NATION_ownership<- OWNERSHIP_total %>% inner_join(OWNERSHIP_owned, by = c("YEAR","CITY"))
NATION_ownership<- NATION_ownership %>% inner_join(OWNERSHIP_n_owned,by = c("YEAR","CITY"))
NATION_ownership<- NATION_ownership %>% left_join(OWNERSHIP_owned_unkw,by = c("YEAR","CITY"))

NATION_ownership

##LANDOWN

NATION_full_landown <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,LANDOWN,CITY)
NATION_full_landown <- NATION_full_landown %>% filter(LANDOWN !=00)

table(NATION_full_landown$LANDOWN)

LANDOWN_total <- NATION_full_landown %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LANDOWN_total = sum(HHWT))

LANDOWN_owned <- NATION_full_landown %>% filter (LANDOWN==10) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LANDOWN_owned = sum(HHWT))

LANDOWN_n_owned <- NATION_full_landown %>% filter (LANDOWN==20) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LANDOWN_n_owned = sum(HHWT))

LANDOWN_owned_unkw <- NATION_full_landown  %>% filter (LANDOWN==99) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(LANDOWN_owned_unkw = sum(HHWT))

NATION_landown<- LANDOWN_total %>% inner_join(LANDOWN_owned, by = c("YEAR","CITY"))
NATION_landown<- NATION_landown %>% inner_join(LANDOWN_n_owned,by = c("YEAR","CITY"))
NATION_landown<- NATION_landown %>% left_join(LANDOWN_owned_unkw,by = c("YEAR","CITY"))

NATION_landown

##ELECTRIC

NATION_full_eletric <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,ELECTRIC,CITY)
NATION_full_eletric <- NATION_full_eletric %>% filter(ELECTRIC !=00)

table(NATION_full_eletric$ELECTRIC)

ELECTRIC_total <- NATION_full_eletric %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ELECTRIC_total = sum(HHWT))

ELECTRIC_yes <- NATION_full_eletric %>% filter (ELECTRIC==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ELECTRIC_yes = sum(HHWT))

ELECTRIC_no <- NATION_full_eletric %>% filter (ELECTRIC==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ELECTRIC_no = sum(HHWT))

NATION_eletric<- ELECTRIC_total %>% inner_join(ELECTRIC_yes, by = c("YEAR","CITY"))
NATION_eletric<- NATION_eletric %>% inner_join(ELECTRIC_no,by = c("YEAR","CITY"))


NATION_eletric

##WATSUP

NATION_full_watsup <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,WATSUP,CITY)
NATION_full_watsup <- NATION_full_watsup %>% filter(WATSUP !=00)

table(NATION_full_watsup$WATSUP)

WATSUP_total <- NATION_full_watsup %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WATSUP_total = sum(HHWT))

WATSUP_yes <- NATION_full_watsup %>% filter (WATSUP==10|WATSUP==11|WATSUP==16) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WATSUP_yes = sum(HHWT))

WATSUP_no <- NATION_full_watsup %>% filter (WATSUP==20) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WATSUP_no = sum(HHWT))

WATSUP_unkw <- NATION_full_watsup  %>% filter (WATSUP==99) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(WATSUP_unkw = sum(HHWT))

NATION_watsup<- WATSUP_total %>% inner_join(WATSUP_yes, by = c("YEAR","CITY"))
NATION_watsup<- NATION_watsup %>% inner_join(WATSUP_no,by = c("YEAR","CITY"))
NATION_watsup<- NATION_watsup %>% full_join(WATSUP_unkw,by = c("YEAR","CITY"))

NATION_watsup

##SEWAGE

NATION_full_SEWAGE <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,SEWAGE,CITY)
NATION_full_SEWAGE <- NATION_full_SEWAGE %>% filter(SEWAGE !=00)

table(NATION_full_SEWAGE$SEWAGE)

SEWAGE_total <- NATION_full_SEWAGE %>% 
  group_by(CITY,YEAR) %>% 
  summarise(SEWAGE_total = sum(HHWT))

SEWAGE_yes <- NATION_full_SEWAGE %>% filter (SEWAGE==11|SEWAGE==12) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(SEWAGE_yes = sum(HHWT))

SEWAGE_no <- NATION_full_SEWAGE %>% filter (SEWAGE==20) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(SEWAGE_no = sum(HHWT))

SEWAGE_unkw <- NATION_full_SEWAGE  %>% filter (SEWAGE==99) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(SEWAGE_unkw = sum(HHWT))

NATION_SEWAGE<- SEWAGE_total %>% inner_join(SEWAGE_yes, by = c("YEAR","CITY"))
NATION_SEWAGE<- NATION_SEWAGE %>% inner_join(SEWAGE_no,by = c("YEAR","CITY"))
NATION_SEWAGE<- NATION_SEWAGE %>% full_join(SEWAGE_unkw,by = c("YEAR","CITY"))

NATION_SEWAGE

##FUELCOOK

NATION_full_FUELCOOK <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,FUELCOOK,CITY)
NATION_full_FUELCOOK <- NATION_full_FUELCOOK %>% filter(FUELCOOK !=00)

table(NATION_full_FUELCOOK$FUELCOOK)

FUELCOOK_total <- NATION_full_FUELCOOK %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FUELCOOK_total = sum(HHWT))

FUELCOOK_gas_elet <- NATION_full_FUELCOOK %>% filter (FUELCOOK==20|FUELCOOK==31|FUELCOOK==32) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FUELCOOK_gas_elet = sum(HHWT))

FUELCOOK_no <- NATION_full_FUELCOOK %>% filter (FUELCOOK==42|FUELCOOK==56|FUELCOOK==70) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FUELCOOK_no = sum(HHWT))


NATION_FUELCOOK<- FUELCOOK_total %>% inner_join(FUELCOOK_gas_elet, by = c("YEAR","CITY"))
NATION_FUELCOOK<- NATION_FUELCOOK %>% inner_join(FUELCOOK_no,by = c("YEAR","CITY"))

##PHONE

NATION_full_PHONE <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,PHONE,CITY)
NATION_full_PHONE <- NATION_full_PHONE %>% filter(PHONE !=00)

table(NATION_full_PHONE$PHONE)

PHONE_total <- NATION_full_PHONE %>% 
  group_by(CITY,YEAR) %>% 
  summarise(PHONE_total = sum(HHWT))

PHONE_yes <- NATION_full_PHONE %>% filter (PHONE==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(PHONE_yes = sum(HHWT))

PHONE_no <- NATION_full_PHONE %>% filter (PHONE==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(PHONE_no = sum(HHWT))


NATION_PHONE<- PHONE_total %>% inner_join(PHONE_yes, by = c("YEAR","CITY"))
NATION_PHONE<- NATION_PHONE %>% inner_join(PHONE_no,by = c("YEAR","CITY"))


NATION_PHONE
names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_CELL <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,CELL,CITY)
NATION_full_CELL <- NATION_full_CELL %>% filter(CELL !=00)

table(NATION_full_CELL$CELL)

CELL_total <- NATION_full_CELL %>% 
  group_by(CITY,YEAR) %>% 
  summarise(CELL_total = sum(HHWT))

CELL_yes <- NATION_full_CELL %>% filter (CELL==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(CELL_yes = sum(HHWT))

CELL_no <- NATION_full_CELL %>% filter (CELL==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(CELL_no = sum(HHWT))


NATION_CELL<- CELL_total %>% inner_join(CELL_yes, by = c("YEAR","CITY"))
NATION_CELL<- NATION_CELL %>% inner_join(CELL_no,by = c("YEAR","CITY"))


NATION_CELL
names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_INTERNET <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,INTERNET,CITY)
NATION_full_INTERNET <- NATION_full_INTERNET %>% filter(INTERNET !=00)

table(NATION_full_INTERNET$INTERNET)

INTERNET_total <- NATION_full_INTERNET %>% 
  group_by(CITY,YEAR) %>% 
  summarise(INTERNET_total = sum(HHWT))

INTERNET_yes <- NATION_full_INTERNET %>% filter (INTERNET==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(INTERNET_yes = sum(HHWT))

INTERNET_no <- NATION_full_INTERNET %>% filter (INTERNET==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(INTERNET_no = sum(HHWT))


NATION_INTERNET<- INTERNET_total %>% inner_join(INTERNET_yes, by = c("YEAR","CITY"))
NATION_INTERNET<- NATION_INTERNET %>% inner_join(INTERNET_no,by = c("YEAR","CITY"))


NATION_INTERNET
names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_COMPUTER <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,COMPUTER,CITY)
NATION_full_COMPUTER <- NATION_full_COMPUTER %>% filter(COMPUTER !=00)

table(NATION_full_COMPUTER$COMPUTER)

COMPUTER_total <- NATION_full_COMPUTER %>% 
  group_by(CITY,YEAR) %>% 
  summarise(COMPUTER_total = sum(HHWT))

COMPUTER_yes <- NATION_full_COMPUTER %>% filter (COMPUTER==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(COMPUTER_yes = sum(HHWT))

COMPUTER_no <- NATION_full_COMPUTER %>% filter (COMPUTER==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(COMPUTER_no = sum(HHWT))

NATION_COMPUTER<- COMPUTER_total %>% inner_join(COMPUTER_yes, by = c("YEAR","CITY"))
NATION_COMPUTER<- NATION_COMPUTER %>% inner_join(COMPUTER_no,by = c("YEAR","CITY"))

NATION_COMPUTER
names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_WASHER <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,WASHER,CITY)
NATION_full_WASHER <- NATION_full_WASHER %>% filter(WASHER !=00)

table(NATION_full_WASHER$WASHER)

WASHER_total <- NATION_full_WASHER %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WASHER_total = sum(HHWT))

WASHER_yes <- NATION_full_WASHER %>% filter (WASHER==3|WASHER==4) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WASHER_yes = sum(HHWT))

WASHER_no <- NATION_full_WASHER %>% filter (WASHER==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WASHER_no = sum(HHWT))

NATION_WASHER<- WASHER_total %>% inner_join(WASHER_yes, by = c("YEAR","CITY"))
NATION_WASHER<- NATION_WASHER %>% inner_join(WASHER_no,by = c("YEAR","CITY"))

NATION_WASHER

names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_REFRIG <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,REFRIG,CITY)
NATION_full_REFRIG <- NATION_full_REFRIG %>% filter(REFRIG !=00)

table(NATION_full_REFRIG$REFRIG)

REFRIG_total <- NATION_full_REFRIG %>% 
  group_by(CITY,YEAR) %>% 
  summarise(REFRIG_total = sum(HHWT))

REFRIG_yes <- NATION_full_REFRIG %>% filter (REFRIG==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(REFRIG_yes = sum(HHWT))

REFRIG_no <- NATION_full_REFRIG %>% filter (REFRIG==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(REFRIG_no = sum(HHWT))

NATION_REFRIG<- REFRIG_total %>% inner_join(REFRIG_yes, by = c("YEAR","CITY"))
NATION_REFRIG<- NATION_REFRIG %>% inner_join(REFRIG_no,by = c("YEAR","CITY"))

NATION_REFRIG

names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_VCR <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,VCR,CITY)
NATION_full_VCR <- NATION_full_VCR %>% filter(VCR !=00)

table(NATION_full_VCR$VCR)

VCR_total <- NATION_full_VCR %>% 
  group_by(CITY,YEAR) %>% 
  summarise(VCR_total = sum(HHWT))

VCR_yes <- NATION_full_VCR %>% filter (VCR==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(VCR_yes = sum(HHWT))

VCR_no <- NATION_full_VCR %>% filter (VCR==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(VCR_no = sum(HHWT))

NATION_VCR<- VCR_total %>% inner_join(VCR_yes, by = c("YEAR","CITY"))
NATION_VCR<- NATION_VCR %>% inner_join(VCR_no,by = c("YEAR","CITY"))

NATION_VCR 

names(NATION_full)

VCR

###excluding observations not in the universe for specific variables 

NATION_full_ROOMS <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,ROOMS,CITY)
NATION_full_ROOMS <- NATION_full_ROOMS %>% filter(ROOMS !=00)
NATION_full_ROOMS <- NATION_full_ROOMS %>% filter(ROOMS !=98)
NATION_full_ROOMS <- NATION_full_ROOMS %>% filter(ROOMS !=99)



table(NATION_full_ROOMS$ROOMS)

ROOMS_total <- NATION_full_ROOMS %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ROOMS_total = sum(HHWT))

NATION_full_ROOMS$ROOMS_HHWT <- NATION_full_ROOMS$HHWT*NATION_full_ROOMS$ROOMS

ROOMS_HH_total <- NATION_full_ROOMS %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ROOMS_total = sum(ROOMS_HHWT))

NATION_ROOMS<- ROOMS_total %>% inner_join(ROOMS_HH_total, by = c("YEAR","CITY"))
NATION_ROOMS$avg_ROOMS <- NATION_ROOMS$ROOMS_total.y/NATION_ROOMS$ROOMS_total.x


NATION_ROOMS = subset(NATION_ROOMS, select = -c(3,4) )


names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_BEDROOMS <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,BEDROOMS,CITY)
NATION_full_BEDROOMS <- NATION_full_BEDROOMS %>% filter(BEDROOMS !=00)
NATION_full_BEDROOMS <- NATION_full_BEDROOMS %>% filter(BEDROOMS !=98)
NATION_full_BEDROOMS <- NATION_full_BEDROOMS %>% filter(BEDROOMS !=99)

table(NATION_full_BEDROOMS$BEDROOMS)

BEDROOMS_total <- NATION_full_BEDROOMS %>% 
  group_by(CITY,YEAR) %>% 
  summarise(BEDROOMS_total = sum(HHWT))

NATION_full_BEDROOMS$BEDROOMS_HHWT <- NATION_full_BEDROOMS$HHWT*NATION_full_BEDROOMS$BEDROOMS

BEDROOMS_HH_total <- NATION_full_BEDROOMS %>% 
  group_by(CITY,YEAR) %>% 
  summarise(BEDROOMS_total = sum(BEDROOMS_HHWT))

NATION_BEDROOMS<- BEDROOMS_total %>% inner_join(BEDROOMS_HH_total, by = c("YEAR","CITY"))
NATION_BEDROOMS$avg_BEDROOMS <- NATION_BEDROOMS$BEDROOMS_total.y/NATION_BEDROOMS$BEDROOMS_total.x


NATION_BEDROOMS = subset(NATION_BEDROOMS, select = -c(3,4) )

NATION_BEDROOMS

###excluding observations not in the universe for specific variables 

NATION_full_KITCHEN <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,KITCHEN,CITY)
NATION_full_KITCHEN <- NATION_full_KITCHEN %>% filter(KITCHEN !=00)

table(NATION_full_KITCHEN$KITCHEN)

KITCHEN_total <- NATION_full_KITCHEN %>% 
  group_by(CITY,YEAR) %>% 
  summarise(KITCHEN_total = sum(HHWT))

KITCHEN_yes <- NATION_full_KITCHEN %>% filter (KITCHEN==20) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(KITCHEN_yes = sum(HHWT))

KITCHEN_no <- NATION_full_KITCHEN %>% filter (KITCHEN==10) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(KITCHEN_no = sum(HHWT))

NATION_KITCHEN<- KITCHEN_total %>% inner_join(KITCHEN_yes, by = c("YEAR","CITY"))
NATION_KITCHEN<- NATION_KITCHEN %>% inner_join(KITCHEN_no,by = c("YEAR","CITY"))

NATION_KITCHEN 

names(NATION_full)


###excluding observations not in the universe for specific variables 

NATION_full_TOILET <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,TOILET,CITY)
NATION_full_TOILET <- NATION_full_TOILET %>% filter(TOILET !=00)

table(NATION_full_TOILET$TOILET)

TOILET_total <- NATION_full_TOILET %>% 
  group_by(CITY,YEAR) %>% 
  summarise(TOILET_total = sum(HHWT))

TOILET_yes <- NATION_full_TOILET %>% filter (TOILET==21 |TOILET==22|TOILET==23) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(TOILET_yes = sum(HHWT))

TOILET_no <- NATION_full_TOILET %>% filter (TOILET==10|TOILET==11) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(TOILET_no = sum(HHWT))


TOILET_unkw <- NATION_full_TOILET  %>% filter (TOILET==99) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(TOILET_unkw = sum(HHWT))

NATION_TOILET<- TOILET_total %>% inner_join(TOILET_yes, by = c("YEAR","CITY"))
NATION_TOILET<- NATION_TOILET %>% inner_join(TOILET_no,by = c("YEAR","CITY"))
NATION_TOILET<- NATION_TOILET %>% full_join(TOILET_unkw,by = c("YEAR","CITY"))

NATION_TOILET 

###excluding observations not in the universe for specific variables 

NATION_full_BATH <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,BATH,CITY)
NATION_full_BATH <- NATION_full_BATH %>% filter(BATH !=00)

table(NATION_full_BATH$BATH)

BATH_total <- NATION_full_BATH %>% 
  group_by(CITY,YEAR) %>% 
  summarise(BATH_total = sum(HHWT))

BATH_yes <- NATION_full_BATH %>% filter (BATH==3 |BATH==4) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(BATH_yes = sum(HHWT))

BATH_no <- NATION_full_BATH %>% filter (BATH==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(BATH_no = sum(HHWT))


NATION_BATH<- BATH_total %>% inner_join(BATH_yes, by = c("YEAR","CITY"))
NATION_BATH<- NATION_BATH %>% inner_join(BATH_no,by = c("YEAR","CITY"))


NATION_BATH 

names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_FLOOR <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,FLOOR,CITY)
NATION_full_FLOOR <- NATION_full_FLOOR %>% filter(FLOOR !=00)

table(NATION_full_FLOOR$FLOOR)

FLOOR_total <- NATION_full_FLOOR %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FLOOR_total = sum(HHWT))

FLOOR_yes <- NATION_full_FLOOR %>% filter (FLOOR==210 |FLOOR==236) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FLOOR_yes = sum(HHWT))

FLOOR_no <- NATION_full_FLOOR %>% filter (FLOOR==100) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(FLOOR_no = sum(HHWT))

FLOOR_unkw <- NATION_full_FLOOR  %>% filter (FLOOR==999) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(FLOOR_unkw = sum(HHWT))

NATION_FLOOR<- FLOOR_total %>% inner_join(FLOOR_yes, by = c("YEAR","CITY"))
NATION_FLOOR<- NATION_FLOOR %>% inner_join(FLOOR_no,by = c("YEAR","CITY"))
NATION_FLOOR<- NATION_FLOOR %>% full_join(FLOOR_unkw,by = c("YEAR","CITY"))

NATION_FLOOR 

names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_WALL <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,WALL,CITY)
NATION_full_WALL <- NATION_full_WALL %>% filter(WALL !=00)

table(NATION_full_WALL$WALL)

WALL_total <- NATION_full_WALL %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WALL_total = sum(HHWT))

WALL_brick <- NATION_full_WALL %>% filter (WALL==501 |WALL==508 |WALL==509) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WALL_yes = sum(HHWT))

WALL_no_brick <- NATION_full_WALL %>% filter (WALL==300 |WALL==525 |WALL==526|WALL==527 |WALL==537|WALL==553 |WALL==600) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(WALL_no = sum(HHWT))


NATION_WALL<- WALL_total %>% inner_join(WALL_brick, by = c("YEAR","CITY"))
NATION_WALL<- NATION_WALL %>% inner_join(WALL_no_brick,by = c("YEAR","CITY"))

NATION_WALL 

names(NATION_full)

###excluding observations not in the universe for specific variables 

NATION_full_ROOF <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,ROOF,CITY)
NATION_full_ROOF <- NATION_full_ROOF %>% filter(ROOF !=00)

table(NATION_full_ROOF$ROOF)

ROOF_total <- NATION_full_ROOF %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ROOF_total = sum(HHWT))

ROOF_adequate <- NATION_full_ROOF %>% filter (ROOF==14 |ROOF==15 |ROOF==19|ROOF==20|ROOF==21 |ROOF==24) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ROOF_adequate = sum(HHWT))

ROOF_inadequate <- NATION_full_ROOF %>% filter (ROOF==31 |ROOF==46 |ROOF==72|ROOF==80) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(ROOF_inadequate = sum(HHWT))

ROOF_unkw <- NATION_full_ROOF  %>% filter (ROOF==99) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(ROOF_unkw = sum(HHWT))

NATION_ROOF<- ROOF_total %>% inner_join(ROOF_adequate, by = c("YEAR","CITY"))
NATION_ROOF<- NATION_ROOF %>% inner_join(ROOF_inadequate,by = c("YEAR","CITY"))
NATION_ROOF<- NATION_ROOF %>% full_join(ROOF_unkw,by = c("YEAR","CITY"))

NATION_ROOF

###excluding observations not in the universe for specific variables 

NATION_full_NFAMS <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,HHWT,URBAN,NFAMS,CITY)
NATION_full_NFAMS <- NATION_full_NFAMS %>% filter(NFAMS !=00)

table(NATION_full_NFAMS$NFAMS)

NFAMS_total <- NATION_full_NFAMS %>% 
  group_by(CITY,YEAR) %>% 
  summarise(NFAMS_total = sum(HHWT))

NFAMS_one <- NATION_full_NFAMS %>% filter (NFAMS==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(NFAMS_one = sum(HHWT))

NFAMS_m_one <- NATION_full_NFAMS %>% filter (NFAMS>1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(NFAMS_m_one = sum(HHWT))


NATION_NFAMS<- NFAMS_total %>% inner_join(NFAMS_one, by = c("YEAR","CITY"))
NATION_NFAMS<- NATION_NFAMS %>% inner_join(NFAMS_m_one,by = c("YEAR","CITY"))

NATION_NFAMS 

###excluding observations not in the universe for specific variables 

NATION_full_LIT <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,PERWT,URBAN,LIT,CITY)
NATION_full_LIT <- NATION_full_LIT %>% filter(LIT !=00)

table(NATION_full_LIT$LIT)

LIT_total <- NATION_full_LIT %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LIT_total = sum(PERWT))

LIT_no <- NATION_full_LIT %>% filter (LIT==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LIT_no = sum(PERWT))

LIT_yes <- NATION_full_LIT %>% filter (LIT==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(LIT_yes = sum(PERWT))

LIT_unkw <- NATION_full_LIT  %>% filter (LIT==9) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(ROOF_unkw = sum(PERWT))

NATION_LIT<- LIT_total %>% inner_join(LIT_no, by = c("YEAR","CITY"))
NATION_LIT<- NATION_LIT %>% inner_join(LIT_yes,by = c("YEAR","CITY"))
NATION_LIT<- NATION_LIT %>% full_join(LIT_unkw,by = c("YEAR","CITY"))

NATION_LIT 

###excluding observations not in the universe for specific variables 

NATION_full_EDATTAIN <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,PERWT,URBAN,EDATTAIN,CITY)
NATION_full_EDATTAIN <- NATION_full_EDATTAIN %>% filter(EDATTAIN !=0)

table(NATION_full_EDATTAIN$EDATTAIN)

EDATTAIN_full <- NATION_full_EDATTAIN %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EDATTAIN_total = sum(PERWT))

EDATTAIN_no_primary <- NATION_full_EDATTAIN %>% filter (EDATTAIN==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EDATTAIN_no_primary = sum(PERWT))

EDATTAIN_primary <- NATION_full_EDATTAIN %>% filter (EDATTAIN==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EDATTAIN_primary = sum(PERWT))

EDATTAIN_secondary <- NATION_full_EDATTAIN  %>% filter (EDATTAIN==3) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(EDATTAIN_secondary = sum(PERWT))

EDATTAIN_superior <- NATION_full_EDATTAIN  %>% filter (EDATTAIN==4) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(EDATTAIN_superior = sum(PERWT))

EDATTAIN_unkown <- NATION_full_EDATTAIN  %>% filter (EDATTAIN==5) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(EDATTAIN_unkown = sum(PERWT))

NATION_EDATTAIN<- EDATTAIN_full %>% inner_join(EDATTAIN_no_primary, by = c("YEAR","CITY"))
NATION_EDATTAIN<- NATION_EDATTAIN %>% left_join(EDATTAIN_primary,by = c("YEAR","CITY"))
NATION_EDATTAIN<- NATION_EDATTAIN %>% left_join(EDATTAIN_secondary,by = c("YEAR","CITY"))
NATION_EDATTAIN<- NATION_EDATTAIN %>% left_join(EDATTAIN_superior,by = c("YEAR","CITY"))
NATION_EDATTAIN<- NATION_EDATTAIN %>% left_join(EDATTAIN_unkown,by = c("YEAR","CITY"))

NATION_EDATTAIN

###excluding observations not in the universe for specific variables 

NATION_full_EMPSTAT <- NATION_full %>% select(COUNTRY,YEAR,SAMPLE,SERIAL,PERWT,URBAN,EMPSTAT,CITY)
NATION_full_EMPSTAT <- NATION_full_EMPSTAT %>% filter(EMPSTAT !=0)

table(NATION_full_EMPSTAT$EMPSTAT)

EMPSTAT_full <- NATION_full_EMPSTAT %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EMPSTAT_total = sum(PERWT))

EMPSTAT_employed <- NATION_full_EMPSTAT %>% filter (EMPSTAT==1) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EMPSTAT_employed = sum(PERWT))

EMPSTAT_unemployed <- NATION_full_EMPSTAT %>% filter (EMPSTAT==2) %>% 
  group_by(CITY,YEAR) %>% 
  summarise(EMPSTAT_unemployed = sum(PERWT))

EMPSTAT_inactive <- NATION_full_EMPSTAT  %>% filter (EMPSTAT==3) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(EMPSTAT_inactive = sum(PERWT))

EMPSTAT_unknow <- NATION_full_EMPSTAT  %>% filter (EMPSTAT==9) %>% 
  group_by(CITY,YEAR)  %>% 
  summarise(EMPSTAT_unknow = sum(PERWT))



NATION_EMPSTAT<- EMPSTAT_full %>% inner_join(EMPSTAT_employed,by = c("YEAR","CITY"))
NATION_EMPSTAT<- NATION_EMPSTAT %>% left_join(EMPSTAT_unemployed,by = c("YEAR","CITY"))
NATION_EMPSTAT<- NATION_EMPSTAT %>% left_join(EMPSTAT_inactive,by = c("YEAR","CITY"))
NATION_EMPSTAT<- NATION_EMPSTAT %>% left_join(EMPSTAT_unknow,by = c("YEAR","CITY"))

NATION_EMPSTAT

####merging tables

NATION_merge <- full_join(NATION_landown,NATION_eletric, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_ownership, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_watsup, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_SEWAGE, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_FUELCOOK, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_PHONE, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_CELL, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_INTERNET, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_COMPUTER, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_WASHER, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_REFRIG, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_VCR, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_ROOMS, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_BEDROOMS, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_KITCHEN, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_TOILET, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_BATH, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_FLOOR, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_WALL, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_ROOF, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_NFAMS, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_LIT, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_EDATTAIN, by = c("YEAR","CITY"))
NATION_merge <- full_join(NATION_merge,NATION_EMPSTAT, by = c("YEAR","CITY"))

##Creating the input file for merging
##save(NATION_full,file="NATION_full.Rda")