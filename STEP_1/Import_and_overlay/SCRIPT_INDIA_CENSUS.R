library(dbplyr)
library(tidyverse)
library(reshape2)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/India/CENSUS_INDIA")

#reading table with the relatin of the teshil for the selected cities (considering only urban population according to the census)

Tehsil_2011 <- read.csv("List_tehsil_2011_review.csv")


names(Tehsil_2011)

Tehsil_2011 <- Tehsil_2011 %>% select (-c(7:17))
Tehsil_2011 <- Tehsil_2011 %>% dplyr::rename ("State" = "ï..State")


Tehsil_2001 <- read.csv("List_tehsil_2001_a.csv")

names(Tehsil_2001)

Tehsil_2001 <- Tehsil_2001 %>% dplyr::rename ("State" = "ï..State")
Tehsil_2001 <- Tehsil_2001 %>% dplyr::rename ("District" = "Distt.")
Tehsil_2001 <- Tehsil_2001 %>% dplyr::rename ("Tehsil" = "Tehsil.")

#reading tables for census water

HH6_AndraPadresh_R <- read.csv("HH6AndhraPradesh_R.csv")
HH6Gujarat_R <- read.csv("HH6Gujarat_R.csv")
HH6Karnataka_R <- read.csv("HH6Karnakata_R.csv")
HH6Kerala_R <- read.csv("HH6Kerala_R.csv")
HH6Maharastra_R <- read.csv("HH6Maharastra_R.csv")
HH6Rajasthan_R <- read.csv("HH6Rajasthan_R.csv")
HH6TamilNadu_R <- read.csv("HH6Tamilnadu_R.csv")
HH6UttarPradesh_R <- read.csv("HH6Utarpadresh_R.csv")
HH6WestBengal_R <- read.csv("HH6WestBengal_R.csv")

HH6 <- rbind(HH6_AndraPadresh_R,HH6Gujarat_R,HH6Karnataka_R,HH6Kerala_R,HH6Maharastra_R,HH6Rajasthan_R,HH6TamilNadu_R,HH6UttarPradesh_R,HH6WestBengal_R)

HH6$State <- as.integer(HH6$State)

HH6_selct<- inner_join(HH6,Tehsil_2011,by=c("State","District","Tehsil","Town"))

HH6_selct[is.na(HH6_selct)] <- 0

sapply(HH6_selct, class)

HH6_selct$Total <- as.numeric(gsub(",","",HH6_selct$Total))
HH6_selct$tap_treated <- as.numeric(gsub(",","",HH6_selct$tap_treated))
HH6_selct$tap_untreated <- as.numeric(gsub(",","",HH6_selct$tap_untreated))
HH6_selct$covered_well <- as.numeric(gsub(",","",HH6_selct$covered_well))
HH6_selct$uncovered.well <- as.numeric(gsub(",","",HH6_selct$uncovered.well))
HH6_selct$handpump <- as.numeric(gsub(",","",HH6_selct$handpump))
HH6_selct$borehole <- as.numeric(gsub(",","",HH6_selct$borehole))
HH6_selct$Spring <- as.numeric(gsub(",","",HH6_selct$Spring))
HH6_selct$river <- as.numeric(gsub(",","",HH6_selct$X.river))
HH6_selct$tank <- as.numeric(gsub(",","",HH6_selct$tank))
HH6_selct$other <- as.numeric(gsub(",","",HH6_selct$other))

HH6_selct[is.na(HH6_selct)] <- 0

names(HH6_selct)

HH6_t <- HH6_selct %>% 
  group_by(city_AUE,Local) %>% 
  summarise(Total = sum(Total), tap_treated = sum(tap_treated), tap_untreated = sum(tap_untreated),covered_well = sum(covered_well),uncovered_well = sum(uncovered.well),handpump = sum(handpump),borehole = sum(borehole),river=sum(river), tank = sum(tank),other=sum(other))

H8_AndraPadresh_R <- read.csv("H8AndhraPradesh_R .csv")
H8_Gujarat_R <- read.csv("H8Gujarat_R .csv")
H8_Karnataka_R <- read.csv("H8Karnataka_R.csv")
H8_Kerala_R <- read.csv("H8Kerala_R .csv")
H8_Maharastra_R <- read.csv("H8Maharastra_R.csv")
H8_Rajasthan_R <- read.csv("H8Rajasthan_R.csv")
H8_TamilNadu_R <- read.csv("H8Tamilnadu_R.csv")
H8_UttarPradesh_R <- read.csv("H8UttarPradesh_R.csv")
H8_WestBengal_R <- read.csv("H8WestBengal_R.csv")

H8 <- rbind(H8_AndraPadresh_R,H8_Gujarat_R,H8_Karnataka_R,H8_Kerala_R,H8_Maharastra_R,H8_Rajasthan_R,H8_TamilNadu_R,H8_UttarPradesh_R,H8_WestBengal_R)

names(Tehsil_2001)
names(H8)

sapply(H8, class)
sapply(Tehsil_2001, class)

H8_selct<- inner_join(H8,Tehsil_2001,by=c("State","District","Tehsil"))

sapply(H8_selct, class)

H8_selct$Total <- as.numeric(gsub(",","",H8_selct$Total))
H8_selct$tap <- as.numeric(gsub(",","",H8_selct$tap))
H8_selct$handpump <- as.numeric(gsub(",","",H8_selct$handpump))
H8_selct$covered_well <- as.numeric(gsub(",","",H8_selct$covered_well))
H8_selct$uncovered.well <- as.numeric(gsub(",","",H8_selct$uncovered.well))
H8_selct$tank <- as.numeric(gsub(",","",H8_selct$tank))
H8_selct$river <- as.numeric(gsub(",","",H8_selct$X.river))
H8_selct$Spring <- as.numeric(gsub(",","",H8_selct$Spring))
H8_selct$other <- as.numeric(gsub(",","",H8_selct$other))

H8_t <- H8_selct %>% 
  group_by(city_AUE,Local) %>% 
  summarise(Total = sum(Total),tap = sum(tap),handpump=sum(handpump), covered_well=sum(covered_well),uncovered.well=sum(uncovered.well),tank=sum(tank),river = sum(river),Spring = sum(Spring), other=sum(other))

HH6_selct$tap <- NA
H8_selct$tap_treated<-NA
H8_selct$tap_untreated<-NA
H8_selct$YEAR<-2001
H8_selct$TIME<-"T2"
HH6_selct$YEAR <- 2011
HH6_selct$TIME <- "T3"
H8_selct <- H8_selct %>% dplyr::select(-c(Area.x))
H8_selct <- H8_selct %>% dplyr::select(-c(X.river))
HH6_selct <- HH6_selct %>% dplyr::select(-c(Town))
HH6_selct <- HH6_selct %>% dplyr::select(-c(Area.y))
HH6_selct <- HH6_selct %>% dplyr::select(-c(X.river))
H8_selct <- H8_selct %>% dplyr::rename(CITY = city_AUE)
HH6_selct <- HH6_selct %>% dplyr::rename(CITY = city_AUE)
HH6_selct <- HH6_selct %>% dplyr::rename(Area = Area.x)
H8_selct <- H8_selct %>% dplyr::rename(Area = Area.y)
H8_selct$borehole <- NA

names(HH6_selct)
names(H8_selct)
water_india <- rbind(HH6_selct,H8_selct)
names(water_india)
water_india_t <- water_india %>% 
  group_by(CITY,TIME,YEAR,Local) %>% 
  summarise(Total = sum(Total),tap = sum(tap),tap_untreated=sum(tap_untreated),tap_treated = sum(tap_treated), handpump=sum(handpump), covered_well=sum(covered_well),uncovered.well=sum(uncovered.well),tank=sum(tank),river = sum(river),Spring = sum(Spring), other=sum(other), tap=sum(tap))

#############################################################################################################

##########toilet

H10_AndraPadresh_R <- read.csv("H10AndhraPradesh_R .csv")
H10_Gujarat_R <- read.csv("H10Gujarat_R .csv")
H10_Karnataka_R <- read.csv("H10Karnataka_R.csv")
H10_Kerala_R <- read.csv("H10Kerala_R .csv")
H10_Maharastra_R <- read.csv("H10Maharastra_R.csv")
H10_Rajasthan_R <- read.csv("H10Rajasthan_R.csv")
H10_TamilNadu_R <- read.csv("H10Tamilnadu_R.csv")
H10_UttarPradesh_R <- read.csv("H10UttarPradesh_R.csv")
H10_WestBengal_R <- read.csv("H10WestBengal_R.csv")

H10 <- rbind(H10_AndraPadresh_R,H10_Gujarat_R,H10_Karnataka_R,H10_Kerala_R,H10_Maharastra_R,H10_Rajasthan_R,H10_TamilNadu_R,H10_UttarPradesh_R,H10_WestBengal_R)

names(Tehsil_2001)
names(H10)

sapply(H10, class)
sapply(Tehsil_2001, class)
H10_selct<- inner_join(H10,Tehsil_2001,by=c("State","District","Tehsil"))

table(H10_selct$city_AUE)

sapply(H10_selct, class)

H10_selct$Total <- as.numeric(gsub(",","",H10_selct$Total))
H10_selct$a_bathroom <- as.numeric(gsub(",","",H10_selct$a_bathroom))
H10_selct$a_pit_latrine <- as.numeric(gsub(",","",H10_selct$a_pit_latrine))
H10_selct$a_water_closet <- as.numeric(gsub(",","",H10_selct$a_water_closet))
H10_selct$a_other_latrine <- as.numeric(gsub(",","",H10_selct$a_other_latrine))
H10_selct$b_closed_drainage <- as.numeric(gsub(",","",H10_selct$b_closed_drainage))
H10_selct$b_open_drainage <- as.numeric(gsub(",","",H10_selct$b_open_drainage))
H10_selct$b_no_drainage <- as.numeric(gsub(",","",H10_selct$b_no_drainage))

H10_t <- H10_selct %>% 
  group_by(city_AUE) %>% 
  summarise(Total = sum(Total),a_bathroom = sum(a_bathroom),a_pit_latrine=sum(a_pit_latrine), a_water_closet=sum(a_water_closet),a_other_latrine=sum(a_other_latrine),b_closed_drainage=sum(b_closed_drainage),b_open_drainage = sum(b_open_drainage),b_no_drainage = sum(b_no_drainage))

HH9_AndraPadresh_R <- read.csv("HH9AndhraPradesh_R.csv")
HH9Gujarat_R <- read.csv("HH9Gujarat_R.csv")
HH9Karnataka_R <- read.csv("HH9Karnakata_R.csv")
HH9Kerala_R <- read.csv("HH9Kerala_R.csv")
HH9Maharastra_R <- read.csv("HH9Maharastra_R.csv")
HH9Rajasthan_R <- read.csv("HH9Rajasthan_R.csv")
HH9TamilNadu_R <- read.csv("HH9Tamilnadu_R.csv")
HH9UttarPradesh_R <- read.csv("HH9UttarPradesh_R.csv")
HH9WestBengal_R <- read.csv("HH9WestBengal_R.csv")

HH9 <- rbind(HH9_AndraPadresh_R,HH9Gujarat_R,HH9Karnataka_R,HH9Kerala_R,HH9Maharastra_R,HH9Rajasthan_R,HH9TamilNadu_R,HH9UttarPradesh_R,HH9WestBengal_R)

sapply(HH9, class)
sapply(Tehsil_2011, class)


##HH9$State <- as.factor(HH9$State)

HH9_selct<- inner_join(HH9,Tehsil_2011,by=c("State","District","Town","Tehsil"))
table(HH9_selct$city_AUE)
sapply(HH9_selct, class)

HH9_selct$Total <- as.numeric(gsub(",","",HH9_selct$Total))
HH9_selct$a_bathroom <- as.numeric(gsub(",","",HH9_selct$a_bathroom))
HH9_selct$a_enclosure <- as.numeric(gsub(",","",HH9_selct$a_enclosure))
HH9_selct$a_none <- as.numeric(gsub(",","",HH9_selct$a_none))
HH9_selct$b_closed_drainage <- as.numeric(gsub(",","",HH9_selct$b_closed_drainage))
HH9_selct$b_open_drainage <- as.numeric(gsub(",","",HH9_selct$b_open_drainage))
HH9_selct$b_no_drainage <- as.numeric(gsub(",","",HH9_selct$b_no_drainage))

HH9_t <- HH9_selct %>% 
  group_by(city_AUE) %>% 
  summarise(Total = sum(Total,na.rm = TRUE),a_bathroom = sum(a_bathroom,na.rm = TRUE),a_enclosure=sum(a_enclosure,na.rm = TRUE), a_none=sum(a_none,na.rm = TRUE),b_closed_drainage=sum(b_closed_drainage,na.rm = TRUE),b_open_drainage = sum(b_open_drainage,na.rm = TRUE),b_no_drainage = sum(b_no_drainage,na.rm = TRUE))

HH9_t$YEAR <- 2011
H10_t$YEAR <- 2001
HH9_t$a_other_latrine <- NA
HH9_t$a_pit_latrine <- NA
HH9_t$a_water_closet <- NA
H10_t$a_enclosure <- NA
H10_t$a_none <- NA

names(HH9_t)
names(H10_t)
Bath <- rbind(HH9_t,H10_t)

write.csv(Bath, file="bath_indai.csv")
write.csv(water_india_t, file="water_india_t.csv")
