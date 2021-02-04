library(ipumsr)
library(dbplyr)
library(tidyverse)
library(factoextra)
library(data.table)
library(splitstackshape)
library(ggplot2)
library(gridExtra)


getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/inequality_index")

###########################################calculating for Kenya
###########################################import IPUMS

ddi_soa <-read_ipums_ddi("ipumsi_00103.xml")
southafrica <- read_ipums_micro(ddi_soa)

southafrica <- transform(southafrica,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(southafrica)


##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

southafrica_HH_bhs_11 <- read.csv(file="jburg_11.csv", header = TRUE)
jburg_11['CITY']='johannesburg'

jburg_01 <- read.csv(file="jburg_01.csv",header = TRUE)
jburg_01['CITY']='johannesburg'

##Importing new extraction from IPUMS with the geographical variable 

southafrica_index <- southafrica [,c("SERIAL_YEAR","GEO2_ZA2001","GEO2_ZA2011")]

###########################################import areas

southafrica_index$IPUM2001 <- as.integer(southafrica_index$GEO2_ZA2001)
southafrica_index$IPUM2011 <- as.integer(southafrica_index$GEO2_ZA2011)


southafrica_01 <- southafrica_index %>% inner_join(jburg_01, by="IPUM2001")
southafrica_11 <- southafrica_index %>% inner_join(jburg_11, by="IPUM2011")

rm(jburg_01,jburg_11)

names(southafrica_01)
names(southafrica_11)

southafrica_01 <- select(southafrica_01, -c(DIST2001))
southafrica_11 <- select(southafrica_11, -c(DIST2011))
##Merging all years into one table

southafrica_full <- rbind(southafrica_01,southafrica_11)
rm(southafrica_01,southafrica_11)

southafrica_index <- southafrica_full[,c("SERIAL_YEAR","CITY")]
southafrica_index <- southafrica_index[!duplicated(southafrica_index$SERIAL_YEAR),]

rm(ddi_soa,southafrica_full)

southafrica <- southafrica %>% left_join(southafrica_index,by="SERIAL_YEAR")
rm(southafrica_index)

names(southafrica)
table(southafrica$CITY)

southafrica$owner_b <- ifelse(southafrica$OWNERSHIP ==1,1,0)
southafrica$toilet_b <- ifelse(southafrica$TOILET ==21,1,0)
southafrica$eletric_b <- ifelse(southafrica$ELECTRIC ==1,1,0)
southafrica$water_b <- ifelse(southafrica$WATSUP ==11,1,0)
southafrica$fuelc_b <- ifelse(southafrica$FUELCOOK ==20|southafrica$FUELCOOK ==30,1,0)
southafrica$phone_b <- ifelse(southafrica$PHONE ==2,1,0)
southafrica$tv_b <- ifelse(southafrica$TV==20,1,0)
southafrica$radio_b <- ifelse(southafrica$RADIO ==2,1,0)
southafrica$refrig_b <- ifelse(southafrica$REFRIG == 2,1,0)
southafrica$trash_b<- ifelse(southafrica$TRASH==10,1,0)
southafrica$computer_b <- ifelse(southafrica$COMPUTER == 2,1,0)
southafrica$cell_b <- ifelse(southafrica$CELL == 1,1 ,0)
southafrica$internet_b <- ifelse(southafrica$INTERNET ==2 ,1 ,0 )
southafrica$fuelh_b <- ifelse(southafrica$FUELHEAT == 2, 1, 0)

##########################################################calculating number of people per household

southafrica <- southafrica %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(southafrica_)

southafrica_ <- southafrica [,c("YEAR","HHWT","SERIAL_YEAR","CITY","ROOMS","owner_b","toilet_b","eletric_b","water_b","fuelc_b","phone_b","tv_b","radio_b","refrig_b","trash_b","computer_b","cell_b","fuelh_b")]

#######aggregating by household


southafrica_HH_b <- southafrica_ %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars("YEAR","HHWT","CITY","owner_b","toilet_b","eletric_b","water_b","fuelc_b","phone_b","tv_b","radio_b","refrig_b","trash_b","computer_b","cell_b","fuelh_b"),.funs = c("max"))

southafrica_HH_n <- southafrica_ %>% group_by(SERIAL_YEAR) %>%
  count()

southafrica_ <- as.data.table(southafrica_)
southafrica_HH_r <- southafrica_[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
southafrica_HH_rn <- full_join(southafrica_HH_n,southafrica_HH_r)
southafrica_HH_rn$room_b <- southafrica_HH_rn$n/southafrica_HH_rn$V1
southafrica_HH_rn <- southafrica_HH_rn[,c(1,4)]
southafrica_HH_b<-as.data.frame(southafrica_HH_b)
southafrica_HH_b <- full_join(southafrica_HH_b,southafrica_HH_rn)

rm(southafrica_HH_n,southafrica_HH_r,southafrica_HH_rn)

names(southafrica_HH_b)

table(southafrica_HH_b$YEAR)


southafrica_HH_bh <- expandRows(southafrica_HH_b,'HHWT')
names(southafrica_HH_bh)

jburg <- southafrica_HH_bh %>% filter(CITY=="johannesburg")

jburg.pca <- prcomp(jburg[,c(4:17)])
summary(jburg.pca)                                           
str(jburg.pca)   

fviz_eig(jburg.pca)

jburg_eig <- get_eigenvalue(jburg.pca)

jburg_HH_PC<-cbind(jburg,jburg.pca$x[,1:3])

sd(jburg_HH_PC$PC1)

jburg_inq_index <- jburg_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
jburg_inq_index$eign <- jburg_eig [1,1]
jburg_inq_index$I <- jburg_inq_index$PC1sd/(jburg_inq_index$eign)^0.5
jburg_inq_index$city <- "johannesburg"
hist(jburg_HH_PC$PC1)

save(jburg_inq_index,file="jburg_inq_index.Rda")

southafrica_HH_bhs <- southafrica_HH_bh %>% sample_frac(0.3)
southafrica_HH_bhs.pca <- prcomp(southafrica_HH_bhs[,c(4:17)])
summary(southafrica_HH_bhs.pca)                                           
str(southafrica_HH_bhs.pca)   

fviz_eig(southafrica_HH_bhs.pca)

southafrica_HH_bhs_eig <- get_eigenvalue(southafrica_HH_bhs.pca)

southafrica_HH_bhs_HH_PC<-cbind(southafrica_HH_bhs,southafrica_HH_bhs.pca$x[,1:3])

sd(southafrica_HH_bhs_HH_PC$PC1)

southafrica_HH_bhs_inq_index <- southafrica_HH_bhs_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
southafrica_HH_bhs_inq_index$eign <- southafrica_HH_bhs_eig [1,1]
southafrica_HH_bhs_inq_index$I <- southafrica_HH_bhs_inq_index$PC1sd/(southafrica_HH_bhs_inq_index$eign)^0.5
southafrica_HH_bhs_inq_index$city <- "southafrica"
hist(southafrica_HH_bhs_HH_PC$PC1)

save(southafrica_HH_bhs_inq_index,file="southafrica_HH_bhs_inq_index.Rda")
