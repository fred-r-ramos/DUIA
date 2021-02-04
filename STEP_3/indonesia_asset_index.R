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

ddi_ind <-read_ipums_ddi("ipumsi_00097.xml")
indonesia <- read_ipums_micro(ddi_ind)

indonesia <- transform(indonesia,SERIAL_YEAR=paste0(SERIAL,YEAR))
names(indonesia)

indonesia_index <- indonesia [,c("SERIAL_YEAR","GEO2_ID2000","GEO2_ID2010")]

###########################################import areas

indonesia_index$IPUM2000 <- as.integer(indonesia_index$GEO2_ID2000)
indonesia_index$IPUM2010 <- as.integer(indonesia_index$GEO2_ID2010)

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

cirebon_00 <- read.csv(file="cirebon_00.csv",header = TRUE)
cirebon_00['CITY']='cirebon'

cirebon_10 <- read.csv(file="cirebon_10.csv",header = TRUE)
cirebon_10['CITY']='cirebon'

medan_00 <- read.csv(file="medan_00.csv",header = TRUE)
medan_00['CITY']='medan'

medan_10 <- read.csv(file="medan_10.csv",header = TRUE)
medan_10['CITY']='medan'

palembang_00 <- read.csv(file="palembang_00.csv",header = TRUE)
palembang_00['CITY']='palembang'

palembang_10 <- read.csv(file="palembang_10.csv",header = TRUE)
palembang_10['CITY']='palembang'

geo_indonesia_00 <- rbind(medan_00,palembang_00,cirebon_00)
geo_indonesia_10 <- rbind(medan_10,palembang_10,cirebon_10)

indonesia_00 <- indonesia_index %>% inner_join(geo_indonesia_00, by="IPUM2000")
indonesia_10 <- indonesia_index %>% inner_join(geo_indonesia_10, by="IPUM2010")

rm(medan_00,palembang_00,cirebon_00,medan_10,palembang_10,cirebon_10)
gc()

indonesia_00 <- select(indonesia_00, -c(REGY2000))
indonesia_10 <- select(indonesia_10, -c(REGY2010))

indonesia_full <- rbind(indonesia_00,indonesia_10)
rm(indonesia_00,indonesia_10)

indonesia_index <- indonesia_full[,c("SERIAL_YEAR","CITY")]
indonesia_index <- indonesia_index[!duplicated(indonesia_index$SERIAL_YEAR),]

indonesia <- indonesia %>% left_join(indonesia_index,by="SERIAL_YEAR")
rm(indonesia_index,geo_indonesia_00,geo_indonesia_10,indonesia_full,ddi_ind)

indonesia <- indonesia[!duplicated(indonesia$SERIAL_YEAR),]

######coding variables indonesia

indonesia$owner_b <- ifelse(indonesia$OWNERSHIP ==1,1,0)
indonesia$toilet_b <- ifelse(indonesia$TOILET ==21,1,0)
indonesia$eletric_b <- ifelse(indonesia$ELECTRIC ==1,1,0)
indonesia$water_b <- ifelse(indonesia$WATSUP ==11|indonesia$WATSUP ==10,1,0)
indonesia$fuelc_b <- ifelse(indonesia$FUELCOOK ==20|indonesia$FUELCOOK ==30,1,0)
indonesia$phone_b <- ifelse(indonesia$PHONE ==2,1,0)
indonesia$floor_b <- ifelse(indonesia$FLOOR != 100,1,0)
indonesia$sewage_b <- ifelse(indonesia$SEWAGE ==10,1,0)

names(indonesia)
table(indonesia$HHWT)
indonesia <- indonesia %>% filter(HHWT !=0)
indonesia_HH_b <-indonesia[,c(21:30)]
names(indonesia_HH_b)

indonesia_HH_b.pca <- prcomp(indonesia_HH_b[,c(3:10)])
summary(indonesia_HH_b.pca)                                           
str(indonesia_HH_b.pca)   

fviz_eig(indonesia_HH_b.pca)

indonesia_eig <- get_eigenvalue(indonesia_HH_b.pca)

indonesia_HH_PC<-cbind(indonesia_HH_b,indonesia_HH_b.pca$x[,1:3])

sd(indonesia_HH_PC$PC1)

indonesia_inq_index <- indonesia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
indonesia_inq_index$eign <- indonesia_eig [1,1]
indonesia_inq_index$I <- indonesia_inq_index$PC1sd/(indonesia_inq_index$eign)^0.5
indonesia_inq_index$city <- "indonesia"
hist(indonesia_HH_PC$PC1)

