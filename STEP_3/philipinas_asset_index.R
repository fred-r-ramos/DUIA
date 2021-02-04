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

###########################################calculating for egypt
###########################################import IPUMS

ddi_phi <-read_ipums_ddi("ipumsi_00087.xml")
philipines <- read_ipums_micro(ddi_phi)

philipines <- transform(philipines,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(philipines)

philipines_index <- philipines [,c("SERIAL_YEAR","GEO2_PH2000","GEO2_PH2010")]

###########################################import areas

philipines_index$IPUM2000 <- as.integer(philipines_index$GEO2_PH2000)
philipines_index$IPUM2010 <- as.integer(philipines_index$GEO2_PH2010)

bacolod_00 <- read.csv(file="bacolod_00.csv", header = TRUE)
bacolod_00['CITY']='bacolod'

cebu_00 <- read.csv(file="cebu_00.csv",header = TRUE)
cebu_00['CITY']='cebu'

manila_00 <- read.csv(file="manila_00.csv", header = TRUE)
manila_00['CITY']='manila'

bacolod_10 <- read.csv(file="bacolod_10.csv", header = TRUE)
bacolod_10['CITY']='bacolod'

cebu_10 <- read.csv(file="cebu_10.csv",header = TRUE)
cebu_10['CITY']='cebu'

manila_10 <- read.csv(file="manila_10.csv", header = TRUE)
manila_10['CITY']='manila'

geo_philippines_00 <- rbind(cebu_00,bacolod_00,manila_00)
geo_philippines_10 <- rbind(cebu_10,bacolod_10,manila_10)

philippines_00 <- philipines_index %>% inner_join(geo_philippines_00, by="IPUM2000")
philippines_10 <- philipines_index %>% inner_join(geo_philippines_10, by="IPUM2010")

rm(cebu_10,bacolod_10,manila_00,cebu_00,cebu_10,bacolod_00,manila_10,cebu_10)

names(philippines_00)

philippines_00 <- select(philippines_00, -c(MUNI2000))
philippines_10 <- select(philippines_10, -c(MUNI2010))

##Merging all years into one table
philippines_full <- rbind(philippines_00,philippines_10)
rm(philippines_00,philippines_10)

philipines_index <- philippines_full[,c("SERIAL_YEAR","CITY")]
philipines_index <- philipines_index[!duplicated(philipines_index$SERIAL_YEAR),]

rm(ddi_phi,philippines_full,geo_philippines_00,geo_philippines_10)

philipines <- philipines %>% left_join(philipines_index,by="SERIAL_YEAR")
rm(philipines_index)

names(philipines)
table(philipines$CITY)

philipines$owner_b <- ifelse(philipines$OWNERSHIP ==1,1,0)
philipines$toilet_b <- ifelse(philipines$TOILET ==21,1,0)
philipines$eletric_b <- ifelse(philipines$ELECTRIC ==1,1,0)
philipines$water_b <- ifelse(philipines$WATSUP ==12,1,0)
philipines$fuelc_b <- ifelse(philipines$FUELCOOK ==20|philipines$FUELCOOK ==34,1,0)
philipines$wall_b <- ifelse(philipines$WALL ==502,1,0)
philipines$phone_b <- ifelse(philipines$PHONE ==2,1,0)
philipines$roof_b <- ifelse(philipines$ROOF ==14|philipines$ROOF ==16,1,0)
philipines$tv_b <- ifelse(philipines$TV==20,1,0)
philipines$radio_b <- ifelse(philipines$RADIO ==2,1,0)
philipines$refrig_b <- ifelse(philipines$REFRIG == 2,1,0)
philipines$trash_b<- ifelse(philipines$TRASH==10,1,0)
philipines$washer_b <- ifelse(philipines$WASHER == 2,1,0)
philipines$landown_b <- ifelse(philipines$LANDOWN == 10,0,1)
philipines$vcr_b <- ifelse(philipines$VCR == 2,1 ,0 )

table(philipines$HHWT)

philipines <- philipines [,c("SERIAL_YEAR","YEAR","CITY","HHWT","owner_b","toilet_b","eletric_b","water_b","fuelc_b","wall_b","phone_b","roof_b","tv_b","radio_b","refrig_b","trash_b","washer_b","landown_b","vcr_b")]

table(philipines$CITY)
table(philipines$YEAR)
manila <- philipines %>% filter(CITY=="manila")

manila_HH_b <- expandRows(manila,'HHWT')


names(manila_HH_b)
manila_HH_bs <- manila_HH_b %>% sample_frac(0.3)
manila_HH_bs.pca <- prcomp(manila_HH_bs[,c(4:18)])
summary(manila_HH_bs.pca)                                           
str(manila_HH_bs.pca)   

fviz_eig(manila_HH_bs.pca)

manila_eig <- get_eigenvalue(manila_HH_bs.pca)

manila_HH_PC<-cbind(manila_HH_bs,manila_HH_bs.pca$x[,1:3])

sd(manila_HH_PC$PC1)
manila_inq_index <- manila_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
manila_inq_index$eign <- manila_eig [1,1]
manila_inq_index$I <- manila_inq_index$PC1sd/(manila_inq_index$eign)^0.5
manila_inq_index$city <- "manila"
hist(manila_HH_PC$PC1)

##############################################################################

cebu <- philipines %>% filter(CITY=="cebu")
cebu_HH_b <- expandRows(cebu,'HHWT')

names(cebu_HH_b)
cebu_HH_b.pca <- prcomp(cebu_HH_b[,c(4:18)])
summary(cebu_HH_b.pca)                                           
str(cebu_HH_b.pca)   

fviz_eig(cebu_HH_b.pca)

cebu_eig <- get_eigenvalue(cebu_HH_b.pca)

cebu_HH_PC<-cbind(cebu_HH_b,cebu_HH_b.pca$x[,1:3])

sd(cebu_HH_PC$PC1)
cebu_inq_index <- cebu_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cebu_inq_index$eign <- cebu_eig [1,1]
cebu_inq_index$I <- cebu_inq_index$PC1sd/(cebu_inq_index$eign)^0.5
cebu_inq_index$city <- "cebu"
hist(cebu_HH_PC$PC1)

bacolod <- philipines %>% filter(CITY=="bacolod")
bacolod_HH_b <- expandRows(bacolod,'HHWT')

names(bacolod_HH_b)
bacolod_HH_b.pca <- prcomp(bacolod_HH_b[,c(4:18)])
summary(bacolod_HH_b.pca)                                           
str(bacolod_HH_b.pca)   

fviz_eig(bacolod_HH_b.pca)

bacolod_eig <- get_eigenvalue(bacolod_HH_b.pca)

bacolod_HH_PC<-cbind(bacolod_HH_b,bacolod_HH_b.pca$x[,1:3])

sd(bacolod_HH_PC$PC1)
bacolod_inq_index <- bacolod_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bacolod_inq_index$eign <- bacolod_eig [1,1]
bacolod_inq_index$I <- bacolod_inq_index$PC1sd/(bacolod_inq_index$eign)^0.5
bacolod_inq_index$city <- "bacolod"
hist(bacolod_HH_PC$PC1)
save(bacolod_inq_index,file="bacolod_inq_index.Rda")
save(cebu_inq_index,file="cebu_inq_index.Rda")
save(manila_inq_index,file="manila_inq_index.Rda")

names(philipines)
philipines$HHWTs <- philipines$HHWT/11
philipines_HH_b <- expandRows(philipines,'HHWTs')

names(philipines_HH_b)
philipines_HH_b.pca <- prcomp(philipines_HH_b[,c(5:19)])
summary(philipines_HH_b.pca)                                           
str(philipines_HH_b.pca)   

fviz_eig(philipines_HH_b.pca)

philipines_eig <- get_eigenvalue(philipines_HH_b.pca)

philipines_HH_PC<-cbind(philipines_HH_b,philipines_HH_b.pca$x[,1:3])

sd(philipines_HH_PC$PC1)
philipines_inq_index <- philipines_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
philipines_inq_index$eign <- philipines_eig [1,1]
philipines_inq_index$I <- philipines_inq_index$PC1sd/(philipines_inq_index$eign)^0.5
philipines_inq_index$city <- "philipines"
hist(philipines_HH_PC$PC1)
save(philipines_inq_index,file="philipines_inq_index.Rda")
save(cebu_inq_index,file="cebu_inq_index.Rda")
save(manila_inq_index,file="manila_inq_index.Rda")
