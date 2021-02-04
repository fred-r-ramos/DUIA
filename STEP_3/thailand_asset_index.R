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

ddi_thai <-read_ipums_ddi("ipumsi_00092.xml")
thailand <- read_ipums_micro(ddi_thai)

thailand_90 <- read.csv(file="thailand_90.csv", header = TRUE)
thailand_90['CITY']='thailand'

thailand_00 <- read.csv(file="thailand_00.csv", header = TRUE)
thailand_00['CITY']='bankok'

thailand <- transform(thailand,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(thailand)
table(thailand$YEAR)
thailand_index <- thailand [,c("SERIAL_YEAR","GEO2_TH1990","GEO2_TH2000")]

thailand_index$IPUM1990 <- as.integer(thailand_index$GEO2_TH1990)
thailand_index$IPUM2000 <- as.integer(thailand_index$GEO2_TH2000)

thailand_90 <- thailand_index %>% inner_join(bankok_90, by="IPUM1990")
thailand_00 <- thailand_index %>% inner_join(bankok_00, by="IPUM2000")

rm(bankok_90,bankok_00)

names(thailand_90)
names(thailand_00)

thailand_90 <- select(thailand_90, -c(DIST1990))
thailand_00 <- select(thailand_00, -c(DIST2000))

##Merging all years into one table
thailand_full <- rbind(thailand_90,thailand_00)
rm(thailand_90,thailand_00)

thailand_index <- thailand_full[,c("SERIAL_YEAR","CITY")]
thailand_index <- thailand_index[!duplicated(thailand_index$SERIAL_YEAR),]

rm(ddi_thai,thailand_full)
thailand <- thailand %>% left_join(thailand_index,by="SERIAL_YEAR")
rm(thailand_index)
table(thailand$YEAR)

names(thailand)
table(thailand$CITY)
thailand$owner_b <- ifelse(thailand$OWNERSHIP ==1,1,0)
thailand$toilet_b <- ifelse(thailand$TOILET ==21,1,0)
thailand$water_b <- ifelse(thailand$WATSUP ==11,1,0)
thailand$fuelc_b <- ifelse(thailand$FUELCOOK ==20|thailand$FUELCOOK ==30,1,0)
thailand$wall_b <- ifelse(thailand$WALL ==501,1,0)
thailand$phone_b <- ifelse(thailand$PHONE ==2,1,0)
thailand$tv_b <- ifelse(thailand$TV %in% 20:43,1,0)
thailand$radio_b <- ifelse(thailand$RADIO ==2,1,0)
thailand$refrig_b <- ifelse(thailand$REFRIG == 2,1,0)
thailand$auto_b <- ifelse(thailand$AUTOS %in% 1:6,1,0)
thailand$washer_b <- ifelse(thailand$WASHER == 2,1,0)
thailand$landown_b <- ifelse(thailand$LANDOWN == 11|thailand$LANDOWN == 12, 1, 0)
thailand$aircon_b <- ifelse(thailand$AIRCON %in% 20:28,1,0)

#######aggregating by household

table(thailand$HHWT)
gc()

bankok <- thailand %>% filter(CITY=="bankok")

####thailand <- thailand [, c("YEAR","CITY","SERIAL_YEAR","HHWT","owner_b","toilet_b","water_b","fuelc_b","wall_b","phone_b","tv_b","radio_b","refrig_b","auto_b","washer_b","landown_b","aircon_b")]

bankok_HH_b <- bankok %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,water_b,fuelc_b,wall_b,phone_b,tv_b,radio_b,refrig_b,auto_b,washer_b,landown_b,aircon_b),.funs = c("max"))

table(bankok_HH_b$YEAR)

names(bankok_HH_b)

bankok_HH_b <- expandRows(bankok_HH_b,'HHWT')


bankok_HH_b.pca <- prcomp(bankok_HH_b[,c(4:16)])
summary(bankok_HH_b.pca)                                           
str(bankok_HH_b.pca)   

fviz_eig(bankok_HH_b.pca)

bankok_eig <- get_eigenvalue(bankok_HH_b.pca)

bankok_HH_PC<-cbind(bankok_HH_b,bankok_HH_b.pca$x[,1:3])

sd(bankok_HH_PC$PC1)

bankok_inq_index <- bankok_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bankok_inq_index$eign <- bankok_eig [1,1]
bankok_inq_index$I <- bankok_inq_index$PC1sd/(bankok_inq_index$eign)^0.5
bankok_inq_index$city <- "bankok"
hist(bankok_HH_PC$PC1)

##############################################################
thailand_HH_b <- thailand %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,water_b,fuelc_b,wall_b,phone_b,tv_b,radio_b,refrig_b,auto_b,washer_b,landown_b,aircon_b),.funs = c("max"))

table(thailand_HH_b$YEAR)

names(thailand_HH_b)

thailand_HH_b <- expandRows(thailand_HH_b,'HHWT')


thailand_HH_b.pca <- prcomp(thailand_HH_b[,c(4:16)])
summary(thailand_HH_b.pca)                                           
str(thailand_HH_b.pca)   

fviz_eig(thailand_HH_b.pca)

thailand_eig <- get_eigenvalue(thailand_HH_b.pca)

thailand_HH_PC<-cbind(thailand_HH_b,thailand_HH_b.pca$x[,1:3])

sd(thailand_HH_PC$PC1)

thailand_inq_index <- thailand_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
thailand_inq_index$eign <- thailand_eig [1,1]
thailand_inq_index$I <- thailand_inq_index$PC1sd/(thailand_inq_index$eign)^0.5
thailand_inq_index$city <- "thailand"
hist(thailand_HH_PC$PC1)


save(bankok_inq_index,file="bankok_inq_index.Rda")
save(thailand_inq_index,file="thailand_inq_index.Rda")
