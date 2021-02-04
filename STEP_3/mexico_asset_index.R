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

##########################################################################################for méxico city

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

guadalajara_00 <- read.csv(file="guadalajara_00.csv", header = TRUE)
guadalajara_00['CITY']='guadalajara'

guadalajara_15 <- read.csv(file="guadalajara_15.csv", header = TRUE)
guadalajara_15['CITY']='guadalajara'

mexico_city_00 <- read.csv(file="mexico_00.csv", header = TRUE)
mexico_city_00['CITY']='mexico'

mexico_city_15 <- read.csv(file="mexico_15.csv", header = TRUE)
mexico_city_15['CITY']='mexico'

reynosa_00 <- read.csv(file="reynosa_00.csv", header = TRUE)
reynosa_00['CITY']='reynosa'

reynosa_15 <- read.csv(file="reynosa_15.csv", header = TRUE)
reynosa_15['CITY']='reynosa'

tijuana_00 <- read.csv(file="tijuana_00.csv", header = TRUE)
tijuana_00['CITY']='tijuana'

tijuana_15 <- read.csv(file="tijuana_15.csv", header = TRUE)
tijuana_15['CITY']='tijuana'

geo_mexico_00 <- rbind(guadalajara_00,reynosa_00,tijuana_00,mexico_city_00)
geo_mexico_15 <- rbind(guadalajara_15,reynosa_15,tijuana_15,mexico_city_15)

##Importing new extraction from IPUMS with the geographical variable 

ddi_mex <-read_ipums_ddi("ipumsi_00096.xml")
mex <- read_ipums_micro(ddi_mex)

mexico <- transform(mex,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(mexico)

mexico_index <- mexico [,c("SERIAL_YEAR","GEO2_MX2000","GEO2_MX2015")]

mexico_index$IPUM2000 <- as.integer(mexico_index$GEO2_MX2000)
mexico_index$IPUM2015 <- as.integer(mexico_index$GEO2_MX2015)

##Joining by year

mexico_00 <- mexico_index %>% inner_join(geo_mexico_00, by="IPUM2000")
mexico_15 <- mexico_index %>% inner_join(geo_mexico_15, by="IPUM2015")

names(mexico_00)
names(mexico_15)

rm(mexico_city_00,mexico_city_15,reynosa_00,reynosa_15,tijuana_00,tijuana_15,guadalajara_00,guadalajara_15)

mexico_00 <- select(mexico_00, -c(MUNI2000))
mexico_15 <- select(mexico_15, -c(MUNI2015))

##Merging all years into one table
mexico_full <- rbind(mexico_00,mexico_15)
rm(mexico_00,mexico_15)

mexico_index <- mexico_full[,c("SERIAL_YEAR","CITY")]
mexico_index <- mexico_index[!duplicated(mexico_index$SERIAL_YEAR),]

rm(ddi_mex,mexico_full,geo_mexico_00,geo_mexico_15)

mexico <- mexico %>% left_join(mexico_index,by="SERIAL_YEAR")
rm(mexico_index)

names(mexico)
table(mexico$CITY)

mexico$owner_b <- ifelse(mexico$OWNERSHIP ==1,1,0)
mexico$toilt_b <- ifelse(mexico$TOILET == 21,1,0)
mexico$eletr_b <- ifelse(mexico$ELECTRIC ==1,1,0)
mexico$water_b <- ifelse(mexico$WATSUP ==11,1,0)
mexico$fuel_b <- ifelse(mexico$FUELCOOK == 30 | mexico$FUELCOOK == 20,1,0)
mexico$wall_b <- ifelse(mexico$WALL==501,1,0 )
mexico$phone_b <- ifelse(mexico$PHONE == 2,1,0)
mexico$roof_b <- ifelse(mexico$ROOF == 10,1,0)
mexico$floors_b <- ifelse(mexico$FLOOR !=100,1,0)
mexico$tv_b <- ifelse(mexico$TV == 20 |mexico$TV == 30,1,0)
mexico$sewage_b <- ifelse(mexico$SEWAGE ==11,1,0)
mexico$radio_b <- ifelse(mexico$RADIO == 2,1,0)
mexico$kitchen_b <- ifelse(mexico$KITCHEN == 20,1,0)
mexico$refrig_b <- ifelse(mexico$REFRIG == 2,1,0)
mexico$autos_b <- ifelse(mexico$AUTOS == 7,1,0)
mexico$trash_b <- ifelse(mexico$TRASH ==11|mexico$TRASH ==12,1,0)
mexico$computer_b <- ifelse(mexico$COMPUTER == 2,1,0)
mexico$washer_b <- ifelse(mexico$WASHER == 2,1,0)
mexico$hotwater_b <- ifelse(mexico$HOTWATER == 2,1,0)

names(mexico)

mexicoc <- mexico [,c("YEAR","SAMPLE","SERIAL","HHWT","SERIAL_YEAR","CITY","ROOMS","owner_b","toilt_b","eletr_b","water_b","fuel_b","wall_b","phone_b","roof_b","floors_b","tv_b","sewage_b","radio_b","kitchen_b","refrig_b","autos_b","trash_b","computer_b","washer_b","hotwater_b")]
                                     
table(mexicoc$ROOMS)

mexicoc <- mexicoc %>% filter (ROOMS < 98)
gc()

mexico_city <- mexicoc %>% filter(CITY == "mexico")     
guadalajara <- mexicoc %>% filter(CITY == "guadalajara")
reynosa <- mexicoc %>% filter (CITY == "reynosa")
tijuana <- mexicoc %>% filter (CITY == "tijuana")                                  

rm(mex)

#######aggregating by household

mexico_city$HHWTs <- mexico_city$HHWT/10
mexico_city <- expandRows(mexico_city,'HHWTs')

names(mexico_city)

mexico_city_HH_b <- mexico_city %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,ROOMS,owner_b,toilt_b,eletr_b,water_b,fuel_b,wall_b,phone_b,roof_b,floors_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,autos_b,trash_b,computer_b,washer_b,hotwater_b),.funs = c("max"))

names(mexico_city_HH_b)

mexico_city_HH_n <- mexico_city %>% group_by(SERIAL_YEAR) %>%
  count()

mexico_city <- as.data.table(mexico_city)
mexico_city_HH_r <- mexico_city[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
mexico_city_HH_rn <- full_join(mexico_city_HH_n,mexico_city_HH_r)
mexico_city_HH_rn$room_b <- mexico_city_HH_rn$n/mexico_city_HH_rn$V1
mexico_city_HH_rn <- mexico_city_HH_rn[,c(1,4)]
mexico_city_HH_b<-as.data.frame(mexico_city_HH_b)
mexico_city_HH_b <- full_join(mexico_city_HH_b,mexico_city_HH_rn)

rm(mexico_city_HH_n,mexico_city_HH_r,mexico_city_HH_rn)

names(mexico_city_HH_b)

table(mexico_city_HH_b$YEAR)

mexico_city_HH_bs <- mexico_city_HH_b %>% sample_frac(0.2)
mexico_city_HH_bs.pca <- prcomp(mexico_city_HH_bs[,c(4:24)])
summary(mexico_city_HH_bs.pca)                                           
str(mexico_city_HH_bs.pca)   

fviz_eig(mexico_city_HH_bs.pca)

mexico_city_eig <- get_eigenvalue(mexico_city_HH_bs.pca)

mexico_city_HH_PC<-cbind(mexico_city_HH_bs,mexico_city_HH_bs.pca$x[,1:3])

sd(mexico_city_HH_PC$PC1)

mexico_city_inq_index <- mexico_city_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
mexico_city_inq_index$eign <- mexico_city_eig [1,1]
mexico_city_inq_index$I <- mexico_city_inq_index$PC1sd/(mexico_city_inq_index$eign)^0.5
mexico_city_inq_index$city <- "mexico_city"
hist(mexico_city_HH_PC$PC1)

save(mexico_city_inq_index,file="mexico_city_inq_index.Rda")

#############################################################################

names(guadalajara)

guadalajara_HH_b <- guadalajara %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,ROOMS,owner_b,toilt_b,eletr_b,water_b,fuel_b,wall_b,phone_b,roof_b,floors_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,autos_b,trash_b,computer_b,washer_b,hotwater_b),.funs = c("max"))

names(guadalajara_HH_b)

guadalajara_HH_n <- guadalajara %>% group_by(SERIAL_YEAR) %>%
  count()

gc()
guadalajara <- as.data.table(guadalajara)
guadalajara_HH_r <- guadalajara[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
guadalajara_HH_rn <- full_join(guadalajara_HH_n,guadalajara_HH_r)
guadalajara_HH_rn$room_b <- guadalajara_HH_rn$n/guadalajara_HH_rn$V1
guadalajara_HH_rn <- guadalajara_HH_rn[,c(1,4)]
guadalajara_HH_b<-as.data.frame(guadalajara_HH_b)
guadalajara_HH_b <- full_join(guadalajara_HH_b,guadalajara_HH_rn)

rm(guadalajara_HH_n,guadalajara_HH_r,guadalajara_HH_rn)

names(guadalajara_HH_b)

table(guadalajara_HH_b$YEAR)
guadalajara <- expandRows(guadalajara,'HHWT')

###guadalajara_HH_b <- guadalajara_HH_b %>% sample_frac(0.2)
guadalajara_HH_b.pca <- prcomp(guadalajara_HH_b[,c(5:24)])
summary(guadalajara_HH_b.pca)                                           
str(guadalajara_HH_b.pca)   

fviz_eig(guadalajara_HH_b.pca)

guadalajara_eig <- get_eigenvalue(guadalajara_HH_b.pca)

guadalajara_HH_PC<-cbind(guadalajara_HH_b,guadalajara_HH_b.pca$x[,1:3])

sd(guadalajara_HH_PC$PC1)

guadalajara_inq_index <- guadalajara_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
guadalajara_inq_index$eign <- guadalajara_eig [1,1]
guadalajara_inq_index$I <- guadalajara_inq_index$PC1sd/(guadalajara_inq_index$eign)^0.5
guadalajara_inq_index$city <- "guadalajara"
hist(guadalajara_HH_PC$PC1)

save(guadalajara_inq_index,file="guadalajara_inq_index.Rda")

#############################################################################

names(tijuana)

tijuana_HH_b <- tijuana %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,ROOMS,owner_b,toilt_b,eletr_b,water_b,fuel_b,wall_b,phone_b,roof_b,floors_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,autos_b,trash_b,computer_b,washer_b,hotwater_b),.funs = c("max"))

names(tijuana_HH_b)

tijuana_HH_n <- tijuana %>% group_by(SERIAL_YEAR) %>%
  count()

gc()
tijuana <- as.data.table(tijuana)
tijuana_HH_r <- tijuana[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
tijuana_HH_rn <- full_join(tijuana_HH_n,tijuana_HH_r)
tijuana_HH_rn$room_b <- tijuana_HH_rn$n/tijuana_HH_rn$V1
tijuana_HH_rn <- tijuana_HH_rn[,c(1,4)]
tijuana_HH_b<-as.data.frame(tijuana_HH_b)
tijuana_HH_b <- full_join(tijuana_HH_b,tijuana_HH_rn)

rm(tijuana_HH_n,tijuana_HH_r,tijuana_HH_rn)

names(tijuana_HH_b)

table(tijuana_HH_b$YEAR)
tijuana <- expandRows(tijuana,'HHWT')

###tijuana_HH_b <- tijuana_HH_b %>% sample_frac(0.2)
tijuana_HH_b.pca <- prcomp(tijuana_HH_b[,c(5:24)])
summary(tijuana_HH_b.pca)                                           
str(tijuana_HH_b.pca)   

fviz_eig(tijuana_HH_b.pca)

tijuana_eig <- get_eigenvalue(tijuana_HH_b.pca)

tijuana_HH_PC<-cbind(tijuana_HH_b,tijuana_HH_b.pca$x[,1:3])

sd(tijuana_HH_PC$PC1)

tijuana_inq_index <- tijuana_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
tijuana_inq_index$eign <- tijuana_eig [1,1]
tijuana_inq_index$I <- tijuana_inq_index$PC1sd/(tijuana_inq_index$eign)^0.5
tijuana_inq_index$city <- "tijuana"
hist(tijuana_HH_PC$PC1)

save(tijuana_inq_index,file="tijuana_inq_index.Rda")

#############################################################################

names(reynosa)

reynosa_HH_b <- reynosa %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,ROOMS,owner_b,toilt_b,eletr_b,water_b,fuel_b,wall_b,phone_b,roof_b,floors_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,autos_b,trash_b,computer_b,washer_b,hotwater_b),.funs = c("max"))

names(reynosa_HH_b)

reynosa_HH_n <- reynosa %>% group_by(SERIAL_YEAR) %>%
  count()

gc()
reynosa <- as.data.table(reynosa)
reynosa_HH_r <- reynosa[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
reynosa_HH_rn <- full_join(reynosa_HH_n,reynosa_HH_r)
reynosa_HH_rn$room_b <- reynosa_HH_rn$n/reynosa_HH_rn$V1
reynosa_HH_rn <- reynosa_HH_rn[,c(1,4)]
reynosa_HH_b<-as.data.frame(reynosa_HH_b)
reynosa_HH_b <- full_join(reynosa_HH_b,reynosa_HH_rn)

rm(reynosa_HH_n,reynosa_HH_r,reynosa_HH_rn)

names(reynosa_HH_b)

table(reynosa_HH_b$YEAR)
reynosa <- expandRows(reynosa,'HHWT')

###reynosa_HH_b <- reynosa_HH_b %>% sample_frac(0.2)
reynosa_HH_b.pca <- prcomp(reynosa_HH_b[,c(5:24)])
summary(reynosa_HH_b.pca)                                           
str(reynosa_HH_b.pca)   

fviz_eig(reynosa_HH_b.pca)

reynosa_eig <- get_eigenvalue(reynosa_HH_b.pca)

reynosa_HH_PC<-cbind(reynosa_HH_b,reynosa_HH_b.pca$x[,1:3])

sd(reynosa_HH_PC$PC1)

reynosa_inq_index <- reynosa_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
reynosa_inq_index$eign <- reynosa_eig [1,1]
reynosa_inq_index$I <- reynosa_inq_index$PC1sd/(reynosa_inq_index$eign)^0.5
reynosa_inq_index$city <- "reynosa"
hist(reynosa_HH_PC$PC1)

save(reynosa_inq_index,file="reynosa_inq_index.Rda")

###############################################################################################
mexico_HH_b <- mexicoc %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,HHWT,owner_b,toilt_b,eletr_b,water_b,fuel_b,wall_b,phone_b,roof_b,floors_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,autos_b,trash_b,computer_b,washer_b,hotwater_b),.funs = c("max"))
rm(mexico)
gc()
mexicoc <- as.data.table(mexicoc)
mexicoc_HH_r <- mexicoc[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
mexicoc_HH_rn <- full_join(mexicoc_HH_n,mexicoc_HH_r)
mexicoc_HH_rn$room_b <- mexicoc_HH_rn$n/mexicoc_HH_rn$V1
mexicoc_HH_rn <- mexicoc_HH_rn[,c(1,4)]
mexicoc_HH_b<-as.data.frame(mexicoc_HH_b)
mexicoc_HH_b <- full_join(mexicoc_HH_b,mexicoc_HH_rn)

rm(mexicoc_HH_n,mexicoc_HH_r,mexicoc_HH_rn)

names(mexicoc_HH_b)

rm(mexicoc_HH_n,mexicoc_HH_r,mexicoc_HH_rn)

names(mexicoc_HH_b)

table(mexicoc_HH_b$YEAR)
mexicoc <- expandRows(mexicoc,'HHWT')

###mexicoc_HH_b <- mexicoc_HH_b %>% sample_frac(0.2)
mexicoc_HH_b.pca <- prcomp(mexicoc_HH_b[,c(5:24)])
summary(mexicoc_HH_b.pca)                                           
str(mexicoc_HH_b.pca)   

fviz_eig(mexicoc_HH_b.pca)

mexicoc_eig <- get_eigenvalue(mexicoc_HH_b.pca)

mexicoc_HH_PC<-cbind(mexicoc_HH_b,mexicoc_HH_b.pca$x[,1:3])

sd(mexicoc_HH_PC$PC1)

mexicoc_inq_index <- mexicoc_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
mexicoc_inq_index$eign <- mexicoc_eig [1,1]
mexicoc_inq_index$I <- mexicoc_inq_index$PC1sd/(mexicoc_inq_index$eign)^0.5
mexicoc_inq_index$city <- "mexicoc"
hist(mexicoc_HH_PC$PC1)

save(mexicoc_inq_index,file="mexicoc_inq_index.Rda")


