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

ddi_egy <-read_ipums_ddi("ipumsi_00075.xml")
egypt <- read_ipums_micro(ddi_egy)

egypt <- transform(egypt,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(egypt)

egypt_index <- egypt [,c("SERIAL_YEAR","GEO2_EG1996","GEO2_EG2006")]

###########################################import areas

egypt_index$IPUM1996 <- as.integer(egypt_index$GEO2_EG1996)
egypt_index$IPUM2006 <- as.integer(egypt_index$GEO2_EG2006)

alexandria_96 <- read.csv(file="alexandria_96.csv",header = TRUE)
alexandria_96['CITY']='alexandria'

alexandria_06 <- read.csv(file="alexandria_06.csv",header = TRUE)
alexandria_06['CITY']='alexandria'

cairo_96 <- read.csv(file="cairo_96.csv",header = TRUE)
cairo_96['CITY']='cairo'

cairo_06 <- read.csv(file="cairo_06.csv",header = TRUE)
cairo_06['CITY']='cairo'

geo_egypt_96 <- rbind(alexandria_96,cairo_96)
geo_egypt_06 <- rbind(alexandria_06,cairo_06)

##Joining by year

egypt_96 <- egypt_index %>% inner_join(geo_egypt_96, by="IPUM1996")
egypt_06 <- egypt_index %>% inner_join(geo_egypt_06, by="IPUM2006")

names(egypt_96)
names(egypt_06)

rm(alexandria_96,alexandria_06,cairo_96,cairo_06)

egypt_96 <- select(egypt_96, -c(DIST1996))
egypt_06 <- select(egypt_06, -c(DIST2006))

##Merging all years into one table
egypt_full <- rbind(egypt_96,egypt_06)
rm(egypt_96,egypt_06)

egypt_index <- egypt_full[,c("SERIAL_YEAR","CITY")]
egypt_index <- egypt_index[!duplicated(egypt_index$SERIAL_YEAR),]

rm(ddi_egy,egypt_full,geo_egypt_96,geo_egypt_06)

egypt <- egypt %>% left_join(egypt_index,by="SERIAL_YEAR")
rm(egypt_index)

names(egypt)
table(egypt$CITY)

egypt$owner_b <- ifelse(egypt$OWNERSHIP ==1,1,0)
egypt$toilt_b <- ifelse(egypt$TOILET == 20,1,0)
egypt$eletr_b <- ifelse(egypt$ELECTRIC ==1,1,0)
egypt$water_b <- ifelse(egypt$WATSUP ==11,1,0)
egypt$fuel_b <- ifelse(egypt$FUELCOOK==20|egypt$FUELCOOK==30|egypt$FUELCOOK==33,1,0)
egypt$tv_b <- ifelse(egypt$TV %in% 20:40,1,0)
egypt$sewage_b <- ifelse(egypt$SEWAGE==10|egypt$SEWAGE==11|egypt$SEWAGE==12,1,0)
egypt$refrig_b <- ifelse(egypt$REFRIG == 2,1,0)
egypt$auto_b <- ifelse(egypt$AUTOS!=0|egypt$AUTOS!=8|egypt$AUTOS!=9,1,0)
egypt$bath_b <- ifelse(egypt$BATH == 3,1,0)
egypt$comp_b <- ifelse(egypt$COMPUTER==2,1,0)
egypt$washer_b <- ifelse(egypt$WASHER!=0|egypt$WASHER!=1,1,0)
egypt$kitchen_b <- ifelse(egypt$KITCHEN == 24,1,0)
egypt$bath_b <- ifelse(egypt$BATH == 3,1,0)
egypt$aircon_b <- ifelse(egypt$AIRCON %in% 20:25,1,0)
egypt$vcr_b <- ifelse(egypt$VCR == 2,1,0)
egypt$freezer_b <- ifelse(egypt$FREEZER == 2,1,0)

egypt <- egypt %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(egypt)

#######aggregating by household

table(egypt$HHWT)

egypt_HH_b <- egypt %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilt_b,eletr_b,water_b,fuel_b,tv_b,sewage_b,refrig_b,auto_b,bath_b,comp_b,washer_b,kitchen_b,aircon_b,vcr_b,freezer_b),.funs = c("max"))

names(egypt_HH_b)

egypt_HH_n <- egypt %>% group_by(SERIAL_YEAR) %>%
  count()

egypt <- as.data.table(egypt)
egypt_HH_r <- egypt[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
egypt_HH_rn <- full_join(egypt_HH_n,egypt_HH_r)
egypt_HH_rn$room_b <- egypt_HH_rn$n/egypt_HH_rn$V1
egypt_HH_rn <- egypt_HH_rn[,c(1,4)]
egypt_HH_b<-as.data.frame(egypt_HH_b)
egypt_HH_b <- full_join(egypt_HH_b,egypt_HH_rn)

rm(egypt_HH_n,egypt_HH_r,egypt_HH_rn)

names(egypt_HH_b)

egypt_HH_b <- expandRows(egypt_HH_b, 'HHWT')

egypt_HH_bs <- egypt_HH_b %>%  sample_frac(0.1)

egypt_HH_bs.pca <- prcomp(egypt_HH_bs[,c(4:20)])
summary(egypt_HH_bs.pca)                                           
str(egypt_HH_bs.pca)   

fviz_eig(egypt_HH_bs.pca)

egypt_eig <- get_eigenvalue(egypt_HH_bs.pca)

egypt_HH_PC<-cbind(egypt_HH_bs,egypt_HH_bs.pca$x[,1:3])

sd(egypt_HH_PC$PC1)
egypt_inq_index <- egypt_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
egypt_inq_index$eign <- egypt_eig [1,1]
egypt_inq_index$I <- egypt_inq_index$PC1sd/(egypt_inq_index$eign)^0.5
egypt_inq_index$city <- "egypt"
hist(egypt_HH_PC$PC1)

cairo_HH_b <- egypt_HH_b %>% filter(egypt_HH_b$CITY=="cairo")
names(cairo_HH_b)
cairo_HH_bs <-cairo_HH_b %>% sample_frac(0.5)

cairo_HH_bs.pca <- prcomp(cairo_HH_bs[,c(4:20)])
summary(cairo_HH_bs.pca)                                           
str(cairo_HH_bs.pca)   

fviz_eig(cairo_HH_bs.pca)

cairo_eig <- get_eigenvalue(cairo_HH_bs.pca)

cairo_HH_PC<-cbind(cairo_HH_bs,cairo_HH_bs.pca$x[,1:3])

sd(cairo_HH_PC$PC1)

cairo_inq_index <- cairo_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cairo_inq_index$eign <- cairo_eig [1,1]
cairo_inq_index$I <- cairo_inq_index$PC1sd/(cairo_inq_index$eign)^0.5
cairo_inq_index$city <- "cairo"
hist(cairo_HH_PC$PC1)

alexandria_HH_b <- egypt_HH_b %>% filter(egypt_HH_b$CITY=="alexandria")
names(alexandria_HH_b)

alexandria_HH_b.pca <- prcomp(alexandria_HH_b[,c(4:20)])
summary(alexandria_HH_b.pca)                                           
str(alexandria_HH_b.pca)   

fviz_eig(alexandria_HH_b.pca)

alexandria_eig <- get_eigenvalue(alexandria_HH_b.pca)

alexandria_HH_PC<-cbind(alexandria_HH_b,alexandria_HH_b.pca$x[,1:3])

sd(alexandria_HH_PC$PC1)

alexandria_inq_index <- alexandria_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
alexandria_inq_index$eign <- alexandria_eig [1,1]
alexandria_inq_index$I <- alexandria_inq_index$PC1sd/(alexandria_inq_index$eign)^0.5
alexandria_inq_index$city <- "alexandria"
hist(alexandria_HH_PC$PC1)

save(alexandria_inq_index,file="alexandria_inq_index.Rda")
save(egypt_inq_index,file="egypt_inq_index.Rda")
save(cairo_inq_index,file="cairo_inq_index.Rda")
