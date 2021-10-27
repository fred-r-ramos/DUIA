
library(ipumsr)
library(dbplyr)
library(tidyverse)
library(factoextra)
library(data.table)
library(splitstackshape)
library(ggplot2)
library(gridExtra)


getwd()
setwd("C:/")

###########################################calculating for Argentina
###########################################import IPUMS

ddi_arg <-read_ipums_ddi("ipumsi_00070.xml")
argentina <- read_ipums_micro(ddi_arg)

argentina <- transform(argentina,SERIAL_YEAR=paste0(SERIAL,YEAR))

###########################################coding the variables

argentina$owner_b <- ifelse(argentina$OWNERSHIP ==1,1,0)
argentina$toilt_b <- ifelse(argentina$TOILET == 21,1,0)
argentina$water_b <- ifelse(argentina$WATSUP ==11|argentina$WATSUP ==10,1,0)
argentina$fuel_b <- ifelse(argentina$FUELCOOK == 20 | argentina$FUELCOOK == 31|argentina$FUELCOOK == 32,1,0)
argentina$phone_b <- ifelse(argentina$PHONE ==2,1,0)
argentina$roof_b <- ifelse(argentina$ROOF ==14,1,0)
argentina$floors_b <- ifelse(argentina$FLOOR !=100,1,0)
argentina$sewage_b <- ifelse(argentina$SEWAGE ==11|argentina$SEWAGE ==12,1,0)
argentina$refrig_b <- ifelse(argentina$REFRIG == 2,1,0)
argentina$computer_b <- ifelse(argentina$COMPUTER == 2,1,0)
argentina$cell_b <- ifelse(argentina$CELL == 1,1,0)
argentina$landown_b <- ifelse(argentina$LANDOWN == 10,1,0)

##calculating number of people per household

argentina <- argentina %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(argentina)
argentina_HH_b <- argentina %>% group_by(SERIAL_YEAR,YEAR) %>% 
  summarise_at(.vars = vars(owner_b,toilt_b,water_b,fuel_b,phone_b,roof_b,floors_b,sewage_b,refrig_b,computer_b,cell_b,landown_b),.funs = c(hh="max"))

names(argentina_HH_b)

argentina_HH_n <- argentina %>% group_by(SERIAL_YEAR) %>%
  count()

argentina <- as.data.table(argentina)
argentina_HH_r <- argentina[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
argentina_HH_rn <- full_join(argentina_HH_n,argentina_HH_r)
argentina_HH_rn$room_b <- argentina_HH_rn$n/argentina_HH_rn$V1
argentina_HH_rn <- argentina_HH_rn[,c(1,4)]
argentina_HH_b<-as.data.frame(argentina_HH_b)
argentina_HH_b <- full_join(argentina_HH_b,argentina_HH_rn)

names(argentina_HH_b)

table(argentina_HH_b$YEAR)
###########################################calculating pca

argentina_HH_b.pca <- prcomp(argentina_HH_b[,c(3:15)])
summary(argentina_HH_b.pca)                                           
str(argentina_HH_b.pca)   

fviz_eig(argentina_HH_b.pca)

argentina_eig <- get_eigenvalue(argentina_HH_b.pca)

argentina_HH_PC<-cbind(argentina_HH_b,argentina_HH_b.pca$x[,1:3])

sd(argentina_HH_PC$PC1)

argentina_inq_index <- argentina_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
argentina_inq_index$eign <- argentina_eig [1,1]
argentina_inq_index$I <- argentina_inq_index$PC1sd/(argentina_inq_index$eign)^0.5
argentina_inq_index$city <- "Argentina"
hist(argentina_HH_PC$PC1)

###########################################import areas

argentina$IPUM2001 <- as.integer(argentina$GEO2_AR2001)
argentina$IPUM2010 <- as.integer(argentina$GEO2_AR2010)

buenos_aires_91 <- read.csv(file="buenos_aires_91_geo2.csv", header = TRUE)
buenos_aires_91['CITY']='buenos aires'

buenos_aires_01 <- read.csv(file="buenos_aires_01_geo2.csv",header = TRUE)
buenos_aires_01['CITY']='buenos aires'

buenos_aires_10 <- read.csv(file="buenos_aires_10_GEO2.csv", header = TRUE)
buenos_aires_10['CITY']='buenos aires'

cordoba_91 <- read.csv(file="cordoba_91_geo2.csv", header = TRUE)
cordoba_91['CITY']='cordoba'

cordoba_01 <- read.csv(file="cordoba_01_geo2.csv", header = TRUE)
cordoba_01['CITY']='cordoba'

cordoba_10 <- read.csv(file="corodba_10_GEO2.csv",header = TRUE)
cordoba_10['CITY']='cordoba'

geo_argentina_91 <- rbind(buenos_aires_91,cordoba_91)
geo_argentina_01 <- rbind(buenos_aires_01,cordoba_01)
geo_argentina_10 <- rbind(buenos_aires_10,cordoba_10)

##Joining by year

argentina_01 <- argentina %>% inner_join(geo_argentina_01, by="IPUM2001")
argentina_10 <- argentina %>% inner_join(geo_argentina_10, by="IPUM2010")

names(argentina_01)
names(argentina_10)

argentina_01 <- select(argentina_01, -c(DEPT2001))
argentina_10 <- select(argentina_10, -c(DEPT2010))

##Merging all years into one table
argentina_full <- rbind(argentina_01,argentina_10)
names(argentina_full)

argentina_full <- argentina_full %>% inner_join(argentina_HH_rn,by="SERIAL_YEAR")

##########for Buenos Aires

buenos_aires_HH_b <- argentina_full %>% filter(argentina_full$CITY=="buenos aires")
names(buenos_aires_HH_b)
buenos_aires_HH_b <- buenos_aires_HH_b[!duplicated(buenos_aires_HH_b$SERIAL_YEAR),]
buenos_aires_HH_b <- select (buenos_aires_HH_b,-c(8:30))
buenos_aires_HH_b <- select (buenos_aires_HH_b,-c(3:7))
buenos_aires_HH_b <- select (buenos_aires_HH_b,-c(16:23))
buenos_aires_HH_b.pca <- prcomp(buenos_aires_HH_b[,c(4:16)])
summary(buenos_aires_HH_b.pca)                                           
str(buenos_aires_HH_b.pca)   

fviz_eig(buenos_aires_HH_b.pca)

buenos_aires_eig <- get_eigenvalue(buenos_aires_HH_b.pca)

buenos_aires_HH_PC<-cbind(buenos_aires_HH_b,buenos_aires_HH_b.pca$x[,1:3])

sd(buenos_aires_HH_PC$PC1)

buenos_aires_inq_index <- buenos_aires_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
buenos_aires_inq_index$eign <- buenos_aires_eig [1,1]
buenos_aires_inq_index$I <- buenos_aires_inq_index$PC1sd/(buenos_aires_inq_index$eign)^0.5
buenos_aires_inq_index$city <- "Buenos_Aires"
hist(buenos_aires_HH_PC$PC1)

##########for Cordoba

cordoba_HH_b <- argentina_full %>% filter(argentina_full$CITY=="cordoba")
names(cordoba_HH_b)
cordoba_HH_b <- cordoba_HH_b[!duplicated(cordoba_HH_b$SERIAL_YEAR),]
cordoba_HH_b <- select (cordoba_HH_b,-c(8:30))
cordoba_HH_b <- select (cordoba_HH_b,-c(3:7))
cordoba_HH_b <- select (cordoba_HH_b,-c(16:23))
cordoba_HH_b.pca <- prcomp(cordoba_HH_b[,c(4:16)])
summary(cordoba_HH_b.pca)                                           
str(cordoba_HH_b.pca)   

fviz_eig(cordoba_HH_b.pca)

cordoba_eig <- get_eigenvalue(cordoba_HH_b.pca)

cordoba_HH_PC<-cbind(cordoba_HH_b,cordoba_HH_b.pca$x[,1:3])

sd(cordoba_HH_PC$PC1)

cordoba_inq_index <- cordoba_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cordoba_inq_index$eign <- cordoba_eig [1,1]
cordoba_inq_index$I <- cordoba_inq_index$PC1sd/(cordoba_inq_index$eign)^0.5
cordoba_inq_index$city <- "cordoba"
hist(cordoba_HH_PC$PC1)

I_arg_asset_Index <- rbind(cordoba_inq_index,buenos_aires_inq_index)
names(I_arg_asset_Index)
arg_plot <- I_arg_asset_Index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~city) +
  geom_line()+
  geom_point() + theme(plot.title = element_text(size = 12, face = "bold"))+
theme_bw()

##########graph for cities and country

arg_bua_plot <- buenos_aires_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = argentina_inq_index,color="red")+
  geom_point(data = argentina_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()

arg_co_plot<- cordoba_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = argentina_inq_index,color="red")+
  geom_point(data = argentina_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()

grid.arrange(arg_bua_plot, arg_co_plot, ncol=2)

rm(argentina,argentina_01,argentina_10,argentina_eig,argentina_full,argentina_HH_b,argentina_HH_b.pca,argentina_HH_rn,argentina_01,argentina)
rm(geo_argentina_01,geo_argentina_10,geo_argentina_91,ddi_arg,cordoba_01,cordoba_10,cordoba_91,cordoba_eig,cordoba_HH_b,cordoba_HH_b.pca,cordoba_HH_PC)
rm(buenos_aires_01,buenos_aires_10,buenos_aires_91,buenos_aires_eig,buenos_aires_HH_b,buenos_aires_HH_b.pca,buenos_aires_HH_PC)
rm(argentina_full,argentina_HH_n,argentina_HH_PC,argentina_HH_r,argentina_10)

###########################################calculating for Bolivia
###########################################import IPUMS

ddi_bol <-read_ipums_ddi("ipumsi_00069.xml")
bolivia <- read_ipums_micro(ddi_bol)

bolivia <- transform(bolivia,SERIAL_YEAR=paste0(SERIAL,YEAR))

###########################################coding the variables

bolivia$owner_b <- ifelse(bolivia$OWNERSHIP ==1,1,0)
bolivia$toilt_b <- ifelse(bolivia$TOILET == 21|bolivia$TOILET==20,1,0)
bolivia$eletr_b <- ifelse(bolivia$ELECTRIC ==1,1,0)
bolivia$water_b <- ifelse(bolivia$WATSUP ==11,1,0)
bolivia$fuel_b <- ifelse(bolivia$FUELCOOK == 20 | bolivia$FUELCOOK == 30,1,0)
bolivia$wall_b <- ifelse(bolivia$WALL == 501,1,0)
bolivia$roof_b <- ifelse(bolivia$ROOF ==14|bolivia$ROOF ==11|bolivia$ROOF ==12,1,0)
bolivia$floors_b <- ifelse(bolivia$FLOOR !=100,1,0)
bolivia$sewage_b <- ifelse(bolivia$SEWAGE ==11|bolivia$SEWAGE ==12,1,0)
bolivia$kitchen_b <- ifelse(bolivia$KITCHEN == 20,1,0)

##calculating number of people per household

bolivia <- bolivia %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(bolivia)
bolivia_HH_b <- bolivia %>% group_by(SERIAL_YEAR,YEAR) %>% 
  summarise_at(.vars = vars(owner_b,toilt_b,eletr_b,water_b,fuel_b,roof_b,floors_b,sewage_b,kitchen_b),.funs = c(hh="max"))

names(bolivia_HH_b)

bolivia_HH_n <- bolivia %>% group_by(SERIAL_YEAR) %>%
  count()

bolivia <- as.data.table(bolivia)
bolivia_HH_r <- bolivia[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
bolivia_HH_rn <- full_join(bolivia_HH_n,bolivia_HH_r)
bolivia_HH_rn$room_b <- bolivia_HH_rn$n/bolivia_HH_rn$V1
bolivia_HH_rn <- bolivia_HH_rn[,c(1,4)]
bolivia_HH_b<-as.data.frame(bolivia_HH_b)
bolivia_HH_b <- full_join(bolivia_HH_b,bolivia_HH_rn)

names(bolivia_HH_b)

table(bolivia_HH_b$YEAR)
###########################################calculating pca

bolivia_HH_b.pca <- prcomp(bolivia_HH_b[,c(3:12)])
summary(bolivia_HH_b.pca)                                           
str(bolivia_HH_b.pca)   

fviz_eig(bolivia_HH_b.pca)

bolivia_eig <- get_eigenvalue(bolivia_HH_b.pca)

bolivia_HH_PC<-cbind(bolivia_HH_b,bolivia_HH_b.pca$x[,1:3])

sd(bolivia_HH_PC$PC1)

bolivia_inq_index <- bolivia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bolivia_inq_index$eign <- bolivia_eig [1,1]
bolivia_inq_index$I <- bolivia_inq_index$PC1sd/(bolivia_inq_index$eign)^0.5
bolivia_inq_index$city <- "Bolivia"
hist(bolivia_HH_PC$PC1)

###########################################import areas

bolivia$IPUM1992 <- as.integer(bolivia$GEO2_BO1992)
bolivia$IPUM2001 <- as.integer(bolivia$GEO2_BO2001)

cochabamba_92 <- read.csv(file="cochabamba_92.csv", header = TRUE)
cochabamba_92['CITY']='cochabamba'

cochabamba_01 <- read.csv(file="cochabamba_01.csv", header = TRUE)
cochabamba_01['CITY']='cochabamba'

bolivia_92 <- bolivia %>% inner_join(cochabamba_92, by="IPUM1992")
bolivia_01 <- bolivia %>% inner_join(cochabamba_01, by="IPUM2001")

names(bolivia_92)
names(bolivia_01)

bolivia_92 <- select(bolivia_92, -c(PROV1992))
bolivia_01 <- select(bolivia_01, -c(PROV2001))

##Merging all years into one table
bolivia_full <- rbind(bolivia_92,bolivia_01)
names(bolivia_full)
bolivia_full <- transform(bolivia_full,SERIAL_YEAR=paste0(SERIAL,YEAR))
bolivia_full <- bolivia_full %>% inner_join(bolivia_HH_rn,by="SERIAL_YEAR")

##########for Cochabamba

cochabamba_HH_b <- bolivia_full %>% filter(bolivia_full$CITY=="cochabamba")
names(cochabamba_HH_b)
cochabamba_HH_b.pca <- prcomp(cochabamba_HH_b[,c(29:38),47])
summary(cochabamba_HH_b.pca)                                           
str(cochabamba_HH_b.pca)   

fviz_eig(cochabamba_HH_b.pca)

cochabamba_eig <- get_eigenvalue(cochabamba_HH_b.pca)

cochabamba_HH_PC<-cbind(cochabamba_HH_b,cochabamba_HH_b.pca$x[,1:3])

sd(cochabamba_HH_PC$PC1)

cochabamba_inq_index <- cochabamba_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cochabamba_inq_index$eign <- cochabamba_eig [1,1]
cochabamba_inq_index$I <- cochabamba_inq_index$PC1sd/(cochabamba_inq_index$eign)^0.5
cochabamba_inq_index$city <- "cochabamba"
hist(cochabamba_HH_PC$PC1)

##########graph for cities and country

bol_coch_plot <- cochabamba_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = bolivia_inq_index,color="red")+
  geom_point(data = bolivia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()


grid.arrange(arg_bua_plot, arg_co_plot,bol_coch_plot, ncol=3)

###########################################calculating for Argentina
###########################################import IPUMS

ddi_bra <-read_ipums_ddi("ipumsi_00071.xml")
brasil <- read_ipums_micro(ddi_bra)

brasil <- transform(brasil,SERIAL_YEAR=paste0(SERIAL,YEAR))
table(brasil$YEAR)

###########################################coding the variables

brasil$owner_b <- ifelse(brasil$OWNERSHIP ==1,1,0)
brasil$eletric_b <- ifelse(brasil$ELECTRIC ==1,1,0)
brasil$water_b <- ifelse(brasil$WATSUP ==11,1,0)
brasil$phone_b <- ifelse(brasil$PHONE ==2,1,0)
brasil$tv_b <- ifelse(brasil$TV == 21|brasil$TV==29|brasil$TV==20,1,0)
brasil$sewage_b <- ifelse(brasil$SEWAGE ==11|brasil$SEWAGE ==12,1,0)
brasil$radio_b <- ifelse(brasil$RADIO ==2,1,0)
brasil$refrig_b <- ifelse(brasil$REFRIG == 2,1,0)
brasil$auto_b <- ifelse(brasil$AUTOS == 1|brasil$AUTOS==6|brasil$AUTOS==7,1,0)
brasil$bath_b <-ifelse(brasil$BATH == 2,1,0)
brasil$trash_b<- ifelse(brasil$TRASH==11|brasil$TRASH==12,1,0)
brasil$computer_b <- ifelse(brasil$COMPUTER == 2,1,0)
brasil$washer_b <- ifelse(brasil$WASHER == 2,1,0)

##calculating number of people per household

brasil <- brasil %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(brasil)
brasil_HH_b <- brasil %>% group_by(SERIAL_YEAR,YEAR) %>% 
  summarise_at(.vars = vars(HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,computer_b,trash_b,washer_b),.funs = c(hh="max"))

names(brasil_HH_b)

brasil_HH_n <- brasil %>% group_by(SERIAL_YEAR) %>%
  count()

brasil <- as.data.table(brasil)
brasil_HH_r <- brasil[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
brasil_HH_rn <- full_join(brasil_HH_n,brasil_HH_r)
brasil_HH_rn$room_b <- brasil_HH_rn$n/brasil_HH_rn$V1
brasil_HH_rn <- brasil_HH_rn[,c(1,4)]
brasil_HH_b<-as.data.frame(brasil_HH_b)
brasil_HH_b <- full_join(brasil_HH_b,brasil_HH_rn)

names(brasil_HH_b)

table(brasil_HH_b$YEAR)

brasil_HH_b <- full_join(brasil_HH_b,brasil_HH_rn)
names(brasil_HH_b)

brasil_HH_b <- expandRows(brasil_HH_b, 'HHWT_hh')
names(brasil_HH_b)

###################calculating PCA
memory.limit(size=NA) 

brasil_HH_b.pca <- prcomp(brasil_HH_b[,c(3:15)])
rm(brasil)
summary(brasil_HH_b.pca)                                           
str(brasil_HH_b.pca)   

fviz_eig(brasil_HH_b.pca)

brasil_eig <- get_eigenvalue(brasil_HH_b.pca)

brasil_HH_PC<-cbind(brasil_HH_b,brasil_HH_b.pca$x[,1:3])

sd(brasil_HH_PC$PC1)

brasil_inq_index <- brasil_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
brasil_inq_index$eign <- brasil_eig [1,1]
brasil_inq_index$I <- brasil_inq_index$PC1sd/(brasil_inq_index$eign)^0.5
brasil_inq_index$city <- "brasil"
hist(brasil_HH_PC$PC1)
