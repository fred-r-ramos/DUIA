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

###########################################calculating for Argentina
###########################################import IPUMS

ddi_arg <-read_ipums_ddi("ipumsi_00070.xml")
argentina <- read_ipums_micro(ddi_arg)

argentina <- transform(argentina,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(argentina)

argentina_index <- argentina [,c("SERIAL_YEAR","GEO2_AR2001","GEO2_AR2010")]
                           
###########################################import areas

argentina_index$IPUM2001 <- as.integer(argentina_index$GEO2_AR2001)
argentina_index$IPUM2010 <- as.integer(argentina_index$GEO2_AR2010)

buenos_aires_01 <- read.csv(file="buenos_aires_01_geo2.csv",header = TRUE)
buenos_aires_01['CITY']='buenos aires'

buenos_aires_10 <- read.csv(file="buenos_aires_10_GEO2.csv", header = TRUE)
buenos_aires_10['CITY']='buenos aires'

cordoba_01 <- read.csv(file="cordoba_01_geo2.csv", header = TRUE)
cordoba_01['CITY']='cordoba'

cordoba_10 <- read.csv(file="corodba_10_GEO2.csv",header = TRUE)
cordoba_10['CITY']='cordoba'

geo_argentina_01 <- rbind(buenos_aires_01,cordoba_01)
geo_argentina_10 <- rbind(buenos_aires_10,cordoba_10)

##Joining by year

argentina_01 <- argentina_index %>% inner_join(geo_argentina_01, by="IPUM2001")
argentina_10 <- argentina_index %>% inner_join(geo_argentina_10, by="IPUM2010")

names(argentina_01)
names(argentina_10)

rm(buenos_aires_01,buenos_aires_10,cordoba_01,cordoba_10)

argentina_01 <- select(argentina_01, -c(DEPT2001))
argentina_10 <- select(argentina_10, -c(DEPT2010))

##Merging all years into one table
argentina_full <- rbind(argentina_01,argentina_10)
rm(argentina_01,argentina_10)

argentina_index <- argentina_full[,c("SERIAL_YEAR","CITY")]
argentina_index <- argentina_index[!duplicated(argentina_index$SERIAL_YEAR),]

rm(ddi_arg,argentina_full,geo_argentina_01,geo_argentina_10)

argentina <- argentina %>% left_join(argentina_index,by="SERIAL_YEAR")
rm(argentina_index)

names(argentina)
table(argentina$CITY)

##########for Buenos Aires

argentina$owner_b <- ifelse(argentina$OWNERSHIP ==1,1,0)
argentina$toilet_b <- ifelse(argentina$TOILET ==21,1,0)
argentina$water_b <- ifelse(argentina$WATSUP ==11|argentina$WATSUP ==10,1,0)
argentina$fuelc_b <- ifelse(argentina$FUELCOOK ==20|argentina$FUELCOOK ==32|argentina$FUELCOOK ==31,1,0)
argentina$phone_b <- ifelse(argentina$PHONE ==2,1,0)
argentina$roof_b <- ifelse(argentina$ROOF ==14,1,0)
argentina$floors_b <- ifelse(argentina$FLOOR !=100,1,0)
argentina$sewage_b <- ifelse(argentina$SEWAGE ==11|argentina$SEWAGE ==12,1,0)
argentina$refrig_b <- ifelse(argentina$REFRIG == 2,1,0)
argentina$computer_b <- ifelse(argentina$COMPUTER == 2,1,0)
argentina$cell_b <- ifelse(argentina$CELL == 1,1,0)
argentina$landown_b <- ifelse(argentina$LANDOWN == 10,1,0)

##########################################################calculating number of people per household

argentina <- argentina %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
table(argentina$YEAR)
names(argentina)

#######aggregating by household

table(argentina$HHWT)

argentina_HH_b <- argentina %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,water_b,fuelc_b,phone_b,roof_b,floors_b,sewage_b,refrig_b,computer_b,cell_b,landown_b),.funs = c("max"))

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

rm(argentina_HH_n,argentina_HH_r,argentina_HH_rn)

names(argentina_HH_b)

table(argentina_HH_b$YEAR)
###########################################calculating pca

argentina_HH_b.pca <- prcomp(argentina_HH_b[,c(5:17)])
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

##########for Buenos Aires

buenos_aires_HH_b <- argentina_HH_b %>% filter(argentina_HH_b$CITY=="buenos aires")
names(buenos_aires_HH_b)

buenos_aires_HH_b.pca <- prcomp(buenos_aires_HH_b[,c(5:17)])
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

cordoba_HH_b <- argentina_HH_b %>% filter(argentina_HH_b$CITY=="cordoba")
names(cordoba_HH_b)

cordoba_HH_b.pca <- prcomp(cordoba_HH_b[,c(5:17)])
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


save(buenos_aires_inq_index,file="buenos_aires_inq_index.Rda")
save(cordoba_inq_index,file="cordoba_inq_index.Rda")
save(argentina_inq_index,file="argentina_inq_index.Rda")


arg_plot <- I_arg_asset_Index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~city) +
  geom_line()+
  geom_point() + theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()

arg_plot

arg_bua_plot <- buenos_aires_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = argentina_inq_index,color="red")+
  geom_point(data = argentina_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  ylim(0.9,1.2)+labs(title="Buenos Aires")+
  theme_bw()

arg_co_plot<- cordoba_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = argentina_inq_index,color="red")+
  geom_point(data = argentina_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  ylim(0.9,1.2)+labs(title="Cordoba")+
  theme_bw()

grid.arrange(arg_bua_plot, arg_co_plot, ncol=2)

rm(argentina,argentina_eig,argentina_HH_b,argentina_HH_b.pca,argentina_HH_PC)
rm(cordoba_eig,cordoba_HH_b,cordoba_HH_b.pca,cordoba_HH_PC)
rm(buenos_aires_eig,buenos_aires_HH_b,buenos_aires_HH_b.pca,buenos_aires_HH_PC)
gc()

