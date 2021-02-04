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

ddi_zam <-read_ipums_ddi("ipumsi_00095.xml")
zambia <- read_ipums_micro(ddi_zam)

ndola_00 <- read.csv(file="ndola_00.csv", header = TRUE)
ndola_00['CITY']='ndola'

ndola_10 <- read.csv(file="ndola_10.csv", header = TRUE)
ndola_10['CITY']='ndola'

zambia <- transform(zambia,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(zambia)

zambia_index <- zambia [,c("SERIAL_YEAR","GEO2_ZM2000","GEO2_ZM2010")]

zambia_index$IPUM2000 <- as.integer(zambia_index$GEO2_ZM2000)
zambia_index$IPUM2010 <- as.integer(zambia_index$GEO2_ZM2010)

zambia_00 <- zambia_index %>% inner_join(ndola_00, by="IPUM2000")
zambia_10 <- zambia_index %>% inner_join(ndola_10, by="IPUM2010")

rm(ndola_00,ndola_10)

names(zambia_00)
names(zambia_10)

zambia_00 <- select(zambia_00, -c(DIST2000))
zambia_10 <- select(zambia_10, -c(DIST2010))

##Merging all years into one table
zambia_full <- rbind(zambia_00,zambia_10)
rm(zambia_00,zambia_10)

zambia_index <- zambia_full[,c("SERIAL_YEAR","CITY")]
zambia_index <- zambia_index[!duplicated(zambia_index$SERIAL_YEAR),]

rm(ddi_zam,zambia_full)

zambia <- zambia %>% left_join(zambia_index,by="SERIAL_YEAR")
rm(zambia_index)

names(zambia)
table(zambia$CITY)
table(zambia$YEAR)

zambia$owner_b <- ifelse(zambia$OWNERSHIP ==1,1,0)
zambia$toilet_b <- ifelse(zambia$TOILET ==21,1,0)
zambia$eletric_b <- ifelse(zambia$ELECTRIC ==1,1,0)
zambia$water_b <- ifelse(zambia$WATSUP ==11,1,0)
zambia$fuelc_b <- ifelse(zambia$FUELCOOK ==20|zambia$FUELCOOK ==30,1,0)
zambia$wall_b <- ifelse(zambia$WALL ==510|zambia$WALL ==501|zambia$WALL ==517,1,0)
zambia$phone_b <- ifelse(zambia$PHONE ==2,1,0)
zambia$roof_b <- ifelse(zambia$ROOF ==11|zambia$ROOF ==14 , 1,0 )
zambia$floor_b <- ifelse(zambia$FLOOR != 100,1,0)
zambia$tv_b <- ifelse(zambia$TV==20,1,0)
zambia$sewage_b <- ifelse(zambia$SEWAGE ==10,1,0)
zambia$radio_b <- ifelse(zambia$RADIO ==2,1,0)
zambia$kitchen_b <- ifelse(zambia$KITCHEN==20 ,1 ,0 )
zambia$refrig_b <- ifelse(zambia$REFRIG == 2,1,0)
zambia$auto_b <- ifelse(zambia$AUTOS==7,1,0)
zambia$trash_b<- ifelse(zambia$TRASH==10,1,0)
zambia$fuelh_b <- ifelse(zambia$FUELHEAT == 2,1 , 0)

names(zambia)

table(zambia$HHWT)
zambia <- zambia %>% filter(HHWT != 0)
zambia <- zambia %>% filter(ROOMS>0)

zambia_HH_b <- zambia %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,tv_b,sewage_b,radio_b,kitchen_b,refrig_b,auto_b,trash_b,fuelh_b),.funs = c("max"))

names(zambia_HH_b)

zambia_HH_n <- zambia %>% group_by(SERIAL_YEAR) %>%
  count()

zambia <- as.data.table(zambia)
zambia_HH_r <- zambia[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
zambia_HH_rn <- full_join(zambia_HH_n,zambia_HH_r)
zambia_HH_rn$room_b <- zambia_HH_rn$n/zambia_HH_rn$V1
zambia_HH_rn <- zambia_HH_rn[,c(1,4)]
zambia_HH_b<-as.data.frame(zambia_HH_b)
zambia_HH_b <- full_join(zambia_HH_b,zambia_HH_rn)

rm(zambia_HH_rn,zambia_HH_r,zambia_HH_n)

names(zambia_HH_b)

zambia_HH_b.pca <- prcomp(zambia_HH_b[,c(5:22)])
summary(zambia_HH_b.pca)                                           
str(zambia_HH_b.pca)   

fviz_eig(zambia_HH_b.pca)

zambia_eig <- get_eigenvalue(zambia_HH_b.pca)

zambia_HH_PC<-cbind(zambia_HH_b,zambia_HH_b.pca$x[,1:3])
names(zambia_HH_PC)
sd(zambia_HH_PC$PC1)
zambia_inq_index <- zambia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
zambia_inq_index$eign <- zambia_eig [1,1]
zambia_inq_index$I <- zambia_inq_index$PC1sd/(zambia_inq_index$eign)^0.5
zambia_inq_index$city <- "zambia"
hist(zambia_HH_PC$PC1)

ndola_HH_b <- zambia_HH_b %>% filter(zambia_HH_b$CITY=="ndola")
names(ndola_HH_b)

ndola_HH_b.pca <- prcomp(ndola_HH_b[,c(5:22)])
summary(ndola_HH_b.pca)                                           
str(ndola_HH_b.pca)   

fviz_eig(ndola_HH_b.pca)

ndola_eig <- get_eigenvalue(ndola_HH_b.pca)

ndola_HH_PC<-cbind(ndola_HH_b,ndola_HH_b.pca$x[,1:3])

sd(ndola_HH_PC$PC1)

ndola_inq_index <- ndola_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ndola_inq_index$eign <- ndola_eig [1,1]
ndola_inq_index$I <- ndola_inq_index$PC1sd/(ndola_inq_index$eign)^0.5
ndola_inq_index$city <- "ndola"
hist(ndola_HH_PC$PC1)

zam_ndo_plot <- ndola_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = zambia_inq_index,color="red")+
  geom_point(data = zambia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="ndola - zambia")+
  theme_bw()

zam_ndo_plot

save(zambia_inq_index,file="zambia_inq_index.Rda")
save(ndola_inq_index,file="ndola_inq_index.Rda")
