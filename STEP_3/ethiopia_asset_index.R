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

###########################################calculating for ethiopia
###########################################import IPUMS

ddi_eth <-read_ipums_ddi("ipumsi_00077.xml")
ethiopia <- read_ipums_micro(ddi_eth)

ethiopia <- transform(ethiopia,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(ethiopia)

ethiopia_index <- ethiopia [,c("SERIAL_YEAR","GEO2_ET1994","GEO2_ET2007")]

###########################################import areas

ethiopia_index$IPUM1994 <- as.integer(ethiopia_index$GEO2_ET1994)
ethiopia_index$IPUM2007 <- as.integer(ethiopia_index$GEO2_ET2007)

addis_ababa_94 <- read.csv(file="addis_ababa_94.csv", header = TRUE)
addis_ababa_94['CITY']='addis ababa'

addis_ababa_07 <- read.csv(file="addis_ababa_07.csv", header = TRUE)
addis_ababa_07['CITY']='addis ababa'

ethiopia_94 <- ethiopia_index %>% inner_join(addis_ababa_94, by="IPUM1994")
ethiopia_07 <- ethiopia_index %>% inner_join(addis_ababa_07, by="IPUM2007")

rm(addis_ababa_07,addis_ababa_94)

names(ethiopia_94)
names(ethiopia_07)

ethiopia_94 <- select(ethiopia_94, -c(ZONE1994))
ethiopia_07 <- select(ethiopia_07, -c(ZONE2007))

##Merging all years into one table
ethiopia_full <- rbind(ethiopia_94,ethiopia_07)
rm(ethiopia_94,ethiopia_07)

ethiopia_index <- ethiopia_full[,c("SERIAL_YEAR","CITY")]
ethiopia_index <- ethiopia_index[!duplicated(ethiopia_index$SERIAL_YEAR),]

rm(ddi_eth,ethiopia_full)

ethiopia <- ethiopia %>% left_join(ethiopia_index,by="SERIAL_YEAR")
rm(ethiopia_index)
table(ethiopia$CITY)

######coding variables ethiopia

ethiopia$owner_b <- ifelse(ethiopia$OWNERSHIP ==1,1,0)
ethiopia$toilet_b <- ifelse(ethiopia$TOILET ==21,1,0)
ethiopia$eletric_b <- ifelse(ethiopia$ELECTRIC ==1,1,0)
ethiopia$water_b <- ifelse(ethiopia$WATSUP ==11,1,0)
ethiopia$fuelc_b <- ifelse(ethiopia$FUELCOOK ==20|ethiopia$FUELCOOK ==30,1,0)
ethiopia$wall_b <- ifelse(ethiopia$WALL ==501,1,0)
ethiopia$phone_b <- ifelse(ethiopia$PHONE ==2,1,0)
ethiopia$roof_b <- ifelse(ethiopia$ROOF ==11,1,0)
ethiopia$floor_b <- ifelse(ethiopia$FLOOR !=100,1,0)
ethiopia$tv_b <- ifelse(ethiopia$TV==20,1,0)
ethiopia$radio_b <- ifelse(ethiopia$RADIO ==2,1,0)
ethiopia$kitchen_b <- ifelse(ethiopia$KITCHEN %in% 20:26,1,0)
ethiopia$bath_b <-ifelse(ethiopia$BATH ==3,1,0)

ethiopia <- ethiopia %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(ethiopia)

#######aggregating by household

table(ethiopia$HHWT)


ethiopia_HH_b <- ethiopia %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,tv_b,radio_b,kitchen_b,bath_b),.funs = c("max"))

names(ethiopia_HH_b)

ethiopia_HH_n <- ethiopia %>% group_by(SERIAL_YEAR) %>%
  count()

ethiopia <- as.data.table(ethiopia)
ethiopia_HH_r <- ethiopia[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
ethiopia_HH_rn <- full_join(ethiopia_HH_n,ethiopia_HH_r)
ethiopia_HH_rn$room_b <- ethiopia_HH_rn$n/ethiopia_HH_rn$V1
ethiopia_HH_rn <- ethiopia_HH_rn[,c(1,4)]
ethiopia_HH_b<-as.data.frame(ethiopia_HH_b)
ethiopia_HH_b <- full_join(ethiopia_HH_b,ethiopia_HH_rn)

rm(ethiopia_HH_n,ethiopia_HH_r,ethiopia_HH_rn)

names(ethiopia_HH_b)

table(ethiopia_HH_b$YEAR)
###########################################calculating pca

ethiopia_HH_bexp <- expandRows(ethiopia_HH_b,"HHWT")
names(ethiopia_HH_bexp)

ethiopia_HH_bs <- ethiopia_HH_bexp %>% sample_frac(0.3)
ethiopia_HH_bs.pca <- prcomp(ethiopia_HH_bs[,c(4:17)])
summary(ethiopia_HH_bs.pca)                                           
str(ethiopia_HH_bs.pca)   

fviz_eig(ethiopia_HH_bs.pca)

ethiopia_eig <- get_eigenvalue(ethiopia_HH_bs.pca)

ethiopia_HH_PC<-cbind(ethiopia_HH_bs,ethiopia_HH_bs.pca$x[,1:3])

sd(ethiopia_HH_PC$PC1)

ethiopia_inq_index <- ethiopia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ethiopia_inq_index$eign <- ethiopia_eig [1,1]
ethiopia_inq_index$I <- ethiopia_inq_index$PC1sd/(ethiopia_inq_index$eign)^0.5
ethiopia_inq_index$city <- "ethiopia"
hist(ethiopia_HH_PC$PC1)

###################################for addis_ababa

addis_ababa_HH_b <- ethiopia_HH_bexp %>% filter(ethiopia_HH_bexp$CITY=="addis ababa")
names(addis_ababa_HH_b)

addis_ababa_HH_b.pca <- prcomp(addis_ababa_HH_b[,c(4:17)])
summary(addis_ababa_HH_b.pca)                                           
str(addis_ababa_HH_b.pca)   

fviz_eig(addis_ababa_HH_b.pca)

addis_ababa_eig <- get_eigenvalue(addis_ababa_HH_b.pca)

addis_ababa_HH_PC<-cbind(addis_ababa_HH_b,addis_ababa_HH_b.pca$x[,1:3])

sd(addis_ababa_HH_PC$PC1)

addis_ababa_inq_index <- addis_ababa_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
addis_ababa_inq_index$eign <- addis_ababa_eig [1,1]
addis_ababa_inq_index$I <- addis_ababa_inq_index$PC1sd/(addis_ababa_inq_index$eign)^0.5
addis_ababa_inq_index$city <- "addis_ababa"
hist(addis_ababa_HH_PC$PC1)

eth_add_plot <- addis_ababa_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = ethiopia_inq_index,color="red")+
  geom_point(data = ethiopia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="addis_ababa")+
  theme_bw()

eth_add_plot

save(ethiopia_inq_index,file="ethiopia_inq_index.Rda")
save(addis_ababa_inq_index,file="addis_ababa_inq_index.Rda")
