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

###########################################calculating for ecuador
###########################################import IPUMS

ddi_ecu <-read_ipums_ddi("ipumsi_00074.xml")
ecuador <- read_ipums_micro(ddi_ecu)

ecuador <- transform(ecuador,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(ecuador)

ecuador_index <-ecuador [,c("SERIAL_YEAR","GEO2_EC2001","GEO2_EC2010")]

###########################################import areas

ecuador_index$IPUM2001 <- as.integer(ecuador_index$GEO2_EC2001)
ecuador_index$IPUM2010 <- as.integer(ecuador_index$GEO2_EC2010)

quito_01 <- read.csv(file="quito_01.csv", header = TRUE)
quito_01['CITY']='quito'

quito_10 <- read.csv(file="quito_10.csv", header = TRUE)
quito_10['CITY']='quito'

ecuador_01 <- ecuador_index %>% inner_join(quito_01, by="IPUM2001")
ecuador_10 <- ecuador_index %>% inner_join(quito_10, by="IPUM2010")

rm(quito_01,quito_10)

names(ecuador_01)
names(ecuador_10)

ecuador_01 <- select(ecuador_01, -c(CANT2001))
ecuador_10 <- select(ecuador_10, -c(CANT2010))

##Merging all years into one table
ecuador_full <- rbind(ecuador_01,ecuador_10)
rm(ecuador_01,ecuador_10)

ecuador_index <- ecuador_full[,c("SERIAL_YEAR","CITY")]
ecuador_index <- ecuador_index[!duplicated(ecuador_index$SERIAL_YEAR),]

rm(ddi_ecu,ecuador_full)

ecuador <- ecuador %>% left_join(ecuador_index,by="SERIAL_YEAR")
rm(ecuador_index)

names(ecuador)
table(ecuador$CITY)

ecuador$owner_b <- ifelse(ecuador$OWNERSHIP ==1,1,0)
ecuador$toilt_b <- ifelse(ecuador$TOILET == 21|ecuador$TOILET==20,1,0)
ecuador$eletr_b <- ifelse(ecuador$ELECTRIC ==1,1,0)
ecuador$water_b <- ifelse(ecuador$WATSUP ==11,1,0)
ecuador$phone_b <- ifelse(ecuador$PHONE == 2,1,0)
ecuador$floors_b <- ifelse(ecuador$FLOOR !=100,1,0)
ecuador$sewage_b <- ifelse(ecuador$SEWAGE ==11|ecuador$SEWAGE ==12,1,0)
ecuador$kitchen_b <- ifelse(ecuador$KITCHEN == 20,1,0)
ecuador$bath_b <- ifelse(ecuador$BATH == 3|ecuador$BATH ==4,1,0)

ecuador <- ecuador %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(ecuador)

#######aggregating by household

table(ecuador$HHWT)

ecuador <- ecuador %>% filter(ecuador$HHWT != 0)

ecuador_HH_b <- ecuador %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilt_b,eletr_b,water_b,phone_b,floors_b,sewage_b,kitchen_b,bath_b),.funs = c("max"))

names(ecuador_HH_b)

ecuador_HH_n <- ecuador %>% group_by(SERIAL_YEAR) %>%
  count()

ecuador <- as.data.table(ecuador)
ecuador_HH_r <- ecuador[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
ecuador_HH_rn <- full_join(ecuador_HH_n,ecuador_HH_r)
ecuador_HH_rn$room_b <- ecuador_HH_rn$n/ecuador_HH_rn$V1
ecuador_HH_rn <- ecuador_HH_rn[,c(1,4)]
ecuador_HH_b<-as.data.frame(ecuador_HH_b)
ecuador_HH_b <- full_join(ecuador_HH_b,ecuador_HH_rn)

rm(ecuador_HH_n,ecuador_HH_r,ecuador_HH_rn)

names(ecuador_HH_b)

table(ecuador_HH_b$YEAR)

ecuador_HH_b.pca <- prcomp(ecuador_HH_b[,c(5:14)])
summary(ecuador_HH_b.pca)                                           
str(ecuador_HH_b.pca)   

fviz_eig(ecuador_HH_b.pca)

ecuador_eig <- get_eigenvalue(ecuador_HH_b.pca)

ecuador_HH_PC<-cbind(ecuador_HH_b,ecuador_HH_b.pca$x[,1:3])

sd(ecuador_HH_PC$PC1)

ecuador_inq_index <- ecuador_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ecuador_inq_index$eign <- ecuador_eig [1,1]
ecuador_inq_index$I <- ecuador_inq_index$PC1sd/(ecuador_inq_index$eign)^0.5
ecuador_inq_index$city <- "ecuador"
hist(ecuador_HH_PC$PC1)

save(ecuador_inq_index, file="ecuador_inq_index.Rda")

quito_HH_b <- ecuador_HH_b %>% filter(ecuador_HH_b$CITY=="quito")
names(quito_HH_b)

quito_HH_b.pca <- prcomp(quito_HH_b[,c(5:14)])
summary(quito_HH_b.pca)                                           
str(quito_HH_b.pca)   

fviz_eig(quito_HH_b.pca)

quito_eig <- get_eigenvalue(quito_HH_b.pca)

quito_HH_PC<-cbind(quito_HH_b,quito_HH_b.pca$x[,1:3])

sd(quito_HH_PC$PC1)

quito_inq_index <- quito_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
quito_inq_index$eign <- quito_eig [1,1]
quito_inq_index$I <- quito_inq_index$PC1sd/(quito_inq_index$eign)^0.5
quito_inq_index$city <- "quito"
hist(quito_HH_PC$PC1)

save(quito_inq_index,file="quito_inq_index.Rda")

ecu_qui_plot <- quito_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = ecuador_inq_index,color="red")+
  geom_point(data = ecuador_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="Quito")+
  theme_bw()

ecu_qui_plot
