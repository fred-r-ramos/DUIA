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

###########################################calculating for el_salvador
###########################################import IPUMS

ddi_els <-read_ipums_ddi("ipumsi_00076.xml")
el_salvador<- read_ipums_micro(ddi_els)

el_salvador <- transform(el_salvador,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(el_salvador)

el_salvador_index <- el_salvador [,c("SERIAL_YEAR","GEO2_SV1992","GEO2_SV2007")]

###########################################import areas

el_salvador_index$IPUM1992 <- as.integer(el_salvador_index$GEO2_SV1992)
el_salvador_index$IPUM2007 <- as.integer(el_salvador_index$GEO2_SV2007)

san_salvador_92 <- read.csv(file="san_salvador_92.csv", header = TRUE)
san_salvador_92['CITY']='san salvador'

san_salvador_07 <- read.csv(file="san_salvador_07.csv", header = TRUE)
san_salvador_07['CITY']='san salvador'

el_salvador_92 <- el_salvador_index %>% inner_join(san_salvador_92, by="IPUM1992")
el_salvador_07 <- el_salvador_index %>% inner_join(san_salvador_07, by="IPUM2007")

rm(san_salvador_92,san_salvador_07)

names(el_salvador_92)
names(el_salvador_07)

el_salvador_92 <- select(el_salvador_92, -c(MUNI1992))
el_salvador_07 <- select(el_salvador_07, -c(MUNI2007))

##Merging all years into one table
el_salvador_full <- rbind(el_salvador_92,el_salvador_07)
rm(el_salvador_92,el_salvador_07)

el_salvador_index <- el_salvador_full[,c("SERIAL_YEAR","CITY")]
el_salvador_index <- el_salvador_index[!duplicated(el_salvador_index$SERIAL_YEAR),]

rm(ddi_els,el_salvador_full)

el_salvador <- el_salvador %>% left_join(el_salvador_index,by="SERIAL_YEAR")
rm(el_salvador_index)

names(el_salvador)
table(el_salvador$CITY)

######coding variables el_salvador

el_salvador$owner_b <- ifelse(el_salvador$OWNERSHIP ==1,1,0)
el_salvador$toilet_b <- ifelse(el_salvador$TOILET ==21,1,0)
el_salvador$eletric_b <- ifelse(el_salvador$ELECTRIC ==1,1,0)
el_salvador$water_b <- ifelse(el_salvador$WATSUP ==11,1,0)
el_salvador$fuelc_b <- ifelse(el_salvador$FUELCOOK ==20|el_salvador$FUELCOOK ==33,1,0)
el_salvador$wall_b <- ifelse(el_salvador$WALL ==515,1,0)
el_salvador$phone_b <- ifelse(el_salvador$PHONE ==2,1,0)
el_salvador$roof_b <- ifelse(el_salvador$ROOF == 12|el_salvador$ROOF == 14,1,0)
el_salvador$floor_b <- ifelse(el_salvador$FLOOR !=100,1,0)
el_salvador$tv_b <- ifelse(el_salvador$TV==20,1,0)
el_salvador$sewage_b <- ifelse(el_salvador$SEWAGE ==11|el_salvador$SEWAGE ==12,1,0)
el_salvador$kitchen_b <- ifelse(el_salvador$KITCHEN ==20,1,0)
el_salvador$refrig_b <- ifelse(el_salvador$REFRIG == 2,1,0)
el_salvador$auto_b <- ifelse(el_salvador$AUTOS==7,1,0)
el_salvador$trash_b<- ifelse(el_salvador$TRASH==11|el_salvador$TRASH==12,1,0)
el_salvador$washer_b <- ifelse(el_salvador$WASHER == 2,1,0)

##########################################################calculating number of people per household

el_salvador <- el_salvador %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(el_salvador)

#######aggregating by household

table(el_salvador$HHWT)

el_salvador <- el_salvador %>% filter(el_salvador$HHWT != 0)

el_salvador_HH_b <- el_salvador %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,tv_b,sewage_b,kitchen_b,refrig_b,auto_b,trash_b,washer_b),.funs = c("max"))

names(el_salvador_HH_b)

el_salvador_HH_n <- el_salvador %>% group_by(SERIAL_YEAR) %>%
  count()

el_salvador <- as.data.table(el_salvador)
el_salvador_HH_r <- el_salvador[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
el_salvador_HH_rn <- full_join(el_salvador_HH_n,el_salvador_HH_r)
el_salvador_HH_rn$room_b <- el_salvador_HH_rn$n/el_salvador_HH_rn$V1
el_salvador_HH_rn <- el_salvador_HH_rn[,c(1,4)]
el_salvador_HH_b<-as.data.frame(el_salvador_HH_b)
el_salvador_HH_b <- full_join(el_salvador_HH_b,el_salvador_HH_rn)

rm(el_salvador_HH_n,el_salvador_HH_r,el_salvador_HH_rn)

names(el_salvador_HH_b)

table(el_salvador_HH_b$YEAR)

el_salvador_HH_b.pca <- prcomp(el_salvador_HH_b[,c(5:21)])
summary(el_salvador_HH_b.pca)                                           
str(el_salvador_HH_b.pca)   

fviz_eig(el_salvador_HH_b.pca)

el_salvador_eig <- get_eigenvalue(el_salvador_HH_b.pca)

el_salvador_HH_PC<-cbind(el_salvador_HH_b,el_salvador_HH_b.pca$x[,1:3])

sd(el_salvador_HH_PC$PC1)

el_salvador_inq_index <- el_salvador_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
el_salvador_inq_index$eign <- el_salvador_eig [1,1]
el_salvador_inq_index$I <- el_salvador_inq_index$PC1sd/(el_salvador_inq_index$eign)^0.5
el_salvador_inq_index$city <- "el_salvador"
hist(el_salvador_HH_PC$PC1)

###################################for san_salvador

san_salvador_HH_b <- el_salvador_HH_b %>% filter(el_salvador_HH_b$CITY=="san salvador")
names(san_salvador_HH_b)

san_salvador_HH_b.pca <- prcomp(san_salvador_HH_b[,c(5:21)])
summary(san_salvador_HH_b.pca)                                           
str(san_salvador_HH_b.pca)   

fviz_eig(san_salvador_HH_b.pca)

san_salvador_eig <- get_eigenvalue(san_salvador_HH_b.pca)

san_salvador_HH_PC<-cbind(san_salvador_HH_b,san_salvador_HH_b.pca$x[,1:3])

sd(san_salvador_HH_PC$PC1)

san_salvador_inq_index <- san_salvador_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
san_salvador_inq_index$eign <- san_salvador_eig [1,1]
san_salvador_inq_index$I <- san_salvador_inq_index$PC1sd/(san_salvador_inq_index$eign)^0.5
san_salvador_inq_index$city <- "san_salvador"
hist(san_salvador_HH_PC$PC1)

els_san_plot <- san_salvador_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = el_salvador_inq_index,color="red")+
  geom_point(data = el_salvador_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="San Salvador")+
  theme_bw()

els_san_plot

save(el_salvador_inq_index,file="el_salvador_inq_index.Rda")
save(san_salvador_inq_index,file="san_salvador_inq_index.Rda")
