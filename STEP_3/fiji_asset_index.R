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

###########################################calculating for bolivia
###########################################import IPUMS

ddi_fij <-read_ipums_ddi("ipumsi_00078.xml")
fiji <- read_ipums_micro(ddi_fij)

fiji <- transform(fiji,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(fiji)

fiji_index <- fiji [,c("SERIAL_YEAR","GEO2_FJ2007","GEO2_FJ2014")]

###########################################import areas

fiji_index$IPUM2007 <- as.integer(fiji_index$GEO2_FJ2007)
fiji_index$IPUM2014 <- as.integer(fiji_index$GEO2_FJ2014)

suva_07 <- read.csv(file="suva_07.csv", header = TRUE)
suva_07['CITY']='suva'

suva_14 <- read.csv(file="suva_14.csv", header = TRUE)
suva_14['CITY']='suva'

fiji_07 <- fiji_index %>% inner_join(suva_07, by="IPUM2007")
fiji_14 <- fiji_index %>% inner_join(suva_14, by="IPUM2014")

rm(suva_07,suva_14)

names(fiji_07)
names(fiji_14)

fiji_07 <- select(fiji_07, -c(6))
fiji_14 <- select(fiji_14, -c(6))

fiji_07 <- select(fiji_07, -c(PROV2007))
fiji_14 <- select(fiji_14, -c(PROV2014))

##Merging all years into one table
fiji_full <- rbind(fiji_07,fiji_14)
rm(fiji_07,fiji_14)

fiji_index <- fiji_full[,c("SERIAL_YEAR","CITY")]
fiji_index <- fiji_index[!duplicated(fiji_index$SERIAL_YEAR),]

rm(ddi_fij,fiji_full)

fiji <- fiji %>% left_join(fiji_index,by="SERIAL_YEAR")
rm(fiji_index)

names(fiji)
table(fiji$CITY)

######coding variables fiji

fiji$owner_b <- ifelse(fiji$OWNERSHIP ==1,1,0)
fiji$toilet_b <- ifelse(fiji$TOILET ==20,1,0)
fiji$eletric_b <- ifelse(fiji$ELECTRIC ==1,1,0)
fiji$fuelc_b <- ifelse(fiji$FUELCOOK ==20|fiji$FUELCOOK==34,1,0)
fiji$wall_b <- ifelse(fiji$WALL ==501,1,0)
fiji$phone_b <- ifelse(fiji$PHONE ==2,1,0)
fiji$tv_b <- ifelse(fiji$TV %in% 21:24,1,0)
fiji$radio_b <- ifelse(fiji$RADIO ==2,1,0)
fiji$refrig_b <- ifelse(fiji$REFRIG == 2,1,0)
fiji$auto_b <- ifelse(fiji$AUTOS %in% 1:4,1,0)
fiji$trash_b<- ifelse(fiji$TRASH==14|fiji$TRASH==13,1,0)
fiji$computer_b <- ifelse(fiji$COMPUTER == 2,1,0)
fiji$cell_b <- ifelse(fiji$CELL==1,1,0)
fiji$washer_b <- ifelse(fiji$WASHER == 2,1,0)
fiji$internet_b <- ifelse(fiji$INTERNET==2,1,0)
fiji$landown_b <- ifelse(fiji$LANDOWN ==10,1,0)
fiji$aircon_b <- ifelse(fiji$AIRCON %in% 1:4,1,0)
fiji$hotwater_b <- ifelse(fiji$HOTWATER ==2,1,0)
fiji$vcr_b <- ifelse(fiji$VCR ==2,1,0)

##########################################################calculating number of people per household

fiji <- fiji %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(fiji)

#######aggregating by household

table(fiji$HHWT)

fiji <- fiji %>% filter(fiji$HHWT != 0)

fiji_HH_b <- fiji %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,fuelc_b,wall_b,phone_b,tv_b,radio_b,refrig_b,auto_b,trash_b,computer_b,cell_b,washer_b,internet_b,landown_b,aircon_b,hotwater_b,vcr_b),.funs = c("max"))

names(fiji_HH_b)

fiji_HH_n <- fiji %>% group_by(SERIAL_YEAR) %>%
  count()

fiji <- as.data.table(fiji)
fiji_HH_r <- fiji[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
fiji_HH_rn <- full_join(fiji_HH_n,fiji_HH_r)
fiji_HH_rn$room_b <- fiji_HH_rn$n/fiji_HH_rn$V1
fiji_HH_rn <- fiji_HH_rn[,c(1,4)]
fiji_HH_b<-as.data.frame(fiji_HH_b)
fiji_HH_b <- full_join(fiji_HH_b,fiji_HH_rn)

rm(fiji_HH_n,fiji_HH_r,fiji_HH_rn)

names(fiji_HH_b)

table(fiji_HH_b$YEAR)
###########################################calculating pca

fiji_HH_b.pca <- prcomp(fiji_HH_b[,c(5:24)])
summary(fiji_HH_b.pca)                                           
str(fiji_HH_b.pca)   

fviz_eig(fiji_HH_b.pca)

fiji_eig <- get_eigenvalue(fiji_HH_b.pca)

fiji_HH_PC<-cbind(fiji_HH_b,fiji_HH_b.pca$x[,1:3])

sd(fiji_HH_PC$PC1)

fiji_inq_index <- fiji_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
fiji_inq_index$eign <- fiji_eig [1,1]
fiji_inq_index$I <- fiji_inq_index$PC1sd/(fiji_inq_index$eign)^0.5
fiji_inq_index$city <- "fiji"
hist(fiji_HH_PC$PC1)

suva_HH_b <- fiji_HH_b %>% filter(fiji_HH_b$CITY=="suva")
names(suva_HH_b)

suva_HH_b.pca <- prcomp(suva_HH_b[,c(5:24)])
summary(suva_HH_b.pca)                                           
str(suva_HH_b.pca)   

fviz_eig(suva_HH_b.pca)

suva_eig <- get_eigenvalue(suva_HH_b.pca)

suva_HH_PC<-cbind(suva_HH_b,suva_HH_b.pca$x[,1:3])

sd(suva_HH_PC$PC1)

suva_inq_index <- suva_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
suva_inq_index$eign <- suva_eig [1,1]
suva_inq_index$I <- suva_inq_index$PC1sd/(suva_inq_index$eign)^0.5
suva_inq_index$city <- "suva"
hist(suva_HH_PC$PC1)

fij_suv_plot <- suva_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = fiji_inq_index,color="red")+
  geom_point(data = fiji_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="suva")+
  theme_bw()

fij_suv_plot
save(suva_inq_index,file="suva_inq_index.Rda")
save(fiji_inq_index,file="fiji_inq_index.Rda")
