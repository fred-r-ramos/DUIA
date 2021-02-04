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

###########################################calculating for Kenya
###########################################import IPUMS

ddi_rwa <-read_ipums_ddi("ipumsi_00088.xml")
rwanda <- read_ipums_micro(ddi_rwa)

rwanda <- transform(rwanda,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(rwanda)


rwanda_index <- rwanda [,c("SERIAL_YEAR","GEO2_RW2002","GEO2_RW2012")]

rwanda_index$IPUM2002 <- as.integer(rwanda_index$GEO2_RW2002)
rwanda_index$IPUM2012 <- as.integer(rwanda_index$GEO2_RW2012)

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

kigali_02 <- read.csv(file="kigali_02.csv", header = TRUE)
kigali_02['CITY']='kigali'

kigali_12 <- read.csv(file="kigali_12.csv", header = TRUE)
kigali_12['CITY']='kigali'

rwanda_02 <- rwanda_index %>% inner_join(kigali_02, by="IPUM2002")
rwanda_12 <- rwanda_index %>% inner_join(kigali_12, by="IPUM2012")

rm(kigali_02,kigali_12)

names(rwanda_02)
names(rwanda_12)

rwanda_02 <- select(rwanda_02, -c(DIST2002))
rwanda_12 <- select(rwanda_12, -c(PROV2012))

##Merging all years into one table
rwanda_full <- rbind(rwanda_02,rwanda_12)
rm(rwanda_02,rwanda_12)

rwanda_index <- rwanda_full[,c("SERIAL_YEAR","CITY")]
rwanda_index <- rwanda_index[!duplicated(rwanda_index$SERIAL_YEAR),]

rm(ddi_rwa,rwanda_full)

rwanda <- rwanda %>% left_join(rwanda_index,by="SERIAL_YEAR")
rm(rwanda_index)

names(rwanda)
table(rwanda$CITY)

rwanda$owner_b <- ifelse(rwanda$OWNERSHIP ==1,1,0)
rwanda$toilet_b <- ifelse(rwanda$TOILET ==21,1,0)
rwanda$eletric_b <- ifelse(rwanda$ELECTRIC ==1,1,0)
rwanda$water_b <- ifelse(rwanda$WATSUP ==11,1,0)
rwanda$fuelc_b <- ifelse(rwanda$FUELCOOK ==20|rwanda$FUELCOOK ==30,1,0)
rwanda$wall_b <- ifelse(rwanda$WALL ==510|rwanda$WALL ==517,1,0)
rwanda$phone_b <- ifelse(rwanda$PHONE ==2,1,0)
rwanda$roof_b <- ifelse(rwanda$ROOF == 11|rwanda$ROOF == 17,1,0)
rwanda$floor_b <- ifelse(rwanda$FLOOR !=100 ,1,0)
rwanda$tv_b <- ifelse(rwanda$TV == 21|rwanda$TV==23|rwanda$TV==20,1,0)
rwanda$radio_b <- ifelse(rwanda$RADIO ==2,1,0)
rwanda$auto_b <- ifelse(rwanda$AUTOS %in% 1:4,1,0)
rwanda$trash_b<- ifelse(rwanda$TRASH==11|rwanda$TRASH==12,1,0)
rwanda$computer_b <- ifelse(rwanda$COMPUTER == 2,1,0)
rwanda$cell_b <- ifelse(rwanda$CELL ==1 ,1 ,0)
rwanda$internet_b <- ifelse(rwanda$INTERNET == 2, 1, 0)

rwanda <- rwanda %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(rwanda)

#######aggregating by household

table(rwanda$HHWT)

rwanda <- rwanda %>% filter(HHWT != 0)

rwanda_HH_b <- rwanda %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,tv_b,radio_b,auto_b,trash_b,computer_b,cell_b,internet_b),.funs = c("max"))

names(rwanda_HH_b)

rwanda_HH_n <- rwanda %>% group_by(SERIAL_YEAR) %>%
  count()

rwanda <- as.data.table(rwanda)
rwanda_HH_r <- rwanda[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
rwanda_HH_rn <- full_join(rwanda_HH_n,rwanda_HH_r)
rwanda_HH_rn$room_b <- rwanda_HH_rn$n/rwanda_HH_rn$V1
rwanda_HH_rn <- rwanda_HH_rn[,c(1,4)]
rwanda_HH_b<-as.data.frame(rwanda_HH_b)
rwanda_HH_b <- full_join(rwanda_HH_b,rwanda_HH_rn)

rm(rwanda_HH_rn,rwanda_HH_r,rwanda_HH_n)

names(rwanda_HH_b)
rwanda_HH_b.pca <- prcomp(rwanda_HH_b[,c(5:21)])
summary(rwanda_HH_b.pca)                                           
str(rwanda_HH_b.pca)   

fviz_eig(rwanda_HH_b.pca)

rwanda_eig <- get_eigenvalue(rwanda_HH_b.pca)

rwanda_HH_PC<-cbind(rwanda_HH_b,rwanda_HH_b.pca$x[,1:3])
names(rwanda_HH_PC)
sd(rwanda_HH_PC$PC1)
rwanda_inq_index <- rwanda_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
rwanda_inq_index$eign <- rwanda_eig [1,1]
rwanda_inq_index$I <- rwanda_inq_index$PC1sd/(rwanda_inq_index$eign)^0.5
rwanda_inq_index$city <- "rwanda"
hist(rwanda_HH_PC$PC1)

kigali_HH_b <- rwanda_HH_b %>% filter(rwanda_HH_b$CITY=="kigali")
names(kigali_HH_b)

kigali_HH_b.pca <- prcomp(kigali_HH_b[,c(5:21)])
summary(kigali_HH_b.pca)                                           
str(kigali_HH_b.pca)   

fviz_eig(kigali_HH_b.pca)

kigali_eig <- get_eigenvalue(kigali_HH_b.pca)

kigali_HH_PC<-cbind(kigali_HH_b,kigali_HH_b.pca$x[,1:3])

sd(kigali_HH_PC$PC1)

kigali_inq_index <- kigali_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
kigali_inq_index$eign <- kigali_eig [1,1]
kigali_inq_index$I <- kigali_inq_index$PC1sd/(kigali_inq_index$eign)^0.5
kigali_inq_index$city <- "kigali"
hist(kigali_HH_PC$PC1)

rwa_kig_plot <- kigali_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = rwanda_inq_index,color="red")+
  geom_point(data = rwanda_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="kigali - rwanda")+
  theme_bw()

rwa_kig_plot

save(kigali_inq_index,file="kigali_inq_index.Rda")
save(rwanda_inq_index,file="rwanda_inq_index.Rda")
