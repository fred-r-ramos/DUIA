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

ddi_taz <-read_ipums_ddi("ipumsi_00098.xml")
tanzania <- read_ipums_micro(ddi_taz)

arusha_02 <- read.csv(file="arusha_2002.csv", header = TRUE)
arusha_02['CITY']='arusha'

arusha_12 <- read.csv(file="arusha_2012.csv", header = TRUE)
arusha_12['CITY']='arusha'

tanzania <- transform(tanzania,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(tanzania)

tanzania_index <- tanzania [,c("SERIAL_YEAR","GEO2_TZ2002","GEO2_TZ2012")]

tanzania_index$IPUM2002 <- as.integer(tanzania_index$GEO2_TZ2002)
tanzania_index$IPUM2012 <- as.integer(tanzania_index$GEO2_TZ2012)

tanzania_02 <- tanzania_index %>% inner_join(arusha_02, by="IPUM2002")
tanzania_12 <- tanzania_index %>% inner_join(arusha_12, by="IPUM2012")

rm(arusha_02,arusha_12)

names(tanzania_02)
names(tanzania_12)

tanzania_02 <- select(tanzania_02, -c(DIST2002))
tanzania_12 <- select(tanzania_12, -c(DIST2012))

##Merging all years into one table
tanzania_full <- rbind(tanzania_02,tanzania_12)
rm(tanzania_02,tanzania_12)

tanzania_index <- tanzania_full[,c("SERIAL_YEAR","CITY")]
tanzania_index <- tanzania_index[!duplicated(tanzania_index$SERIAL_YEAR),]

rm(ddi_taz,tanzania_full)

tanzania <- tanzania %>% left_join(tanzania_index,by="SERIAL_YEAR")
rm(tanzania_index)

names(tanzania)
table(tanzania$CITY)

tanzania$toilet_b <- ifelse(tanzania$TOILET ==21,1,0)
tanzania$eletric_b <- ifelse(tanzania$ELECTRIC ==1,1,0)
tanzania$water_b <- ifelse(tanzania$WATSUP ==11|tanzania$WATSUP ==10,1,0)
tanzania$fuelc_b <- ifelse(tanzania$FUELCOOK ==30|tanzania$FUELCOOK ==20,1,0)
tanzania$wall_b <- ifelse(tanzania$WALL ==510|tanzania$WALL ==518,1,0)
tanzania$phone_b <- ifelse(tanzania$PHONE ==2,1,0)
tanzania$roof_b <- ifelse(tanzania$ROOF == 11|tanzania$ROOF == 14, 1, 0)
tanzania$floor_b <- ifelse(tanzania$FLOOR == 1,1 ,0 )
tanzania$radio_b <- ifelse(tanzania$RADIO ==2,1,0)

##########################################################calculating number of people per household

tanzania <- tanzania %>% filter(BEDROOMS != 99) %>%  filter (BEDROOMS != 98)
names(tanzania)

#######aggregating by household

table(tanzania$HHWT)
gc()

arusha <- tanzania %>% filter(CITY=="arusha")

##tanzania <- tanzania [, c("YEAR","CITY","SERIAL_YEAR","HHWT","toilet_b","eletric_b","water_b","fuelc_b","wall_b","phone_b","roof_b","floor_b","radio_b")]


arusha_HH_b <- arusha %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,radio_b),.funs = c("max"))

tanzania_HH_b <- tanzania %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,toilet_b,eletric_b,water_b,fuelc_b,wall_b,phone_b,roof_b,floor_b,radio_b),.funs = c("max"))


names(tanzania_HH_b)

arusha_HH_n <- arusha %>% group_by(SERIAL_YEAR) %>%
  count()

arusha <- as.data.table(arusha)
arusha_HH_r <- arusha[BEDROOMS>0, min(BEDROOMS), by=SERIAL_YEAR]
arusha_HH_rn <- full_join(arusha_HH_n,arusha_HH_r)
arusha_HH_rn$room_b <- arusha_HH_rn$n/arusha_HH_rn$V1
arusha_HH_rn <- arusha_HH_rn[,c(1,4)]
arusha_HH_b<-as.data.frame(arusha_HH_b)
arusha_HH_b <- full_join(arusha_HH_b,arusha_HH_rn)

rm(arusha_HH_rn,arusha_HH_r,arusha_HH_n)

names(arusha_HH_b)

arusha_HH_b <- expandRows(arusha_HH_b,'HHWT')

arusha_HH_b.pca <- prcomp(arusha_HH_b[,c(4:13)])
summary(arusha_HH_b.pca)                                           
str(arusha_HH_b.pca)   

fviz_eig(arusha_HH_b.pca)

arusha_eig <- get_eigenvalue(arusha_HH_b.pca)

arusha_HH_PC<-cbind(arusha_HH_b,arusha_HH_b.pca$x[,1:3])

sd(arusha_HH_PC$PC1)

arusha_inq_index <- arusha_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
arusha_inq_index$eign <- arusha_eig [1,1]
arusha_inq_index$I <- arusha_inq_index$PC1sd/(arusha_inq_index$eign)^0.5
arusha_inq_index$city <- "arusha"
hist(arusha_HH_PC$PC1)
###################################################
tanzania_HH_n <- tanzania %>% group_by(SERIAL_YEAR) %>%
  count()

tanzania <- as.data.table(tanzania)
tanzania_HH_r <- tanzania[BEDROOMS>0, min(BEDROOMS), by=SERIAL_YEAR]
tanzania_HH_rn <- full_join(tanzania_HH_n,tanzania_HH_r)
tanzania_HH_rn$room_b <- tanzania_HH_rn$n/tanzania_HH_rn$V1
tanzania_HH_rn <- tanzania_HH_rn[,c(1,4)]
tanzania_HH_b<-as.data.frame(tanzania_HH_b)
tanzania_HH_b <- full_join(tanzania_HH_b,tanzania_HH_rn)

rm(tanzania_HH_rn,tanzania_HH_r,tanzania_HH_n)

names(tanzania_HH_b)

tanzania_HH_b <- expandRows(tanzania_HH_b,'HHWT')

tanzania_HH_b.pca <- prcomp(tanzania_HH_b[,c(4:13)])
summary(tanzania_HH_b.pca)                                           
str(tanzania_HH_b.pca)   

fviz_eig(tanzania_HH_b.pca)

tanzania_eig <- get_eigenvalue(tanzania_HH_b.pca)

tanzania_HH_PC<-cbind(tanzania_HH_b,tanzania_HH_b.pca$x[,1:3])

sd(tanzania_HH_PC$PC1)

tanzania_inq_index <- tanzania_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
tanzania_inq_index$eign <- tanzania_eig [1,1]
tanzania_inq_index$I <- tanzania_inq_index$PC1sd/(tanzania_inq_index$eign)^0.5
tanzania_inq_index$city <- "tanzania"
hist(tanzania_HH_PC$PC1)

aru_tan_plot <- arusha_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = tanzania_inq_index,color="red")+
  geom_point(data = tanzania_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="arusha - rwanda")+
  theme_bw()

aru_tan_plot
save(arusha_inq_index,file="arusha_inq_index.Rda")
save(tanzania_inq_index,file="tanzania_inq_index.Rda")

