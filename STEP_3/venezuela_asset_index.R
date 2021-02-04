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

ddi_ven <-read_ipums_ddi("ipumsi_00094.xml")
venezuela <- read_ipums_micro(ddi_ven)

cabimas_90 <- read.csv(file="cabimas_90.csv", header = TRUE)
cabimas_90['CITY']='cabimas'

cabimas_01 <- read.csv(file="cabimas_01.csv", header = TRUE)
cabimas_01['CITY']='cabimas'

venezuela <- transform(venezuela,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(venezuela)

venezuela_index <- venezuela [,c("SERIAL_YEAR","GEO2_VE1990","GEO2_VE2001")]

venezuela_index$IPUM1990 <- as.integer(venezuela_index$GEO2_VE1990)
venezuela_index$IPUM2001 <- as.integer(venezuela_index$GEO2_VE2001)

venezuela_90 <- venezuela_index %>% inner_join(cabimas_90, by="IPUM1990")
venezuela_01 <- venezuela_index %>% inner_join(cabimas_01, by="IPUM2001")

rm(cabimas_90,cabimas_01)

names(venezuela_90)
names(venezuela_01)

venezuela_90 <- select(venezuela_90, -c(MUNI1990))
venezuela_01 <- select(venezuela_01, -c(MUNI2001))

##Merging all years into one table
venezuela_full <- rbind(venezuela_90,venezuela_01)
rm(venezuela_90,venezuela_01)

venezuela_index <- venezuela_full[,c("SERIAL_YEAR","CITY")]
venezuela_index <- venezuela_index[!duplicated(venezuela_index$SERIAL_YEAR),]

rm(ddi_ven,venezuela_full)

venezuela <- venezuela %>% left_join(venezuela_index,by="SERIAL_YEAR")
rm(venezuela_index)

names(venezuela)
table(venezuela$CITY)
table(venezuela$YEAR)

venezuela$owner_b <- ifelse(venezuela$OWNERSHIP ==1,1,0)
venezuela$toilet_b <- ifelse(venezuela$TOILET ==21,1,0)
venezuela$eletric_b <- ifelse(venezuela$ELECTRIC ==1,1,0)
venezuela$water_b <- ifelse(venezuela$WATSUP ==11|venezuela$WATSUP ==10,1,0)
venezuela$phone_b <- ifelse(venezuela$PHONE ==2,1,0)
venezuela$floor_b <- ifelse(venezuela$FLOOR != 100,1 ,0 )
venezuela$tv_b <- ifelse(venezuela$TV==20,1,0)
venezuela$sewage_b <- ifelse(venezuela$SEWAGE ==11|venezuela$SEWAGE ==12,1,0)
venezuela$kitchen_b <- ifelse(venezuela$KITCHEN== 20,1 ,0 )
venezuela$refrig_b <- ifelse(venezuela$REFRIG == 2,1,0)
venezuela$auto_b <- ifelse(venezuela$AUTOS==7,1,0)
venezuela$bath_b <-ifelse(venezuela$BATH %in% 2:4,1,0)

table(venezuela$HHWT)
table(venezuela$ROOMS)

names(venezuela)

venezuela_HH_b <- venezuela %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,phone_b,floor_b,tv_b,sewage_b,kitchen_b,refrig_b,auto_b,bath_b),.funs = c("max"))

names(venezuela_HH_b)

venezuela <- venezuela %>% filter(ROOMS != 99)
venezuela <- venezuela %>% filter(ROOMS != 98)

venezuela_HH_n <- venezuela %>% group_by(SERIAL_YEAR) %>%
  count()


venezuela <- as.data.table(venezuela)
venezuela_HH_r <- venezuela[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
venezuela_HH_rn <- full_join(venezuela_HH_n,venezuela_HH_r)
venezuela_HH_rn$room_b <- venezuela_HH_rn$n/venezuela_HH_rn$V1
venezuela_HH_rn <- venezuela_HH_rn[,c(1,4)]
venezuela_HH_b<-as.data.frame(venezuela_HH_b)
venezuela_HH_b <- full_join(venezuela_HH_b,venezuela_HH_rn)

rm(venezuela_HH_rn,venezuela_HH_r,venezuela_HH_n)

names(venezuela_HH_b)

venezuela_HH_b <- venezuela_HH_b %>% filter(HHWT>0)

venezuela_HH_b <- expandRows(venezuela_HH_b,'HHWT')

venezuela_HH_b.pca <- prcomp(venezuela_HH_b[,c(4:16)])
summary(venezuela_HH_b.pca)                                           
str(venezuela_HH_b.pca)   

fviz_eig(venezuela_HH_b.pca)

venezuela_eig <- get_eigenvalue(venezuela_HH_b.pca)

venezuela_HH_PC<-cbind(venezuela_HH_b,venezuela_HH_b.pca$x[,1:3])
names(venezuela_HH_PC)
sd(venezuela_HH_PC$PC1)
venezuela_inq_index <- venezuela_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
venezuela_inq_index$eign <- venezuela_eig [1,1]
venezuela_inq_index$I <- venezuela_inq_index$PC1sd/(venezuela_inq_index$eign)^0.5
venezuela_inq_index$city <- "venezuela"
hist(venezuela_HH_PC$PC1)

cabimas_HH_b <- venezuela_HH_b %>% filter(venezuela_HH_b$CITY=="cabimas")
names(cabimas_HH_b)

cabimas_HH_b.pca <- prcomp(cabimas_HH_b[,c(4:16)])
summary(cabimas_HH_b.pca)                                           
str(cabimas_HH_b.pca)   

fviz_eig(cabimas_HH_b.pca)

cabimas_eig <- get_eigenvalue(cabimas_HH_b.pca)

cabimas_HH_PC<-cbind(cabimas_HH_b,cabimas_HH_b.pca$x[,1:3])

sd(cabimas_HH_PC$PC1)

cabimas_inq_index <- cabimas_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cabimas_inq_index$eign <- cabimas_eig [1,1]
cabimas_inq_index$I <- cabimas_inq_index$PC1sd/(cabimas_inq_index$eign)^0.5
cabimas_inq_index$city <- "cabimas"
hist(cabimas_HH_PC$PC1)

ven_cab_plot <- cabimas_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = venezuela_inq_index,color="red")+
  geom_point(data = venezuela_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="cabimas - venezuela")+
  theme_bw()

ven_cab_plot

save(cabimas_inq_index,file="cabimas_inq_index.Rda")
save(venezuela_inq_index,file="venezuela_inq_index.Rda")
