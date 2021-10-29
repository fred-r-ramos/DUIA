library(ipumsr)
library(dbplyr)
library(tidyverse)
library(factoextra)
library(data.table)
library(splitstackshape)
library(ggplot2)
library(gridExtra)


getwd()
setwd("C:workingdirectory")

###########################################calculating for Chile

###########################################import IPUMS

ddi_chi <-read_ipums_ddi("ipumsi_00072.xml")
chile <- read_ipums_micro(ddi_chi)

chile <- transform(chile,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(chile)

###########################################creating an index table

chile_index <- chile [,c("SERIAL_YEAR","GEO2_CL1992","GEO2_CL2002")]

chile_index$IPUM1992 <- as.integer(chile_index$GEO2_CL1992)
chile_index$IPUM2002 <- as.integer(chile_index$GEO2_CL2002)

###########################################import areas for selected cities

santiago_92 <- read.csv(file="santiago_92.csv",header = TRUE)
santiago_92['CITY']='santiago'

santiago_02 <- read.csv(file="santiago_02.csv",header = TRUE)
santiago_02['CITY']='santiago'

chile_92 <- chile_index %>% inner_join(santiago_92, by="IPUM1992")
chile_02 <- chile_index %>% inner_join(santiago_02, by="IPUM2002")
names(chile_02)

rm(santiago_92,santiago_02)

chile_92 <- select(chile_92, -c(MUNI1992))
chile_02 <- select(chile_02, -c(MUNI2002))

chile_full <- rbind(chile_92,chile_02)
rm(chile_92,chile_02)

chile_index <- chile_full[,c("SERIAL_YEAR","CITY")]
chile_index <- chile_index[!duplicated(chile_index$SERIAL_YEAR),]

rm(ddi_chi,chile_full)

chile <- chile %>% left_join(chile_index,by="SERIAL_YEAR")
rm(chile_index)

names(chile)
table(chile$CITY)

chile$owner_b <- ifelse(chile$OWNERSHIP ==1,1,0)
chile$toilt_b <- ifelse(chile$TOILET == 21,1,0)
chile$eletr_b <- ifelse(chile$ELECTRIC ==1,1,0)
chile$water_b <- ifelse(chile$WATSUP ==11,1,0)
chile$phone_b <- ifelse(chile$PHONE == 2,1,0)
chile$floors_b <- ifelse(chile$FLOOR !=100,1,0)
chile$tv_b <- ifelse(chile$TV == 30 | chile$TV ==40,1,0)
chile$sewage_b <- ifelse(chile$SEWAGE ==10|chile$SEWAGE ==11|chile$SEWAGE ==12,1,0)
chile$kitchen_b <- ifelse(chile$KITCHEN == 20,1,0)
chile$refrig_b <- ifelse(chile$REFRIG ==2,1,0)
chile$auto_b <- ifelse(chile$AUTOS==7,1,0)
chile$bath_b <- ifelse(chile$BATH==2,1,0)
chile$cell_b <- ifelse(chile$CELL==1,1,0)

##########################################################calculating number of people per household

chile <- chile %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(chile)

#######aggregating by household

table(chile$HHWT)

chile <- chile %>% filter(HHWT != 0)

chile_HH_b <- chile %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilt_b,eletr_b,water_b,phone_b,floors_b,tv_b,sewage_b,kitchen_b,refrig_b,auto_b,bath_b,cell_b),.funs = c("max"))

names(chile_HH_b)

chile_HH_n <- chile %>% group_by(SERIAL_YEAR) %>%
  count()

chile <- as.data.table(chile)
chile_HH_r <- chile[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
chile_HH_rn <- full_join(chile_HH_n,chile_HH_r)
chile_HH_rn$room_b <- chile_HH_rn$n/chile_HH_rn$V1
chile_HH_rn <- chile_HH_rn[,c(1,4)]
chile_HH_b <- as.data.frame(chile_HH_b)
chile_HH_b <- full_join(chile_HH_b,chile_HH_rn)

rm(chile_HH_n,chile_HH_r,chile_HH_rn)

names(chile_HH_b)

table(chile_HH_b$YEAR)

###########################################calculating pca

chile_HH_b.pca <- prcomp(chile_HH_b[,c(5:18)])
summary(chile_HH_b.pca)                                           
str(chile_HH_b.pca)   

fviz_eig(chile_HH_b.pca)

chile_eig <- get_eigenvalue(chile_HH_b.pca)

chile_HH_PC<-cbind(chile_HH_b,chile_HH_b.pca$x[,1:3])

sd(chile_HH_PC$PC1)

chile_inq_index <- chile_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
chile_inq_index$eign <- chile_eig [1,1]
chile_inq_index$I <- chile_inq_index$PC1sd/(chile_inq_index$eign)^0.5
chile_inq_index$city <- "chile"
hist(chile_HH_PC$PC1)

save(chile_inq_index,file="chile_inq_index.Rda")

###################################for santiago

santiago_HH_b <- chile_HH_b %>% filter(chile_HH_b$CITY=="santiago")
names(santiago_HH_b)

santiago_HH_b.pca <- prcomp(santiago_HH_b[,c(5:18)])
summary(santiago_HH_b.pca)                                           
str(santiago_HH_b.pca)   

fviz_eig(santiago_HH_b.pca)

santiago_eig <- get_eigenvalue(santiago_HH_b.pca)

santiago_HH_PC<-cbind(santiago_HH_b,santiago_HH_b.pca$x[,1:3])

sd(santiago_HH_PC$PC1)

santiago_inq_index <- santiago_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
santiago_inq_index$eign <- santiago_eig [1,1]
santiago_inq_index$I <- santiago_inq_index$PC1sd/(santiago_inq_index$eign)^0.5
santiago_inq_index$city <- "santiago"
hist(santiago_HH_PC$PC1)

chi_san_plot <- santiago_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = chile_inq_index,color="red")+
  geom_point(data = chile_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="Santiago")+
  theme_bw()

save(santiago_inq_index, file="santiago_inq_index.Rda")

chi_san_plot

