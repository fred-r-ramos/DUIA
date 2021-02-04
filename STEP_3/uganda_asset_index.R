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

ddi_uga <-read_ipums_ddi("ipumsi_00093.xml")
uganda <- read_ipums_micro(ddi_uga)

kampala_91 <- read.csv(file="kampala_91.csv", header = TRUE)
kampala_91['CITY']='kampala'

kampala_02 <- read.csv(file="kampala_02.csv",header = TRUE)
kampala_02['CITY']='kampala'

uganda <- transform(uganda,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(uganda)

uganda_index <- uganda [,c("SERIAL_YEAR","GEO2_UG1991","GEO2_UG2002")]

uganda_index$IPUM1991 <- as.integer(uganda_index$GEO2_UG1991)
uganda_index$IPUM2002 <- as.integer(uganda_index$GEO2_UG2002)

uganda_91 <- uganda_index %>% inner_join(kampala_91, by="IPUM1991")
uganda_02 <- uganda_index %>% inner_join(kampala_02, by="IPUM2002")

rm(kampala_91,kampala_02)

names(uganda_91)
names(uganda_02)

uganda_91 <- select(uganda_91, -c(CNTY1991))
uganda_02 <- select(uganda_02, -c(CNTY2002))

##Merging all years into one table
uganda_full <- rbind(uganda_91,uganda_02)
rm(uganda_91,uganda_02)

uganda_index <- uganda_full[,c("SERIAL_YEAR","CITY")]
uganda_index <- uganda_index[!duplicated(uganda_index$SERIAL_YEAR),]

rm(ddi_uga,uganda_full)

uganda <- uganda %>% left_join(uganda_index,by="SERIAL_YEAR")
rm(uganda_index)

names(uganda)
table(uganda$CITY)
table(uganda$YEAR)

uganda$owner_b <- ifelse(uganda$OWNERSHIP ==1,1,0)
uganda$toilet_b <- ifelse(uganda$TOILET ==21,1,0)
uganda$eletric_b <- ifelse(uganda$ELECTRIC ==1,1,0)
uganda$water_b <- ifelse(uganda$WATSUP ==11|uganda$WATSUP ==10,1,0)
uganda$fuelc_b <- ifelse(uganda$FUELCOOK ==20|uganda$FUELCOOK ==30,1,0)
uganda$wall_b <- ifelse(uganda$WALL ==510|uganda$WALL ==515,1,0)
uganda$roof_b <- ifelse(uganda$ROOF == 11|uganda$ROOF == 14,1 , 0)
uganda$floor_b <- ifelse(uganda$FLOOR != 100,1 ,0)
uganda$kitchen_b <- ifelse(uganda$KITCHEN %in% 20:23, 1, 0)
uganda$bath_b <-ifelse(uganda$BATH %in% 2:4,1,0)

names(uganda)

table(uganda$HHWT)

uganda <- uganda %>% filter(HHWT != 0)

uganda_HH_b <- uganda %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,roof_b,floor_b,kitchen_b,bath_b),.funs = c("max"))

names(uganda_HH_b)

uganda_HH_b <- expandRows(uganda_HH_b,'HHWT')
uganda_HH_b.pca <- prcomp(uganda_HH_b[,c(4:13)])
summary(uganda_HH_b.pca)                                           
str(uganda_HH_b.pca)   

fviz_eig(uganda_HH_b.pca)

uganda_eig <- get_eigenvalue(uganda_HH_b.pca)

uganda_HH_PC<-cbind(uganda_HH_b,uganda_HH_b.pca$x[,1:3])
names(uganda_HH_PC)
sd(uganda_HH_PC$PC1)
uganda_inq_index <- uganda_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
uganda_inq_index$eign <- uganda_eig [1,1]
uganda_inq_index$I <- uganda_inq_index$PC1sd/(uganda_inq_index$eign)^0.5
uganda_inq_index$city <- "uganda"
hist(uganda_HH_PC$PC1)

kampala_HH_b <- uganda_HH_b %>% filter(uganda_HH_b$CITY=="kampala")
names(kampala_HH_b)

kampala_HH_b.pca <- prcomp(kampala_HH_b[,c(4:13)])
summary(kampala_HH_b.pca)                                           
str(kampala_HH_b.pca)   

fviz_eig(kampala_HH_b.pca)

kampala_eig <- get_eigenvalue(kampala_HH_b.pca)

kampala_HH_PC<-cbind(kampala_HH_b,kampala_HH_b.pca$x[,1:3])

sd(kampala_HH_PC$PC1)

kampala_inq_index <- kampala_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
kampala_inq_index$eign <- kampala_eig [1,1]
kampala_inq_index$I <- kampala_inq_index$PC1sd/(kampala_inq_index$eign)^0.5
kampala_inq_index$city <- "kampala"
hist(kampala_HH_PC$PC1)

uga_kam_plot <- kampala_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = uganda_inq_index,color="red")+
  geom_point(data = uganda_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="kampala - uganda")+
  theme_bw()

uga_kam_plot

save(kampala_inq_index,file="kampala_inq_index.Rda")
save(uganda_inq_index,file="uganda_inq_index.Rda")
