library(ipumsr)
library(dbplyr)
library(tidyverse)
library(factoextra)
library(data.table)
library(splitstackshape)
library(ggplot2)
library(gridExtra)
gc()

getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/inequality_index")

###########################################calculating for Kenya
###########################################import IPUMS

ddi_mor <-read_ipums_ddi("ipumsi_00085.xml")
moroco <- read_ipums_micro(ddi_mor)

moroco <- transform(moroco,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(moroco)

moroco_index <- moroco [,c("SERIAL_YEAR","GEO2_MA1994","GEO2_MA2004")]

###########################################import areas

moroco_index$IPUM1994 <- as.integer(moroco_index$GEO2_MA1994)
moroco_index$IPUM2004 <- as.integer(moroco_index$GEO2_MA2004)

marrakesh_04 <- read.csv(file="marrakesh_04.csv",header = TRUE)
marrakesh_04['CITY']='marrakesh'

marrakesh_94 <- read.csv(file="marrakesh_94.csv",header = TRUE)
marrakesh_94['CITY']='marrakesh'

moroco_94 <- moroco_index %>% inner_join(marrakesh_94, by="IPUM1994")
moroco_04 <- moroco_index %>% inner_join(marrakesh_04, by="IPUM2004")

rm(marrakesh_94,marrakesh_04)

names(moroco_94)
names(moroco_04)

moroco_94 <- select(moroco_94, -c(PROV1994))
moroco_04 <- select(moroco_04, -c(PROV2004))

##Merging all years into one table
moroco_full <- rbind(moroco_94,moroco_04)
rm(moroco_94,moroco_04)

moroco_index <- moroco_full[,c("SERIAL_YEAR","CITY")]
moroco_index <- moroco_index[!duplicated(moroco_index$SERIAL_YEAR),]
rm(ddi_mor,moroco_full)

moroco <- moroco %>% left_join(moroco_index,by="SERIAL_YEAR")
rm(moroco_index)

names(moroco)
table(moroco$CITY)

######coding variables mali

moroco$owner_b <- ifelse(moroco$OWNERSHIP ==1,1,0)
moroco$toilet_b <- ifelse(moroco$TOILET ==20|moroco$TOILET ==21,1,0)
moroco$eletric_b <- ifelse(moroco$ELECTRIC ==1,1,0)
moroco$water_b <- ifelse(moroco$WATSUP ==12,1,0)
moroco$kitchen_b <- ifelse(moroco$KITCHEN==24|moroco$KITCHEN==25,1,0)
moroco$bath_b <-ifelse(moroco$BATH == 3|moroco$BATH == 4,1,0)

##########################################################calculating number of people per household

moroco <- moroco %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(moroco)

#######aggregating by household

table(moroco$HHWT)

moroco <- moroco %>% filter(HHWT != 0)

moroco_HH_b <- moroco %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,kitchen_b,bath_b),.funs = c("max"))

names(moroco_HH_b)

moroco_HH_n <- moroco %>% group_by(SERIAL_YEAR) %>%
  count()

moroco <- as.data.table(moroco)
moroco_HH_r <- moroco[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
moroco_HH_rn <- full_join(moroco_HH_n,moroco_HH_r)
moroco_HH_rn$room_b <- moroco_HH_rn$n/moroco_HH_rn$V1
moroco_HH_rn <- moroco_HH_rn[,c(1,4)]
moroco_HH_b<-as.data.frame(moroco_HH_b)
moroco_HH_b <- full_join(moroco_HH_b,moroco_HH_rn)

rm(moroco_HH_rn,moroco_HH_r,moroco_HH_n)

names(moroco_HH_b)

moroco_HH_b.pca <- prcomp(moroco_HH_b[,c(5:11)])
summary(moroco_HH_b.pca)                                           
str(moroco_HH_b.pca)   

fviz_eig(moroco_HH_b.pca)

moroco_eig <- get_eigenvalue(moroco_HH_b.pca)

moroco_HH_PC<-cbind(moroco_HH_b,moroco_HH_b.pca$x[,1:3])
names(moroco_HH_PC)
sd(moroco_HH_PC$PC1)
moroco_inq_index <- moroco_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
moroco_inq_index$eign <- moroco_eig [1,1]
moroco_inq_index$I <- moroco_inq_index$PC1sd/(moroco_inq_index$eign)^0.5
moroco_inq_index$city <- "moroco"
hist(moroco_HH_PC$PC1)

table(moroco$CITY)

marrakesh_HH_b <- moroco_HH_b %>% filter(moroco_HH_b$CITY=="marrakesh")

names(marrakesh_HH_b)

marrakesh_HH_b.pca <- prcomp(marrakesh_HH_b[,c(5:11)])
summary(marrakesh_HH_b.pca)                                           
str(marrakesh_HH_b.pca)   

fviz_eig(marrakesh_HH_b.pca)

marrakesh_eig <- get_eigenvalue(marrakesh_HH_b.pca)

marrakesh_HH_PC<-cbind(marrakesh_HH_b,(marrakesh_HH_b.pca$x[,1:3]))

sd(marrakesh_HH_PC$PC1)

marrakesh_inq_index <- marrakesh_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
marrakesh_inq_index$eign <- marrakesh_eig [1,1]
marrakesh_inq_index$I <- marrakesh_inq_index$PC1sd/(marrakesh_inq_index$eign)^0.5
marrakesh_inq_index$city <- "marrakesh"
hist(marrakesh_HH_PC$PC1)

mor_bam_plot <- marrakesh_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = moroco_inq_index,color="red")+
  geom_point(data = moroco_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="Marrakesh - Moroco")+
  theme_bw()

mor_bam_plot

save(marrakesh_inq_index, file="marrakesh_inq_index.Rda")
save(moroco_inq_index, file="moroco_inq_index.Rda")
