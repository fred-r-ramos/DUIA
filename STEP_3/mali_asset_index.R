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

ddi_mal <-read_ipums_ddi("ipumsi_00083.xml")
mali <- read_ipums_micro(ddi_mal)

mali <- transform(mali,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(mali)

mali_index <- mali [,c("SERIAL_YEAR","GEO2_ML1998","GEO2_ML2009")]

###########################################import areas

mali_index$IPUM1998 <- as.integer(mali_index$GEO2_ML1998)
mali_index$IPUM2009 <- as.integer(mali_index$GEO2_ML2009)

bamako_98 <- read.csv(file="bamako_98.csv",header = TRUE)
bamako_98['CITY']='bamako'

bamako_09 <- read.csv(file="bamako_09.csv",header = TRUE)
bamako_09['CITY']='bamako'

mali_98 <- mali_index %>% inner_join(bamako_98, by="IPUM1998")
mali_09 <- mali_index %>% inner_join(bamako_09, by="IPUM2009")

rm(bamako_98,bamako_09)

names(mali_98)
names(mali_09)

mali_98 <- select(mali_98, -c(CIRC1998))
mali_09 <- select(mali_09, -c(CIRC2009))

##Merging all years into one table
mali_full <- rbind(mali_98,mali_09)
rm(mali_98,mali_09)

mali_index <- mali_full[,c("SERIAL_YEAR","CITY")]
mali_index <- mali_index[!duplicated(mali_index$SERIAL_YEAR),]

rm(ddi_mal,mali_full)

mali <- mali %>% left_join(mali_index,by="SERIAL_YEAR")
rm(mali_index)

names(mali)
table(mali$CITY)

######coding variables mali

mali$owner_b <- ifelse(mali$OWNERSHIP ==1,1,0)
mali$toilet_b <- ifelse(mali$TOILET ==21,1,0)
mali$eletric_b <- ifelse(mali$ELECTRIC ==1,1,0)
mali$fuelc_b <- ifelse(mali$FUELCOOK ==20|mali$FUELCOOK ==30,1,0)
mali$wall_b <- ifelse(mali$WALL ==501,1,0)
mali$roof_b <- ifelse(mali$ROOF ==11|mali$ROOF ==14,1,0)
mali$floor_b <- ifelse(mali$FLOOR != 100,1,0)

##########################################################calculating number of people per household

mali <- mali %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(mali)

#######aggregating by household

table(mali$HHWT)

mali <- mali %>% filter(HHWT != 0)

mali_HH_b <- mali %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,fuelc_b,wall_b,roof_b,floor_b),.funs = c("max"))

names(mali_HH_b)

mali_HH_n <- mali %>% group_by(SERIAL_YEAR) %>%
  count()

mali <- as.data.table(mali)
mali_HH_r <- mali[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
mali_HH_rn <- full_join(mali_HH_n,mali_HH_r)
mali_HH_rn$room_b <- mali_HH_rn$n/mali_HH_rn$V1
mali_HH_rn <- mali_HH_rn[,c(1,4)]
mali_HH_b<-as.data.frame(mali_HH_b)
mali_HH_b <- full_join(mali_HH_b,mali_HH_rn)

rm(mali_HH_rn,mali_HH_r,mali_HH_n)

names(mali_HH_b)

mali_HH_b.pca <- prcomp(mali_HH_b[,c(5:11)])
summary(mali_HH_b.pca)                                           
str(mali_HH_b.pca)   

fviz_eig(mali_HH_b.pca)

mali_eig <- get_eigenvalue(mali_HH_b.pca)

mali_HH_PC<-cbind(mali_HH_b,mali_HH_b.pca$x[,1:3])
names(mali_HH_PC)
sd(mali_HH_PC$PC1)
mali_inq_index <- mali_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
mali_inq_index$eign <- mali_eig [1,1]
mali_inq_index$I <- mali_inq_index$PC1sd/(mali_inq_index$eign)^0.5
mali_inq_index$city <- "mali"
hist(mali_HH_PC$PC1)

bamako_HH_b <- mali_HH_b %>% filter(mali_HH_b$CITY=="bamako")
names(bamako_HH_b)

bamako_HH_b.pca <- prcomp(bamako_HH_b[,c(4:11)])
summary(bamako_HH_b.pca)                                           
str(bamako_HH_b.pca)   

fviz_eig(bamako_HH_b.pca)

bamako_eig <- get_eigenvalue(bamako_HH_b.pca)

bamako_HH_PC<-cbind(bamako_HH_b,bamako_HH_b.pca$x[,1:3])

sd(bamako_HH_PC$PC1)

bamako_inq_index <- bamako_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bamako_inq_index$eign <- bamako_eig [1,1]
bamako_inq_index$I <- bamako_inq_index$PC1sd/(bamako_inq_index$eign)^0.5
bamako_inq_index$city <- "bamako"
hist(bamako_HH_PC$PC1)

mal_bam_plot <- bamako_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = mali_inq_index,color="red")+
  geom_point(data = mali_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="Bamako - Mali")+
  theme_bw()

mal_bam_plot

save(mali_inq_index,file="mali_inq_index.Rda")
save(bamako_inq_index,file="bamako_inq_index.Rda")
