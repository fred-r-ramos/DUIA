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

###########################################calculating for ghana
###########################################import IPUMS

ddi_gha <-read_ipums_ddi("ipumsi_00079.xml")
ghana <- read_ipums_micro(ddi_gha)

ghana <- transform(ghana,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(ghana)

ghana_index <- ghana [,c("SERIAL_YEAR","GEO2_GH2000","GEO2_GH2010")]

###########################################import areas

ghana_index$IPUM2000 <- as.integer(ghana_index$GEO2_GH2000)
ghana_index$IPUM2010 <- as.integer(ghana_index$GEO2_GH2010)

accra_2000 <- read.csv(file="accra_00.csv", header = TRUE)
accra_2000['CITY']='accra'

accra_2010 <- read.csv(file="accra_10.csv", header = TRUE)
accra_2010['CITY']='accra'

ghana_00 <- ghana_index %>% inner_join(accra_2000, by="IPUM2000")
ghana_10 <- ghana_index %>% inner_join(accra_2010, by="IPUM2010")

rm(accra_2000,accra_2010)

names(ghana_00)
names(ghana_10)

ghana_00 <- select(ghana_00, -c(DIST2000))
ghana_10 <- select(ghana_10, -c(DIST2010))

##Merging all years into one table
ghana_full <- rbind(ghana_00,ghana_10)
rm(ghana_00,ghana_10)

ghana_index <- ghana_full[,c("SERIAL_YEAR","CITY")]
ghana_index <- ghana_index[!duplicated(ghana_index$SERIAL_YEAR),]

rm(ddi_gha,ghana_full)

ghana <- ghana %>% left_join(ghana_index,by="SERIAL_YEAR")
rm(ghana_index)

names(ghana)
table(ghana$CITY)

######coding variables ghana

ghana$owner_b <- ifelse(ghana$OWNERSHIP ==1,1,0)
ghana$toilet_b <- ifelse(ghana$TOILET ==21,1,0)
ghana$eletric_b <- ifelse(ghana$ELECTRIC ==1,1,0)
ghana$water_b <- ifelse(ghana$WATSUP ==11,1,0)
ghana$fuelc_b <- ifelse(ghana$FUELCOOK ==20|ghana$FUELCOOK ==30,1,0)
ghana$wall_b <- ifelse(ghana$WALL==510|ghana$WALL==516|ghana$WALL==517,1,0)
ghana$roof_b <- ifelse(ghana$ROOF==11|ghana$ROOF==14,1,0)
ghana$floor_b <- ifelse(ghana$FLOOR !=100,1,0)
ghana$kitchen_b <- ifelse(ghana$KITCHEN==21|ghana$KITCHEN==22|ghana$KITCHEN==26,1,0)

##########################################################calculating number of people per household

ghana <- ghana %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(ghana)

#######aggregating by household

table(ghana$HHWT)

ghana <- ghana %>% filter(ghana$HHWT != 0)

ghana_HH_b <- ghana %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,toilet_b,eletric_b,water_b,fuelc_b,wall_b,roof_b,floor_b,kitchen_b),.funs = c("max"))

names(ghana_HH_b)

ghana_HH_n <- ghana %>% group_by(SERIAL_YEAR) %>%
  count()

ghana <- as.data.table(ghana)
ghana_HH_r <- ghana[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
ghana_HH_rn <- full_join(ghana_HH_n,ghana_HH_r)
ghana_HH_rn$room_b <- ghana_HH_rn$n/ghana_HH_rn$V1
ghana_HH_rn <- ghana_HH_rn[,c(1,4)]
ghana_HH_b<-as.data.frame(ghana_HH_b)
ghana_HH_b <- full_join(ghana_HH_b,ghana_HH_rn)

rm(ghana_HH_n,ghana_HH_r,ghana_HH_rn)

names(ghana_HH_b)

table(ghana_HH_b$YEAR)
###########################################calculating pca

ghana_HH_b.pca <- prcomp(ghana_HH_b[,c(5:14)])
summary(ghana_HH_b.pca)                                           
str(ghana_HH_b.pca)   

fviz_eig(ghana_HH_b.pca)

ghana_eig <- get_eigenvalue(ghana_HH_b.pca)

ghana_HH_PC<-cbind(ghana_HH_b,ghana_HH_b.pca$x[,1:3])

sd(ghana_HH_PC$PC1)

ghana_inq_index <- ghana_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ghana_inq_index$eign <- ghana_eig [1,1]
ghana_inq_index$I <- ghana_inq_index$PC1sd/(ghana_inq_index$eign)^0.5
ghana_inq_index$city <- "ghana"
hist(ghana_HH_PC$PC1)

###################################for accra

accra_HH_b <- ghana_HH_b %>% filter(ghana_HH_b$CITY=="accra")
names(accra_HH_b)

accra_HH_b.pca <- prcomp(accra_HH_b[,c(5:14)])
summary(accra_HH_b.pca)                                           
str(accra_HH_b.pca)   

fviz_eig(accra_HH_b.pca)

accra_eig <- get_eigenvalue(accra_HH_b.pca)

accra_HH_PC<-cbind(accra_HH_b,accra_HH_b.pca$x[,1:3])

sd(accra_HH_PC$PC1)

accra_inq_index <- accra_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
accra_inq_index$eign <- accra_eig [1,1]
accra_inq_index$I <- accra_inq_index$PC1sd/(accra_inq_index$eign)^0.5
accra_inq_index$city <- "accra"
hist(accra_HH_PC$PC1)

gha_acc_plot <- accra_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = ghana_inq_index,color="red")+
  geom_point(data = ghana_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="accra")+
  theme_bw()

gha_acc_plot

save(accra_inq_index,file="accra_inq_index.Rda")
save(ghana_inq_index,file="ghana_inq_index.Rda")
