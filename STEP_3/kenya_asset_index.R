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

ddi_ken <-read_ipums_ddi("ipumsi_00082.xml")
kenya <- read_ipums_micro(ddi_ken)

kenya <- transform(kenya,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(kenya)

kenya_index <- kenya [,c("SERIAL_YEAR","GEO2_KE1999","GEO2_KE2009")]

###########################################import areas

kenya_index$IPUM1999 <- as.integer(kenya_index$GEO2_KE1999)
kenya_index$IPUM2009 <- as.integer(kenya_index$GEO2_KE2009)

nakuru_99 <- read.csv(file="nakuru_1999.csv",header = TRUE)
nakuru_99['CITY']='nakuru'

nakuru_09 <- read.csv(file="nakuru_2009.csv",header = TRUE)
nakuru_09['CITY']='nakuru'

kenya_99 <- kenya_index %>% inner_join(nakuru_99, by="IPUM1999")
kenya_09 <- kenya_index %>% inner_join(nakuru_09, by="IPUM2009")

rm(nakuru_99,nakuru_09)

names(kenya_99)
names(kenya_09)

kenya_99 <- select(kenya_99, -c(DIST1999))
kenya_09 <- select(kenya_09, -c(DIST2009))


##Merging all years into one table
kenya_full <- rbind(kenya_99,kenya_09)
rm(kenya_99,kenya_09)

kenya_index <- kenya_full[,c("SERIAL_YEAR","CITY")]
kenya_index <- kenya_index[!duplicated(kenya_index$SERIAL_YEAR),]

rm(ddi_ken,kenya_full)

kenya <- kenya %>% left_join(kenya_index,by="SERIAL_YEAR")
rm(kenya_index)

names(kenya)
table(kenya$CITY)

######coding variables kenya

kenya$owner_b <- ifelse(kenya$OWNERSHIP ==1,1,0)
kenya$eletric_b <- ifelse(kenya$ELECTRIC ==1,1,0)
kenya$fuelc_b <- ifelse(kenya$FUELCOOK ==20|kenya$FUELCOOK ==30,1,0)
kenya$wall_b <- ifelse(kenya$WALL ==507,1,0)
kenya$roof_b <- ifelse(kenya$ROOF == 11|kenya$ROOF == 14,1,0)
kenya$floor_b <- ifelse(kenya$FLOOR != 100,1,0)
kenya$sewage_b <- ifelse(kenya$SEWAGE ==11|kenya$SEWAGE ==12,1,0)

##########################################################calculating number of people per household

kenya <- kenya %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(kenya)

#######aggregating by household

table(kenya$HHWT)

kenya_HH_b <- kenya %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,fuelc_b,wall_b,roof_b,floor_b,sewage_b),.funs = c("max"))

names(kenya_HH_b)

kenya_HH_n <- kenya %>% group_by(SERIAL_YEAR) %>%
  count()

kenya <- as.data.table(kenya)
kenya_HH_r <- kenya[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
kenya_HH_rn <- full_join(kenya_HH_n,kenya_HH_r)
kenya_HH_rn$room_b <- kenya_HH_rn$n/kenya_HH_rn$V1
kenya_HH_rn <- kenya_HH_rn[,c(1,4)]
kenya_HH_b<-as.data.frame(kenya_HH_b)
kenya_HH_b <- full_join(kenya_HH_b,kenya_HH_rn)

rm(kenya_HH_rn,kenya_HH_r,kenya_HH_n)

names(kenya_HH_b)

table(kenya_HH_b$YEAR)

kenya_HH_b <- expandRows(kenya_HH_b, 'HHWT')

kenya_HH_b.pca <- prcomp(kenya_HH_b[,c(4:11)])
summary(kenya_HH_b.pca)                                           
str(kenya_HH_b.pca)   

fviz_eig(kenya_HH_b.pca)

kenya_eig <- get_eigenvalue(kenya_HH_b.pca)

kenya_HH_PC<-cbind(kenya_HH_b,kenya_HH_b.pca$x[,1:3])
names(kenya_HH_PC)
sd(kenya_HH_PC$PC1)
kenya_inq_index <- kenya_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
kenya_inq_index$eign <- kenya_eig [1,1]
kenya_inq_index$I <- kenya_inq_index$PC1sd/(kenya_inq_index$eign)^0.5
kenya_inq_index$city <- "kenya"
hist(kenya_HH_PC$PC1)

###################################for nakuru

nakuru_HH_b <- kenya_HH_b %>% filter(kenya_HH_b$CITY=="nakuru")
names(nakuru_HH_b)

nakuru_HH_b.pca <- prcomp(nakuru_HH_b[,c(4:11)])
summary(nakuru_HH_b.pca)                                           
str(nakuru_HH_b.pca)   

fviz_eig(nakuru_HH_b.pca)

nakuru_eig <- get_eigenvalue(nakuru_HH_b.pca)

nakuru_HH_PC<-cbind(nakuru_HH_b,nakuru_HH_b.pca$x[,1:3])

sd(nakuru_HH_PC$PC1)

nakuru_inq_index <- nakuru_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
nakuru_inq_index$eign <- nakuru_eig [1,1]
nakuru_inq_index$I <- nakuru_inq_index$PC1sd/(nakuru_inq_index$eign)^0.5
nakuru_inq_index$city <- "nakuru"
hist(nakuru_HH_PC$PC1)

ken_nak_plot <- nakuru_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = kenya_inq_index,color="red")+
  geom_point(data = kenya_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="Nakuru - Kenya")+
  theme_bw()

ken_nak_plot
save(kenya_inq_index,file="kenya_inq_index.Rda")
save(nakuru_inq_index,file="nakuru_inq_index.Rda")

