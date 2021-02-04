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

ddi_col <-read_ipums_ddi("ipumsi_00073.xml")
colombia <- read_ipums_micro(ddi_col)

colombia <- transform(colombia,SERIAL_YEAR=paste0(SERIAL,YEAR))

###########################################coding the variables

colombia$owner_b <- ifelse(colombia$OWNERSHIP ==1,1,0)
colombia$toilt_b <- ifelse(colombia$TOILET == 21,1,0)
colombia$elect_b <- ifelse(colombia$ELECTRIC==1,1,0)
colombia$water_b <- ifelse(colombia$WATSUP ==10,1,0)
colombia$fuel_b <- ifelse(colombia$FUELCOOK == 20 | colombia$FUELCOOK == 31|colombia$FUELCOOK == 30|colombia$FUELCOOK == 32,1,0)
colombia$wall_b <- ifelse(colombia$WALL==500|colombia$WALL==502,1,0)
colombia$phone_b <- ifelse(colombia$PHONE ==2,1,0)
colombia$kitchen_b <- ifelse(colombia$KITCHEN==20,1,0)
colombia$floors_b <- ifelse(colombia$FLOOR !=100,1,0)
colombia$trash_b <- ifelse(colombia$TRASH ==10,1,0)

##calculating number of people per household

colombia <- colombia %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(colombia)
colombia_HH_b <- colombia %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,HHWT,owner_b,toilt_b,elect_b,water_b,fuel_b,wall_b,phone_b,kitchen_b,floors_b,trash_b),.funs = c("max"))

names(colombia_HH_b)

colombia_HH_n <- colombia %>% group_by(SERIAL_YEAR) %>%
  count()

colombia <- as.data.table(colombia)
colombia_HH_r <- colombia[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
colombia_HH_rn <- full_join(colombia_HH_n,colombia_HH_r)
colombia_HH_rn$room_b <- colombia_HH_rn$n/colombia_HH_rn$V1
colombia_HH_rn <- colombia_HH_rn[,c(1,4)]
colombia_HH_b<-as.data.frame(colombia_HH_b)
colombia_HH_b <- full_join(colombia_HH_b,colombia_HH_rn)

names(colombia_HH_b)

table(colombia_HH_b$YEAR)

colombia_HH_b <- full_join(colombia_HH_b,colombia_HH_rn)
names(colombia_HH_b)
table(colombia_HH_b$HHWT)
colombia_HH_b <- expandRows(colombia_HH_b, 'HHWT')
names(colombia_HH_b)

###########################################calculating pca

colombia_HH_b.pca <- prcomp(colombia_HH_b[,c(3:13)])
summary(colombia_HH_b.pca)                                           
str(colombia_HH_b.pca)   

fviz_eig(colombia_HH_b.pca)

colombia_eig <- get_eigenvalue(colombia_HH_b.pca)

colombia_HH_PC<-cbind(colombia_HH_b,colombia_HH_b.pca$x[,1:3])

sd(colombia_HH_PC$PC1)

colombia_inq_index <- colombia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
colombia_inq_index$eign <- colombia_eig [1,1]
colombia_inq_index$I <- colombia_inq_index$PC1sd/(colombia_inq_index$eign)^0.5
colombia_inq_index$city <- "colombia"
hist(colombia_HH_PC$PC1)

save(colombia_inq_index, file="colombia_inq_index.Rda")

colombia$IPUM1993 <- as.integer(colombia$GEO2_CO1993)
colombia$IPUM2005 <- as.integer(colombia$GEO2_CO2005)

bogota_93 <- read.csv(file="bogota_93.csv", header = TRUE)
bogota_93['CITY']='bogota'

bogota_05 <- read.csv(file="bogota_05.csv", header = TRUE)
bogota_05['CITY']='bogota'

valledupar_93 <- read.csv(file="valledupar_93.csv", header = TRUE)
valledupar_93['CITY']='valledupar'

valledupar_05 <- read.csv(file="valledupar_05.csv", header = TRUE)
valledupar_05['CITY']='valledupar'

geo_colombia_93 <- rbind(bogota_93,valledupar_93)
geo_colombia_05 <- rbind(bogota_05,valledupar_05)

##Joining by year

colombia_93 <- colombia %>% inner_join(geo_colombia_93, by="IPUM1993")
colombia_05 <- colombia %>% inner_join(geo_colombia_05, by="IPUM2005")

names(colombia_93)
names(colombia_05)

colombia_93 <- select(colombia_93, -c(MUNI1993))
colombia_05 <- select(colombia_05, -c(MUNI2005))

##Merging all years into one table
colombia_full <- rbind(colombia_93,colombia_05)
names(colombia_full)

colombia_full <- colombia_full %>% inner_join(colombia_HH_rn,by="SERIAL_YEAR")

##########for Bogota

bogota_HH_b <- colombia_full %>% filter(colombia_full$CITY=="bogota")
names(bogota_HH_b)
bogota_HH_b <- bogota_HH_b[!duplicated(bogota_HH_b$SERIAL_YEAR),]
bogota_HH_b <- select (bogota_HH_b,-c(1,3,4,6:32,43:49))
bogota_HH_b <- bogota_HH_b[,c(1,2,13,3:12,14)]
bogota_HH_b.pca <- prcomp(bogota_HH_b[,c(4:13)])
summary(bogota_HH_b.pca)                                           
str(bogota_HH_b.pca)   

fviz_eig(bogota_HH_b.pca)

bogota_eig <- get_eigenvalue(bogota_HH_b.pca)

bogota_HH_PC<-cbind(bogota_HH_b,bogota_HH_b.pca$x[,1:3])

sd(bogota_HH_PC$PC1)

bogota_inq_index <- bogota_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bogota_inq_index$eign <- bogota_eig [1,1]
bogota_inq_index$I <- bogota_inq_index$PC1sd/(bogota_inq_index$eign)^0.5
bogota_inq_index$city <- "Bogota"
hist(bogota_HH_PC$PC1)
save(bogota_inq_index,file="bogota_inq_index.Rda")

##########for valledupar

valledupar_HH_b <- colombia_full %>% filter(colombia_full$CITY=="valledupar")
names(valledupar_HH_b)
valledupar_HH_b <- valledupar_HH_b[!duplicated(valledupar_HH_b$SERIAL_YEAR),]
valledupar_HH_b <- select (valledupar_HH_b,-c(1,3,4,6:32,43:49))
valledupar_HH_b <- valledupar_HH_b[,c(1,2,13,3:12,14)]
valledupar_HH_b.pca <- prcomp(valledupar_HH_b[,c(4:13)])
summary(valledupar_HH_b.pca)                                           
str(valledupar_HH_b.pca)   

fviz_eig(valledupar_HH_b.pca)

valledupar_eig <- get_eigenvalue(valledupar_HH_b.pca)

valledupar_HH_PC<-cbind(valledupar_HH_b,valledupar_HH_b.pca$x[,1:3])

sd(valledupar_HH_PC$PC1)

valledupar_inq_index <- valledupar_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
valledupar_inq_index$eign <- valledupar_eig [1,1]
valledupar_inq_index$I <- valledupar_inq_index$PC1sd/(valledupar_inq_index$eign)^0.5
valledupar_inq_index$city <- "valledupar"
hist(valledupar_HH_PC$PC1)

save(valledupar_inq_index,file="valledupar_inq_index.Rda")

I_col_asset_Index <- rbind(bogota_inq_index,valledupar_inq_index)
names(I_col_asset_Index)
col_plot <- I_col_asset_Index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~city) +
  geom_line()+
  geom_point() + theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()

##########graph for cities and country

col_bog_plot <- bogota_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = colombia_inq_index,color="red")+
  geom_point(data = colombia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()
col_val_plot<- valledupar_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = colombia_inq_index,color="red")+
  geom_point(data = colombia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  theme_bw()

grid.arrange(col_bog_plot,col_val_plot , ncol=2)
