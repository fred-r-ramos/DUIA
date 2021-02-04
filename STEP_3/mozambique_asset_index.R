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

ddi_moz <-read_ipums_ddi("ipumsi_00086.xml")
mozambique <- read_ipums_micro(ddi_moz)

mozambique <- transform(mozambique,SERIAL_YEAR=paste0(SERIAL,YEAR))

names(mozambique)

mozambique_index <- mozambique [,c("SERIAL_YEAR","GEO2_MZ1997","GEO2_MZ2007")]

###########################################import areas

mozambique_index$IPUM1997 <- as.integer(mozambique_index$GEO2_MZ1997)
mozambique_index$IPUM2007 <- as.integer(mozambique_index$GEO2_MZ2007)

beira_97 <- read.csv(file="beira_97.csv",header = TRUE)
beira_97['CITY']='beira'

beira_07 <- read.csv(file="beira_07.csv",header = TRUE)
beira_07['CITY']='beira'

mozambique_97 <- mozambique_index %>% inner_join(beira_97, by="IPUM1997")
mozambique_07 <- mozambique_index %>% inner_join(beira_07, by="IPUM2007")

rm(beira_97,beira_07)

names(mozambique_97)
names(mozambique_07)

mozambique_97 <- select(mozambique_97, -c(DIST1997))
mozambique_07 <- select(mozambique_07, -c(DIST2007))

##Merging all years into one table
mozambique_full <- rbind(mozambique_97,mozambique_07)
rm(mozambique_97,mozambique_07)

mozambique_index <- mozambique_full[,c("SERIAL_YEAR","CITY")]
mozambique_index <- mozambique_index[!duplicated(mozambique_index$SERIAL_YEAR),]
rm(ddi_moz,mozambique_full)

mozambique <- mozambique %>% left_join(mozambique_index,by="SERIAL_YEAR")
rm(mozambique_index)

names(mozambique)
table(mozambique$CITY)

######coding variables mali

mozambique$toilet_b <- ifelse(mozambique$TOILET ==21,1,0)
mozambique$eletric_b <- ifelse(mozambique$ELECTRIC ==1,1,0)
mozambique$water_b <- ifelse(mozambique$WATSUP ==11,1,0)
mozambique$roof_b <- ifelse(mozambique$ROOF ==12|mozambique$ROOF ==14,1,0)

##########################################################calculating number of people per household

mozambique <- mozambique %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(mozambique)

#######aggregating by household

table(mozambique$HHWT)

mozambique <- mozambique %>% filter(HHWT != 0)

mozambique_HH_b <- mozambique %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,toilet_b,eletric_b,water_b,roof_b),.funs = c("max"))

names(mozambique_HH_b)

mozambique_HH_n <- mozambique %>% group_by(SERIAL_YEAR) %>%
  count()

mozambique <- as.data.table(mozambique)
mozambique_HH_r <- mozambique[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
mozambique_HH_rn <- full_join(mozambique_HH_n,mozambique_HH_r)
mozambique_HH_rn$room_b <- mozambique_HH_rn$n/mozambique_HH_rn$V1
mozambique_HH_rn <- mozambique_HH_rn[,c(1,4)]
mozambique_HH_b<-as.data.frame(mozambique_HH_b)
mozambique_HH_b <- full_join(mozambique_HH_b,mozambique_HH_rn)

rm(mozambique_HH_rn,mozambique_HH_r,mozambique_HH_n)

names(mozambique_HH_b)

mozambique_HH_b.pca <- prcomp(mozambique_HH_b[,c(5:9)])
summary(mozambique_HH_b.pca)                                           
str(mozambique_HH_b.pca)   

fviz_eig(mozambique_HH_b.pca)

mozambique_eig <- get_eigenvalue(mozambique_HH_b.pca)

mozambique_HH_PC<-cbind(mozambique_HH_b,mozambique_HH_b.pca$x[,1:3])
names(mozambique_HH_PC)
sd(mozambique_HH_PC$PC1)
mozambique_inq_index <- mozambique_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
mozambique_inq_index$eign <- mozambique_eig [1,1]
mozambique_inq_index$I <- mozambique_inq_index$PC1sd/(mozambique_inq_index$eign)^0.5
mozambique_inq_index$city <- "mozambique"
hist(mozambique_HH_PC$PC1)

table(mozambique$CITY)

beira_HH_b <- mozambique_HH_b %>% filter(mozambique_HH_b$CITY=="beira")

names(beira_HH_b)

beira_HH_b.pca <- prcomp(beira_HH_b[,c(5:9)])
summary(beira_HH_b.pca)                                           
str(beira_HH_b.pca)   

fviz_eig(beira_HH_b.pca)

beira_eig <- get_eigenvalue(beira_HH_b.pca)

beira_HH_PC<-cbind(beira_HH_b,(beira_HH_b.pca$x[,1:3]))

sd(beira_HH_PC$PC1)

beira_inq_index <- beira_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
beira_inq_index$eign <- beira_eig [1,1]
beira_inq_index$I <- beira_inq_index$PC1sd/(beira_inq_index$eign)^0.5
beira_inq_index$city <- "beira"
hist(beira_HH_PC$PC1)

moz_bei_plot <- beira_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = mozambique_inq_index,color="red")+
  geom_point(data = mozambique_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(title="beira - mozambique")+
  theme_bw()

moz_bei_plot
save(beira_inq_index,file="beira_inq_index.Rda")
save(mozambique_inq_index,file="mozambique_inq_index.Rda")
