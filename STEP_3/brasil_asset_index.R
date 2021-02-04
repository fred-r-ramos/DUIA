library(ipumsr)
library(dbplyr)
library(tidyverse)
library(factoextra)
library(data.table)
library(splitstackshape)
library(ggplot2)
library(gridExtra)
library(RSQLite)
library(DBI)

getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/household_asset_index/inequality_index")

###########################################calculating for brasil
###########################################import IPUMS


ddi_bra <-read_ipums_ddi("ipumsi_00071.xml")
brasil <- read_ipums_micro(ddi_bra)

brasil <- transform(brasil,SERIAL_YEAR=paste0(SERIAL,YEAR))

brasil_index <- brasil [,c("SERIAL_YEAR","GEO2_BR2000","GEO2_BR2010")]

###########################################import areas

brasil_index$IPUM2000 <- as.integer(brasil_index$GEO2_BR2000)
brasil_index$IPUM2010 <- as.integer(brasil_index$GEO2_BR2010)

BH_00 <- read.csv(file="BH_00.csv", header = TRUE)
BH_00['CITY']='belo horizonte'

BH_10 <- read.csv(file="BH_10.csv", header = TRUE)
BH_10['CITY']='belo horizonte'

curitba_00 <- read.csv(file="curitiba_00.csv", header = TRUE)
curitba_00['CITY']='curitiba'

curitba_10 <- read.csv(file="curitiba_10.csv", header = TRUE)
curitba_10['CITY']='curitiba'

floripa_00 <- read.csv(file="floripa_00.csv", header = TRUE)
floripa_00['CITY']='florianopolis'

floripa_10 <- read.csv(file="floripa_10.csv", header = TRUE)
floripa_10['CITY']='florianopolis'

ilheus_00 <- read.csv(file="ilheus_00.csv", header = TRUE)
ilheus_00['CITY']='ilheus'

ilheus_10 <- read.csv(file="ilheus_10.csv", header = TRUE)
ilheus_10['CITY']='ilheus'

jequie_00 <- read.csv(file="jequie_00.csv", header = TRUE)
jequie_00['CITY']='jequie'

jequie_10 <- read.csv(file="jequie_10.csv", header = TRUE)
jequie_10['CITY']='jequie'

palmas_00 <- read.csv(file="palmas_00.csv", header = TRUE)
palmas_00['CITY']='palmas'

palmas_10 <- read.csv(file="palmas_10.csv", header = TRUE)
palmas_10['CITY']='palmas'

ribeirao_00 <- read.csv(file="ribeirao_00.csv", header = TRUE)
ribeirao_00['CITY']='ribeirao preto'

ribeirao_10 <- read.csv(file="ribeirao_10.csv", header = TRUE)
ribeirao_10['CITY']='ribeirao preto'

SP_00 <- read.csv(file="SP_00.csv", header = TRUE)
SP_00['CITY']='Sao Paulo'

SP_10 <- read.csv(file="SP_10.csv", header = TRUE)
SP_10['CITY']='Sao Paulo'

geo_brazil_00 <- rbind(BH_00,floripa_00, ilheus_00,jequie_00,palmas_00,ribeirao_00,SP_00,curitba_00)
geo_brazil_10 <- rbind(BH_10,floripa_10, ilheus_10,jequie_10,palmas_10,ribeirao_10,SP_10,curitba_10)


##Joining by year

brasil_00 <- brasil_index %>% inner_join(geo_brazil_00, by="IPUM2000")
brasil_10 <- brasil_index %>% inner_join(geo_brazil_10, by="IPUM2010")

names(brasil_00)
names(brasil_10)

rm(BH_00,floripa_00, ilheus_00,jequie_00,palmas_00,ribeirao_00,SP_00,curitba_00,BH_10,floripa_10, ilheus_10,jequie_10,palmas_10,ribeirao_10,SP_10,curitba_10)
gc()

brasil_00 <- select(brasil_00, -c(MUNI2000))
brasil_10 <- select(brasil_10, -c(MUNI2010))

##Merging all years into one table
brasil_full <- rbind(brasil_00,brasil_10)
rm(brasil_00,brasil_10)

brasil_index <- brasil_full[,c("SERIAL_YEAR","CITY")]
brasil_index <- brasil_index[!duplicated(brasil_index$SERIAL_YEAR),]

rm(ddi_bra,brasil_full,geo_brazil_00,geo_brazil_10)

brasil <- brasil %>% left_join(brasil_index,by="SERIAL_YEAR")
rm(brasil_index)

names(brasil)
table(brasil$CITY)

######coding variables brazil

brasil$owner_b <- ifelse(brasil$OWNERSHIP ==1,1,0)
brasil$eletric_b <- ifelse(brasil$ELECTRIC ==1,1,0)
brasil$water_b <- ifelse(brasil$WATSUP ==11,1,0)
brasil$phone_b <- ifelse(brasil$PHONE ==2,1,0)
brasil$tv_b <- ifelse(brasil$TV == 21|brasil$TV==29|brasil$TV==20,1,0)
brasil$sewage_b <- ifelse(brasil$SEWAGE ==11|brasil$SEWAGE ==12,1,0)
brasil$radio_b <- ifelse(brasil$RADIO ==2,1,0)
brasil$refrig_b <- ifelse(brasil$REFRIG == 2,1,0)
brasil$auto_b <- ifelse(brasil$AUTOS %in% 1:7,1,0)
brasil$bath_b <-ifelse(brasil$BATH == 2,1,0)
brasil$trash_b<- ifelse(brasil$TRASH==11|brasil$TRASH==12,1,0)
brasil$computer_b <- ifelse(brasil$COMPUTER == 2,1,0)
brasil$washer_b <- ifelse(brasil$WASHER == 2,1,0)

brasil <- select (brasil,-c(OWNERSHIP,ELECTRIC,WATSUP,PHONE,TV,SEWAGE,RADIO,REFRIG,AUTOS,BATH,TRASH,COMPUTER,WASHER))
gc()
table(brasil$CITY)
table(brasil$YEAR)
######################creating for cities

sp <- brasil %>% filter(CITY=="Sao Paulo")
bh <- brasil %>% filter(CITY=="belo horizonte")
curitiba <- brasil %>%  filter (CITY=="curitiba")
florianopolis <- brasil %>% filter (CITY == "florianopolis")
ilheus <- brasil %>%  filter(CITY=="ilheus")
jequie <- brasil %>%  filter(CITY=="jequie")
palmas <- brasil %>% filter (CITY == "palmas")
ribeirao_preto <- brasil %>% filter (CITY =="ribeirao preto")

##########################################################calculating number of people per household

table(sp$ROOMS)

sp <- sp %>% filter(ROOMS != 99)

#######aggregating by household

gc()
table(sp$HHWT)
names(sp)

sp_HH_b <- sp %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(sp_HH_b)

sp_HH_n <- sp %>% group_by(SERIAL_YEAR) %>%
  count()

sp <- as.data.table(sp)
sp_HH_r <- sp[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
sp_HH_rn <- full_join(sp_HH_n,sp_HH_r)
sp_HH_rn$room_b <- sp_HH_rn$n/sp_HH_rn$V1
sp_HH_rn <- sp_HH_rn[,c(1,4)]
sp_HH_b<-as.data.frame(sp_HH_b)
sp_HH_b <- full_join(sp_HH_b,sp_HH_rn)

rm(sp_HH_n,sp_HH_r,sp_HH_rn)

names(sp_HH_b)

table(sp_HH_b$YEAR)

sp_HH_b <- expandRows(sp_HH_b,'HHWT')
sp_HH_bs <- sp_HH_b %>% sample_frac(0.2)
sp_HH_bs.pca <- prcomp(sp_HH_bs[,c(4:17)])
summary(sp_HH_bs.pca)                                           
str(sp_HH_bs.pca)   

fviz_eig(sp_HH_bs.pca)

sp_eig <- get_eigenvalue(sp_HH_bs.pca)

sp_HH_PC<-cbind(sp_HH_bs,sp_HH_bs.pca$x[,1:3])

sd(sp_HH_PC$PC1)

sp_inq_index <- sp_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
sp_inq_index$eign <- sp_eig [1,1]
sp_inq_index$I <- sp_inq_index$PC1sd/(sp_inq_index$eign)^0.5
sp_inq_index$city <- "sp"
hist(sp_HH_PC$PC1)

save(sp_inq_index,file="sp_inq_index.Rda")

table(bh$ROOMS)

bh <- bh %>% filter(ROOMS != 99)

#######aggregating by household

gc()
table(bh$HHWT)
names(bh)

bh_HH_b <- bh %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(bh_HH_b)

bh_HH_n <- bh %>% group_by(SERIAL_YEAR) %>%
  count()

bh <- as.data.table(bh)
bh_HH_r <- bh[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
bh_HH_rn <- full_join(bh_HH_n,bh_HH_r)
bh_HH_rn$room_b <- bh_HH_rn$n/bh_HH_rn$V1
bh_HH_rn <- bh_HH_rn[,c(1,4)]
bh_HH_b<-as.data.frame(bh_HH_b)
bh_HH_b <- full_join(bh_HH_b,bh_HH_rn)

rm(bh_HH_n,bh_HH_r,bh_HH_rn)

names(bh_HH_b)

table(bh_HH_b$YEAR)

bh_HH_b <- expandRows(bh_HH_b,'HHWT')

bh_HH_b.pca <- prcomp(bh_HH_b[,c(4:17)])
summary(bh_HH_b.pca)                                           
str(bh_HH_b.pca)   

fviz_eig(bh_HH_b.pca)

bh_eig <- get_eigenvalue(bh_HH_b.pca)

bh_HH_PC<-cbind(bh_HH_b,bh_HH_b.pca$x[,1:3])

sd(bh_HH_PC$PC1)

bh_inq_index <- bh_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bh_inq_index$eign <- bh_eig [1,1]
bh_inq_index$I <- bh_inq_index$PC1sd/(bh_inq_index$eign)^0.5
bh_inq_index$city <- "bh"
hist(bh_HH_PC$PC1)

save(bh_inq_index,file="bh_inq_index.Rda")

table(ilheus$ROOMS)

ilheus <- ilheus %>% filter(ROOMS != 99)

#######aggregating by household

gc()
table(ilheus$HHWT)
names(ilheus)

ilheus_HH_b <- ilheus %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(ilheus_HH_b)

ilheus_HH_n <- ilheus %>% group_by(SERIAL_YEAR) %>%
  count()

ilheus <- as.data.table(ilheus)
ilheus_HH_r <- ilheus[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
ilheus_HH_rn <- full_join(ilheus_HH_n,ilheus_HH_r)
ilheus_HH_rn$room_b <- ilheus_HH_rn$n/ilheus_HH_rn$V1
ilheus_HH_rn <- ilheus_HH_rn[,c(1,4)]
ilheus_HH_b<-as.data.frame(ilheus_HH_b)
ilheus_HH_b <- full_join(ilheus_HH_b,ilheus_HH_rn)

rm(ilheus_HH_n,ilheus_HH_r,ilheus_HH_rn)

names(ilheus_HH_b)

table(ilheus_HH_b$YEAR)

ilheus_HH_b <- expandRows(ilheus_HH_b,'HHWT')

ilheus_HH_b.pca <- prcomp(ilheus_HH_b[,c(4:17)])
summary(ilheus_HH_b.pca)                                           
str(ilheus_HH_b.pca)   

fviz_eig(ilheus_HH_b.pca)

ilheus_eig <- get_eigenvalue(ilheus_HH_b.pca)

ilheus_HH_PC<-cbind(ilheus_HH_b,ilheus_HH_b.pca$x[,1:3])

sd(ilheus_HH_PC$PC1)

ilheus_inq_index <- ilheus_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ilheus_inq_index$eign <- ilheus_eig [1,1]
ilheus_inq_index$I <- ilheus_inq_index$PC1sd/(ilheus_inq_index$eign)^0.5
ilheus_inq_index$city <- "ilheus"
hist(ilheus_HH_PC$PC1)

save(ilheus_inq_index,file="ilheus_inq_index.Rda")

#######for florianopolis

table(florianopolis$ROOMS)

florianopolis <- florianopolis %>% filter(ROOMS != 99)



gc()
table(florianopolis$HHWT)
names(florianopolis)

florianopolis_HH_b <- florianopolis %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(florianopolis_HH_b)

florianopolis_HH_n <- florianopolis %>% group_by(SERIAL_YEAR) %>%
  count()

florianopolis <- as.data.table(florianopolis)
florianopolis_HH_r <- florianopolis[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
florianopolis_HH_rn <- full_join(florianopolis_HH_n,florianopolis_HH_r)
florianopolis_HH_rn$room_b <- florianopolis_HH_rn$n/florianopolis_HH_rn$V1
florianopolis_HH_rn <- florianopolis_HH_rn[,c(1,4)]
florianopolis_HH_b<-as.data.frame(florianopolis_HH_b)
florianopolis_HH_b <- full_join(florianopolis_HH_b,florianopolis_HH_rn)

rm(florianopolis_HH_n,florianopolis_HH_r,florianopolis_HH_rn)

names(florianopolis_HH_b)

table(florianopolis_HH_b$YEAR)

florianopolis_HH_b <- expandRows(florianopolis_HH_b,'HHWT')

florianopolis_HH_b.pca <- prcomp(florianopolis_HH_b[,c(4:17)])
summary(florianopolis_HH_b.pca)                                           
str(florianopolis_HH_b.pca)   

fviz_eig(florianopolis_HH_b.pca)

florianopolis_eig <- get_eigenvalue(florianopolis_HH_b.pca)

florianopolis_HH_PC<-cbind(florianopolis_HH_b,florianopolis_HH_b.pca$x[,1:3])

sd(florianopolis_HH_PC$PC1)

florianopolis_inq_index <- florianopolis_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
florianopolis_inq_index$eign <- florianopolis_eig [1,1]
florianopolis_inq_index$I <- florianopolis_inq_index$PC1sd/(florianopolis_inq_index$eign)^0.5
florianopolis_inq_index$city <- "florianopolis"
hist(florianopolis_HH_PC$PC1)

save(florianopolis_inq_index,file="florianopolis_inq_index.Rda")

#######for jequie

table(jequie$ROOMS)

jequie <- jequie %>% filter(ROOMS != 99)



gc()
table(jequie$HHWT)
names(jequie)

jequie_HH_b <- jequie %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(jequie_HH_b)

jequie_HH_n <- jequie %>% group_by(SERIAL_YEAR) %>%
  count()

jequie <- as.data.table(jequie)
jequie_HH_r <- jequie[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
jequie_HH_rn <- full_join(jequie_HH_n,jequie_HH_r)
jequie_HH_rn$room_b <- jequie_HH_rn$n/jequie_HH_rn$V1
jequie_HH_rn <- jequie_HH_rn[,c(1,4)]
jequie_HH_b<-as.data.frame(jequie_HH_b)
jequie_HH_b <- full_join(jequie_HH_b,jequie_HH_rn)

rm(jequie_HH_n,jequie_HH_r,jequie_HH_rn)

names(jequie_HH_b)

table(jequie_HH_b$YEAR)

jequie_HH_b <- expandRows(jequie_HH_b,'HHWT')

jequie_HH_b.pca <- prcomp(jequie_HH_b[,c(4:17)])
summary(jequie_HH_b.pca)                                           
str(jequie_HH_b.pca)   

fviz_eig(jequie_HH_b.pca)

jequie_eig <- get_eigenvalue(jequie_HH_b.pca)

jequie_HH_PC<-cbind(jequie_HH_b,jequie_HH_b.pca$x[,1:3])

sd(jequie_HH_PC$PC1)

jequie_inq_index <- jequie_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
jequie_inq_index$eign <- jequie_eig [1,1]
jequie_inq_index$I <- jequie_inq_index$PC1sd/(jequie_inq_index$eign)^0.5
jequie_inq_index$city <- "jequie"
hist(jequie_HH_PC$PC1)

save(jequie_inq_index,file="jequie_inq_index.Rda")

#######for palmas

table(palmas$ROOMS)

palmas <- palmas %>% filter(ROOMS != 99)



gc()
table(palmas$HHWT)
names(palmas)

palmas_HH_b <- palmas %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(palmas_HH_b)

palmas_HH_n <- palmas %>% group_by(SERIAL_YEAR) %>%
  count()

palmas <- as.data.table(palmas)
palmas_HH_r <- palmas[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
palmas_HH_rn <- full_join(palmas_HH_n,palmas_HH_r)
palmas_HH_rn$room_b <- palmas_HH_rn$n/palmas_HH_rn$V1
palmas_HH_rn <- palmas_HH_rn[,c(1,4)]
palmas_HH_b<-as.data.frame(palmas_HH_b)
palmas_HH_b <- full_join(palmas_HH_b,palmas_HH_rn)

rm(palmas_HH_n,palmas_HH_r,palmas_HH_rn)

names(palmas_HH_b)

table(palmas_HH_b$YEAR)

palmas_HH_b <- expandRows(palmas_HH_b,'HHWT')

palmas_HH_b.pca <- prcomp(palmas_HH_b[,c(4:17)])
summary(palmas_HH_b.pca)                                           
str(palmas_HH_b.pca)   

fviz_eig(palmas_HH_b.pca)

palmas_eig <- get_eigenvalue(palmas_HH_b.pca)

palmas_HH_PC<-cbind(palmas_HH_b,palmas_HH_b.pca$x[,1:3])

sd(palmas_HH_PC$PC1)

palmas_inq_index <- palmas_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
palmas_inq_index$eign <- palmas_eig [1,1]
palmas_inq_index$I <- palmas_inq_index$PC1sd/(palmas_inq_index$eign)^0.5
palmas_inq_index$city <- "palmas"
hist(palmas_HH_PC$PC1)

save(palmas_inq_index,file="palmas_inq_index.Rda")

#######for curitiba

table(curitiba$ROOMS)

curitiba <- curitiba %>% filter(ROOMS != 99)



gc()
table(curitiba$HHWT)
names(curitiba)

curitiba_HH_b <- curitiba %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(curitiba_HH_b)

curitiba_HH_n <- curitiba %>% group_by(SERIAL_YEAR) %>%
  count()

curitiba <- as.data.table(curitiba)
curitiba_HH_r <- curitiba[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
curitiba_HH_rn <- full_join(curitiba_HH_n,curitiba_HH_r)
curitiba_HH_rn$room_b <- curitiba_HH_rn$n/curitiba_HH_rn$V1
curitiba_HH_rn <- curitiba_HH_rn[,c(1,4)]
curitiba_HH_b<-as.data.frame(curitiba_HH_b)
curitiba_HH_b <- full_join(curitiba_HH_b,curitiba_HH_rn)

rm(curitiba_HH_n,curitiba_HH_r,curitiba_HH_rn)

names(curitiba_HH_b)

table(curitiba_HH_b$YEAR)

curitiba_HH_b <- expandRows(curitiba_HH_b,'HHWT')

curitiba_HH_b.pca <- prcomp(curitiba_HH_b[,c(4:17)])
summary(curitiba_HH_b.pca)                                           
str(curitiba_HH_b.pca)   

fviz_eig(curitiba_HH_b.pca)

curitiba_eig <- get_eigenvalue(curitiba_HH_b.pca)

curitiba_HH_PC<-cbind(curitiba_HH_b,curitiba_HH_b.pca$x[,1:3])

sd(curitiba_HH_PC$PC1)

curitiba_inq_index <- curitiba_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
curitiba_inq_index$eign <- curitiba_eig [1,1]
curitiba_inq_index$I <- curitiba_inq_index$PC1sd/(curitiba_inq_index$eign)^0.5
curitiba_inq_index$city <- "curitiba"
hist(curitiba_HH_PC$PC1)

save(curitiba_inq_index,file="curitiba_inq_index.Rda")

#######for ribeirao_preto

table(ribeirao_preto$ROOMS)

ribeirao_preto <- ribeirao_preto %>% filter(ROOMS != 99)



gc()
table(ribeirao_preto$HHWT)
names(ribeirao_preto)

ribeirao_preto_HH_b <- ribeirao_preto %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,CITY,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(ribeirao_preto_HH_b)

ribeirao_preto_HH_n <- ribeirao_preto %>% group_by(SERIAL_YEAR) %>%
  count()

ribeirao_preto <- as.data.table(ribeirao_preto)
ribeirao_preto_HH_r <- ribeirao_preto[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
ribeirao_preto_HH_rn <- full_join(ribeirao_preto_HH_n,ribeirao_preto_HH_r)
ribeirao_preto_HH_rn$room_b <- ribeirao_preto_HH_rn$n/ribeirao_preto_HH_rn$V1
ribeirao_preto_HH_rn <- ribeirao_preto_HH_rn[,c(1,4)]
ribeirao_preto_HH_b<-as.data.frame(ribeirao_preto_HH_b)
ribeirao_preto_HH_b <- full_join(ribeirao_preto_HH_b,ribeirao_preto_HH_rn)

rm(ribeirao_preto_HH_n,ribeirao_preto_HH_r,ribeirao_preto_HH_rn)

names(ribeirao_preto_HH_b)

table(ribeirao_preto_HH_b$YEAR)

ribeirao_preto_HH_b <- expandRows(ribeirao_preto_HH_b,'HHWT')

ribeirao_preto_HH_b.pca <- prcomp(ribeirao_preto_HH_b[,c(4:17)])
summary(ribeirao_preto_HH_b.pca)                                           
str(ribeirao_preto_HH_b.pca)   

fviz_eig(ribeirao_preto_HH_b.pca)

ribeirao_preto_eig <- get_eigenvalue(ribeirao_preto_HH_b.pca)

ribeirao_preto_HH_PC<-cbind(ribeirao_preto_HH_b,ribeirao_preto_HH_b.pca$x[,1:3])

sd(ribeirao_preto_HH_PC$PC1)

ribeirao_preto_inq_index <- ribeirao_preto_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
ribeirao_preto_inq_index$eign <- ribeirao_preto_eig [1,1]
ribeirao_preto_inq_index$I <- ribeirao_preto_inq_index$PC1sd/(ribeirao_preto_inq_index$eign)^0.5
ribeirao_preto_inq_index$city <- "ribeirao_preto"
hist(ribeirao_preto_HH_PC$PC1)

save(ribeirao_preto_inq_index,file="ribeirao_preto_inq_index.Rda")

###########################################calculating for brasil
###########################################import IPUMS


ddi_bras <-read_ipums_ddi("ipumsi_00101.xml")
brasils <- read_ipums_micro(ddi_bras)

brasils <- transform(brasils,SERIAL_YEAR=paste0(SERIAL,YEAR))

brasils$owner_b <- ifelse(brasils$OWNERSHIP ==1,1,0)
brasils$eletric_b <- ifelse(brasils$ELECTRIC ==1,1,0)
brasils$water_b <- ifelse(brasils$WATSUP ==11,1,0)
brasils$phone_b <- ifelse(brasils$PHONE ==2,1,0)
brasils$tv_b <- ifelse(brasils$TV == 21|brasils$TV==29|brasils$TV==20,1,0)
brasils$sewage_b <- ifelse(brasils$SEWAGE ==11|brasils$SEWAGE ==12,1,0)
brasils$radio_b <- ifelse(brasils$RADIO ==2,1,0)
brasils$refrig_b <- ifelse(brasils$REFRIG == 2,1,0)
brasils$auto_b <- ifelse(brasils$AUTOS %in% 1:7,1,0)
brasils$bath_b <-ifelse(brasils$BATH == 2,1,0)
brasils$trash_b<- ifelse(brasils$TRASH==11|brasils$TRASH==12,1,0)
brasils$computer_b <- ifelse(brasils$COMPUTER == 2,1,0)
brasils$washer_b <- ifelse(brasils$WASHER == 2,1,0)

table(brasils$HHWT)
names(brasils)

brasils_HH_b <- brasils %>% group_by(SERIAL_YEAR) %>% 
  summarise_at(.vars = vars(YEAR,HHWT,owner_b,eletric_b,water_b,phone_b,tv_b,sewage_b,radio_b,refrig_b,auto_b,bath_b,trash_b,computer_b,washer_b),.funs = c("max"))

names(brasils_HH_b)

brasils_HH_n <- brasils %>% group_by(SERIAL_YEAR) %>%
  count()

brasils <- as.data.table(brasils)
brasils_HH_r <- brasils[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
brasils_HH_rn <- full_join(brasils_HH_n,brasils_HH_r)
brasils_HH_rn$room_b <- brasils_HH_rn$n/brasils_HH_rn$V1
brasils_HH_rn <- brasils_HH_rn[,c(1,4)]
brasils_HH_b<-as.data.frame(brasils_HH_b)
brasils_HH_b <- full_join(brasils_HH_b,brasils_HH_rn)

rm(brasils_HH_n,brasils_HH_r,brasils_HH_rn)

names(brasils_HH_b)

table(brasils_HH_b$YEAR)
gc()

brasils_HH_b$HHWTs <- brasils_HH_b$HHWT/10
brasils_HH_b <- expandRows(brasils_HH_b,'HHWTs')

brasils_HH_b.pca <- prcomp(brasils_HH_b[,c(4:17)])
summary(brasils_HH_b.pca)                                           
str(brasils_HH_b.pca)   

fviz_eig(brasils_HH_b.pca)

brasils_eig <- get_eigenvalue(brasils_HH_b.pca)

brasils_HH_PC<-cbind(brasils_HH_b,brasils_HH_b.pca$x[,1:3])

sd(brasils_HH_PC$PC1)

brasils_inq_index <- brasils_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
brasils_inq_index$eign <- brasils_eig [1,1]
brasils_inq_index$I <- brasils_inq_index$PC1sd/(brasils_inq_index$eign)^0.5
brasils_inq_index$city <- "brasils"
hist(brasils_HH_PC$PC1)

save(brasils_inq_index,file="brasils_inq_index.Rda")
