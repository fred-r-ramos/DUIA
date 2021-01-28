library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/Brazil")

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

BH_91 <- read.csv(file="BH_91.csv", header = TRUE)
BH_91['CITY']='belo horizonte'

BH_00 <- read.csv(file="BH_00.csv", header = TRUE)
BH_00['CITY']='belo horizonte'

BH_10 <- read.csv(file="BH_10.csv", header = TRUE)
BH_10['CITY']='belo horizonte'

curitba_91 <- read.csv(file="curitiba_91.csv", header = TRUE)
curitba_91['CITY']='curitiba'

curitba_00 <- read.csv(file="curitiba_00.csv", header = TRUE)
curitba_00['CITY']='curitiba'

curitba_10 <- read.csv(file="curitiba_10.csv", header = TRUE)
curitba_10['CITY']='curitiba'

floripa_91 <- read.csv(file="floripa_91.csv", header = TRUE)
floripa_91['CITY']='florianopolis'

floripa_00 <- read.csv(file="floripa_00.csv", header = TRUE)
floripa_00['CITY']='florianopolis'

floripa_10 <- read.csv(file="floripa_10.csv", header = TRUE)
floripa_10['CITY']='florianopolis'

ilheus_91 <- read.csv(file="ilheus_91.csv", header = TRUE)
ilheus_91['CITY']='ilheus'

ilheus_00 <- read.csv(file="ilheus_00.csv", header = TRUE)
ilheus_00['CITY']='ilheus'

ilheus_10 <- read.csv(file="ilheus_10.csv", header = TRUE)
ilheus_10['CITY']='ilheus'

jequie_91 <- read.csv(file="jequie_91.csv", header = TRUE)
jequie_91['CITY']='jequie'

jequie_00 <- read.csv(file="jequie_00.csv", header = TRUE)
jequie_00['CITY']='jequie'

jequie_10 <- read.csv(file="jequie_10.csv", header = TRUE)
jequie_10['CITY']='jequie'

palmas_91 <- read.csv(file="palmas_91.csv", header = TRUE)
palmas_91['CITY']='palmas'

palmas_00 <- read.csv(file="palmas_00.csv", header = TRUE)
palmas_00['CITY']='palmas'

palmas_10 <- read.csv(file="palmas_10.csv", header = TRUE)
palmas_10['CITY']='palmas'

ribeirao_91 <- read.csv(file="ribeirao_91.csv", header = TRUE)
ribeirao_91['CITY']='ribeirao preto'

ribeirao_00 <- read.csv(file="ribeirao_00.csv", header = TRUE)
ribeirao_00['CITY']='ribeirao preto'

ribeirao_10 <- read.csv(file="ribeirao_10.csv", header = TRUE)
ribeirao_10['CITY']='ribeirao preto'

SP_91 <- read.csv(file="SP_91.csv", header = TRUE)
SP_91['CITY']='Sao Paulo'

SP_00 <- read.csv(file="SP_00.csv", header = TRUE)
SP_00['CITY']='Sao Paulo'

SP_10 <- read.csv(file="SP_10.csv", header = TRUE)
SP_10['CITY']='Sao Paulo'

geo_brazil_91 <- rbind(BH_91,floripa_91, ilheus_91,jequie_91,palmas_91,ribeirao_91,SP_91,curitba_91)
geo_brazil_00 <- rbind(BH_00,floripa_00, ilheus_00,jequie_00,palmas_00,ribeirao_00,SP_00,curitba_00)
geo_brazil_10 <- rbind(BH_10,floripa_10, ilheus_10,jequie_10,palmas_10,ribeirao_10,SP_10,curitba_10)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00024.xml")
brasil <- read_ipums_micro(ddi)

names(brasil)

##Creating field for join

brasil$IPUM1991 <- as.integer(brasil$GEO2_BR1991)
brasil$IPUM2000 <- as.integer(brasil$GEO2_BR2000)
brasil$IPUM2010 <- as.integer(brasil$GEO2_BR2010)

geo_brazil_91 <- brasil %>% inner_join(geo_brazil_91, by="IPUM1991")
geo_brazil_00 <- brasil %>% inner_join(geo_brazil_00, by="IPUM2000")
geo_brazil_10 <- brasil %>% inner_join(geo_brazil_10, by="IPUM2010")

names(geo_brazil_91)
names(geo_brazil_00)
names(geo_brazil_10)

geo_brazil_91 <- select(geo_brazil_91, -c(MUNI1991))
geo_brazil_00 <- select(geo_brazil_00, -c(MUNI2000))
geo_brazil_10 <- select(geo_brazil_10, -c(MUNI2010))


##Merging all years into one table
brazil_full <- rbind(geo_brazil_91,geo_brazil_00,geo_brazil_10)
names(brazil_full)

##Excluding specific columns for the unifeied dataset
brazil_full<- select(brazil_full, -c(GEO2_BR1991,GEO2_BR2000,GEO2_BR2010,IPUM1991,IPUM2000,IPUM2010))
table(brazil_full$CITY)
save(brazil_full,file="brazil_full.Rda")
