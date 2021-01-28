library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/Argentina")


##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

buenos_aires_91 <- read.csv(file="buenos_aires_91_geo2.csv", header = TRUE)
buenos_aires_91['CITY']='buenos aires'

buenos_aires_01 <- read.csv(file="buenos_aires_01_geo2.csv",header = TRUE)
buenos_aires_01['CITY']='buenos aires'

buenos_aires_10 <- read.csv(file="buenos_aires_10_GEO2.csv", header = TRUE)
buenos_aires_10['CITY']='buenos aires'

cordoba_91 <- read.csv(file="cordoba_91_geo2.csv", header = TRUE)
cordoba_91['CITY']='cordoba'

cordoba_01 <- read.csv(file="cordoba_01_geo2.csv", header = TRUE)
cordoba_01['CITY']='cordoba'

cordoba_10 <- read.csv(file="corodba_10_GEO2.csv",header = TRUE)
cordoba_10['CITY']='cordoba'

geo_argentina_91 <- rbind(buenos_aires_91,cordoba_91)
geo_argentina_01 <- rbind(buenos_aires_01,cordoba_01)
geo_argentina_10 <- rbind(buenos_aires_10,cordoba_10)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00021.xml")
argentina <- read_ipums_micro(ddi)

names(argentina)

##Creating field for join

argentina$IPUM1991 <- as.integer(argentina$GEO2_AR1991)
argentina$IPUM2001 <- as.integer(argentina$GEO2_AR2001)
argentina$IPUM2010 <- as.integer(argentina$GEO2_AR2010)

##Joining by year

argentina_91 <- argentina %>% inner_join(geo_argentina_91, by="IPUM1991")
argentina_01 <- argentina %>% inner_join(geo_argentina_01, by="IPUM2001")
argentina_10 <- argentina %>% inner_join(geo_argentina_10, by="IPUM2010")

names(argentina_91)
names(argentina_01)
names(argentina_10)

argentina_91 <- select(argentina_91, -c(DEPT1991))
argentina_01 <- select(argentina_01, -c(DEPT2001))
argentina_10 <- select(argentina_10, -c(DEPT2010))


##Merging all years into one table
argentina_full <- rbind(argentina_91,argentina_01,argentina_10)
names(argentina_full)

##Excluding specific columns for the unifeied dataset
argentina_full<- select(argentina_full, -c(GEO2_AR1991,GEO2_AR2001,GEO2_AR2010,IPUM1991,IPUM2001,IPUM2010))
table(argentina_full$CITY)
save(argentina_full,file="argentina_full.Rda")


