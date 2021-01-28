library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/Bangladesh")

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

dhaka_91 <- read.csv(file="dhaka_91.csv", header = TRUE)
dhaka_91['CITY']='dhaka'

dhaka_01 <- read.csv(file="dhaka_01.csv", header = TRUE)
dhaka_01['CITY']='dhaka'

dhaka_10 <- read.csv(file="dhaka_10.csv", header = TRUE)
dhaka_10['CITY']='dhaka'

rajshahi_91 <- read.csv(file="rajshahi_91.csv", header = TRUE)
rajshahi_91['CITY']='rajshahi'

rajshahi_01 <- read.csv(file="rajshahi_01.csv", header = TRUE)
rajshahi_01['CITY']='rajshahi'

rajshahi_10 <- read.csv(file="rajshahi_10.csv", header = TRUE)
rajshahi_10['CITY']='rajshahi'

geo_bangladesh_91 <- rbind(dhaka_91,rajshahi_91)
geo_bangladesh_01 <- rbind(dhaka_01,rajshahi_01)
geo_bangladesh_11 <- rbind(dhaka_10,rajshahi_10)


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00022.xml")
bangladesh <- read_ipums_micro(ddi)

##Creating field for join

bangladesh$IPUM1991 <- as.integer(bangladesh$GEO2_BD1991)
bangladesh$IPUM2001 <- as.integer(bangladesh$GEO2_BD2001)
bangladesh$IPUM2011 <- as.integer(bangladesh$GEO2_BD2011)

##Joining by year

bangladesh_91 <- bangladesh %>% inner_join(geo_bangladesh_91, by="IPUM1991")
bangladesh_01 <- bangladesh %>% inner_join(geo_bangladesh_01, by="IPUM2001")
bangladesh_11 <- bangladesh %>% inner_join(geo_bangladesh_11, by="IPUM2011")

names(bangladesh_91)
names(bangladesh_01)
names(bangladesh_11)

bangladesh_91 <- select(bangladesh_91, -c(ZILL1991))
bangladesh_01 <- select(bangladesh_01, -c(ZILL2001))
bangladesh_11 <- select(bangladesh_11, -c(ZILL2011))


##Merging all years into one table
bangladesh_full <- rbind(bangladesh_91,bangladesh_01,bangladesh_11)
names(bangladesh_full)

##Excluding specific columns for the unifeied dataset
bangladesh_full<- select(bangladesh_full, -c(GEO2_BD1991,GEO2_BD2001,GEO2_BD2011,IPUM1991,IPUM2001,IPUM2011))
table(bangladesh_full$CITY)
save(bangladesh_full,file="bangladesh_full.Rda")

