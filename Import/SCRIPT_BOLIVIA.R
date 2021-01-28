library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/Bolivia")


##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

cochabamba_92 <- read.csv(file="cochabamba_92.csv", header = TRUE)
cochabamba_92['CITY']='cochabamba'

cochabamba_01 <- read.csv(file="cochabamba_01.csv",header = TRUE)
cochabamba_01['CITY']='cochabamba'


##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00023.xml")
bolivia <- read_ipums_micro(ddi)

names(bolivia)

##Creating field for join

bolivia$IPUM1992 <- as.integer(bolivia$GEO2_BO1992)
bolivia$IPUM2001 <- as.integer(bolivia$GEO2_BO2001)

##Joining by year

cochabamba_92 <- bolivia %>% inner_join(cochabamba_92, by="IPUM1992")
cochabamba_01 <- bolivia %>% inner_join(cochabamba_01, by="IPUM2001")

names(cochabamba_92)
names(cochabamba_01)

cochabamba_92 <- select(cochabamba_92, -c(PROV1992))
cochabamba_01 <- select(cochabamba_01, -c(PROV2001))


##Merging all years into one table
bolivia_full <- rbind(cochabamba_92,cochabamba_01)
names(bolivia_full)

##Excluding specific columns for the unifeied dataset
bolivia_full<- select(bolivia_full, -c(GEO2_BO1992,GEO2_BO2001,IPUM1992,IPUM2001))
table(bolivia_full$CITY)
save(bolivia_full,file="bolivia_full.Rda")
