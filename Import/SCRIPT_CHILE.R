library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/Chile")

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

santiago_92 <- read.csv(file="santiago_92.csv", header = TRUE)
santiago_92['CITY']='santiago'

santiago_02 <- read.csv(file="santiago_02.csv",header = TRUE)
santiago_02['CITY']='santiago'

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00025.xml")
chile <- read_ipums_micro(ddi)

##Creating field for join

chile$IPUM1992 <- as.integer(chile$GEO2_CL1992)
chile$IPUM2002 <- as.integer(chile$GEO2_CL2002)

##Joining by year

chile_92 <- chile %>% inner_join(santiago_92, by="IPUM1992")
chile_02 <- chile %>% inner_join(santiago_02, by="IPUM2002")

names(chile_92)
names(chile_02)

chile_92 <- select(chile_92, -c(MUNI1992))
chile_02 <- select(chile_02, -c(MUNI2002))

##Merging all years into one table
chile_full <- rbind(chile_92,chile_02)
names(chile_full)


##Excluding specific columns for the unifeied dataset
chile_full<- select(chile_full, -c(GEO2_CL1992,GEO2_CL2002,IPUM1992,IPUM2002))
table(chile_full$CITY)
save(chile_full,file="chile_full.Rda")
