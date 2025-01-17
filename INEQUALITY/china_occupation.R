library(ipumsr)
library(dbplyr)
library(tidyverse)
getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/Data_new/china")

##Reading the csv with selected geographical features for seleccted cities and adding the column with city name

anqing_90 <- read.csv(file="anqing_91.csv", header = TRUE)
anqing_90['CITY']='anqing, anhui'

anqing_00 <- read.csv(file="anqing_00.csv", header = TRUE)
anqing_00['CITY']='anqing, anhui'

beijing_90 <- read.csv(file="beijing_90.csv", header = TRUE)
beijing_90['CITY']='beijing, beijing	'

beijing_00 <- read.csv(file="beijing_00.csv", header = TRUE)
beijing_00['CITY']='beijing, beijing	'

changzhou_90 <- read.csv(file="changzhou_90.csv", header = TRUE)
changzhou_90['CITY']='changzhou, jingsu	'

changzhou_00 <- read.csv(file="changzhou_00.csv", header = TRUE)
changzhou_00['CITY']='changzhou, jingsu	'

changzhi_90 <- read.csv(file="changzhi_90.csv", header = TRUE)
changzhi_90['CITY']='changzhi, hunan	'

changzhi_00 <- read.csv(file="changzhi_00.csv", header = TRUE)
changzhi_00['CITY']='changzhi, hunan	'

chengdu_90 <- read.csv(file="chengdu_90.csv", header = TRUE)
chengdu_90['CITY']='chengdu, sichuan	'

chengdu_00 <- read.csv(file="chengdu_00.csv", header = TRUE)
chengdu_00['CITY']='chengdu, sichuan	'

guangzhou_90 <- read.csv(file="guangzhou_90.csv", header = TRUE)
guangzhou_90['CITY']='guangzhou, guangdong	'

guangzhou_00 <- read.csv(file="guangzhou_00.csv", header = TRUE)
guangzhou_00['CITY']='guangzhou, guangdong	'

haikou_90 <- read.csv(file="haikou_90.csv", header = TRUE)
haikou_90['CITY']='haikou, hainan	'

haikou_00 <- read.csv(file="haikou_00.csv", header = TRUE)
haikou_00['CITY']='haikou, hainan	'

jinan_90 <- read.csv(file="jinan_90.csv", header = TRUE)
jinan_90['CITY']='jinan, shandong	'

jinan_00 <- read.csv(file="jinan_00.csv", header = TRUE)
jinan_00['CITY']='jinan, shandong	'	

leshan_90 <- read.csv(file="leshan_90.csv", header = TRUE)
leshan_90['CITY']='leshan, sichuan	'

leshan_00 <- read.csv(file="leshan_00.csv", header = TRUE)
leshan_00['CITY']='leshan, sichuan	'

pingxiang_90 <- read.csv(file="pingxiang_90.csv", header = TRUE)
pingxiang_90['CITY']='pingxiang, jiangxi	'

pingxiang_00 <- read.csv(file="pingxiang_00.csv", header = TRUE)
pingxiang_00['CITY']='pingxiang, jiangxi	'

qingdao_90 <- read.csv(file="qingdao_90.csv", header = TRUE)
qingdao_90['CITY']='qingdao, shandong	'

qingdao_00 <- read.csv(file="qingdao_00.csv", header = TRUE)
qingdao_00['CITY']='qingdao, shandong	'

shanghai_90 <- read.csv(file="shangai_90.csv", header = TRUE)
shanghai_90['CITY']='shanghai, shanghai	'

shanghai_00 <- read.csv(file="shangai_00.csv", header = TRUE)
shanghai_00['CITY']='shanghai, shanghai	'

shenzhen_90 <- read.csv(file="shenzhen_90.csv", header = TRUE)
shenzhen_90['CITY']='shenzhen, guangdong	'

shenzhen_00 <- read.csv(file="shenzhen_00.csv", header = TRUE)
shenzhen_00['CITY']='shenzhen, guangdong	'

suining_90 <- read.csv(file="suining_90.csv", header = TRUE)
suining_90['CITY']='suining, sichuan	'

suining_00 <- read.csv(file="suining_00.csv", header = TRUE)
suining_00['CITY']='suining, sichuan	'

tangshan_90 <- read.csv(file="tangshan_90.csv", header = TRUE)
tangshan_90['CITY']='tangshan, hebei	'

tangshan_00 <- read.csv(file="tangshan_00.csv", header = TRUE)
tangshan_00['CITY']='tangshan, hebei	'

tianjin_90 <- read.csv(file="tianijn_90.csv", header = TRUE)
tianjin_90['CITY']='tianjin,  tianjin	'

tianjin_00 <- read.csv(file="tianijn_00.csv", header = TRUE)
tianjin_00['CITY']='tianjin,  tianjin	'

wuhan_90 <- read.csv(file="wuhan_90.csv", header = TRUE)
wuhan_90['CITY']='wuhan, hubei	'

wuhan_00 <- read.csv(file="wuhan_00.csv", header = TRUE)
wuhan_00['CITY']='wuhan, hubei	'

yiyang_90 <- read.csv(file="yiyang_90.csv", header = TRUE)
yiyang_90['CITY']='yiyang, hunan	'

yiyang_00 <- read.csv(file="yiyang_00.csv", header = TRUE)
yiyang_00['CITY']='yiyang, hunan	'

yulin_90 <- read.csv(file="yulin_90.csv", header = TRUE)
yulin_90['CITY']='yulin, guangxi	'

yulin_00 <- read.csv(file="yulin_00.csv", header = TRUE)
yulin_00['CITY']='yulin, guangxi	'

zunyi_90 <- read.csv(file="zunyi_90.csv", header = TRUE)
zunyi_90['CITY']='zunyi, guizhou	'

zunyi_00 <- read.csv(file="zunyi_00.csv", header = TRUE)
zunyi_00['CITY']='zunyi, guizhou	'

hangzhou_90 <- read.csv(file="hangzhou_90.csv", header = TRUE)
hangzhou_90['CITY']='hangzhou, zhejiang	'

hangzhou_00 <- read.csv(file="hangzhou_00.csv", header = TRUE)
hangzhou_00['CITY']='hangzhou, zhejiang	'

geo_china_90 <- rbind(hangzhou_90,anqing_90,beijing_90,changzhi_90, changzhou_90, chengdu_90,guangzhou_90,haikou_90,jinan_90,leshan_90,pingxiang_90,qingdao_90,shanghai_90,shenzhen_90,suining_90,tangshan_90,tianjin_90,wuhan_90,yiyang_90,yulin_90,zunyi_90)
geo_china_00 <- rbind(hangzhou_00,anqing_00,beijing_00,changzhi_00, changzhou_00, chengdu_00,guangzhou_00,haikou_00,jinan_00,leshan_00,pingxiang_00,qingdao_00,shanghai_00,shenzhen_00,suining_00,tangshan_00,tianjin_00,wuhan_00,yiyang_00,yulin_00,zunyi_00)

##Importing new extraction from IPUMS with the geographical variable 

ddi <-read_ipums_ddi("ipumsi_00240.xml")
china<- read_ipums_micro(ddi)

names(china)

##Creating field for join

china$IPUM1990 <- as.integer(china$GEO2_CN1990)
china$IPUM2000 <- as.integer(china$GEO2_CN2000)

##Joining by year

china_90 <- china %>% inner_join(geo_china_90, by="IPUM1990")
china_00 <- china %>% inner_join(geo_china_00, by="IPUM2000")

names(china_90)
names(china_00)

china_90 <- select(china_90, -c(PREF1990))
china_00 <- select(china_00, -c(PREF2000))

##Merging all years into one table
china_full <- rbind(china_90,china_00)
names(china_full)
table(china_full$CITY)

##Excluding specific columns for the unifeied dataset

china_full<- select(china_full, -c(GEO2_CN1990,GEO2_CN2000,IPUM1990,IPUM2000))
table(china_full$CITY)

table(china_full$OCCISCO)


china_full$OCCISCO_b <- ifelse(china_full$OCCISCO ==1|china_full$OCCISCO ==2,1,0)
china_full$OCCISCO_b <- ifelse(china_full$OCCISCO ==3|china_full$OCCISCO ==4|china_full$OCCISCO ==5,2,china_full$OCCISCO_b)
china_full$OCCISCO_b <- ifelse(china_full$OCCISCO ==6|china_full$OCCISCO ==7|china_full$OCCISCO ==8|china_full$OCCISCO ==9,3,china_full$OCCISCO_b)
table(china_full$OCCISCO_b)

china_full_OCCISCO_b <- china_full %>% select(YEAR,CITY,OCCISCO_b,PERWT)
china_full_OCCISCO_b <- china_full_OCCISCO_b %>% filter(OCCISCO_b !=0)

table(china_full_OCCISCO_b$OCCISCO_b)

OCCISCO_b_total <- china_full_OCCISCO_b %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_total = sum(PERWT))

OCCISCO_b_TOP <- china_full_OCCISCO_b %>% filter (OCCISCO_b==1) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_TOP = sum(PERWT))

OCCISCO_b_MIDDLE <- china_full_OCCISCO_b %>% filter (OCCISCO_b==2) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_MIDDLE = sum(PERWT))

OCCISCO_b_BOTTOM <- china_full_OCCISCO_b %>% filter (OCCISCO_b==3) %>% 
  group_by(YEAR,CITY) %>% 
  summarise(OCCISCO_b_BOTTOM = sum(PERWT))

china_OCCISCO_b<- OCCISCO_b_total %>% full_join(OCCISCO_b_TOP, by = c("YEAR","CITY"))
china_OCCISCO_b<- china_OCCISCO_b %>% full_join(OCCISCO_b_MIDDLE,by = c("YEAR","CITY"))
china_OCCISCO_b<- china_OCCISCO_b %>% full_join(OCCISCO_b_BOTTOM,by = c("YEAR","CITY"))

china_OCCISCO_b
write.csv2(china_OCCISCO_b,file="china_occupation.csv")

