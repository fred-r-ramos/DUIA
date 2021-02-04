library(ipumsr)
library(dbplyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(reshape)
library(leaflet)


getwd()
setwd("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/0_CITIES_DATAFRAMES")

##loading dataframe

load("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/0_CITIES_DATAFRAMES/cities_full_sample.Rda")

load("C:/Users/fredr/Dropbox/B_POSTDOC_UvA/Cities_Sample/ATLAS_URB_EXPANS/Data/0_CITIES_DATAFRAMES/aue_edit.Rda")

##creating a columns with the datatime for AUE comparison 

##already applied for the cities_full_sample.RDA keep just for information

cities_full_sample$TIME <- ifelse(cities_full_sample$YEAR<1993,"T1",
                                  ifelse(cities_full_sample$YEAR>1996 & cities_full_sample$YEAR<=2004,"T2",
                                  ifelse(cities_full_sample$YEAR>=2008,"T3",
                                  ifelse(cities_full_sample$YEAR==1994 & cities_full_sample$COUNTRY==504,"T1",
                                  ifelse(cities_full_sample$YEAR==1996 & cities_full_sample$COUNTRY==710,"T1",
                                  ifelse(cities_full_sample$YEAR==2007 & cities_full_sample$COUNTRY==222,"T2",
                                  ifelse(cities_full_sample$YEAR==2007 & cities_full_sample$COUNTRY==566,"T2",
                                  ifelse(cities_full_sample$YEAR==1993 & cities_full_sample$COUNTRY==170,"T2",
                                  ifelse(cities_full_sample$YEAR==1994 & cities_full_sample$COUNTRY==231,"T2",
                                  ifelse(cities_full_sample$YEAR==1995 & cities_full_sample$COUNTRY==558,"T2",
                                  ifelse(cities_full_sample$YEAR==1996 & cities_full_sample$COUNTRY==242,"T2",
                                  ifelse(cities_full_sample$YEAR==1996 & cities_full_sample$COUNTRY==818,"T2",
                                  ifelse(cities_full_sample$YEAR==2005 & cities_full_sample$COUNTRY==170,"T3",
                                  ifelse(cities_full_sample$YEAR==2005 & cities_full_sample$COUNTRY==558,"T3",
                                  ifelse(cities_full_sample$YEAR==2006 & cities_full_sample$COUNTRY==818,"T3",
                                  ifelse(cities_full_sample$YEAR==2007 & cities_full_sample$COUNTRY==231,"T3",
                                  ifelse(cities_full_sample$YEAR==2007 & cities_full_sample$COUNTRY==242,"T3",
                                  ifelse(cities_full_sample$YEAR==2007 & cities_full_sample$COUNTRY==508,"T3",NA
                                  ))))))))))))))))))

#########################################################################################################################correcting names for chinese cities

table(cities_full_sample$CITY)
cities_full_sample$CITY <- as.character(cities_full_sample$CITY)

cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "anqing, anhui","anqing"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "beijing, beijing\t","beijing"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "changzhi, hunan\t","changzhi"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "changzhou, jingsu\t","changzhou"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "chengdu, sichuan\t","chengdu"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "guangzhou, guangdong\t","guangzhou"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "haikou, hainan\t","haikou"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "jinan, shandong\t","jinan"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "leshan, sichuan\t","leshan"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "pingxiang, jiangxi\t","pingxiang"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "qingdao, shandong\t","qingdao"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "shanghai, shanghai\t","shanghai"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "shenzhen, guangdong\t","shenzhen"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "suining, sichuan\t","suining"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "tangshan, hebei\t","tangshan"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "tianjin,  tianjin\t","tianjin"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "wuhan, hubei\t","wuhan"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "yiyang, hunan\t","yiyang"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "yulin, guangxi\t","yulin"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "hangzhou, zhejiang\t","hangzhou"))
cities_full_sample <- cities_full_sample %>% mutate(CITY = replace(CITY, CITY == "zunyi, guizhou\t","zunyi"))

names(aue)
colnames(aue)[1] <- "CITY"

table(aue$CITY)

aue$CITY <- as.character(aue$CITY)
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="anqing, anhui", "anqing"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="beijing, beijing", "beijing"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="changzhi, hunan", "changzhi"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="changzhou, jingsu", "changzhou"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="chengdu, sichuan", "chengdu"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="guangzhou, guangdong", "guangzhou"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="haikou, hainan", "haikou"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="jinan, shandong", "jinan"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="leshan, sichuan", "leshan"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="pingxiang, jiangxi", "pingxiang"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="qingdao, shandong", "qingdao"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="shanghai, shanghai", "shanghai"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="shenzhen, guangdong", "shenzhen"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="suining, sichuan", "suining"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="tangshan, hebei","tangshan"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="tianjin,  tianjin","tianjin"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="wuhan, hubei","wuhan"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="yiyang, hunan","yiyang"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="hangzhou, zhejiang\t","hangzhou"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="yiyang, hunan","yiyang"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="yulin, guangxi", "yulin"))
aue <- aue %>% mutate(CITY = replace(CITY, CITY=="zunyi, guizhou", "zunyi"))

###correcting coordinates for some cities 

aue <- aue %>% mutate(LAT = replace(LAT, LAT == 6.842,-26.2040 ))
aue <- aue %>% mutate(LONG = replace(LONG, LONG == 3.634,28.0473 ))


aue <- aue %>% mutate(LAT = replace(LAT, LAT == 36.6953,-3.3699 ))
aue <- aue %>% mutate(LONG = replace(LONG, LONG == -3.3699,36.6953 ))

aue <- aue %>% mutate(LONG = replace(LONG, LONG == -39.045,-39.1 ))


##creating data frames for especific varibles

##for some variables we have external data from census

##in these cases we import external table formated from a `ad hod`` R script

###########################################################################for water supply

cities_full_sample <- cities_full_sample%>%
  mutate(WATSUP_factor = as_factor(WATSUP))

cities_water <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,WATSUP_factor) %>%
  summarise(n_watsup = sum(PERWT))%>%
  spread(key = "WATSUP_factor", value = "n_watsup")

names(cities_water)

cities_water <- cities_water%>% dplyr::rename("NIU_w"="NIU (not in universe)")  
cities_water <- cities_water%>% dplyr::rename("Yes_piped_w"="Yes, piped water")  
cities_water <- cities_water%>% dplyr::rename("Piped_inside_w"="Piped inside dwelling")  
cities_water <- cities_water%>% dplyr::rename("Piped_exclusive_w"="Piped, exclusively to this household") 
cities_water <- cities_water%>% dplyr::rename("Piped_shared_w"="Piped, shared with other households" )  
cities_water <- cities_water%>% dplyr::rename("Piped_outside_w"="Piped outside the dwelling" ) 
cities_water <- cities_water%>% dplyr::rename("Piped_outside_in_build_w"="Piped outside dwelling, in building" ) 
cities_water <- cities_water%>% dplyr::rename("Piped_within_build_w"="Piped within the building or plot of land" ) 
cities_water <- cities_water%>% dplyr::rename("Piped_outside_plot_w"="Piped outside the building or lot") 
cities_water <- cities_water%>% dplyr::rename("Acces_public_water_w"="Have access to public piped water")  
cities_water <- cities_water%>% dplyr::rename("No_piped_w"="No piped water")  
cities_water <- cities_water%>% dplyr::rename("Unknown_w"="Unknown")  

cities_water <- cities_water%>% dplyr::rename("<NA>_w"="<NA>")

names(cities_water)

cities_water$Total_w <- rowSums(cities_water[,c("Yes_piped_w","Piped_inside_w","Piped_exclusive_w","Piped_shared_w","Piped_outside_w","Piped_outside_in_build_w","Piped_within_build_w","Piped_outside_plot_w","Acces_public_water_w","No_piped_w","Unknown_w")],na.rm=TRUE)

cities_water$P_Yes_piped_w <- (cities_water$Yes_piped_w/cities_water$Total_w)*100
cities_water$P_No_piped_w <- (cities_water$No_piped_w/cities_water$Total_w)*100

cities_water %>% filter(!is.na(P_No_piped_w)) %>%
  ggplot(aes(x=TIME, y=P_No_piped_w,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage without piped water") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

######for water supply we have information for China and India importing specific census data for chinese cities

water_china <- read.csv("cities_china_water.csv")
load('water_india.Rda')

names(water_china)
names(cities_water)
names(water_india_t)

water_china <- water_china %>% dplyr::rename(CITY = ï..CITY)
water_china$CITY <- as.character(water_china$CITY)
water_china$TIME <- as.character(water_china$TIME)


water_india_t$CITY <- as.character(water_india_t$CITY)
water_india_t <- water_india_t %>% ungroup(CITY) 
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Ahmadabad","ahmadabad"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Belgaum","belgaum"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Coimbatore","coimbatore"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Hyderabad","hyderabad"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Jaipur","jaipur"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Jalna","jalna"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Kanpur","kanpur"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Kanpur ","kanpur"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Kozhikode","kozhikode"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Mumbai","mumbai"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Pune","pune"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Sitapur","sitapur"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Sitapur ","sitapur"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Parbhani","parbhani"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Kolkata ","kolkata"))
water_india_t <- water_india_t %>% mutate(CITY = replace(CITY, CITY=="Kolkata","kolkata"))
#water_india_synt$YEAR <- as.integer(water_india_synt$YEAR)

water_india_t$No_piped <- water_india_t$handpump + water_india_t$covered_well + water_india_t$uncovered.well + water_india_t$tank + water_india_t$river + water_india_t$Spring + water_india_t$other
water_india_t$P_No_piped_w <- water_india_t$No_piped/water_india_t$Total*100

#creating a table with indian cities considering only the total by category (not considering if the ategory is within premisses or not)

water_india <- water_india_t[(water_india_t$Local=="Total"),]

cities_water_synt <- cities_water %>% select(CITY,TIME,YEAR,P_No_piped_w)
water_china_synt <- water_china %>% select(CITY,TIME,YEAR,P_No_piped_w)
water_india_synt <- water_india %>% select(CITY,TIME,YEAR,P_No_piped_w)

sapply(cities_water_synt, class)
sapply(water_china_synt, class)
sapply(water_india_synt, class)

cities_water_int_synt <- rbind.data.frame(cities_water_synt,water_china_synt,water_india_synt)

cities_water_int_synt$P_piped_w <- 100 - cities_water_int_synt$P_No_piped_w

sapply(cities_water_int_synt, class)
table(cities_water_int_synt$CITY)
cities_water_int_synt <- as.data.frame(cities_water_int_synt)

###creating graph for piped water

cities_water_int_synt %>% filter(!is.na(P_piped_w)) %>%
  ggplot(aes(x=TIME, y=P_piped_w, group=CITY,label=YEAR))+
  facet_wrap(~CITY) +
  geom_line() +
  geom_point() + geom_text(nudge_y = -18, size=2.4, colour="gray30")+
  labs(title = "Percentage of households witht access to piped water")+
  theme_bw()

#creating a table with aue and water to present the general analysis

cities_merge_water <- merge(aue,cities_water_int_synt, by=c("CITY","TIME"))

table(cities_merge_water$CITY)

#creating a map

names(cities_merge_water)

leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% addCircles(data=cities_merge_water,popup = ~as.character(CITY), label = ~as.character(CITY),color = "darkred", radius = ~ (Urb_Ext_Pop/100))



###creating a table for detailed water analysis for indian cities

water_india_t$TAP_total <- rowSums(water_india_t[,c("tap", "tap_untreated","tap_treated")], na.rm=TRUE)

names(water_india_t)

water_india_t$other_type <- water_india_t$Spring + water_india_t$tank + water_india_t$river +water_india_t$other 

water_india_t <- water_india_t[c(1,2,3,4,5,18,9,10,11,19)]

water_india_t <- water_india_t%>% dplyr::rename("Tap"="TAP_total")
water_india_t <- water_india_t%>% dplyr::rename("Handpump"="handpump")
water_india_t <- water_india_t%>% dplyr::rename("Covered_well"="covered_well")
water_india_t <- water_india_t%>% dplyr::rename("Uncovered_well"="uncovered.well")
water_india_t <- water_india_t%>% dplyr::rename("Other_type"="other_type")

water_india_detail<-water_india_detail[!water_india_detail$Local=="Total",]

water_india_detail <- water_india_t %>%  gather(water_type,value,c(6:10))
                                                                             
table(water_india_detail$water_type)

names(water_india_detail)

water_india_detail <- water_india_detail %>% 
  group_by(CITY,YEAR) %>% 
  mutate(sum = sum(value))

water_india_detail <- water_india_detail %>% 
  mutate(percentage=(value/sum)*100)

table(water_india_detail$water_type)
water_india_detail$water_type_fac <- factor(water_india_detail$water_type, levels = c("Tap", "Handpump",  "Covered_well", "Uncovered_well", "Other_type"))

names(water_india)


save(water_india_detail, file="water_india_detail.Rda")

water_india_detail %>% filter(TIME=="T3") %>%  ggplot() + facet_wrap(~CITY) +
  geom_col(aes(x=water_type_fac,y=percentage,fill=Local))+
  scale_fill_manual(values = c("navajowhite1","navajowhite3","navajowhite4"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Detailed water provision selected Indian cities Census 2011")

water_india_detail %>% filter(TIME=="T2") %>%  ggplot() + facet_wrap(~CITY) +
  geom_col(aes(x=water_type,y=percentage,fill=Local))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Detailed water provision selected Indian cities Census 2001")

########################################################################## for ownership

cities_full_sample <- cities_full_sample%>%
  mutate(OWNERSHIP_factor = as_factor(OWNERSHIP))

names(cities_full_sample)

cities_ownership <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,OWNERSHIP_factor) %>%
  summarise(n_ownwership = sum(PERWT))%>%
  spread(key = "OWNERSHIP_factor", value = "n_ownwership")

table(cities_full_sample$OWNERSHIP_factor)
names(cities_ownership)

##changing names of columns

cities_ownership <- cities_ownership%>% dplyr::rename("NIU_ow"="NIU (not in universe)")  
cities_ownership <- cities_ownership%>% dplyr::rename("NotOwned_ow"="Not owned")  
cities_ownership <- cities_ownership%>% dplyr::rename("Owned_ow"="Owned") 
cities_ownership <- cities_ownership%>% dplyr::rename("Unknown_ow"="Unknown")
cities_ownership <- cities_ownership%>% dplyr::rename("<NA>_ow"="<NA>")

##creating a variable (total) to sum all the values counts of interest avoding NA values

cities_ownership$total_ow <- rowSums(cities_ownership[,c("Owned_ow","NotOwned_ow","Unknown_ow")],na.rm=TRUE)

##calculating percentages of relevant variables

cities_ownership$P_Owned_ow <- (cities_ownership$Owned_ow/cities_ownership$total_ow)*100
cities_ownership$P_NotOwned_ow <- (cities_ownership$NotOwned_ow/cities_ownership$total_ow)*100

##graph bar for selected variable by Time

cities_ownership %>% filter(!is.na(P_Owned_ow)) %>%
  ggplot(aes(x=TIME, y=P_Owned_ow,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage of Owned Household") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#########################################################################creating for ownershipd

cities_full_sample <- cities_full_sample%>%
  mutate(OWNERSHIPD_factor = as_factor(OWNERSHIPD))

cities_ownershipd <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,OWNERSHIPD_factor) %>%
  summarise(n_ownwershipd = sum(PERWT))%>%
  spread(key = "OWNERSHIPD_factor", value = "n_ownwershipd")

names(cities_ownershipd)

cities_ownershipd <- cities_ownershipd%>% dplyr::rename("<NA>_owd"="<NA>")

##agregating variables for owned, rented, occupied

cities_ownershipd$Rent_all_owd <- rowSums(cities_ownershipd[,c("Renting, not specified","Renting, government","Renting, local authority","Renting, parastatal","Renting, private","Renting, private company","Renting, individual","Renting, public subsidized","Renting, private subsidized","Renting, cooperative","Renting, with a job or business","Renting, loan-backed habitation","Renting, mixed contract","Rent to own","Renting, other" )],na.rm=TRUE)
cities_ownershipd$Owned_all_owd <- rowSums(cities_ownershipd[,c("Owned","Owned, already paid","Owned, still paying","Owned, constructed","Owned, inherited","Owned, other","Owned, condominium")],na.rm=TRUE)
cities_ownershipd$Occupied_all_owd <- rowSums(cities_ownershipd[,c("Not owned","Occupied de facto/squatting","Free/usufruct (no cash rent)","Free, provided by employer","Free, without work or services","Free, provided by family or friend","Free, private","Free, public","Free, other","Not owned, other" )],na.rm=TRUE)
cities_ownershipd$Total_all_owd <- rowSums(cities_ownershipd[,c("Rent_all_owd","Owned_all_owd","Occupied_all_owd")],na.rm=TRUE)

##calculating percentages of relevant variables

cities_ownershipd$P_Rent_all_owd <- (cities_ownershipd$Rent_all_owd/cities_ownershipd$Total_all_owd)*100
cities_ownershipd$P_Owned_all_owd <- (cities_ownershipd$Owned_all_owd/cities_ownershipd$Total_all_owd)*100
cities_ownershipd$P_Occupied_all_owd <- (cities_ownershipd$Occupied_all_owd/cities_ownershipd$Total_all_owd)*100

##graph bar for selected variable by Time

cities_ownershipd %>% filter(!is.na(P_Occupied_all_owd)) %>%
  ggplot(aes(x=TIME, y=P_Occupied_all_owd,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Occupied") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

##############################################################################creating for eletric

cities_full_sample <- cities_full_sample%>%
  mutate(ELECTRIC_factor = as_factor(ELECTRIC))

cities_electric <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,ELECTRIC_factor) %>%
  summarise(n_electric = sum(PERWT))%>%
  spread(key = "ELECTRIC_factor", value = "n_electric")

names(cities_electric)

cities_electric <- cities_electric%>% dplyr::rename("<NA>_e"="<NA>")

##renaming including sufix for eletricity (_e)

cities_electric <- cities_electric%>% dplyr::rename("NIU_e"="NIU (not in universe)")  
cities_electric <- cities_electric%>% dplyr::rename("Yes_e"="Yes")  
cities_electric <- cities_electric%>% dplyr::rename("No_e"="No") 
cities_electric <- cities_electric%>% dplyr::rename("Unknown_e"="Unknown")

cities_electric$Total_e <- rowSums(cities_electric[,c("Yes_e","No_e","Unknown_e")],na.rm=TRUE)

##calculating percentages for table electric

cities_electric$P_Yes_e <- (cities_electric$Yes_e/cities_electric$Total_e)*100
cities_electric$P_No_e <- (cities_electric$No_e/cities_electric$Total_e)*100

cities_electric %>% filter(!is.na(No_e)) %>%
  ggplot(aes(x=TIME, y=P_No_e,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage without electricity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#################################################################for sewage

cities_full_sample <- cities_full_sample%>%
  mutate(SEWAGE_factor = as_factor(SEWAGE))

cities_sewage <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,SEWAGE_factor) %>%
  summarise(n_sewage = sum(PERWT))%>%
  spread(key = "SEWAGE_factor", value = "n_sewage")

names(cities_sewage)

cities_sewage <- cities_sewage%>% dplyr::rename("NIU_sw"="NIU (not in universe)") 
cities_sewage <- cities_sewage%>% dplyr::rename("Unknown_sw"="Unknown")
cities_sewage$Adequate_sw <- rowSums(cities_sewage[,c("Connected to sewage system or septic tank","Sewage system (public sewage disposal)","Septic tank (private sewage disposal)")],na.rm=TRUE)
cities_sewage$Inadequate_sw <- rowSums(cities_sewage[,c("Not connected to sewage disposal system")],na.rm=TRUE)
cities_sewage$Total_sw <- rowSums(cities_sewage[,c("Adequate_sw","Inadequate_sw","Unknown_sw")],na.rm=TRUE)

cities_sewage <- cities_sewage%>% dplyr::rename("<NA>_sw"="<NA>")

cities_sewage$P_adequated_sw <- (cities_sewage$Adequate_sw/cities_sewage$Total_sw)*100

cities_sewage %>% filter(!is.na(P_adequated_sw)) %>%
  ggplot(aes(x=TIME, y=P_adequated_sw,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage with Adequated Sewage Destination") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

######################################################################### for hhtype

cities_full_sample <- cities_full_sample%>%
  mutate(HHTYPE_factor = as_factor(HHTYPE))

cities_hhtype <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,HHTYPE_factor) %>%
  summarise(n_hhtype = sum(PERWT))%>%
  spread(key = "HHTYPE_factor", value = "n_hhtype")

names(cities_hhtype)

cities_hhtype$Family_ht <- rowSums(cities_hhtype[,c("Married/cohab couple, no children","Married/cohab couple with children","Single-parent family","Polygamous family","Extended family, relatives only")],na.rm=TRUE)
cities_hhtype$Single_ht <- rowSums(cities_hhtype[,c("One-person household" )],na.rm=TRUE)
cities_hhtype$Multy_family_ht <- rowSums(cities_hhtype[,c("Composite household, family and non-relatives","Non-family household","Other relative or non-relative household","Group quarters","Unclassifiable" )],na.rm=TRUE)
cities_hhtype$Total_ht <- rowSums(cities_hhtype[,c("Family_ht","Single_ht","Multy_family_ht")],na.rm=TRUE)

cities_hhtype <- cities_hhtype%>% dplyr::rename("<NA>_ht"="<NA>")

cities_hhtype$P_Multy_family_ht <- (cities_hhtype$Multy_family_ht/cities_hhtype$Total_ht)*100

cities_hhtype %>% filter(!is.na(Multy_family_ht)) %>%
  ggplot(aes(x=TIME, y=P_Multy_family_ht,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage living in a multifamliar housing") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#############################################################for literate

cities_full_sample <- cities_full_sample%>%
  mutate(LIT_factor = as_factor(LIT))

cities_lit <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,LIT_factor) %>%
  summarise(n_lit = sum(PERWT))%>%
  spread(key = "LIT_factor", value = "n_lit")

names(cities_lit)

cities_lit <- cities_lit%>% dplyr::rename("NIU_lt"="NIU (not in universe)") 
cities_lit <- cities_lit%>% dplyr::rename("Unknown_lt"="Unknown/missing")
cities_lit <- cities_lit%>% dplyr::rename("Illiterate_lt"="No, illiterate")
cities_lit <- cities_lit%>% dplyr::rename("Literate_lt"= "Yes, literate")
cities_lit$Total_lt <- rowSums(cities_lit[,c("Literate_lt","Unknown_lt","Illiterate_lt")],na.rm=TRUE)

cities_lit <- cities_lit%>% dplyr::rename("<NA>_lt"="<NA>")

cities_lit$P_Illiterate_lt <- (cities_lit$Illiterate_lt/cities_lit$Total_lt)*100

cities_lit %>% filter(!is.na(Illiterate_lt)) %>%
  ggplot(aes(x=TIME, y=P_Illiterate_lt,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage illiterate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

################################################################# for employment status

cities_full_sample <- cities_full_sample%>%
  mutate(EMPSTAT_factor = as_factor(EMPSTAT))

cities_empstat <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,EMPSTAT_factor) %>%
  summarise(n_empstat = sum(PERWT))%>%
  spread(key = "EMPSTAT_factor", value = "n_empstat")

names(cities_empstat)

cities_empstat <- cities_empstat%>% dplyr::rename("NIU_es"="NIU (not in universe)") 
cities_empstat <- cities_empstat%>% dplyr::rename("Employed_es"="Employed")
cities_empstat <- cities_empstat%>% dplyr::rename("Unknown_es"="Unknown/missing")
cities_empstat <- cities_empstat%>% dplyr::rename("Inactive_es"= "Inactive")
cities_empstat <- cities_empstat%>% dplyr::rename("Unemployed_es"= "Unemployed")
cities_empstat$Total_es <- rowSums(cities_empstat[,c("Employed_es","Unknown_es","Inactive_es","Unemployed_es")],na.rm=TRUE)

cities_empstat <- cities_empstat%>% dplyr::rename("<NA>_es"="<NA>")

cities_empstat$P_Unemployed_es <- (cities_empstat$Unemployed_es/cities_empstat$Total_es)*100

cities_empstat %>% filter(!is.na(Unemployed_es)) %>%
  ggplot(aes(x=TIME, y=P_Unemployed_es,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage illiterate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#######################################################################

cities_full_sample <- cities_full_sample%>%
  mutate(TRASH_factor = as_factor(TRASH))

cities_trash <- cities_full_sample %>%
  group_by(CITY,TIME,YEAR,TRASH_factor) %>%
  summarise(n_trash = sum(PERWT))%>%
  spread(key = "TRASH_factor", value = "n_trash")

cities_trash <- cities_trash%>% dplyr::rename("NIU_tr"="0") 
cities_trash <- cities_trash%>% dplyr::rename("Collected_tr"="10")
cities_trash <- cities_trash%>% dplyr::rename("Collect_house_tr"="11")
cities_trash <- cities_trash%>% dplyr::rename("Collect_containet_tr"="12")
cities_trash <- cities_trash%>% dplyr::rename("Collect_sanitation_tr"="13")
cities_trash <- cities_trash%>% dplyr::rename("Collect_sanitation_disp_tr"="14")
cities_trash <- cities_trash%>% dplyr::rename("Disposed_other_manner_tr"="20")
cities_trash <- cities_trash%>% dplyr::rename("Burned_or_buried_tr"="21")
cities_trash <- cities_trash%>% dplyr::rename("Burned_tr"="22")
cities_trash <- cities_trash%>% dplyr::rename("Buried_tr"="23")
cities_trash <- cities_trash%>% dplyr::rename("Street_vac_land_tr"="24")
cities_trash <- cities_trash%>% dplyr::rename("River_Lake_tr"="25")
cities_trash <- cities_trash%>% dplyr::rename("Canyon_gulley_tr"="26")
cities_trash <- cities_trash%>% dplyr::rename("Dumped_pit_tr"="27")
cities_trash <- cities_trash%>% dplyr::rename("Communal_dump_tr"="28")
cities_trash <- cities_trash%>% dplyr::rename("Own_dump_tr"="29")
cities_trash <- cities_trash%>% dplyr::rename("Authorized_dump"="30")
cities_trash <- cities_trash%>% dplyr::rename("Illegal_dump"="31")
cities_trash <- cities_trash%>% dplyr::rename("Other_dumping"="32")
cities_trash <- cities_trash%>% dplyr::rename("Outside"="33")
cities_trash <- cities_trash%>% dplyr::rename("In_fields"="34")
cities_trash <- cities_trash%>% dplyr::rename("Fed_animals"="35")
cities_trash <- cities_trash%>% dplyr::rename("Composted"="36")
cities_trash <- cities_trash%>% dplyr::rename("Heap"="37")
cities_trash <- cities_trash%>% dplyr::rename("Garden"="38")
cities_trash <- cities_trash%>% dplyr::rename("Other"="39")
cities_trash <- cities_trash%>% dplyr::rename("Unknown"="99")

cities_trash <- cities_trash%>% dplyr::rename("<NA>_tr"="<NA>")

cities_trash$Collected_all_tr <- rowSums(cities_trash[,c("Collected_tr","Collect_house_tr","Collect_containet_tr","Collect_sanitation_tr","Collect_sanitation_disp_tr")],na.rm=TRUE)
cities_trash$Inadequated_all_tr <- rowSums(cities_trash[,c("Disposed_other_manner_tr","Burned_or_buried_tr","Burned_tr","Buried_tr","Street_vac_land_tr","River_Lake_tr","Canyon_gulley_tr","Dumped_pit_tr","Communal_dump_tr","Own_dump_tr","Authorized_dump","Illegal_dump","Other_dumping","Outside","In_fields","Fed_animals","Composted")],na.rm=TRUE)
cities_trash$Total_all_tr <- rowSums(cities_trash[,c("Collected_all_tr","Inadequated_all_tr")],na.rm=TRUE)

cities_trash$P_Inadequated_all_tr <- (cities_trash$Inadequated_all_tr/cities_trash$Total_all_tr)*100

cities_trash %>% filter(!is.na(Inadequated_all_tr)) %>%
  ggplot(aes(x=TIME, y=P_Inadequated_all_tr,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  facet_wrap(~CITY) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("Percentage inadequated trash disposal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

cities_empstat$P_Unemployed_es <- (cities_empstat$Unemployed_es/cities_empstat$Total_es)*100
cities_empstat$P_Unemp_and_Inact_es <- ((cities_empstat$Unemployed_es+cities_empstat$Inactive_es)/cities_empstat$Total_es)*100

names(cities_trash)

##to merge multiple datasets you should include a pipe with the following tables specifing the columns 

names(aue)

names(aue)[1] <- "CITY" 
names(aue)[4] <- "year"

table(aue$CITY)

cities_merge_all <- merge(aue,cities_electric, by=c("CITY","TIME")) %>%
    merge(cities_empstat,by=c("CITY","TIME","YEAR")) %>%
    merge(cities_hhtype,by=c("CITY","TIME","YEAR")) %>% 
    merge(cities_lit,by=c("CITY","TIME","YEAR")) %>%
    merge(cities_ownership,by=c("CITY","TIME","YEAR")) %>%
    merge(cities_sewage,by=c("CITY","TIME","YEAR")) %>%
    merge(cities_trash,by=c("CITY","TIME","YEAR")) %>%
    merge(cities_water,by=c("CITY","TIME","YEAR"))

cities_merge_all %>% ggplot() + 
  geom_point(aes(x=YEAR,y=year))

names(aue)



