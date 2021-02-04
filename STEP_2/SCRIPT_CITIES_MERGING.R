library(ipumsr)
library(dbplyr)
library(tidyverse)

##set the working directory

getwd()
setwd("C:/workingdirectory")

##loading dataframes generated in Step 1

load("argentina_full.Rda")
load("bangladesh_full.Rda")
load("bolivia_full.Rda")
load("brazil_full.Rda")
load("chile_full.Rda")
load("colombia_full.Rda")
load("ecuador_full.Rda")
load("egypt_full.Rda")
load("el_salvador_full.Rda")
load("ethiopia_full.Rda")
load("fiji_full.Rda")
load("ghana_full.Rda")
load("greece_full.Rda")
load("indonisia_full.Rda")
load("kenya_full.Rda")
load("mali_full.Rda")
load("mexico_full.Rda")
load("moroco_full.Rda")
load("mozambique_full.Rda")
load("nicaragua_full.Rda")
load("nigeria_full.Rda")
load("pakistan_full.Rda")
load("philippines_full.Rda")
load("rwanda_full.Rda")
load("southafrica_full.Rda")
load("spain_full.Rda")
load("sudan_full.Rda")
load("tanzania_full.Rda")
load("thailand_full.Rda")
load("turkey_full.Rda")
load("uganda_full.Rda")
load("venezuela_full.Rda")
load("zambia_full.Rda")
load("china_full.Rda")

##checking variables 

names(ecuador_full)
names(argentina_full)
names(bangladesh_full)
names(bolivia_full)
names(brazil_full)
names(chile_full)
names(egypt_full)
names(ethiopia_full)
names(fiji_full)
names(greece_full)
names(indonesia_full)
names(kenya_full)
names(mali_full)
names(moroco_full)
names(mozambique_full)
names(pakistan_full)
names(philippines_full)
names(spain_full)
names(sudan_full)
names(southafrica_full)
names(tanzania_full)
names(thailand_full)
names(turkey_full)
names(uganda_full)
names(zambia_full)
names(cities_full_sample)
names(rwanda_full)
names(china_full)

## all the dataframes must contain the same variables for the merging process
## include variables for  

china_full$OWNERSHIP<-NA
china_full$OWNERSHIPD<-NA
china_full$OWNERSHIPD<-NA
china_full$ELECTRIC<-NA
china_full$WATSUP<-NA
china_full$SEWAGE<-NA
china_full$TRASH<-NA
china_full$ROOMS<-NA
china_full$YRSCHOOL<-NA
argentina_full$TRASH<-NA
bangladesh_full$WATSUP<-NA
bangladesh_full$SEWAGE<-NA
bangladesh_full$TRASH<-NA
bangladesh_full$ROOMS<-NA
bolivia_full$TRASH<-NA
chile_full$TRASH<-NA
egypt_full$TRASH<-NA
egypt_full$YRSCHOOL<-NA
ethiopia_full <- select(ethiopia_full, -c(FORMTYPE))
ethiopia_full <- select(ethiopia_full, -c(RESIDENT))
ethiopia_full$SEWAGE<-NA
fiji_full$WATSUP<-NA
fiji_full$SEWAGE<-NA
fiji_full$LIT<-NA
greece_full$TRASH<-NA
greece_full$YRSCHOOL<-NA
indonesia_full$TRASH<-NA
indonesia_full$ROOMS<-NA
kenya_full$TRASH<-NA
mali_full <- select(mali_full, -c(RESIDENT))
moroco_full$TRASH<-NA
mozambique_full$SEWAGE <-NA
mozambique_full$YRSCHOOL<-NA
mozambique_full <- select(mozambique_full, -c(RESIDENT))
mozambique_full$TRASH<-NA
pakistan_full$SEWAGE<-NA
pakistan_full <- select(pakistan_full, -c(RESIDENT))
pakistan_full$TRASH<-NA
pakistan_full$YRSCHOOL<-NA
pakistan_full$EMPSTAT<-NA
pakistan_full$EMPSTATD<-NA
philippines_full$SEWAGE<-NA
philippines_full$ROOMS<-NA
rwanda_full <- select(rwanda_full, -c(RESIDENT))
spain_full$TRASH<-NA
spain_full$YRSCHOOL<-NA
southafrica_full <- select(southafrica_full, -c(RESIDENT))
southafrica_full$LIT<-NA
sudan_full$SEWAGE<-NA
sudan_full$TRASH<-NA
sudan_full$ROOMS<-NA
sudan_full$YRSCHOOL<-NA
tanzania_full <- select(tanzania_full, -c(IPUM1988,IPUM2002,IPUM2012,GEO2_TZ1988,GEO2_TZ2002,GEO2_TZ2012))
tanzania_full$SEWAGE<-NA
thailand_full$SEWAGE<-NA
thailand_full$TRASH<-NA
thailand_full$ROOMS<-NA
thailand_full$EMPSTAT<-NA
thailand_full$EMPSTATD<-NA
turkey_full$SEWAGE<-NA
turkey_full$ELECTRIC<-NA
turkey_full$TRASH<-NA
turkey_full$YRSCHOOL<-NA
turkey_full$HHTYPE<-NA
turkey_full$NFAMS<-NA
uganda_full <- select(uganda_full, -c(RESIDENT))
uganda_full$SEWAGE<-NA
zambia_full <- select(zambia_full, -c(RESIDENT))

##joining dataframes

cities_full_sample <- rbind(argentina_full,bangladesh_full,bolivia_full,brazil_full,chile_full,colombia_full,china_full,ecuador_full,egypt_full,el_salvador_full,ethiopia_full,fiji_full,ghana_full,greece_full,indonesia_full,kenya_full,mali_full,mexico_full,moroco_full,mozambique_full,nicaragua_full,nigeria_full,pakistan_full,philippines_full,southafrica_full,spain_full,sudan_full,rwanda_full,tanzania_full,thailand_full,turkey_full,uganda_full,venezuela_full,zambia_full)
save(cities_full_sample,file="cities_full_sample.Rda")
