###########################################calculating for Bolivia
###########################################import IPUMS

ddi_bol <-read_ipums_ddi("ipumsi_00069.xml")
bolivia <- read_ipums_micro(ddi_bol)

bolivia <- transform(bolivia,SERIAL_YEAR=paste0(SERIAL,YEAR))

###########################################coding the variables

bolivia$owner_b <- ifelse(bolivia$OWNERSHIP ==1,1,0)
bolivia$toilt_b <- ifelse(bolivia$TOILET == 21|bolivia$TOILET==20,1,0)
bolivia$eletr_b <- ifelse(bolivia$ELECTRIC ==1,1,0)
bolivia$water_b <- ifelse(bolivia$WATSUP ==11,1,0)
bolivia$fuel_b <- ifelse(bolivia$FUELCOOK == 20 | bolivia$FUELCOOK == 30,1,0)
bolivia$wall_b <- ifelse(bolivia$WALL == 501,1,0)
bolivia$roof_b <- ifelse(bolivia$ROOF ==14|bolivia$ROOF ==11|bolivia$ROOF ==12,1,0)
bolivia$floors_b <- ifelse(bolivia$FLOOR !=100,1,0)
bolivia$sewage_b <- ifelse(bolivia$SEWAGE ==11|bolivia$SEWAGE ==12,1,0)
bolivia$kitchen_b <- ifelse(bolivia$KITCHEN == 20,1,0)

##calculating number of people per household

bolivia <- bolivia %>% filter(ROOMS != 99) %>%  filter (ROOMS != 98)
names(bolivia)

#######aggregating by household

bolivia_HH_b <- bolivia %>% group_by(SERIAL_YEAR,YEAR) %>% 
  summarise_at(.vars = vars(owner_b,toilt_b,eletr_b,water_b,fuel_b,roof_b,floors_b,sewage_b,kitchen_b),.funs = c("max"))

names(bolivia_HH_b)

bolivia_HH_n <- bolivia %>% group_by(SERIAL_YEAR) %>%
  count()

bolivia <- as.data.table(bolivia)
bolivia_HH_r <- bolivia[ROOMS>0, min(ROOMS), by=SERIAL_YEAR]
bolivia_HH_rn <- full_join(bolivia_HH_n,bolivia_HH_r)
bolivia_HH_rn$room_b <- bolivia_HH_rn$n/bolivia_HH_rn$V1
bolivia_HH_rn <- bolivia_HH_rn[,c(1,4)]
bolivia_HH_b<-as.data.frame(bolivia_HH_b)
bolivia_HH_b <- full_join(bolivia_HH_b,bolivia_HH_rn)

names(bolivia_HH_b)

rm(bolivia_HH_n,bolivia_HH_r)

table(bolivia_HH_b$YEAR)
###########################################calculating pca

bolivia_HH_b.pca <- prcomp(bolivia_HH_b[,c(3:12)])
summary(bolivia_HH_b.pca)                                           
str(bolivia_HH_b.pca)   

fviz_eig(bolivia_HH_b.pca)

bolivia_eig <- get_eigenvalue(bolivia_HH_b.pca)

bolivia_HH_PC<-cbind(bolivia_HH_b,bolivia_HH_b.pca$x[,1:3])

sd(bolivia_HH_PC$PC1)

bolivia_inq_index <- bolivia_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
bolivia_inq_index$eign <- bolivia_eig [1,1]
bolivia_inq_index$I <- bolivia_inq_index$PC1sd/(bolivia_inq_index$eign)^0.5
bolivia_inq_index$city <- "Bolivia"
hist(bolivia_HH_PC$PC1)

###########################################import areas

bolivia$IPUM1992 <- as.integer(bolivia$GEO2_BO1992)
bolivia$IPUM2001 <- as.integer(bolivia$GEO2_BO2001)

cochabamba_92 <- read.csv(file="cochabamba_92.csv", header = TRUE)
cochabamba_92['CITY']='cochabamba'

cochabamba_01 <- read.csv(file="cochabamba_01.csv", header = TRUE)
cochabamba_01['CITY']='cochabamba'

bolivia_92 <- bolivia %>% inner_join(cochabamba_92, by="IPUM1992")
bolivia_01 <- bolivia %>% inner_join(cochabamba_01, by="IPUM2001")

names(bolivia_92)
names(bolivia_01)

bolivia_92 <- select(bolivia_92, -c(PROV1992))
bolivia_01 <- select(bolivia_01, -c(PROV2001))

##Merging all years into one table
bolivia_full <- rbind(bolivia_92,bolivia_01)
names(bolivia_full)

bolivia_full <- bolivia_full %>% inner_join(bolivia_HH_rn,by="SERIAL_YEAR")
bolivia_full <- select (bolivia_full,-c(1,3:27,39:45))
bolivia_full <- bolivia_full[,c(1,2,13,3:12,14)]
##########for Cochabamba

cochabamba_HH_b <- bolivia_full %>% filter(bolivia_full$CITY=="cochabamba")
cochabamba_HH_b <- cochabamba_HH_b[!duplicated(cochabamba_HH_b$SERIAL_YEAR),]
names(cochabamba_HH_b)
cochabamba_HH_b.pca <- prcomp(cochabamba_HH_b[,c(4:14)])
summary(cochabamba_HH_b.pca)                                           
str(cochabamba_HH_b.pca)   

fviz_eig(cochabamba_HH_b.pca)

cochabamba_eig <- get_eigenvalue(cochabamba_HH_b.pca)

cochabamba_HH_PC<-cbind(cochabamba_HH_b,cochabamba_HH_b.pca$x[,1:3])

sd(cochabamba_HH_PC$PC1)

cochabamba_inq_index <- cochabamba_HH_PC %>% group_by(YEAR) %>% summarise(PC1sd = sd(PC1))
cochabamba_inq_index$eign <- cochabamba_eig [1,1]
cochabamba_inq_index$I <- cochabamba_inq_index$PC1sd/(cochabamba_inq_index$eign)^0.5
cochabamba_inq_index$city <- "cochabamba"
hist(cochabamba_HH_PC$PC1)

##########graph for cities and country

bol_coch_plot <- cochabamba_inq_index %>%
  ggplot(aes(x=YEAR, y=I,width=0.1,backgroundColor="white",removePanelGrid=TRUE,removePanelBorder=TRUE))+
  geom_line()+
  geom_point()+
  geom_line(data = bolivia_inq_index,color="red")+
  geom_point(data = bolivia_inq_index,color="red")+ theme(plot.title = element_text(size = 12, face = "bold"))+
  ylim(0.9,1.05)+labs(title="Cochabamba")+
  theme_bw()
bol_coch_plot
