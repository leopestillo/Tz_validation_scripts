######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape","mice",
	"tidyverse"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

#data_tz<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Multi_country/bea validation/bea_tz.csv",sep=',')
#data_sl<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Multi_country/bea validation/bea_sl.csv",sep=',')
#data_rw<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Multi_country/bea validation/bea_rw.csv",sep=',')

data_tz<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/BEA/bea_tz.csv",sep=',')
data_sl<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/BEA/bea_sl.csv",sep=',')
data_rw<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/BEA/bea_rw.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################
#clean data from Sri Lanka
data_sl2<-subset(data_sl,data_sl$rater=="e")

# write.csv(data_sl2,"/Users/jnv4/Desktop/sl_bea2.csv")

# building merged dataset
bea_data<-NULL

bea_data$country<-c(rep("tz",length(data_tz[,1])),
					rep("rw",length(data_rw[,1])),
					rep("sl2",length(data_sl2[,1])))

#Recode variables

#ROAD AREA
data_tz$road_area
data_rw$road_urban
data_sl2$road_area

bea_data$road_area<-c(data_tz$road_area,
					  data_rw$road_urban,
					  data_sl2$road_area)


#ROAD DESIGN
data_tz$road_design<-car::recode(data_tz$road_design,"
	0=1;1=0;2=0;3=0;99=NA")
data_rw$aroad_design<-car::recode(data_rw$aroad_design,"
	0=1;1=0;2=0;3=0;99=NA")
data_sl2$road_design<-car::recode(data_sl2$road_design,"
	0=1;1=0;2=0;3=0;99=NA")

bea_data$road_design<-c(data_tz$road_design,
					  data_rw$aroad_design,
					  data_sl2$road_design)

#Intersections
data_tz$intersections
data_rw$intersections
data_sl2$intersections

bea_data$intersections<-c(data_tz$intersections,
					  data_rw$intersections,
					  data_sl2$intersections)

# lane_type

data_tz$lane_type_recoded<-car::recode(data_tz$lane_type,"
	0=0; 1=1; 2=1; 3=0;4=0")
data_rw$alane_type_recoded<-car::recode(data_rw$alane_type,"
	0=0; 1=1; 2=1; 3=0;4=0")
data_sl2$lane_type_recoded<-car::recode(data_sl2$lane_type,"
	0=0; 1=1; 2=1; 3=0;4=0")

bea_data$lane_type<-c(data_tz$lane_type_recoded,
					  data_rw$alane_type_recoded,
					  data_sl2$lane_type_recoded)

# auxiliary_lane
data_tz$auxiliary_lane
data_rw$aauxiliary_lane
data_sl2$auxiliary_lane

bea_data$auxiliary_lane<-c(data_tz$auxiliary_lane,
					  data_rw$aauxiliary_lane,
					  data_sl2$auxiliary_lane)

# pavement
data_tz$pavement_recoded<-car::recode(data_tz$pavement,"
	0=0; 1=1; 2=1; 3=0")
data_rw$pavement_recoded<-data_rw$apavement
data_sl2$pavement_recoded<-car::recode(data_sl2$pavement,"
	0=0; 1=0; 2=1")

bea_data$pavement<-c(data_tz$pavement_recoded,
					  data_rw$pavement_recoded,
					  data_sl2$pavement_recoded)

# road_condition - dry = 1
data_tz$rd_condition___0
data_rw$ard_condition___0
data_sl2$rd_condition___0

bea_data$road_condition<-c(data_tz$rd_condition___0,
					       data_rw$ard_condition___0,
					       data_sl2$rd_condition___0)

#road_narrow - No norrowing - 1
data_tz$road_narrow___0
data_rw$aroad_narrow___0
data_sl2$road_narrow___0

bea_data$road_narrow<-c(data_tz$road_narrow___0,
					       data_rw$aroad_narrow___0,
					       data_sl2$road_narrow___0)

# roadside
data_tz$roadside
data_rw$aroadside
data_sl2$roadside

bea_data$roadside<-c(data_tz$roadside,
					       data_rw$aroadside,
					       data_sl2$roadside)

# walkways
data_tz$walkways<-car::recode(data_tz$walkways,"0=0;1=1;2=1")
data_rw$awalkways
data_sl2$walkways<-car::recode(data_sl2$walkways,"0=0;1=1;2=1")

bea_data$walkways<-c(data_tz$walkways,
					       data_rw$awalkways,
					       data_sl2$walkways)

# bus_stop
data_tz$bus_stop
data_rw$abus_stop
data_sl2$bus_stop

bea_data$bus_stop<-c(data_tz$bus_stop,
					       data_rw$abus_stop,
					       data_sl2$bus_stop)

# bump
data_tz$bump
data_rw$abump
data_sl2$bump

bea_data$bump<-c(data_tz$bump,
					       data_rw$abump,
					       data_sl2$bump)

# traffic_light
data_tz$traffic_light<-c(0)
data_rw$atraffic_light
data_sl2$traffic_light

bea_data$traffic_light<-c(data_tz$traffic_light,
					       data_rw$atraffic_light,
					       data_sl2$traffic_light)

# road_traffic_signs - 1 = no signs
data_tz$road_traffic_signs___0
data_rw$aroad_signs_traffic___0
data_sl2$road_traffic_signs___0

bea_data$road_traffic_signs<-c(data_tz$road_traffic_signs___0,
					       data_rw$aroad_signs_traffic___0,
					       data_sl2$road_traffic_signs___0)

# speed_limit
data_tz$speed_limit
data_rw$aspeed_limit
data_sl2$speed_limit<-car::recode(data_sl2$speed_limit,"
	0=1")

bea_data$speed_limit<-c(data_tz$speed_limit,
					       data_rw$aspeed_limit,
					       data_sl2$speed_limit)

# curves_type

data_tz$curves_type___0
data_rw$acurves_type___0
data_sl2$curves_type___0

bea_data$curves_type<-c(data_tz$curves_type___0,
					       data_rw$acurves_type___0,
					       data_sl2$curves_type___0)
# road_visibility
data_tz$visibility___0
data_rw$avisibility___0
data_sl2$visibility___0

bea_data$road_visibility<-c(data_tz$visibility___0,
					       data_rw$avisibility___0,
					       data_sl2$visibility___0)

# bridges
data_tz$bridges
data_rw$abridge
data_sl2$bridges

bea_data$bridges<-c(data_tz$bridges,
					       data_rw$abridge,
					       data_sl2$bridges)


#pedestrians_crossing
data_tz$pedestrians___0
data_rw$apedestrians___0
data_sl2$pedestrians___0

bea_data$pedestrians_crossing<-c(data_tz$pedestrians___0,
					      		 data_rw$apedestrians___0,
					       		 data_sl2$pedestrians___0)

# #pedestrian_centralrestarea
# data_tz$pedestrians___2<-c(0)
# data_rw$apedestrians___2
# data_sl2$pedestrians___2

# bea_data$pedestrians_center<-c(data_tz$pedestrians___2,
# 					      		 data_rw$apedestrians___2,
# 					       		 data_sl2$pedestrians___2)

#car_density
data_tz$ddensity_cars
data_rw$density_cars
data_sl2$car_density

bea_data$density_car<-c(data_tz$ddensity_cars,
					      		 data_rw$density_cars,
					       		 data_sl2$car_density)

#moto_density
data_tz$ddensity_motos
data_rw$density_motos
data_sl2$motorcycle_density

bea_data$density_moto<-c(data_tz$ddensity_motos,
					      		 data_rw$density_motos,
					       		 data_sl2$motorcycle_density)

#bike_density
data_tz$ddensity_bikes
data_rw$density_bikes
data_sl2$bike_density

bea_data$density_bike<-c(data_tz$ddensity_bikes,
					      		 data_rw$density_bikes,
					       		 data_sl2$bike_density)

#pedestrian_density
data_tz$ddensity_peda_crossing
data_rw$density_peda_crossing
data_sl2$pedestrian_density

bea_data$density_pedestrian<-c(data_tz$ddensity_peda_crossing,
					      		 data_rw$density_peda_crossing,
					       		 data_sl2$pedestrian_density)

#bus_truck_density
data_tz$density_bus_truck<-with(data_tz,ddensity_big_bus+
	ddensity_trucks+ddensity_daladala)
data_rw$density_bus_truck<-data_rw$density_trucks+data_rw$density_big_bus
data_sl2$bus_truck_density

bea_data$density_bus_truck<-c(data_tz$density_bus_truck,
					      		 data_rw$density_bus_truck,
					       		 data_sl2$bus_truck_density)

# data_tz$unevenness_roadside
# data_sl$unevenness_roadside
# data_rw$aunevenness_roadside

# Organizing data set

bea_data<-as.data.frame(bea_data)
# ?summary(bea_data)

# bea_data$road_design<-car::recode(bea_data$road_design,"
# 	0=1;1=0;2=0;3=0;99=NA")
bea_data$intersections<-car::recode(bea_data$intersections,"
	0=0;1=1;2=1;99=NA")
bea_data$auxiliary_lane<-car::recode(bea_data$auxiliary_lane,"
	99=NA")
bea_data$pavement<-car::recode(bea_data$pavement,"
	99=NA")
bea_data$bus_stop<-car::recode(bea_data$bus_stop,"
	99=NA")
bea_data$speed_limit<-car::recode(bea_data$speed_limit,"
	99=0")

## INTERRATER AGREEMENT dataset#

 data_agreement<-with(data_sl,data.frame(
 				road_area,
 				road_design,
 				intersections,
 				lane_type,
 				auxiliary_lane,
 				bridges,
 				pavement,
 				visibility___0,
 				curves_type___0,
 				rd_condition___0,
 				roadside,
 				traffic_light,
 				road_traffic_signs___0,
 				speed_limit,
 				bump,
 				car_density,
 				motorcycle_density,
 				bike_density,
 				bus_truck_density,
 				bus_stop,
 				pedestrians___0,
 				pedestrian_density,
 				walkways,
 				rater))

 data_agreement$road_area<-car::recode(data_agreement$road_area,"
 	0=1;1=0;2=0;3=0;99=NA")

 data_agreement$road_design<-car::recode(data_agreement$road_design,"
 	0=1;1=0;2=0;3=0;99=NA")

 data_agreement$lane_type<-car::recode(data_agreement$lane_type,"
 	0=0; 1=1; 2=1; 3=0;4=0;99=0")

 data_agreement$pavement<-car::recode(data_agreement$pavement,"
 	0=0; 1=0; 2=1")

 data_agreement$walkways<-car::recode(data_agreement$walkways,"0=0;1=1;2=1")

 data_agreement$speed_limit<-car::recode(data_agreement$speed_limit,"
 	0=1")

 data_agreement$intersections<-car::recode(data_agreement$intersections,"
 	0=0;1=1;2=1;99=NA")

 data_agreement$auxiliary_lane<-car::recode(data_agreement$auxiliary_lane,"
 	99=NA")

 data_agreement$pavement<-car::recode(data_agreement$pavement,"
 	99=NA")

data_agreement$visibility___0<-car::recode(data_agreement$visibility___0,"
 	99=NA")

 data_agreement$bus_stop<-car::recode(data_agreement$bus_stop,"
 	99=NA")

 data_agreement$speed_limit<-car::recode(data_agreement$speed_limit,"
 	99=0")

 data_agreement$roadside<-car::recode(data_agreement$roadside,"
 	99=NA")

 data_agreement$bridges<-car::recode(data_agreement$bridges,"
 	99=NA")

 data_agreement$lane_type<-car::recode(data_agreement$lane_type,"
 	99=NA")

 data_agreement$road_area<-car::recode(data_agreement$road_area,"
 	99=NA")

 data_agreement$id<-rep(1:127, times=1, each=3)

 agreement_tidy<- data_agreement %>% 
 					gather(key,value,-rater,-id) %>%
 					unite(items,rater,key,sep='.') %>%
 					spread(items,value)

with(agreement_tidy,data.frame(e.visibility___0,t.visibility___0,s.visibility___0))

##############################################################
#Descriptives
#############################################################
# Road Area
table<-with(bea_data,table(road_area))
table
prop.table(table)
table<-with(bea_data,table(road_area,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(road_design))
table
prop.table(table)
table<-with(bea_data,table(road_design,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(intersections))
table
prop.table(table)
table<-with(bea_data,table(intersections,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(lane_type))
table
prop.table(table)
table<-with(bea_data,table(lane_type,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(auxiliary_lane))
table
prop.table(table)
table<-with(bea_data,table(auxiliary_lane,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(pavement))
table
prop.table(table)
table<-with(bea_data,table(pavement,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(road_condition))
table
prop.table(table)
table<-with(bea_data,table(road_condition,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(road_narrow))
table
prop.table(table)
table<-with(bea_data,table(road_narrow,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(roadside))
table
prop.table(table)
table<-with(bea_data,table(roadside,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(walkways))
table
prop.table(table)
table<-with(bea_data,table(walkways,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(bus_stop))
table
prop.table(table)
table<-with(bea_data,table(bus_stop,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(bump))
table
prop.table(table)
table<-with(bea_data,table(bump,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(traffic_light))
table
prop.table(table)
table<-with(bea_data,table(traffic_light,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(road_traffic_signs))
table
prop.table(table)
table<-with(bea_data,table(road_traffic_signs,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(speed_limit))
table
prop.table(table)
table<-with(bea_data,table(speed_limit,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(curves_type))
table
prop.table(table)
table<-with(bea_data,table(curves_type,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(road_visibility))
table
prop.table(table)
table<-with(bea_data,table(road_visibility,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(bridges))
table
prop.table(table)
table<-with(bea_data,table(bridges,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(pedestrians_crossing))
table
prop.table(table)
table<-with(bea_data,table(pedestrians_crossing,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(pedestrians_center))
table
prop.table(table)
table<-with(bea_data,table(pedestrians_center,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(bea_data,table(pedestrians_walkways))
table
prop.table(table)
table<-with(bea_data,table(pedestrians_walkways,country))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Car density
summary(bea_data$density_car)
ad.test(bea_data$density_car)
#hist(bea_data$density_car)
#ci_func(bea_data$density_car,.95)
by(bea_data$density_car,bea_data$country,summary)
wilcox.test(bea_data$density_car~bea_data$country)

# Moto density
summary(bea_data$density_moto)
ad.test(bea_data$density_moto)
#hist(bea_data$density_moto)
#ci_func(bea_data$density_moto,.95)
by(bea_data$density_moto,bea_data$country,summary)
wilcox.test(bea_data$density_moto~bea_data$country)

# Bike density
summary(bea_data$density_bike)
ad.test(bea_data$density_bike)
#hist(bea_data$density_bike)
#ci_func(bea_data$density_bike,.95)
by(bea_data$density_bike,bea_data$country,summary)
wilcox.test(bea_data$density_bike~bea_data$country)

# Bike density
summary(bea_data$density_pedestrian)
ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
by(bea_data$density_pedestrian,bea_data$country,summary)
wilcox.test(bea_data$density_pedestrian~bea_data$country)

# Bike density
summary(bea_data$density_bus_truck)
ad.test(bea_data$density_bus_truck)
#hist(bea_data$density_bus_truck)
#ci_func(bea_data$density_bus_truck,.95)
by(bea_data$density_bus_truck,bea_data$country,summary)
wilcox.test(bea_data$density_bus_truck~bea_data$country)


##############################################################
#Agreement
#############################################################

#road design - All 0
cohen.kappa(na.omit(agreement_tidy[,c(18,64)]),
	w=NULL,n.obs=127,alpha=.05)  
cohen.kappa(na.omit(agreement_tidy[,c(17,63)]),
	w=NULL,n.obs=127,alpha=.05) 
cohen.kappa(na.omit(agreement_tidy[,c(17,40)]),
	w=NULL,n.obs=127,alpha=.05) 
cohen.kappa(na.omit(agreement_tidy[,c(40,63)]),
	w=NULL,n.obs=127,alpha=.05) 

#intersections
cohen.kappa(na.omit(agreement_tidy[,c(10,56)]),
	w=NULL,n.obs=127,alpha=.05) 

#lane type
cohen.kappa(na.omit(agreement_tidy[,c(11,57)]),
	w=NULL,n.obs=127,alpha=.05) 

#auxilliary lane
cohen.kappa(na.omit(agreement_tidy[,c(2,48)]),
	w=NULL,n.obs=127,alpha=.05) 

#visibility
cohen.kappa(na.omit(agreement_tidy[,c(23,69)]),
	w=NULL,n.obs=127,alpha=.05) 
cohen.kappa(na.omit(agreement_tidy[,c(23,46)]),
	w=NULL,n.obs=127,alpha=.05) 
cohen.kappa(na.omit(agreement_tidy[,c(46,69)]),
	w=NULL,n.obs=127,alpha=.05) 


#rcurve type
cohen.kappa(na.omit(agreement_tidy[,c(9,55)]),
	w=NULL,n.obs=127,alpha=.05) 

#bridges
cohen.kappa(na.omit(agreement_tidy[,c(4,50)]),
	w=NULL,n.obs=127,alpha=.05) 

#road condition
cohen.kappa(na.omit(agreement_tidy[,c(16,62)]),
	w=NULL,n.obs=127,alpha=.05) 

#roadside
cohen.kappa(na.omit(agreement_tidy[,c(20,66)]),
	w=NULL,n.obs=127,alpha=.05) 

#traffic light
cohen.kappa(na.omit(agreement_tidy[,c(22,68)]),
	w=NULL,n.obs=127,alpha=.05) 

#traffic signs
cohen.kappa(na.omit(agreement_tidy[,c(19,65)]),
	w=NULL,n.obs=127,alpha=.05) 

#speed limit
cohen.kappa(na.omit(agreement_tidy[,c(21,67)]),
	w=NULL,n.obs=127,alpha=.05)

#speed bump
cohen.kappa(na.omit(agreement_tidy[,c(5,51)]),
	w=NULL,n.obs=127,alpha=.05)

#bus stop
cohen.kappa(na.omit(agreement_tidy[,c(6,52)]),
	w=NULL,n.obs=127,alpha=.05)

#pedestrian crossing
cohen.kappa(na.omit(agreement_tidy[,c(15,61)]),
	w=NULL,n.obs=127,alpha=.05)

#walkways
cohen.kappa(na.omit(agreement_tidy[,c(24,70)]),
	w=NULL,n.obs=127,alpha=.05)

#pedestrian_density
ICC(na.omit(agreement_tidy[,c(14,60)]))

#car density
ICC(na.omit(agreement_tidy[,c(8,54)]))

#moto density
ICC(na.omit(agreement_tidy[,c(12,58)]))

#bike density
ICC(na.omit(agreement_tidy[,c(3,49)]))

#bus_truck_density
ICC(na.omit(agreement_tidy[,c(7,53)]))

##############################################################
#PCA Score comparisons
#############################################################
#MODEL 1 - Risk due to road deisgn
model1_bea<-with(bea_data,data.frame(
			road_area,
			# road_design,
			intersections,
			lane_type,
			auxiliary_lane,
			bridges,
			#road_visibility,
			curves_type))
			# roadside))

# #MODEL 1 - Risk due to road deisgn
# model1_bea_1<-with(bea_data,data.frame(
# 			road_area,
# 			# road_design,
# 			intersections,
# 			lane_type,
# 			auxiliary_lane,
# 			# bridges,
# 			road_visibility))
# 			# curves_type,
# 			# road_condition,
# 			# roadside))

# #MODEL 1 - Risk due to road deisgn
# model1_bea_1<-with(bea_data,data.frame(
# 			road_area,
# 			# road_design,
# 			intersections,
# 			lane_type,
# 			auxiliary_lane,
# 			# bridges,
# 			road_visibility))
# 			# curves_type,
# 			# road_condition,
# 			# roadside))

#reliability
psych::alpha(model1_bea,n.iter=1000,check.keys=TRUE)
k20(model2_bea)

#correlation
cor_data<-cor_auto(model1_bea)

#pca
model <- principal(cor_data ,nfactors=1, rotate='none', scores=T, cov=T)
 L <- model$loadings            # Just get the loadings matrix
 S <- model$scores              # This gives an incorrect answer in the current version

 d <- model1_bea              # get your data
 dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
 pca1 <- dc %*% L                 # scores are the centered data times the loadings
 # lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

# plot(model)

#MODEL 2 - Built driving safety
model2_bea<-with(bea_data,data.frame(
			traffic_light,
			road_traffic_signs,
			speed_limit,
			bump))


#reliability
psych::alpha(model2_bea,n.iter=1000,check.keys=TRUE)

cor_data<-cor_auto(model2_bea)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
# fa.parallel(cor_data,n.obs=229)

# # Pricipal Components Analysis
# # entering raw data and extracting PCs 
# # from the correlation matrix 
# fit <- psych::principal(cor_data,nfactors=1,rotate="none",scores=TRUE)
# fit
# summary(fit) # print variance accounted for 
# loadings(fit) # pc loadings 
# fit$scores
# predict(fit,bea_data[,-1])
# scores<-scoreItems(fit$weights,bea_data[,-1],totals=TRUE)
# summary(scores)
# describe(scores$scores)
# by(scores$scores,data_bea$risk_classification,summary)
# wilcox.test(scores$scores[,1]~data_bea$risk_classification)
# wilcox.test(scores$scores[,2]~data_bea$risk_classification)
# wilcox.test(scores$scores[,3]~data_bea$risk_classification)
# #wilcox.test(scores$scores[,4]~data_bea$risk_classification)

model <- principal(cor_data ,nfactors=1, rotate='none', scores=T, cov=T)
L <- model$loadings            # Just get the loadings matrix
S <- model$scores              # This gives an incorrect answer in the current version

d <- model2_bea             # get your data
dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
pca2 <- dc %*% L                 # scores are the centered data times the loadings
# lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

# plot(model)


#MODEL 3 - Vehicle density
model3_bea<-with(bea_data,data.frame(
			density_car,
			density_moto,
			density_bike,
			density_bus_truck))

#reliability
psych::alpha(model3_bea,n.iter=1000,check.keys=TRUE)

cor_data<-cor_auto(model3_bea)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
# fa.parallel(cor_data,n.obs=229)

# # Pricipal Components Analysis
# # entering raw data and extracting PCs 
# # from the correlation matrix 
# fit <- psych::principal(cor_data,nfactors=1,rotate="none",scores=TRUE)
# fit
# summary(fit) # print variance accounted for 
# loadings(fit) # pc loadings 
# fit$scores
# predict(fit,bea_data[,-1])
# scores<-scoreItems(fit$weights,bea_data[,-1],totals=TRUE)
# summary(scores)
# describe(scores$scores)
# # by(scores$scores,data_bea$risk_classification,summary)
# # wilcox.test(scores$scores[,1]~data_bea$risk_classification)
# wilcox.test(scores$scores[,2]~data_bea$risk_classification)
# wilcox.test(scores$scores[,3]~data_bea$risk_classification)
# #wilcox.test(scores$scores[,4]~data_bea$risk_classification)

 model <- principal(cor_data ,nfactors=1, rotate='oblimin', scores=T, cov=T)
 L <- model$loadings            # Just get the loadings matrix
 S <- model$scores              # This gives an incorrect answer in the current version

 d <- model3_bea            # get your data
 dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
 pca3 <- dc %*% L                 # scores are the centered data times the loadings
# lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

# plot(model)

#MODEL 4 - Built pedestrian safety
model4_bea<-with(bea_data,data.frame(
			bus_stop,
			pedestrians_crossing,
			density_pedestrian,
			walkways))

#reliability
psych::alpha(model4_bea,n.iter=1000,check.keys=TRUE)

cor_data<-cor_auto(model4_bea)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
# fa.parallel(cor_data,n.obs=229)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
# fit <- psych::principal(cor_data,nfactors=1,rotate="none",scores=TRUE)
# fit
# summary(fit) # print variance accounted for 
# loadings(fit) # pc loadings 
# fit$scores
# predict(fit,model4_bea)
# scores<-scoreItems(fit$weights,model4_bea,totals=TRUE)
# summary(scores)
# describe(scores$scores)
# by(scores$scores,data_bea$risk_classification,summary)
# wilcox.test(scores$scores[,1]~data_bea$risk_classification)
# wilcox.test(scores$scores[,2]~data_bea$risk_classification)
# wilcox.test(scores$scores[,3]~data_bea$risk_classification)
# #wilcox.test(scores$scores[,4]~data_bea$risk_classification)

 model <- principal(cor_data ,nfactors=1, rotate='none', scores=T, cov=T)
  L <- model$loadings            # Just get the loadings matrix
 # S <- model$scores              # This gives an incorrect answer in the current version

  d <- model4_bea              # get your data
  dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
  pca4 <- dc %*% L                 # scores are the centered data times the loadings
#  lowerCor(sc)                   #These scores, being principal components
# #                                # should be orthogonal 

#  plot(model)

# score<-round(pnorm(sc)*100,2)

#Extracting normalized data
bea_data$ID<-c(1:188)
pca_scores_data<-data.frame(PCA1a=pca1[,1],PCA1b=pca1[,2],
							PCA2=pca2[,1],PCA3=pca3[,1],
							PCA4=pca4[,1],id=bea_data$ID,
							country=bea_data[,1])
# pca_scores_data<-na.omit(pca_scores_data)
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(pca_scores_data, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
pca_scores_data<-mice::complete(data_imputed,4)

rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
pca_score_data_rescaled<-lapply(pca_scores_data[,1:5],rescale)
pca_score_data_rescaled<-round(as.data.frame(pca_score_data_rescaled),1)
pca_score_data_rescaled<-data.frame(pca_score_data_rescaled,
	pca_scores_data$id,pca_scores_data$country)

##############################################################
#CLUSTER ANALYSIS 
##############################################################

cluster_data<-with(pca_score_data_rescaled,
	data.frame(PCA1a,PCA1b,PCA2,PCA3,PCA4))

# # ses_data<-na.omit(ses_data)
# clusters <- hclust(dist(cluster_data))
# plot(clusters)
# print(cluster)
# clusterCut <- cutree(clusters, 4)
# print(clusterCut)

fit <- mclustBIC(cluster_data,3)
plot(fit)
cl<-mclustModel(cluster_data,fit)
print(cl) # display the best model
cl$parameters$mean
cl$parameters$pro
x<-NULL
x[cl$z[,1]>=0.51]<-c("Lat1")
x[cl$z[,2]>=0.51]<-c("Lat2")
x[cl$z[,3]>=0.51]<-c("Lat3")


pca_score_data_rescaled$clusters<-as.factor(x)

# write.csv(pca_score_data_rescaled,"/Users/joaovissoci/Desktop/bea_PCAscores.csv")

# pca_score_data_rescaled_sl<-subset(pca_score_data_rescaled,pca_score_data_rescaled[,7]=="sl2")
# pca_score_data_rescaled_sl$risk_classification<-data_sl$risk_classification
# bea_data_sl<-cbind(pca_score_data_rescaled_sl,data_sl2)

# write.csv(bea_data_sl,"/Users/joaovissoci/Desktop/sl_bea_data.csv")

with(pca_score_data_rescaled,
	by(PCA1a,clusters,summary))
with(pca_score_data_rescaled,
	kruskal.test(PCA1a,clusters))

with(pca_score_data_rescaled,
	by(PCA1b,clusters,summary))
with(pca_score_data_rescaled,
	kruskal.test(PCA1b~clusters))

with(pca_score_data_rescaled,
	by(PCA2,clusters,summary))
with(pca_score_data_rescaled,
	kruskal.test(PCA2~clusters))

with(pca_score_data_rescaled,
	by(PCA3,clusters,summary))
with(pca_score_data_rescaled,
	kruskal.test(PCA3~clusters))

with(pca_score_data_rescaled,
	by(PCA4,clusters,summary))
with(pca_score_data_rescaled,
	kruskal.test(PCA4~clusters))

##############################################################
#NETWORK 
##############################################################
# # Define the amout of factor to retain
# #Group of functinos to determine the number os items to be extracted
# cor_data<-cor_auto(bea_data[,-c(1,7,9)])

# # #Community analysis
# # comprehension_network_glasso<-qgraph(cor_data,
# # 	layout="spring",
# # 	vsize=6,esize=20,graph="glasso",
# # 	sampleSize=nrow(bea_data),
# # 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# # #Calculating Community measures
# # g<-as.igraph(comprehension_network_glasso) #creating igraph object
# # h<-walktrap.community(g) #creatin community object
# # h<-spinglass.community(g, weights=NA)
# # plot(h,g) #plotting community network
# # h$membership #extracting community membership for each node on the network
# # community<-data.frame(h$membership,rownames(cor_data))

# #listing grouping variables in the network resulting from the community analysis
# # network_groups<-list(
# # Component1=as.numeric(rownames(community)[community[,1]==1]),
# # Component2=as.numeric(rownames(community)[community[,1]==2]),
# # Component3=as.numeric(rownames(community)[community[,1]==3])
# # )

# network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# # creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# # creating nodes labels vector
# node_names<-c("RD","RT","INT","TLA","AR",
# 	"RC","RS",
# 	"WALK","BS","SB","TLI","TS","SL","CUR",
# 	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

# # creating vector with mean values for each node
# #mean_data<-sapply(network_data,mean)

# #creating vector with mean values adjusted to proportional sizes to be plotted
# #importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

# #building network figures 
# # 3 types are created to get an avarege position and layout
# #GLASSO NETWORK
# # network_glasso<-qgraph(cor_data,layout="spring",
# # 	vsize=6,esize=20,graph="glasso",
# # 	sampleSize=nrow(bea_data),
# # 	legend.cex = 0.5,GLratio=1.5)

# # #PARTIAL CORRELATION NETWORK
# # network_pcor<-qgraph(cor_data,layout="spring",
# # 	vsize=6,esize=20,graph="pcor",threshold="holm",
# # 	sampleSize=nrow(bea_data),
# # 	legend.cex = 0.5,GLratio=1.5)

# # #CORRELATION NETWORK
# # network_cor<-qgraph(cor_data,layout="spring",
# # 	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# # #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # # Organizing both figures to be with the same layout
# # layout_final<-averageLayout(network_glasso,
# # 	network_pcor,
# # 	network_cor)

# #postscript("/home/joao/Desktop/info_consent_figure2.eps",
# #	width = 1500, height = 1200,horizontal = FALSE, 
# #	onefile = FALSE)
# #postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
# #	width = 1500, height = 1200,horizontal = FALSE, 
# #	onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
#  height = 700,compression = 'lzw')
# final_importance_network<-qgraph(cor_data,
# 	esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.6,cut = 0.3, maximum = 1, 
# 	minimum = 0.1, esize = 20,vsize = 5, 
# 	repulsion = 0.8,groups=network_groups,
# 	nodeNames=node_labels,
# 	color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
# 	labels=node_names)#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
# dev.off()
# #legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

##############################################################
#### INTER-RATER Agreement
##############################################################

#MODEL 1
data_sl_temp_model1<-with(data_sl,data.frame(
	road_area,
	intersections,
	lane_type,
	auxiliary_lane,
	visibility___0,
	bridges,
	rater))

# data_sl_agree_model1<-cast(data_sl_temp_model1,id=c("rater"))

#data_sl_agree2<-cast(data_sl_agree_model1,id~rater+variable)

data_sl_agree_model1_e<-subset(data_sl_temp_model1,
	data_sl_temp_model1$rater=='e')
data_sl_agree_model1_s<-subset(data_sl_temp_model1,
	data_sl_temp_model1$rater=='s')
data_sl_agree_model1_t<-subset(data_sl_temp_model1,
	data_sl_temp_model1$rater=='t')

agree_data_sl_model1<-data.frame(
	data_sl_agree_model1_e,
	data_sl_agree_model1_t,
	data_sl_agree_model1_s)

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(1,8)]), tolerance=0) #% of Agreement
agree

agree_data_sl_model1[,1]<-car::recode(agree_data_sl_model1[,1],"NA=0")

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(1,8)])) #Kappa-value
kappa

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(2,9)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(2,9)])) #Kappa-value
kappa

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(3,10)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(3,10)])) #Kappa-value
kappa

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(4,11)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(4,11)])) #Kappa-value
kappa

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(5,12)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(5,12)])) #Kappa-value
kappa

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1[c(6,13)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model1[,c(6,13)])) #Kappa-value
kappa
	
#MODEL 2
data_sl_temp_model2<-with(data_sl,data.frame(
	traffic_light,
	road_traffic_signs___0,
	speed_limit,
	bump,
	rater))

#data_sl_agree2<-cast(data_sl_agree_model2,id~rater+variable)

data_sl_agree_model2_e<-subset(data_sl_temp_model2,
	data_sl_temp_model2$rater=='e')
data_sl_agree_model2_s<-subset(data_sl_temp_model2,
	data_sl_temp_model2$rater=='s')
data_sl_agree_model2_t<-subset(data_sl_temp_model2,
	data_sl_temp_model2$rater=='t')

agree_data_sl_model2<-data.frame(
	data_sl_agree_model2_e,
	data_sl_agree_model2_t)

#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model2[c(1,6)]), tolerance=0) #% of Agreement
agree

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model2[,c(1,6)])) #Kappa-value
kappa

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model2[,c(2,7)])) #Kappa-value
kappa

kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model2[,c(3,8)])) #Kappa-value
kappa
	
kappa<-psych::cohen.kappa(na.omit(agree_data_sl_model2[,c(4,9)])) #Kappa-value
kappa

#MODEL 3
data_sl_temp_model3<-with(data_sl,data.frame(
	car_density,
	motorcycle_density,
	bike_density,
	pedestrian_density,
	bus_truck_density,
	rater,
	id))

data_sl_agree_model3<-melt(data_sl_temp_model3,id=c("rater","id"))

#data_sl_agree2<-cast(data_sl_agree_model3,id~rater+variable)

data_sl_agree_model3_e<-subset(data_sl_agree_model3,
	data_sl_agree_model3=='e')
data_sl_agree_model3_s<-subset(data_sl_agree_model3,
	data_sl_agree_model3=='s')
data_sl_agree_model3_t<-subset(data_sl_agree_model3,
	data_sl_agree_model3=='t')

agree_data_sl_model3<-data.frame(
	data_sl_agree_model3_t$value,
	data_sl_agree_model3_e$value)


#MODEL 4
data_sl_temp_model4<-with(data_sl,data.frame(
	walkways,
	bus_stop,
	pedestrians___0,
	pedestrians___2,
	rater))
	
data_sl_agree_model4<-melt(data_sl_temp_model4,id=c("rater","id"))

#data_sl_agree2<-cast(data_sl_agree_model4,id~rater+variable)

data_sl_agree_model4_e<-subset(data_sl_agree_model4,
	data_sl_agree_model4=='e')
data_sl_agree_model4_s<-subset(data_sl_agree_model4,
	data_sl_agree_model4=='s')
data_sl_agree_model4_t<-subset(data_sl_agree_model4,
	data_sl_agree_model4=='t')

agree_data_sl_model4<-data.frame(
	data_sl_agree_model4_e$value,
	data_sl_agree_model4_s$value)