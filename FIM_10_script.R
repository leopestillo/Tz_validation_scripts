#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
#Desktop
data<-read.csv("D:/Google Drive/Life Discussions/FIM/Tz_MHpostTBI_data.csv",sep=',')

#Mac
data<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_MHpostTBI_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################


#subsetting data set to keep only baseline data
data_validation<-subset(data[data$redcap_event_name=="enrollment_arm_1",])

#recoding gender variable
dataFemale<-car::recode(data_validation$Female,"
                                    0='Masculino';1='Feminino'")

#recoding marital status variable
data_validation$married<-car::recode(data_validation$married,"
                                     0='married';1='married';2='not married';
                                     3='not married';4='married';5='not married'")

#recoding education varibLE
data_validation$education_cat<-car::recode(data_validation$education,"
                                           0:7='Some primary';8:13='Some secondary';
                                           14:16='Some university';89=NA")

# #recoding education varibLE
data_validation$occupation_cat<-car::recode(
  data_validation$occupation,"
  0='Business';1='Farming';
  3='Paid worker';4='Skilled worker';
  5='Paid worker';6='Other';8='Other';89=NA")

#recoding education varibLE
data_validation$age_cat<-car::recode(
  data_validation$age,"
  0:35='<35';36:100='>35'")

#Organize scale datasets

#FIM-10
FIM_data1<-with(data_validation,data.frame(g1,g6,g10,g14,g15,g17,g18,g22,g26,g27))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(FIM_data1, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
FIM_data<-mice::complete(data_imputed,4)

#FIM Motor
FIM_Motor10<-with(FIM_data,data.frame(g1,g6,g10,g14,g15))

#FIM Cognitive
FIM_Cognitive10<-with(FIM_data,data.frame(g17,g18,g22,g26,g27))

######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################
###Section wih several exploratory data analysis functions
###### Exploratory Data Anlysis
###### UNIVARIATE

# Numerical descriptives
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
<-describe(data_validation$age)
x<-describe(FIM_data30)

# Categorical Descriptives
table<-with(data_validation,table(female))
table
prop.table(table)

table<-with(data_validation,table(married))
table
prop.table(table)

table<-with(data_validation,table(education_cat))
table
prop.table(table)

table<-with(data_validation,table(occupation_cat))
table
prop.table(table)

##### BIVARIATE

# #Graphing and homogeneity
# boxplot(data$Idade~data$Classificacao) #will provide a boxplot for the #variables to analysis potential outliers
# ## Bartlett Test of Homogeneity of Variances
# #bartlett.test(data$Idade~data$Classificacao, data=data)
# ## Figner-Killeen Test of Homogeneity of Variances
# #fligner.test(data$Idade~data$Classificacao, data=data)
# #leveneTest(data$Idade~data$Classificacao, data=data)

# # Categorical Descriptives 2x2, 2x3 ...
# table<-with(bea_data,table(road_area,country)) #create cross-tabs with 2 caegorical variables
# table #display cross-tabs
# prop.table(table,2) #find proportions. Argument #2 means  proportion by columns. Change to 1 for rows.
# chisq.test(table) #chi-square test
# fisher.test(table) #fisher's exact correction for cases with less then 5 observations
# assocstats(table) #vcd package, gives all range of associations

# # Numerical descriptives
# # function by allow to appy a function to a vector conditional on a factor vector
# by(bea_data$density_car,bea_data$country,summary)
# #wilcox comparisons, change for t.test for parametric data
# wilcox.test(bea_data$density_car~bea_data$country)

##############################################################
#FIM
##############################################################

#TAXONOMETRIC ANALAYSIS

#Taxonometric Scale
# MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)


#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(FIM_data,n.iter=1000,check.keys=TRUE)
psych::alpha(FIM_Motor10,n.iter=1000,check.keys=TRUE)
psych::alpha(FIM_Cognitive10,n.iter=1000,check.keys=TRUE)

#### INTER-RATER Agreement
# data_agreement<-with(data,data.frame( ))

# data_sl_agree_model1<-melt(data_sl_temp_model1,id=c("rater","id"))

#Executing agreement nalysis
# agree<-agree(na.omit(agree_data_sl_model1), tolerance=0) #% of Agreement
# kappa<-cohen.kappa(na.omit(agree_data_sl_model1)) #Kappa-value
#AC1(kappa$agree)
#cor<-cor(agree_data_sl,method=c("kendall"))
#kendall<-Kendall(agree_data_sl$data_cluster_police_sl.RISK,agree_data_sl$data_cluster_survey_sl.RISK)
#poly<-hetcor(agree_data_sl)

#NETWORK 
##############################################################
# # Define the amout of factor to retain
# #Group of functinos to determine the number os items to be extracted
#calculate correlation matrix
cor_data<-cor_auto(FIM_data) 

#extract thresholds
polycor_data<-polychoric(na.omit(FIM_data),correct=.01) # crossvalidate
cor<-polycor_data$rho #extract correlation matrix
tau<-rowMeans(polycor_data$tau) #extract thresholds

#Community analysis - Walking Trap
#Generate glasso network
network_glasso<-qgraph(
  cor_data,
  layout="spring", 
  #vsize=tau,
  # esize=20,
  graph="glasso",
  sampleSize=nrow(FIM_data),
  legend.cex = 0.5,
  GLratio=1.5,
  minimum=0.1,
  cut=0,
  border.width=1.5,
  shape="square"
)

# #Calculating Community measures
g<-as.igraph(network_glasso) #creating igraph object
h<-walktrap.community(g) #creatin community object
h<-spinglass.community(g, weights=NA) #creatin community object
# h<-fastgreedy.community(g, weights=NA) #creatin community object
# h<-edge.betweenness.community(g, weights=NA) #creatin community object
h<-cluster_leading_eigen(g,weights=NA) #creatin community object
plot(h,g) #plotting community network
h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,rownames(cor_data))

#listing grouping variables in the network resulting from the community analysis
# network_groups<-list(
# Component1=as.numeric(rownames(community)[community[,1]==1]),
# Component2=as.numeric(rownames(community)[community[,1]==2]),
# Component3=as.numeric(rownames(community)[community[,1]==3])
# )

# network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
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

# creating nodes labels vector
node_names<-c('"g1",
              "g2",
              "g3",
              "g4",
              "g5",
              "g6",
              "g7",
              "g8",
              "g9",
              "g10",
              "g11",
              "g12",
              "g13",
              "g14",
              "g15",
              "g16",
              "g17",
              "g18",
              "g19",
              "g20",
              "g21",
              "g22",
              "g23",
              "g24",
              "g25",
              "g26",
              "g27"
              "g28",
              "g29"
              "g30"')

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="pcor",threshold="holm",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
# 	network_pcor,
# 	network_cor)

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
# 	width = 1500, height = 1200,horizontal = FALSE, 
# 	onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
# 	width = 1500, height = 1200,horizontal = FALSE, 
# 	onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
# height = 700,compression = 'lzw')
network_glasso<-qgraph(cor_data,
                       layout='spring',
                       # esize=20,
                       graph="glasso",
                       sampleSize=nrow(FIM_data),
                       legend.cex = 0.5,
                       cut = 0.3,
                       # maximum = 1, 
                       minimum = 0.1,
                       # esize = 20,
                       vsize = tau, 
                       # repulsion = 0.8,
                       # nodeNames=node_labels,
                       shape="square",
                       border.width=5,
                       # groups=network_groups,
                       # color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
                       labels=node_names
                       #gray=T,
)
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Cl????nico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

centrality_auto(network_glasso)
eigen_centrality(as.igraph(network_glasso), 
                 directed = FALSE,
                 scale = TRUE,
                 # weights = NA,
                 options = arpack_defaults)
# Directed network
# library(pcalg)
# names <- node_names
# n = nrow(kessler_data)
# p = ncol(kessler_data)
# indepTest <- gaussCItest
# suffStat <- list(C = cor(data), n = nrow(data)
# alpha <- 0.01
# pc.fit <- pc(suffStat, indepTest, p, alpha)
# qgraph(pc.fit, labels = names, colour = groups)

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(kessler_data, 2, as.factor))
# res<-rsmax2(dag_data,
#             restrict = "si.hiton.pc",
#             maximize = "tabu")
# res2<-(res$arcs)
# qgraph(res2)

#ANALISE PARALELA E EIGEN VALUES
#############################################################
#MODEL 1 - Risk due to road deisgn
# cor_data<-cor_auto(model1_bea)

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(na.omit(FIM_data)) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(cor_auto(FIM_data), n = 192,diag=FALSE)

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# # plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
fa.parallel(FIM_data,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
# fa(cor_data,2,rotate="promax")
# fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
fa.poly(cor_data,2,fm="uls",rotate="oblimin")



#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# MSOS_data1<-lapply(MSOS_data1,ordered)

# 2 factor model
cfa_model <-'
MF =~ g1 + g6 + g10 + g14 + g15
CF =~ g17 + g18 + g22 + g26 + g27
'

fit <- lavaan::cfa(cfa_model,
                   data = FIM_data,
                   estimator="WLSMV",
                   ordered=colnames(FIM_data)
)
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
)
)
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 


### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

### By Group analysis
# fit <- lavaan::cfa(cfa_model, data = data,
# estimator="ULSM",group = "female")
# summary(fit, fit.measures=TRUE)
# lavaan::fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
# parameterEstimates(fit)
# lavaan::inspect(fit,"rsquare")
# Est <- standardizedSolution(fit)
# subset(Est, op == "=~")
# subset(Est, op == "~")
# subset(Est, op == ":=")
# measurementInvariance(cfa_model, data = data, group = "female")

nodeLabels<-c("g1",
              "g2",
              "g3",
              "g4",
              "g5",
              "g6",
              "g8",
              "g9",
              "g10",
              "g11",
              "g12",
              "g14",
              "g15",
              "g17",
              "g18",
              "g22",
              "g26",
              "g27",
              "General")

color<-c(rep("grey",10),rep("white",1))
borders<-c(rep("FALSE",10),rep("TRUE",1))
labelcex<-c(rep(0.7,10),rep(1,1))

tiff("/Users/Leonardo/Desktop/figure2.tiff", units='in', 
     width = 15,
     height = 10,compression = 'lzw',res=1200,bg = "white")
semPlot::semPaths(fit,
                  "model",
                  "std",
                  layout="tree2",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  nodeLabels=nodeLabels,
                  label.scale=FALSE,
                  edge.label.cex=0.8,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
# bifactor="general")
dev.off()

#Composite Reliabilty
sum(Est$std.all[1:5])^2/(sum(Est$std.all[1:5])^2+sum(Est$std.all[71:75]))
sum(Est$std.all[6:10])^2/(sum(Est$std.all[6:10])^2+sum(Est$std.all[76:80]))

#Average Extracted Variance
sum(Est$std.all[1:5]^2)/length(Est$std.all[71:75])
sum(Est$std.all[5:10]^2)/length(Est$std.all[75:80])

#Thresholds
by(Est$std.all[1:10],Est$lhs[1:10],mean)

#Factor scores
FIM10_overall<-lavaan::predict(fit)



# 1 factor model
cfa_model <- '
FIM10 =~ g1 + g6 + g10 + g14 + g15 + g17 + g18 + g22 + g26 + g27
'
fit <- lavaan::cfa(cfa_model,
                   data = FIM_data,
                   estimator="WLSMV",
                   ordered=colnames(FIM_data)
)
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
)
)
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 


### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

### By Group analysis
# fit <- lavaan::cfa(cfa_model, data = data,
# estimator="ULSM",group = "female")
# summary(fit, fit.measures=TRUE)
# lavaan::fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
# parameterEstimates(fit)
# lavaan::inspect(fit,"rsquare")
# Est <- standardizedSolution(fit)
# subset(Est, op == "=~")
# subset(Est, op == "~")
# subset(Est, op == ":=")
# measurementInvariance(cfa_model, data = data, group = "female")

nodeLabels<-c("g1",
              "g2",
              "g3",
              "g4",
              "g5",
              "g6",
              "g8",
              "g9",
              "g10",
              "g11",
              "g12",
              "g14",
              "g15",
              "g17",
              "g18",
              "g22",
              "g26",
              "g27",
              "General")

color<-c(rep("grey",10),rep("white",1))
borders<-c(rep("FALSE",10),rep("TRUE",1))
labelcex<-c(rep(0.7,10),rep(1,1))

tiff("/Users/Leonardo/Desktop/figure2.tiff", units='in', 
     width = 15,
     height = 10,compression = 'lzw',res=1200,bg = "white")
semPlot::semPaths(fit,
                  "model",
                  "std",
                  layout="tree2",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  nodeLabels=nodeLabels,
                  label.scale=FALSE,
                  edge.label.cex=0.8,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
# bifactor="general")
dev.off()

#Composite Reliabilty
sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+sum(Est$std.all[71:80]))

#Average Extracted Variance
sum(Est$std.all[1:10]^2)/length(Est$std.all[1:10])

#Thresholds
by(Est$std.all[1:10],Est$lhs[1:10],mean)

#Factor scores
FIM10_overall<-lavaan::predict(fit)

write.csv(FIM10_overall, "/Users/LeoPestillo/Desktop//FIM10_data.csv")


#Import data - correlation

data01<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_MHpostTBI_data.csv",sep=',')
#Organize scale datasets
data_validation<-data01[data01$redcap_event_name=="enrollment_arm_1",]

#SF8
sf8_data1<-with(data_validation,data.frame(sf8_b1,
                                sf8_b2,
                                sf8_b3,
                                sf8_b4,
                                sf8_b5,
                                sf8_b6,
                                sf8_b7,
                                sf8_b8))
data_imputed <- mice(sf8_data1, seed = 2222, m=10)
sf8_data<-complete(data_imputed,4)
write.csv(sf8_data, "/Users/LeoPestillo/Desktop//sf8_items.csv")


#external validation

data<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/FIM_10/Data_correlation_FIM.csv",sep=',')
data_correlation<-subset(data)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(data_correlation, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_correlation<-mice::complete(data_imputed,4)
summary(data_correlation)

cor_data<-cor_auto(data_correlation)

write.csv(cor_auto(as.matrix(data_correlation)),file="/Users/LeoPestillo/Desktop/FIM10_externalvalidation.csv")


#install.packages("ggpubr")
library("ggpubr")
#install.packages("Hmisc")
library("Hmisc")


cor_data<-rcorr(as.matrix(data_correlation))
write.csv(rcorr(as.matrix(data_correlation)),file="/Users/LeoPestillo/Desktop/FIM10_externalvalidationP.csv")

#install.packages("qgraph")
#library("qgraph")