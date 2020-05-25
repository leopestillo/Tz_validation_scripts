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
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
data<-read.csv("D:/Google Drive/R/Kessler/Tz_MHpostTBI_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################

#subsetting data set to keep only baseline data
data_validation<-data[data$redcap_event_name=="enrollment_arm_1",]

#recoding gender variable
data_validation$female<-car::recode(data_validation$female,"
                                    0='male';1='female'")

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

#Kessler
kessler_data1<-with(data,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,
                                    d10))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(kessler_data1, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
kessler_data<-mice::complete(data_imputed,4)

######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################
###Section wih several exploratory data analysis functions
###### Exploratory Data Anlysis
###### UNIVARIATE

# Numerical descriptives
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
describe(data_validation$age)
describe(data_validation$home_people)

# Categorical Descriptives
table<-with(data_validation,table(married))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(female))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(occupation_cat))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(education_cat))
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
#K-10
##############################################################

#TAXONOMETRIC ANALAYSIS

#Taxonometric Scale
# MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)


#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(kessler_data,n.iter=1000,check.keys=TRUE)

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
cor_data<-cor_auto(kessler_data) 

#extract thresholds
polycor_data<-polychoric(na.omit(kessler_data),correct=.01) # crossvalidate
cor<-polycor_data$rho #extract correlation matrix
tau<-rowMeans(polycor_data$tau) #extract thresholds

#Community analysis - Walking Trap
#Generate glasso network
network_glasso<-qgraph(
  cor_data,
  layout="spring", 
  vsize=tau,
  # esize=20,
  graph="glasso",
  sampleSize=nrow(kessler_data),
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
node_names<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10")

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
                       sampleSize=nrow(kessler_data),
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
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

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
kmo<-kmo(na.omit(kessler_data)) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(cor_auto(kessler_data), n = 297,diag=FALSE)

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
fa.parallel(kessler_data,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
# fa(cor_data,2,rotate="promax")
# fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
fa.poly(cor_data,1,fm="uls",rotate="oblimin")
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
# kessler_data<-lapply(kessler_data,ordered)

# 1 factor model
cfa_model <- '
Kessler =~  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10
#
Kessler ~~ Kessler
#cov
# d2 ~~  d9
d5 ~~  d6
# d8 ~~  d10
'

fit <- lavaan::cfa(cfa_model,
                   data = kessler_data,
                   estimator="WLSMV",
                   ordered=colnames(kessler_data)
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
lavaan::parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 


### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod)#, mi > 10)

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

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
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
sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+sum(Est$std.all[51:60]))

#Average Extracted Variance
sum(Est$std.all[1:10]^2)/length(Est$std.all[1:10])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

#Factor scores
kessler_overall<-lavaan::predict(fit)

# 2 factors model ###########################
cfa_model <- '
Depression =~  d1 + d4 + d7 + d8 + d9 + d10
Anxiety =~ d2 + d3 + d5 + d6
#
# Depression ~~ Depression
# Anxiety ~~ Anxiety
#cov
# d2 ~~  d9
# d5 ~~  d6
# d7 ~~  d8
'

fit <- lavaan::cfa(cfa_model,
                   data = kessler_data,
                   estimator="WLSMV",
                   ordered=colnames(kessler_data))
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled",
                                          "df.scaled"
))
lavaan::parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
lavInspect(fit,what="th")

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod)#, mi > 10)

### By Group analysis
fit <- lavaan::cfa(cfa_model, data = data,
                   estimator="ULS",group = "female")
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
lavaan::parameterEstimates(fit)
lavaan::inspect(fit,"rsquare")
measurementInvariance(cfa_model, data = data, group = "female")

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "Depression",
              "Anxiety")

color<-c(rep("grey",10),rep("white",2))
borders<-c(rep("FALSE",10),rep("TRUE",2))
labelcex<-c(rep(0.7,10),rep(1,2))

tiff("D:/Google Drive/R/Kessler/figure1.tiff", units='in', 
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

#Composite Reliabilty - Depression
sum(Est$std.all[1:6])^2/(sum(Est$std.all[1:6])^2+
                           sum(Est$std.all[48:54]))

#Composite Reliabilty
sum(Est$std.all[7:10])^2/(sum(Est$std.all[7:10])^2+
                            sum(Est$std.all[55:58]))

#Average Extracted Variance
sum(Est$std.all[1:6]^2)/length(Est$std.all[1:6])

#Average Extracted Variance
sum(Est$std.all[7:10]^2)/length(Est$std.all[7:10])

#Factor scores
kessler_dimensions<-lavaan::predict(fit)

# Second ordered factor model ###########################
# cfa_model <- '
# # general =~  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10
# Depression =~  d1 + d4 + d7 + d8 + d9 + d10
# Anxiety =~ d2 + d3 + d5 + d6
# general =~ Depression + Anxiety

# #cov
# general ~~ general
# Anxiety ~~ Anxiety
# Depression ~~ Depression

# #cov
# # d2 ~~  d9
# # d5 ~~  d6
# # d4 ~~  d8
# '

# fit <- lavaan::cfa(cfa_model,
#   data = kessler_data,
#   estimator="WLSMV",
#   ordered=names(kessler_data))
#   # verbose = TRUE)
# summary(fit, fit.measures=TRUE)
# lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
#                                           "rmsea.ci.lower.scaled",
#                                           "rmsea.ci.upper.scaled",
#                                           "cfi.scaled",
#                                           "tli.scaled",
#                                           "nnfi.scaled",
#                                           "chisq.scaled",
#                                           "pvalue.scaled",
#                                           "df.scaled"
#                                           ))
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# lavInspect(fit,what="th")
# subset(Est, op == "=~")
# lavInspect(fit,what="th")

# ### Modification Indexes
# Mod <- lavaan::modificationIndices(fit)
# subset(Mod)#, mi > 10)

# ### By Group analysis
# fit <- lavaan::cfa(cfa_model, data = data,
# estimator="ULS",group = "female")
# summary(fit, fit.measures=TRUE)
# fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
# parameterEstimates(fit)
# lavaan::inspect(fit,"rsquare")
# Est <- standardizedSolution(fit)
# subset(Est, op == "=~")
# subset(Est, op == "~")
# subset(Est, op == ":=")
# measurementInvariance(cfa_model, data = data, group = "female")

# nodeLabels<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8",
#               "Q9",
#               "Q10",
#               "General",
#               "Depression",
#               "Anxiety")

# color<-c(rep("grey",10),rep("white",3))
# borders<-c(rep("FALSE",10),rep("TRUE",3))
# labelcex<-c(rep(0.7,10),rep(1,3))

# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
# semPlot::semPaths(fit,
#                   "model",
#                   "std",
#                   layout="tree2",
#                   style="lisrel",
#                   residuals=TRUE,
#                   # cut=1,
#                   # equalizeManifests=TRUE,
#                   # edge.color="black",
#                   exoCov=FALSE,
#                   # intercepts=FALSE,
#                   nodeLabels=nodeLabels,
#                   label.scale=FALSE,
#                   edge.label.cex=0.8,
#                   label.cex=labelcex,
#                   color=color,
#                   borders=borders,
#                   bifactor="general")

# dev.off()

# #Composite Reliabilty
# sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+sum(Est$std.all[11:20]))


# Bi-factor model ###########################
# cfa_model <- '
# general =~  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10
# Depression =~  d1 + d4 + d7 + d8 + d9 + d10
# Anxiety =~ d2 + d3 + d5 + d6
# # general =~ Depression + Anxiety

# #cov
# # general ~~ general
# # Anxiety ~~ Anxiety
# # Depression ~~ Depression

# #cov
# # d1 ~~  d8
# # d2 ~~  d9
# # d4 ~~  d8
# '

# fit <- lavaan::cfa(cfa_model,
#                    data = kessler_data,
#                    orthogonal=TRUE,
#                    estimator="WLSMV",
#                    ordered=names(kessler_data))
# summary(fit, fit.measures=TRUE)
# lavaan::fitMeasures(fit, fit.measures = "all")
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# lavInspect(fit,what="th")

# ### Modification Indexes
# Mod <- lavaan::modificationIndices(fit)
# subset(Mod)#, mi > 10)

# ### By Group analysis
# fit <- lavaan::cfa(cfa_model, data = data,
# estimator="ULS",group = "female")
# summary(fit, fit.measures=TRUE)
# fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
# parameterEstimates(fit)
# lavaan::inspect(fit,"rsquare")
# Est <- standardizedSolution(fit)
# subset(Est, op == "=~")
# subset(Est, op == "~")
# subset(Est, op == ":=")
# measurementInvariance(cfa_model, data = data, group = "female")

# nodeLabels<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8",
#               "Q9",
#               "Q10",
#               "General",
#               "Depression",
#               "Anxiety")

# color<-c(rep("grey",10),rep("white",3))
# borders<-c(rep("FALSE",10),rep("TRUE",3))
# labelcex<-c(rep(0.7,10),rep(1,3))

# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
# semPlot::semPaths(fit,
#                   "model",
#                   "std",
#                   layout="tree2",
#                   style="lisrel",
#                   residuals=TRUE,
#                   # cut=1,
#                   # equalizeManifests=TRUE,
#                   # edge.color="black",
#                   exoCov=FALSE,
#                   # intercepts=FALSE,
#                   nodeLabels=nodeLabels,
#                   label.scale=FALSE,
#                   edge.label.cex=0.8,
#                   label.cex=labelcex,
#                   color=color,
#                   borders=borders,
#                   bifactor="general")

# dev.off()

# #Composite Reliabilty
# sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+sum(Est$std.all[11:20]))


#ITEM RESPONSE THEORY
##############################################################

# #### USING eRM Package
# IRTRolandMorris <- PCM(kessler_data)
# diff_index<-thresholds(IRTRolandMorris)
# summary(diff_index$threshtable[[1]][,1])
# sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
# plotICC(IRTRolandMorris,item.subset=3,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
# plotPImap(IRTRolandMorris, sorted=FALSE)
# plotPWmap(IRTRolandMorris)
# pp<-eRm::person.parameter(IRTRolandMorris)
# #lrt<-LRtest(IRTRolandMorris,se=TRUE)
# #Waldtest(IRTRolandMorris)
# eRm::itemfit(pp)
# summary(eRm::itemfit(pp)$i.outfitMSQ)
# sd(eRm::itemfit(pp)$i.outfitMSQ)
# summary(eRm::itemfit(pp)$i.infitMSQ)
# sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Dichotomous items
# IRTRolandMorris <- RM(neckdisability2)
# plotICC(IRTRolandMorris,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
# plotPImap(IRTRolandMorris)
# pp<-person.parameter(IRTRolandMorris)
# lrt<-LRtest(IRTRolandMorris,se=TRUE)
# Waldtest(IRTRolandMorris)
# eRm::itemfit(pp)
# summary(itemfit(pp)$i.outfitMSQ)
# sd(itemfit(pp)$i.outfitMSQ)
# summary(itemfit(pp)$i.infitMSQ)
# sd(itemfit(pp)$i.infitMSQ)
# NPtest(IRTRolandMorris,method="T11")
# plotGOF(lrt,conf=list())

#############################################################################
#CAT
#############################################################################
# install.packages("catR")
# require(catR)
# c<-coef(irt_model)
# itemBank <- cbind(c[,2], c[,1], 0, 1)
# catBank<-createItemBank(irt_model, model="2pl")
# catBank
# catBank$itemPar
# plot(catBank$infoTab[,1])
# plot(my2pl, type = "IIC", items=1)

# items_administered<-c(4)
# responses<-c(1)
# it<-itemBank[items_administered, 1:4,drop=F ]
# theta<-thetaEst(it, responses)
# q<-nextItem(catBank, theta,out=items_administered)
# q$item

##############################################################
#K-6
##############################################################

#TAXONOMETRIC ANALAYSIS

#Taxonometric Scale
# MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)

kessler_data_k6<-with(kessler_data,data.frame(
  d4,d8,d9,d10,d2,d5))

#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(kessler_data_k6,n.iter=1000,check.keys=TRUE)

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
cor_data<-cor_auto(kessler_data_k6) 

#extract thresholds
polycor_data<-polychoric(na.omit(kessler_data_k6),correct=.01) # crossvalidate
cor<-polycor_data$rho #extract correlation matrix
tau<-rowSums(polycor_data$tau) #extract thresholds

#Community analysis - Walking Trap
#Generate glasso network
network_glasso<-qgraph(
  cor_data,
  layout="spring", 
  vsize=tau,
  # esize=20,
  graph="glasso",
  sampleSize=nrow(kessler_data_k6),
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
node_names<-c(#"Q1",
  "Q4",
  # "Q3",
  "Q8",
  "Q9",
  # "Q6",
  # "Q7",
  "Q10",
  "Q2",
  "Q5")

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
# height = 700,compression = 'lzw')
network_glasso<-qgraph(cor_data,
                       layout='spring',
                       # esize=20,
                       graph="glasso",
                       sampleSize=nrow(kessler_data_k6),
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
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

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


#ANALISE PARALELA E EIGEN VALUES
#############################################################
#MODEL 1 - Risk due to road deisgn
# cor_data<-cor_auto(model1_bea)

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(na.omit(kessler_data_k6)) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(cor_auto(kessler_data_k6), n = 297,diag=FALSE)

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
fa.parallel(kessler_data_k6,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
# fa(cor_data,2,rotate="promax")
# fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
fa.poly(kessler_data_k6,1,fm="uls",rotate="oblimin")
fa.poly(kessler_data_k6,2,fm="uls",rotate="oblimin")

#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# kessler_data<-lapply(kessler_data,ordered)

# 1 factor model
cfa_model <- '
Kessler =~  d2 + d4 + d5 + d8 + d9 + d10
#
# Kessler ~~ Kessler
#cov
# d2 ~~  d4
# d5 ~~  d6
d5 ~~  d10
'

fit <- lavaan::cfa(cfa_model,
                   data = kessler_data_k6,
                   estimator="WLSMV",
                   ordered=names(kessler_data_k6))
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled",
                                          "df.scaled"
))
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
lavInspect(fit,what="th")

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod)#, mi > 10)

### By Group analysis
# fit <- lavaan::cfa(cfa_model, data = data,
# estimator="ULSM",group = "female")
# summary(fit, fit.measures=TRUE)
# fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
# parameterEstimates(fit)
# lavaan::inspect(fit,"rsquare")
# Est <- standardizedSolution(fit)
# subset(Est, op == "=~")
# subset(Est, op == "~")
# subset(Est, op == ":=")
# semTools::measurementInvariance(fit,
#                       data = kessler_data,
#                       group = "female")

nodeLabels<-c(#"Q1",
  "Q2",
  # "Q3",
  "Q4",
  "Q5",
  # "Q6",
  # "Q7",
  "Q8",
  "Q9",
  "Q10",
  "K-6")


color<-c(rep("grey",6),rep("white",1))
borders<-c(rep("FALSE",6),rep("TRUE",1))
labelcex<-c(rep(0.7,6),rep(1,1))

tiff("D:/Google Drive/R/Kessler/figure2.tiff", units='in', 
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
                  edge.label.cex=1,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
# bifactor="general")
dev.off()

#Composite Reliabilty
sum(Est$std.all[1:6])^2/(sum(Est$std.all[1:6])^2+
                           sum(Est$std.all[31:36]))

#Average Extracted Variance
sum(Est$std.all[1:6]^2)/length(Est$std.all[1:6])

#Thresholds
by(Est$std.all[8:30],Est$lhs[8:30],mean)

#Factor scores
kessler_k6<-lavaan::predict(fit)

#ITEM RESPONSE THEORY
##############################################################

#### USING eRM Package
IRTRolandMorris <- eRm::PCM(kessler_data_k6,se=FALSE)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=3,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-eRm::person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Dichotomous items
# IRTRolandMorris <- RM(neckdisability2)
# plotICC(IRTRolandMorris,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
# plotPImap(IRTRolandMorris)
# pp<-person.parameter(IRTRolandMorris)
# lrt<-LRtest(IRTRolandMorris,se=TRUE)
# Waldtest(IRTRolandMorris)
# eRm::itemfit(pp)
# summary(itemfit(pp)$i.outfitMSQ)
# sd(itemfit(pp)$i.outfitMSQ)
# summary(itemfit(pp)$i.infitMSQ)
# sd(itemfit(pp)$i.infitMSQ)
# NPtest(IRTRolandMorris,method="T11")
# plotGOF(lrt,conf=list())

#############################################################################
#CAT
#############################################################################
# install.packages("catR")
# require(catR)
# c<-coef(irt_model)
# itemBank <- cbind(c[,2], c[,1], 0, 1)
# catBank<-createItemBank(irt_model, model="2pl")
# catBank
# catBank$itemPar
# plot(catBank$infoTab[,1])
# plot(my2pl, type = "IIC", items=1)

# items_administered<-c(4)
# responses<-c(1)
# it<-itemBank[items_administered, 1:4,drop=F ]
# theta<-thetaEst(it, responses)
# q<-nextItem(catBank, theta,out=items_administered)
# q$item

#############################################################################
#GENERATING SCORES
#############################################################################
kessler_scores<-data.frame(kessler_overall,kessler_dimensions,kessler_k6)
colnames(kessler_scores)<-c("K10_overall",
                            "K10depression",
                            "K10anxiety",
                            "K6_overall")
rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
kessler_scores_scaled<-lapply(kessler_scores,rescale)
kessler_scores_scaled<-as.data.frame(kessler_scores_scaled)

write.csv(kessler_scores_scaled,"/Users/jnv4/Desktop/kessler_scores.csv")