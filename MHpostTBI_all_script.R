#Analise de rede-Multimorbidade e cancer

#Recurso/Resources:http://sachaepskamp.com/files/Cookbook.html#network-estimation-binary-data

#Instalar os pacotes importantes/Install important packages
 install.packages ("IsingFit")
 install.packages ("qgraph")
 install.packages ("ggplot2")
 install.packages("car")
 install.packages("mgm")
 install.packages("NetworkComparisonTest")
 install.packages("tidyverse")
 install.packages("haven")
 install.packages("igraph")
 install.packages("lattice")
 install.packages("survival")
 install.packages("Formula")
 install.packages("Hmisc")


library(qgraph)
library(IsingFit)
library(ggplot2)
library(car)
library(tidyverse)
library(haven)
library(igraph)
library(mgm)
library(NetworkComparisonTest)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)

data<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/database/Tz_MHpostTBI_data.csv",sep=',')
data<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/database/Tz_MHpostTBI_data.csv", header = TRUE)

data_validation<-subset(data[data$redcap_event_name=="enrollment_arm_1",])
data_validation<-subset(data[data$redcap_event_name=="month_3_followup_arm_1",])

# Mergind datasets
library("plyr")
data_validation<-rbind(data_validation01,data_validation02)


#FIM 30 items
FIM_data30<-with(data_validation,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))
FIMdata_imputed <- mice(FIM_data30, seed = 2222, m=10)
FIM_data30<-mice::complete(FIMdata_imputed,4)
#FIM Motor
FIM_Motor30<-with(FIM_data30,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16))
#FIM Cognitive
FIM_Cognitive30<-with(FIM_data30,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))

write.csv(FIM_Motor30, "C:/Users/Leonardo/Desktop/fimmotor.csv")
write.csv(FIM_Cognitive30, "C:/Users/Leonardo/Desktop/fimcognitive.csv")
write.csv(FIM_data30, "C:/Users/Leonardo/Desktop/fim30.csv")

#MOCA
MOCA_data1<-with(data_validation,data.frame(f1a,f1c,f1d,f1e,f2c,f2d,f15,f16,f17,f18,f19,f20,f21,f21b,f22,f23))
#SUM f1+f2, f21 item
MOCA_data1$f1 <- rowSums(MOCA_data1[ , 1:5]) 
MOCA_data1$f21 <- rowSums(MOCA_data1[ , 13:14]) 
MOCA_data1$f17 <- rowSums(MOCA_data1[ , 9:10]) 
MOCA_data1<-with(MOCA_data1,data.frame(f1,f15,f16,f17,f19,f20,f21,f21b,f22,f23))
data_imputed <- mice(MOCA_data1, seed = 2222, m=10)
MOCA_data<-mice::complete(data_imputed,4)

write.csv(MOCA_data, "C:/Users/Leonardo/Desktop/moca.csv")

#Kessler
#kessler_data1<-with(data_validation,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
#ALL DATA
kessler_data1<-with(data_validation,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
kesslerdata_imputed <- mice(kessler_data1, seed = 2222, m=10)
kessler_data<-mice::complete(kesslerdata_imputed,4)

write.csv(kessler_data, "C:/Users/Leonardo/Desktop/stress.csv")


#SF8
sf8_data1<-with(data_validation,data.frame(sf8_b1,
                                           sf8_b2,
                                           sf8_b3,
                                           sf8_b4,
                                           sf8_b5,
                                           sf8_b6,
                                           sf8_b7,
                                           sf8_b8))
sf8data_imputed <- mice(sf8_data1, seed = 2222, m=10)
sf8_data<-complete(sf8data_imputed,4)

write.csv(sf8_data, "C:/Users/Leonardo/Desktop/qlife.csv")

#PHQ9
phq9_data1<-with(data_validation,data.frame(phq9_b11,
                                            phq9_b12,
                                            phq9_b13,
                                            phq9_b14,
                                            phq9_b15,
                                            phq9_b16,
                                            phq9_b17,
                                            phq9_b18,
                                            phq9_b19
))
phq9data_imputed <- mice(phq9_data1, seed = 2222, m=10)
phq9_data<-mice::complete(phq9data_imputed,4)

write.csv(phq9_data, "C:/Users/Leonardo/Desktop/depression01.csv")

#AUDIT
audit_data<-with(data_validation,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
# audit_data1<-with(data_validation,data.frame(
# 	h1,h2,h3))
# audit_data2<-with(data_validation,data.frame(
# 	h4,h5,h6,h7,h8,h9,h10))
# audit_data2_3<-with(data_validation,data.frame(
# 	h4,h5,h6))
# audit_data3_3<-with(data_validation,data.frame(
# 	h7,h8,h9,h10))
auditdata_imputed <- mice(audit_data, seed = 2222, m=10)
audit_data<-mice::complete(auditdata_imputed,4)

write.csv(audit_data, "C:/Users/Leonardo/Desktop/alcoholuse01.csv")

#CONFIRMATORY FACTOR ANALYSIS

# FIM 30 itens
# 2 factor model
cfa_model <- '
MF =~ g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9 + g10 + g11 + g12 + g13 + g14 + g15 + g16 
CF =~ g17 + g18 + g19 + g20 + g21 + g22 + g23 + g24 + g25 + g26 + g27 + g28 + g29 + g30
'

fit <- lavaan::cfa(cfa_model,
                   data = FIM_data30,
                   estimator="WLSMV",
                   ordered=colnames(FIM_data30)
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

#Factor scores
FIM_30_overall<-lavaan::predict(fit)
write.csv(FIM_30_overall, "/Users/leopestillo/Desktop/FIM_30_factorscores.csv")
#write.csv(FIM_30_overall, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/FIM_30_factorscores.csv")



#MOCA
# 1 factor model
cfa_model <- '
MOCA =~ f1 + f15 + f16 + f17 + f19 + f20 + f21 + f21b + f22 + f23
f17 ~~ f21
'

fit <- lavaan::cfa(cfa_model,
                   data = MOCA_data,
                   estimator="WLSMV",
                   ordered=colnames(MOCA_data)
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
#subset(Est, op == "=~")
#subset(Est, op == "~~")
#lavInspect(fit,what="th") 

#Factor scores
MOCA_overall<-lavaan::predict(fit)
write.csv(MOCA_overall, "/Users/leopestillo/Desktop/moca_factorscores.csv")

#KESSLER
# 1 factor model
cfa_kessler <- '
Kessler =~  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10
#
Kessler ~~ Kessler
#cov
# d2 ~~  d9
d5 ~~  d6
# d8 ~~  d10
'

fit <- lavaan::cfa(cfa_kessler,
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
#parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th")

#Factor scores
kessler_overall<-lavaan::predict(fit)

write.csv(kessler_overall, "/Users/leopestillo/Desktop/kessler_factorscores.csv")
#write.csv(kessler_overall, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/kessler_factorscores.csv")

#######################################################
library("plyr")
library("dplyr")

database<-with(data,data.frame(redcap_event_name))
#write.csv(database, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/database.csv")
#data_kessler<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/kessler_factorscores.csv", header = TRUE)
write.csv(database, "/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/database.csv")
data_kessler<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/kessler_factorscores.csv", header = TRUE)


kessler_scores1<-subset(data_kessler,redcap_event_name=="enrollment_arm_1")
kessler_overall1<-with(kessler_scores1,data.frame(Kessler))
write.csv(kessler_overall1, "/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/kessler_factorscores.csv")

                                              


########################################################


#SF8
# 1 factor model
cfa_model <- '
SF8 =~  sf8_b1 + sf8_b2 + sf8_b3 + 
sf8_b4 + sf8_b5 + sf8_b6 + sf8_b7 + sf8_b8
#
#cov
sf8_b2 ~~ sf8_b8
sf8_b1 ~~ sf8_b5
'

fit <- lavaan::cfa(cfa_model,
                   data = sf8_data,
                   estimator="WLSMV",
                   ordered=colnames(sf8_data)
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
))
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

#Factor scores
sf8_overall<-lavaan::predict(fit)
write.csv(sf8_overall, "/Users/leopestillo/Desktop/sf8_factorscores.csv")
#write.csv(sf8_overall, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/sf8_2factorscores.csv")


#PHQ9
# 1 factor model
cfa_model <- '
PHQ9 =~  phq9_b11 + phq9_b12 + phq9_b13 + phq9_b14 + 
phq9_b15 + phq9_b16 + phq9_b17 + phq9_b18 + phq9_b19
'

fit <- lavaan::cfa(cfa_model,
                   data = phq9_data,
                   estimator="WLSMV",
                   ordered=colnames(phq9_data)
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
                                          ))
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

#Factor scores
phq9_overall<-lavaan::predict(fit)
write.csv(phq9_overall, "/Users/leopestillo/Desktop/phq9_factorscores.csv")
#write.csv(phq9_overall, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/phq9_factorscores.csv")


#AUDIT
#1factor model ###########
audit_model <- '
Audit =~  h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10
'
fit <- lavaan::cfa(audit_model,
                   data = audit_data,
                   estimator="WLSMV",
                   ordered=names(audit_data))
summary(fit,
        fit.measures=TRUE)
lavaan::fitMeasures(fit,
                    fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

#Factor scores
audit_overall<-lavaan::predict(fit)
write.csv(audit_overall, "/Users/leopestillo/Desktop/audit_factorscores.csv")
#write.csv(audit_overall, "D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/audit_factorscores.csv")


#CORRELATION - ALL INSTRUMENTS
data_all<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/TBI_datacorrelation.csv",sep=',')
data_all<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/TBI_datacorrelation.csv",sep=',')

library("mice")
data_all_imputed <- mice(data_all, seed = 2222, m=10)
data_correlation<-mice::complete(data_all_imputed,4)

#data_correlation<-subset(data_all)

#install.packages("ggpubr")
library("ggpubr")
#install.packages("Hmisc")
library("Hmisc")

#install.packages("qgraph")
library("qgraph")


#Prevalencias
#Prevalences (proportion with disease/total)

table(data_correlation$Alcohol.Use)
prev_alcoholuse<-prop.table(table(data_correlation$Alcohol.Use))

table(data_correlation$Motor.Function)
prev_motorfunction<-prop.table(table(data_correlation$Motor.Function))

table(data_correlation$Cog.Function)
prev_cogfunction<-prop.table(table(data_correlation$Cog.Function))

table(data_correlation$Stress)
prev_stress<-prop.table(table(data_correlation$Stress))

table(data_correlation$Depression)
prev_depression<-prop.table(table(data_correlation$Depression))

table(data_correlation$Qlife)
prev_qlife<-prop.table(table(data_correlation$Qlife))

table(data_correlation$MOCA)
prev_moca<-prop.table(table(data_correlation$MOCA))

prev_all<-c(prev_alcoholuse,
            prev_motorfunction,
            prev_cogfunction,
            prev_stress,
            prev_depression,
            prev_qlife,
            prev_moca)





######### CÓDIGO ANTIGO ##############

cor_data<-cor_auto(data_correlation)

network_glasso<-qgraph(cor_data,
                       graph = "glasso",
                       sampleSize = nrow(data_correlation),
                       layout="spring", 
                       edge.labels=TRUE, 
                       labels=colnames(cor_data), 
                       fade=FALSE,
                       tuning=0.25
                       )

qgraph(cor_data, layout="spring", edge.labels=TRUE, labels=colnames(cor_data), fade=FALSE)



#Medindo centralidade (measuring centrality)
centRes <- centrality(network_glasso) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPaths
# centralityPlot(Graph_Ising1)

#Centrality
setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
  postscript("/Users/leopestillo/Desktop/figure1_mentalhealth.eps",
      width = 10, height = 8)







fit_obj<- mgm(data = data_correlation,
              type = rep('c', ncol(data_correlation)),
              level = rep(1, ncol(data_correlation)),
              lambdaSel = 'EBIC',
              lambdaGam = 0.5,
              ruleReg = 'AND',
              binarySign = TRUE)

graph_obj <- qgraph(fit_obj$pairwise$wadj)

#análise de comunidades
## https://www.nature.com/articles/srep30750
g2 <- as.igraph(graph_obj)

#se não tiver arestas negativas
g2_cl <- cluster_louvain(g2)

#Computing predictability nodes

pred_obj <- predict(object = fit_obj,
                    data = data_correlation,
                    errorCat = 'CC')








####### DATA ALL INSTRUMENTS ############

data_instruments<-with(data_validation,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,
                                                  d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,
                                                  sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5,sf8_b6,sf8_b7,sf8_b8,
                                                  phq9_b11,phq9_b12,phq9_b13,phq9_b14,phq9_b15,phq9_b16,phq9_b17,phq9_b18,phq9_b19,
                                                  h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
instruments_imputed <- mice(data_instruments, seed = 2222, m=10)
instruments<-mice::complete(instruments_imputed,4)

cor_data_instruments<-cor_auto(instruments)

groups <- c(rep("Functional", 30), rep("Stress", 10), rep("QoLife", 8), rep("Depression", 9), rep("AlcoholUse", 10))
qgraph(cor_data_instruments, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(instruments), minimum = 0,
       cut = 0.4, maximum = 1, details = TRUE,
       esize = 20,groups = groups,legend.cex=0.4,
       vsize=5
       )



qgraph(cor_data_instruments, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(instruments), groups = groups,
       edge.labels=TRUE,
       fade=FALSE,
       legend.cex=0.5,
   #    cut=0.4,
       maximum = 1, 
       minimum = 0.4,
       esize = 20,
       repulsion = 0.8,
       border.width=5,
       borders = FALSE,
       tuning=0.25
       )





#network_glasso<-qgraph(cor_data,
#                      layout='spring',
#                     edge.labels=TRUE,
#                      labels=colnames(cor_data), 
#                      fade=FALSE,
#                      # esize=20,
#                      graph="glasso",
#                      sampleSize=nrow(data_correlation),
#                      legend.cex = 0.5,
#                      cut = 0.3,
# maximum = 1, 
#                      minimum = 0.2,
# esize = 20,
# vsize = tau, 
# repulsion = 0.8,
# nodeNames=node_labels,
# shape="square",
#                      border.width=5,
#       groups=colnames(cor_data),
# color=c("gold","steelblue","red","grey80","green"),
#                      borders = FALSE,
#                      tuning=0.25
# labels=node_names
#gray=T,
#)



#############################################

#IF DATABASE NOT IN THE R
data_audit<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/audit_factorscores_1_3.csv",sep=',')
#data_FIM<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/FIM_30_factorscores_1_3.csv",sep=',')
#data_kessler<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/kessler_factorscores_1_3.csv",sep=',')
#data_moca<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/moca_factorscores_1_3.csv",sep=',')
#data_phq9<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/phq9_factorscores_1_3.csv",sep=',')
#data_sf8<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/sf8_factorscores_1_3.csv",sep=',')

library(scales)
audit_overall_rescaled<-round(rescale(audit_overall, to = c(0, 100)))
FIM_overall_rescaled<-round(rescale(FIM_30_overall, to = c(0, 100)))
kessler_overall_rescaled<-round(rescale(kessler_overall, to = c(0, 100)))
moca_overall_rescaled<-round(rescale(MOCA_overall, to = c(0, 100)))
phq9_overall_rescaled<-round(rescale(phq9_overall, to = c(0, 100)))
sf8_overall_rescaled<-round(rescale(sf8_overall, to = c(0, 100)))

data_factorscores<-data.frame(audit_overall_rescaled,FIM_overall_rescaled,kessler_overall_rescaled,moca_overall_rescaled,phq9_overall_rescaled,sf8_overall_rescaled,subject=data_validation$interviewer_id,event=data_validation$redcap_event_name)
#data_factorscores<-data.frame(x1=audit_overall_rescaled,x2=FIM_overall_rescaled$MF,x3=FIM_overall_rescaled$CF,x4=kessler_overall_rescaled,x5=moca_overall_rescaled,x6=phq9_overall_rescaled,x7=sf8_overall_rescaled,subject=data_validation$interviewer_id,event=data_validation$redcap_event_name)


data_event1<-subset(data_factorscores[data_factorscores$event=="enrollment_arm_1",])
data_event_month3<-subset(data_factorscores[data_factorscores$event=="month_3_followup_arm_1",])


total_data <- merge(data_event1,data_event_month3,by="subject")


write.csv(total_data, "/Users/leopestillo/Desktop/total_data.csv")

cor_auto(total_data) 

##############################################################################

data_validation01<-subset(data[data$redcap_event_name=="enrollment_arm_1",])
data_validation02<-subset(data[data$redcap_event_name=="month_3_followup_arm_1",])

# Mergind datasets
library("plyr")
data_col<-rbind(data_validation01,data_validation02)

col_redcap01<-with(data_col,data.frame(redcap_event_name))
#col_imputed <- mice(col_redcap, seed = 2222, m=10)
#col_redcap<-mice::complete(col_imputed,4)


baseline_3m<-read.csv("/Users/leopestillo/Google Drive/[PES] life discussions/Tz_Validations/analysis_Mplus/Factorscores_1_3m.csv",sep=',')
data_joincol<-cbind(baseline_3m,col_redcap01)

factorscores_3month<-subset(data_joincol[data_joincol$redcap_event_name=="month_3_followup_arm_1",])

write.csv(factorscores_3month, "/Users/leopestillo/Desktop/factorscores_3month.csv")

