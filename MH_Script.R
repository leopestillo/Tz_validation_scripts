#####################################################################################
#BASIC R STATISTICS TEMPLATE
#####################################################################################
#
#
#
#
#
#############################################################################
#SETTING ENVIRONMENT
#############################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)

#############################################################################
#IMPORTING DATA
#############################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv",
#                                  "tkxmkg9pybmtsgh",
#                                  sep = ",",
#                                  header = TRUE)

#Desktop
data <- read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_MHpostTBI_data.csv", header = TRUE)
data_registry<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/tz_TBIregistry_data.csv",header=TRUE)

#Mac
data <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_MHpostTBI_data.csv", header = TRUE)
data_registry<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/tz_TBIregistry_data.csv",header=TRUE)

#data_registry<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tbi_registry/tz_TBIregistry_data.csv",header=TRUE)


# data_tbi <- read.csv("/Users/jnv4/Desktop/tz_tbiregistry_data.csv", header = TRUE)

#############################################################################
#MERGING REGISTRY AND MH data
#############################################################################

data_registry$tbi_reg<-data_registry$study_id

data_mhregistry1<-merge(x = data, 
                        y = data_registry, 
                        by = "tbi_reg", 
                        all.x = TRUE)

# write.csv(data_mhregistry,"/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tbi_registry/tz_TBIregistryANDmh_data.csv")

data_mhregistry<-subset(data_mhregistry1,data_mhregistry1$study_id.x != "NA")

#############################################################################
#DATA MANAGEMENT
#############################################################################

data_mhregistry$married_recoded<-car::recode(data_mhregistry$married,
                                             "0=1;1=1;4=1;2=0;3=0;5=0")

data_mhregistry$occupation_recoded<-car::recode(data_mhregistry$occupation,
                                                "89='Other'")

data_mhregistry$education_recoded<-car::recode(data_mhregistry$education,"
                                               1:7='Primary';8:13='Form';14:16='University';")

#recoding gos
data_mhregistry$gos<-as.factor(car::recode(
  data_mhregistry$gos,"1:4='Death';
  5='Alive'"))

#recoding gcs
data_mhregistry$gcs<-as.factor(car::recode(
  data_mhregistry$gcs_tot,"1:12='Severe';
  13:15='Non-severe'"))

#Imputing 0 to audit
audit_data<-with(data_baseline,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))

for(i in 1:length(data_mhregistry$h1)){ 
  
  if(is.na(data_mhregistry$h1[i])==FALSE) {
    
    if(data_mhregistry$h1[i] == 0){
      
      data_mhregistry$h2[i] = 0
      data_mhregistry$h3[i] = 0
      data_mhregistry$h4[i] = 0
      data_mhregistry$h5[i] = 0
      data_mhregistry$h6[i] = 0
      data_mhregistry$h7[i] = 0
      data_mhregistry$h8[i] = 0
      data_mhregistry$h9[i] = 0
      data_mhregistry$h10[i] = 0
      
    }
    
  }
} 

#Recoding clinical conditionMessage
# data_tbi$registry_year<-as.Date(as.character(data_tbi$date_arrival),
#   format = "%m/%d/%y")

#subsetting data set to keep only baseline data
data_baseline<-data_mhregistry[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
data_3fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
data_6fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
data_9fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

#CALCULATING FACTOR SCORES
#############################################################################
#CONFIRMATORY FACTOR ANALYSIS

#FIM 30 items
###############
FIM_data30<-with(data_mhregistry,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))
data_imputed <- mice(FIM_data30, seed = 2222, m=10)
FIM_data30<-mice::complete(data_imputed,4)
#FIM Motor
FIM_Motor30<-with(FIM_data30,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16))
#FIM Cognitive
FIM_Cognitive30<-with(FIM_data30,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))


fim_data_baseline<-FIM_data30[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
fim_data_3fup<-FIM_data30[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
fim_data_6fup<-FIM_data30[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
fim_data_9fup<-FIM_data30[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

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
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")

fim_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=fim_data_baseline,method="EBM")
fim_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=fim_data_3fup,method="EBM")
fim_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=fim_data_6fup,method="EBM")
fim_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=fim_data_9fup,method="EBM")

fim_scores_raw<-rbind(fim_scores_raw_baseline,
                      fim_scores_raw_3fup,
                      fim_scores_raw_6fup,
                      fim_scores_raw_9fup)

data_mhregistry$fim_scores_MF<-scales::rescale(as.data.frame(fim_scores_raw)[,1], 
                                               to = c(0, 100))
data_mhregistry$fim_scores_CF<-scales::rescale(as.data.frame(fim_scores_raw)[,2], 
                                               to = c(0, 100))

#MOCA
#baseline data
MOCA_data1<-with(data_mhregistry,data.frame(f1a,f1c,f1d,f1e,f2c,f2d,f15,f16,f17,f18,f19,f20,f21,f21b,f22,f23))
#SUM f1+f2, f21 item
MOCA_data1$f1 <- rowSums(MOCA_data1[ , 1:5]) 
#MOCA_data1$f21 <- rowSums(MOCA_data1[ , 13:14]) 
MOCA_data1$f17 <- rowSums(MOCA_data1[ , 9:10]) 
MOCA_data1<-with(MOCA_data1,data.frame(f1,f15,f16,f17,f19,f20,f21,f21b,f22,f23))
data_imputed1 <- mice(MOCA_data1, seed = 2222, m=10)
MOCA_data<-mice::complete(data_imputed1,4)

#followupdata
moca_data_baseline<-MOCA_data[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]
xxxxx
#subsetting data set to keep only baseline data
moca_data_3fup<-MOCA_data[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
moca_data_6fup<-MOCA_data[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
moca_data_9fup<-MOCA_data[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

cfa_model <- '
MOCA =~ f1 + f15 + f16 + f17 + f19 + f20 + f21 + f21b + f22 + f23
#f17 ~~ f21
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
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")
#lavInspect(fit,what="th") 

moca_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=moca_data_baseline,method="EBM")
moca_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=moca_data_3fup,method="EBM")
moca_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=moca_data_6fup,method="EBM")
moca_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=moca_data_9fup,method="EBM")

moca_scores_raw<-rbind(moca_scores_raw_baseline,
                       moca_scores_raw_3fup,
                       moca_scores_raw_6fup,
                       moca_scores_raw_9fup)

data_mhregistry$moca_scores<-scales::rescale(as.data.frame(moca_scores_raw)[,1], 
                                             to = c(0, 100))

#KESSLER
###############
#baseline data
kessler_data1<-with(data_mhregistry,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
data_imputed1 <- mice(kessler_data1, seed = 2222, m=10)
kessler_data<-mice::complete(data_imputed1,4)

#followup data
kessler_data_baseline<-kessler_data[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
kessler_data_3fup<-kessler_data[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
kessler_data_6fup<-kessler_data[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
kessler_data_9fup<-kessler_data[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

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
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")
# lavInspect(fit,what="th")

kessler_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=kessler_data_baseline,method="EBM")
kessler_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=kessler_data_3fup,method="EBM")
kessler_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=kessler_data_6fup,method="EBM")
kessler_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=kessler_data_9fup,method="EBM")

kessler_scores_raw<-rbind(kessler_scores_raw_baseline,
                          kessler_scores_raw_3fup,
                          kessler_scores_raw_6fup,
                          kessler_scores_raw_9fup)

data_mhregistry$kessler_scores<-scales::rescale(as.data.frame(kessler_scores_raw)[,1], 
                                                to = c(0, 100))

# #KESSLER
# # 2 factors model ###########################
# cfa_model <- '
# Depression =~  d1 + d4 + d7 + d8 + d9 + d10
# Anxiety =~ d2 + d3 + d5 + d6
# #
# # Depression ~~ Depression
# # Anxiety ~~ Anxiety
# #cov
# # d2 ~~  d9
# # d5 ~~  d6
# # d7 ~~  d8
# '

# fit <- lavaan::cfa(cfa_model,
#                    data = kessler_data,
#                    estimator="WLSMV",
#                    ordered=colnames(kessler_data))
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
# ))
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# lavInspect(fit,what="th")


#SF8
##############
#baseline
sf8_data1<-with(data_mhregistry,data.frame(sf8_b1,
                                           sf8_b2,
                                           sf8_b3,
                                           sf8_b4,
                                           sf8_b5,
                                           sf8_b6,
                                           sf8_b7,
                                           sf8_b8))
data_imputed1 <- mice(sf8_data1, seed = 2222, m=10)
sf8_data<-complete(data_imputed1,4)

#followup
sf8_data_baseline<-sf8_data[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
sf8_data_3fup<-sf8_data[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
sf8_data_6fup<-sf8_data[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
sf8_data_9fup<-sf8_data[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

# cfa_model <- '
# SF8 =~  sf8_b1 + sf8_b2 + sf8_b3 + 
# sf8_b4 + sf8_b5 + sf8_b6 + sf8_b7 + sf8_b8
# #
# #cov
# sf8_b2 ~~ sf8_b8
# sf8_b1 ~~ sf8_b5
# '

# fit <- lavaan::cfa(cfa_model,
#                    data = sf8_data,
#                    estimator="WLSMV",
#                    ordered=colnames(sf8_data)
# )
# summary(fit, fit.measures=TRUE)
# lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
#                                           "rmsea.ci.lower.scaled",
#                                           "rmsea.ci.upper.scaled",
#                                           "cfi.scaled",
#                                           "tli.scaled",
#                                           "nnfi.scaled",
#                                           "chisq.scaled",
#                                           "pvalue.scaled"
# ))
# # AIC(fit)
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")
# lavInspect(fit,what="th")

#SF8
# 2 factors model ###########################
cfa_model <- '
PHC =~  sf8_b1 + sf8_b2 + sf8_b3 + sf8_b4 + sf8_b5
MHC =~  sf8_b6 + sf8_b7 + sf8_b8
#
#cov
# sf8_b2 ~~ sf8_b8
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
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# lavInspect(fit,what="th")

sf8_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=sf8_data_baseline,method="EBM")
sf8_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=sf8_data_3fup,method="EBM")
sf8_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=sf8_data_6fup,method="EBM")
sf8_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=sf8_data_9fup,method="EBM")

sf8_scores_raw<-rbind(sf8_scores_raw_baseline,
                      sf8_scores_raw_3fup,
                      sf8_scores_raw_6fup,
                      sf8_scores_raw_9fup)

data_mhregistry$sf8_scores_MF<-scales::rescale(as.data.frame(sf8_scores_raw)[,1], 
                                               to = c(0, 100))
data_mhregistry$sf8_scores_CF<-scales::rescale(as.data.frame(sf8_scores_raw)[,2], 
                                               to = c(0, 100))

#PHQ9
#############
#baseline
phq9_data1<-with(data_mhregistry,data.frame(phq9_b11,
                                            phq9_b12,
                                            phq9_b13,
                                            phq9_b14,
                                            phq9_b15,
                                            phq9_b16,
                                            phq9_b17,
                                            phq9_b18,
                                            phq9_b19
))
data_imputed1 <- mice(phq9_data1, seed = 22221, m=10)
phq9_data<-mice::complete(data_imputed1,4)

#followup
phq9_data_baseline<-phq9_data[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
phq9_data_3fup<-phq9_data[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
phq9_data_6fup<-phq9_data[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
phq9_data_9fup<-phq9_data[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

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
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")


phq9_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=phq9_data_baseline,method="EBM")
phq9_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=phq9_data_3fup,method="EBM")
phq9_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=phq9_data_6fup,method="EBM")
phq9_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=phq9_data_9fup,method="EBM")

phq9_scores_raw_baseline<-rowSums(phq9_data_baseline)
phq9_scores_raw_3fup<-rowSums(phq9_data_3fup)
phq9_scores_raw_6fup<-rowSums(phq9_data_6fup)
phq9_scores_raw_9fup<-rowSums(phq9_data_9fup)

phq9_scores_raw<-c(phq9_scores_raw_baseline,
                   phq9_scores_raw_3fup,
                   phq9_scores_raw_6fup,
                   phq9_scores_raw_9fup)

data_mhregistry$phq9_scores<-scales::rescale(as.data.frame(phq9_scores_raw)[,1], 
                                             to = c(0, 100))

#AUDIT
#baseline
audit_data<-with(data_mhregistry,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
# audit_data1<-with(data_baseline,data.frame(
# 	h1,h2,h3))
# audit_data2<-with(data_baseline,data.frame(
# 	h4,h5,h6,h7,h8,h9,h10))
# audit_data2_3<-with(data_baseline,data.frame(
# 	h4,h5,h6))
# audit_data3_3<-with(data_baseline,data.frame(
# 	h7,h8,h9,h10))
data_imputed1 <- mice(audit_data, seed = 2222, m=10)
audit_data<-mice::complete(data_imputed1,4)

#followuip
audit_data_baseline<-audit_data[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
audit_data_3fup<-audit_data[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
audit_data_6fup<-audit_data[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
audit_data_9fup<-audit_data[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

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
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")

# #AUDIT
# # 2 factor model ###########
# audit_model2 <- '
# Audit =~  h1 + h2 + h3
# Audit2 =~ h4 + h5 + h6 + h7 + h8 + h9 + h10
# '

# fit <- lavaan::cfa(audit_model2,
#                    data = audit_data,
#                    estimator="WLSM",
#                    ordered=names(audit_data))
# summary(fit,
#         fit.measures=TRUE)
# lavaan::fitMeasures(fit,
#                     fit.measures = "all")
# parameterEstimates(fit)
# Est <- lavaan::parameterEstimates(fit,
#                                   ci = TRUE,
#                                   standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")

audit_scores_raw_baseline<-lavaan::lavPredict(fit,newdata=audit_data_baseline,method="EBM")
audit_scores_raw_3fup<-lavaan::lavPredict(fit,newdata=audit_data_3fup,method="EBM")
audit_scores_raw_6fup<-lavaan::lavPredict(fit,newdata=audit_data_6fup,method="EBM")
audit_scores_raw_9fup<-lavaan::lavPredict(fit,newdata=audit_data_9fup,method="EBM")

audit_scores_raw<-rbind(audit_scores_raw_baseline,
                        audit_scores_raw_3fup,
                        audit_scores_raw_6fup,
                        audit_scores_raw_9fup)

data_mhregistry$audit_scores<-scales::rescale(as.data.frame(audit_scores_raw)[,1], 
                                              to = c(0, 100))

#CATEGORICAL VARIABLES
##########################################################################################

# sf8_PCS<-with(data_mhregistry,data.frame(sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5))
# data_mhregistry$sf8_mcs<-rowSums(sf8_PCS)

# sf8_MCS<-with(data_mhregistry,data.frame(sf8_b6,sf8_b7,sf8_b8))
# data_mhregistry$sf8_pcs<-rowSums(sf8_MCS)

phq9<-with(data_mhregistry,data.frame(phq9_b11,phq9_b12,phq9_b13,phq9_b14,
                                      phq9_b15,phq9_b16,phq9_b17,phq9_b18,phq9_b19))
data_mhregistry$phq9_sum<-rowSums(phq9)

audit<-with(data_mhregistry,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
data_mhregistry$audit_sum<-rowSums(audit)

cage<-with(data_mhregistry,data.frame(h11,h12,h13,h14))
data_mhregistry$cage_sum<-rowSums(cage)

# fim_physical<-with(data_mhregistry,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
# 	g11,g12,g13,g14,g15,g16))
# data_mhregistry$fim_physical_score<-rowSums(fim_physical)/16

# fim_mental<-with(data_mhregistry,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,
# 	g27,g28,g29,g30))
# data_mhregistry$fim_mental_score<-rowSums(fim_mental)/14

# mental<-with(data_mhregistry,data.frame(f1a,f1b,f1c,f1d,f1e,f2a,f2b,f2c,f2d,f2e,f3,
# f4,f5,f6,f7,f8,f9,f10,f11,f12___0,f12___1,f12___2))
# data_mhregistry$mental<-rowSums(mental)

#baseline data
data_mhregistry$moca_sum<-rowSums(MOCA_data)

# kessler<-with(data_mhregistry,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,
# 	d10))
kessler<-with(data_mhregistry,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
data_mhregistry$kessler_sum<-rowSums(kessler)

#classifying variables
data_mhregistry$phq9_cat<-car::recode(data_mhregistry$phq9_sum,"0:7.9='no';8:21='yes'")
data_mhregistry$kessler_score_cat<-car::recode(data_mhregistry$kessler_sum,"10:20='no';21:50='yes'")
data_mhregistry$audit_cat<-car::recode(data_mhregistry$audit_sum,"0:7.9='no';8:35='yes'")
data_mhregistry$fimPC_cat<-car::recode(data_mhregistry$fim_scores_CF,"0:49.999='yes';50:100='no'")
data_mhregistry$fimMC_cat<-car::recode(data_mhregistry$fim_scores_MF,"0:49.999='yes';50:100='no'")
data_mhregistry$sf8MC_cat<-car::recode(data_mhregistry$sf8_scores_MF,"0:49.9='no';50:100='yes'")
data_mhregistry$sf8PC_cat<-car::recode(data_mhregistry$sf8_scores_CF,"0:49.9='no';50:100='yes'")
data_mhregistry$moca_cat<-car::recode(data_mhregistry$moca_sum,"0:25.9='yes';26:30='no'")

#subsetting data set to keep only baseline data
data_baseline<-data_mhregistry[
  data_mhregistry$redcap_event_name=="enrollment_arm_1",]

#subsetting data set to keep only baseline data
data_3fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_3_followup_arm_1",]

#subsetting data set to keep only baseline data
data_6fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_6_followup_arm_1",]

#subsetting data set to keep only baseline data
data_9fup<-data_mhregistry[
  data_mhregistry$redcap_event_name=="month_9_followup_arm_1",]

#############################################################################
#
#PAPER 1 - BASELINE CHARACtERIStiCS
#
#############################################################################

#############################################################################
#DESCRIPTIVES
#############################################################################
table(data_mhregistry$redcap_event_name)
prop.table(table(data_mhregistry$redcap_event_name))

# Gender 0=male 1=female
table<-with(data_baseline,table(female))
table
prop.table(table)
# table<-with(data_mhregistry,table(female,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Marital 0=not_married 1=married
table<-with(data_baseline,table(married_recoded))
table
prop.table(table)
# table<-with(data_baseline,table(married_recoded,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Ocupation
table<-with(data_baseline,table(occupation_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Education
table<-with(data_baseline,table(education_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Home people residing in the house
with(data_baseline,summary(age.x))
# ad.test(data$age)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,summary)
#wilcox.test(data$home_people~data$risk_classification)

######## CLINICAL INDICATORS
# GCS
with(data_baseline,summary(gcs_tot))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# GOS
with(data_baseline,summary(gose))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# MOI
table<-with(data_baseline,table(moi))
table
prop.table(table)

######## MENTAL HEALTH SCALES

#SF8
summary(100-data_baseline$sf8_scores_CF)
summary(100-data_baseline$sf8_scores_MF)
# ad.test(phq9score)
# One Way Anova (Completely Randomized Design)
# kruskal.test(data$phq9score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$phq9score, g=data$redcap_event_name,
#  method="Chisq")
# # summary(fit)
# #hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_clas0sification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(sf8PC_cat))
table
prop.table(table)
table<-with(data_baseline,table(sf8MC_cat))
table
prop.table(table)

# PHQ9
summary(data_baseline$phq9_sum)
by(data_mhregistry$phq9_sum,
   data_mhregistry$redcap_event_name,
   summary)

# ad.test(phq9score)
# One Way Anova (Completely Randomized Design)
# kruskal.test(data$phq9score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$phq9score, g=data$redcap_event_name,
#  method="Chisq")
# summary(fit)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_clas0sification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(phq9_cat))
table
prop.table(table)

# KESSLER
summary(data_baseline$kessler_sum)
by(data_mhregistry$kessler_sum,
   data_mhregistry$redcap_event_name,
   summary)
# ad.test(data_baseline$kessler_scores)
# # One Way Anova (Completely Randomized Design)
# kruskal.test(data$kes_score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$kes_score, g=data$redcap_event_name,
#  method="Chisq")
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(kessler_score_cat))
table
prop.table(table)

# AUDIT
summary(data_baseline$audit_sum)
by(data_mhregistry$audit_sum,
   data_mhregistry$redcap_event_name,
   summary)
# ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(audit_cat))
table
prop.table(table)

# MOCA
summary(data_baseline$moca_sum)
by(data_mhregistry$moca_sum,
   data_mhregistry$redcap_event_name,
   summary)

ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(moca_cat))
table
prop.table(table)

# FIM
summary(data_baseline$fim_scores_CF)
summary(data_baseline$fim_scores_MF)

by(data_mhregistry$fim_scores_MF,
   data_mhregistry$redcap_event_name,
   summary)
ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(fimPC_cat))
table
prop.table(table)
table<-with(data_baseline,table(fimMC_cat))
table
prop.table(table)

#############################################################################
#GRAPH
#############################################################################

library(qgraph)

mhealth_scales<-with(data_baseline,data.frame(fim_data_baseline,
                                              moca_data_baseline,
                                              kessler_data_baseline,
                                              sf8_data_baseline,
                                              phq9_data_baseline,
                                              audit_data_baseline))

cor<-cor_auto(mhealth_scales)

#qsgc<-qsgc$rho

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(Funct_Motor=c(1:15),
                     Funct_Cog=c(16:29),
                     Cognition=c(30:39),
                     Distress=c(40:49),
                     QOL_Mental=c(50:52),
                     QOL_Physical=c(53:57),
                     Depression=c(58:66),
                     Alcohol_use=c(67:76))

# # creating vectors for labels
# importance_node_labels<-c("Why is this study being done?", 
# 	"What is involved in this study?", 
# 	"Who is going to be my doctor in this study?",
# 	"How many people will take part in this study?",
# 	"How long will I be in this study?",
# 	"What are the benefits of being in this study?",
# 	"What about compensation?",
# 	"What are the risks of being in this study?",
# 	"What are the costs?",
# 	"Will my information be kept confidential?",
# 	"What about research related injuries?",
# 	"What are the alternatives to being in this study?",
# 	"What if I want decline participation or withdraw?",
# 	"Whom do I call if I have questions or trouble?",
# 	"Willingness to participate")

# creating nodes labels vector
node_names<-paste("Q ",c(1:81),sep="")

# creating vector with mean values for each node
# mean_data<-sapply(importance_network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
# importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#Calculating Community measures
# g<-as.igraph(importance_network_glasso) #creating igraph object
# #h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network

#Identify SPLs within the graph and extract direct paths to WP
# predictors<-centrality(importance_network_glasso)$ShortestPaths[,15]
# predictors

# #getting edge list with edges originating, receiveing and weights
# importance_network_glasso$Edgelist$from
# importance_network_glasso$Edgelist$to
# importance_network_glasso$Edgelist$weight

# #Extracting edges weights for each direct path
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==1 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==2 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==3 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==10 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==13 & 
# 	importance_network_glasso$Edgelist$to==15)

network_glasso<-qgraph(cor,
                       layout='spring',
                       # esize=20,
                       graph="glasso",
                       sampleSize=nrow(mhealth_scales),
                       legend.cex = 0.5,
                       cut = 0.3,
                       # maximum = 1, 
                       minimum = 0.2,
                       # esize = 20,
                       # vsize = tau, 
                       # repulsion = 0.8,
                       # nodeNames=node_labels,
                       # shape="square",
                       border.width=5,
                       groups=network_groups,
                       # color=c("gold","steelblue","red","grey80","green"),
                       borders = FALSE,
                       tuning=0.25
                       # labels=node_names
                       #gray=T,
)

#############################################################################
#
#PAPER 2 - LONGITUDINAL STUDY CHARACtERIStiCS
#
#############################################################################

#DESCRIPTIVES
#############################################################################

#Figure
library(RColorBrewer)
library(ggplot2)

#
with(data_mhregistry,table(phq9_cat,redcap_event_name))
x<-with(data_mhregistry,prop.table(table(phq9_cat,redcap_event_name),2))
t(x)

with(data_mhregistry,table(kessler_score_cat,redcap_event_name))
z<-with(data_mhregistry,prop.table(table(kessler_score_cat,redcap_event_name),2))
t(z)

with(data_mhregistry,table(audit_cat,redcap_event_name))
a<-with(data_mhregistry,prop.table(table(audit_cat,redcap_event_name),2))
t(a)

with(data_mhregistry,table(fimPC_cat,redcap_event_name))
b<-with(data_mhregistry,prop.table(table(fimPC_cat,redcap_event_name),2))
t(b)

with(data_mhregistry,table(fimMC_cat,redcap_event_name))
c<-with(data_mhregistry,prop.table(table(fimMC_cat,redcap_event_name),2))
t(c)

with(data_mhregistry,table(sf8MC_cat,redcap_event_name))
d<-with(data_mhregistry,prop.table(table(sf8MC_cat,redcap_event_name),2))
t(d)

with(data_mhregistry,table(sf8PC_cat,redcap_event_name))
e<-with(data_mhregistry,prop.table(table(sf8PC_cat,redcap_event_name),2))
t(e)

with(data_mhregistry,table(moca_cat,redcap_event_name))
f<-with(data_mhregistry,prop.table(table(moca_cat,redcap_event_name),2))
t(f)

scales <- rep(c("Depression","Depression",
                "Distress","Distress",
                "Alcohol Use","Alcohol Use",
                "FIM Physical","FIM Physical",
                "FIM Mental","FIM Mental",
                "QoL Physical","QoL Physical",
                "QoL Mental","QoL Mental",
                "Cognition","Cognition"),4)

times <- c(rep("Admission",16),rep("FUP 3",16),rep("FUP 6",16),rep("FUP 9",16))

Prevalence <- rep(c("no","yes","no","yes","no","yes","no","yes",
                    "no","yes","no","yes","no","yes","no","yes"),4)

df <- data.frame(scales,times,Prevalence)
df

df$value<-c(x[,1],z[,1],a[,1],b[,1],c[,1],d[,1],e[,1],f[,1],
            x[,2],z[,2],a[,2],b[,2],c[,2],d[,2],e[,2],f[,2],
            x[,3],z[,3],a[,3],b[,3],c[,3],d[,3],e[,3],f[,3],
            x[,4],z[,4],a[,4],b[,4],c[,4],d[,4],e[,4],f[,4])

plot the stacked bar plot
tiff("/Users/jnv4/Desktop/menta_health1.tiff",
     width = 800, height = 300,compression = 'lzw')
ggplot(df, aes(x = times)) + geom_bar(aes(weight=value, fill = Prevalence),
                                      position = 'fill') + scale_y_continuous("", breaks=NULL) +
  scale_fill_manual(values=c("lightblue","darkblue")) +
  facet_grid(.~scales)+
  xlab("Follow up Times")
# geom_text(aes(y=,x=,label=df$value))
dev.off()

#plot the stacked bar plot with polar coordinates
ggplot(df, aes(x = project)) + 
  geom_bar(aes(weight=numbers, fill = component), position = 'fill') + 
  scale_y_continuous("", breaks=NA) + 
  scale_fill_manual(values = rev(brewer.pal(6, "Purples"))) + 
  coord_polar()



#DESCRIPTIVE Data
table(data_mhregistry$redcap_event_name)
prop.table(table(data_mhregistry$redcap_event_name))

# Gender 0=male 1=female
table<-with(data_baseline,table(female))
table
prop.table(table)
# table<-with(data_mhregistry,table(female,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Marital 0=not_married 1=married
table<-with(data_baseline,table(married_recoded))
table
prop.table(table)
# table<-with(data_baseline,table(married_recoded,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Ocupation
table<-with(data_baseline,table(occupation_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Education
table<-with(data_baseline,table(education_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Home people residing in the house
with(data_baseline,summary(age.x))
# ad.test(data$age)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,summary)
#wilcox.test(data$home_people~data$risk_classification)

######## CLINICAL INDICATORS
# GCS
with(data_baseline,summary(gcs_tot))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# GOS
with(data_baseline,summary(gose))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# MOI
table<-with(data_baseline,table(moi))
table
prop.table(table)

######## MENTAL HEALTH SCALES

#SF8
summary(100-data_baseline$sf8_scores_CF)
summary(100-data_baseline$sf8_scores_MF)
# ad.test(phq9score)
# One Way Anova (Completely Randomized Design)
# kruskal.test(data$phq9score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$phq9score, g=data$redcap_event_name,
#  method="Chisq")
# # summary(fit)
# #hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_clas0sification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(sf8PC_cat))
table
prop.table(table)
table<-with(data_baseline,table(sf8MC_cat))
table
prop.table(table)

# PHQ9
summary(data_baseline$phq9_sum)
# ad.test(phq9score)
# One Way Anova (Completely Randomized Design)
# kruskal.test(data$phq9score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$phq9score, g=data$redcap_event_name,
#  method="Chisq")
# summary(fit)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_clas0sification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(phq9_cat))
table
prop.table(table)

# KESSLER
summary(data_baseline$kessler_sum)
# ad.test(data_baseline$kessler_scores)
# # One Way Anova (Completely Randomized Design)
# kruskal.test(data$kes_score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$kes_score, g=data$redcap_event_name,
#  method="Chisq")
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(kessler_score_cat))
table
prop.table(table)

# AUDIT
summary(data_baseline$audit_sum)
# ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(audit_cat))
table
prop.table(table)

# MOCA
summary(data_baseline$moca_sum)
ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(moca_cat))
table
prop.table(table)

# FIM
summary(data_baseline$fim_scores_CF)
summary(data_baseline$fim_scores_MF)
ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_baseline,table(fimPC_cat))
table
prop.table(table)
table<-with(data_baseline,table(fimMC_cat))
table
prop.table(table)


#############################################################################
#DO NOT RUN - ARCHIVE
#############################################################################

scales<-with(data_mhregistry,c(fim_scores_CF,
                               fim_scores_MF,
                               moca_scores,
                               kessler_scores,
                               sf8_scores_MF,
                               sf8_scores_CF,
                               audit_scores,
                               phq9_scores))

event<-rep(data_mhregistry$redcap_event_name,8)

names<-c(rep("FIM Physical",530),
         rep("FIM Mental",530),
         rep("Cognition",530),
         rep("Distress",530),
         rep("QoL Mental",530),
         rep("QoL Physical",530),
         rep("Alcohol Use",530),
         rep("Depression",530))

lineplotdata<-data.frame(scales,event,names)

lineplotdata1<-with(lineplotdata, tapply(scales, list("FUP"=event, "Scales"=names), mean))
subjmeans <- melt(lineplotdata1,id="FUP")

# Use thicker lines and larger points, and hollow white-filled points
ggplot(data=subjmeans, aes(x=FUP,
                           y=value,
                           group=Scales)) + 
  geom_point(size=3, fill="white") +
  geom_line(size=1.5) + 
  scale_shape_manual(values=c(22,21))












#### TIME STUFF
time_series<-with(data_tbi,data.frame(death,
                                      registry_year,gcs_tot))
# time_series<-na.omit(time_series)

time_series_severe<-subset(time_series,time_series$gcs_tot<9)
#BY month
#recoding data to decompose time series into month based time series
time_series_severe$date_month <- floor_date(time_series_severe$registry_year, 
                                            "year")

# # summarise crash data by month
# time_series_severe_month<-ddply(time_series_severe, "date_year", summarise, 
# 	deaths_month = table(time_series_severe$death))

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series_severe$date_month,
# time_series_severe$death),1))
#get descriptives
# psych::describe(time_series_severe_month)

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series$date_month,
# time_series$death),1))
#get descriptives
# psych::describe(time_series_month)

table_2<-subset(table,table$Var2==1)

#### ICU
time_series<-with(data_tbi,data.frame(death,
                                      registry_year,gcs_tot,surgtoicu))
# time_series<-na.omit(time_series)

time_series_severe<-subset(time_series,time_series$surgtoicu==1)
#BY month
#recoding data to decompose time series into month based time series
time_series_severe$date_month <- floor_date(time_series_severe$registry_year, 
                                            "year")

# # summarise crash data by month
# time_series_severe_month<-ddply(time_series_severe, "date_year", summarise, 
# 	deaths_month = table(time_series_severe$death))

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series_severe$date_month,
# time_series_severe$death),1))
#get descriptives
# psych::describe(time_series_severe_month)

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series$date_month,
# time_series$death),1))
#get descriptives
# psych::describe(time_series_month)

table_2<-subset(table,table$Var2==1)