#install.packages("sem")
#install.packages("ggplot2")
#install.packages("psych")
#install.packages("RCurl")
#install.packages("irr")
#install.packages("nortest")
#install.packages("moments")
#install.packages("GPArotation")
#install.packages("nFactors")
#install.packages("boot")
#install.packages("psy")
#install.packages("car")
#install.packages("vcd")
#install.packages("gridExtra")
#install.packages("mi")
#install.packages("VIM")
#install.packages("gdata")
#install.packages("sqldf")
#install.packages("reshape2")
#install.packages("mclust")
#install.packages("foreign")
#install.packages("survival")
#install.packages("memisc")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("dplyr")
#install.packages("QCA")
#install.packages("VennDiagram")
#install.packages("qgraph")
#install.packages("igraph")
#install.packages("ltm")
#install.packages("gmodels")
#install.packages("eRm")
#install.packages("mirt")
#install.packages("dplyr")
#install.packages("devtools")
#install.packages("reshape")
#install.packages("mice")
#install.packages("quantreg")
#install.packages("graphics")
#install.packages("mgcv")
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram"),
       library, character.only=T)
#library("sem")
#library("ggplot2")
#library("psych")
#library("RCurl")
#library("irr")
#library("nortest")
#library("moments")
#library("GPArotation")
#library("nFactors")
#library("boot")
#library("psy")
#library("car")
#library("vcd")
#library("gridExtra")
#library("mi")
#library("VIM")
#library("gdata")
#library("sqldf")
#library("reshape2")
#library("mclust")
#library("foreign")
#library("survival")
#library("memisc")
#library("lme4")
#library("lmerTest")
#library("dplyr")
#library("QCA")
#library("VennDiagram")
#library("mice")
#library("qgraph")
#library("igraph")
#library("ltm")
#library("gmodels")
#library("eRm")
#library("mirt")
#library("dplyr")
#library("devtools")
#library("reshape")
#library("quantreg")
#library("graphics")
#library("mgcv")

#MAC
data_patients<-read.csv("/Users/temitopegafaar/Documents/Third Year/Research Projects/PAS Validation/Raw data/Tz_bnipatients_data.csv")

data_family<-read.csv("/Users/temitopegafaar/Documents/Third Year/Research Projects/PAS Validation/Raw data/Tz_bniKAfamily_data.csv")


## Gathering data
#HCP dataset excluded since their PAS data is already reported in the Perceived barriers to treatment manuscript

data_patients$group<-c("Patients")
data_family$group<-c("Family")



patients_score_data<-with(data_patients, data.frame(
  alcoholic_close_friend,
  recovered_alcoholic_teacher,
  recover_alcoholic_chldrn,
  recover_alcoholic_hired,
  non_alcoholic_hired,
  recovered_alc_treat_same,
  no_date_hospital_for_alc,
  alc_treatment_intelligent,
  alcoholic_trustworthy,
  alc_treatment_failure,
  think_less_treated_person,
  less_opinion_trtd_person,
  group))

family_score_data<-with(data_family, data.frame(
  alcoholic_close_friend=f_alcoholic_close_friend,
  recovered_alcoholic_teacher=f_recoveralcohol_teacher,
  recover_alcoholic_chldrn=f_recover_alcoholic_chldrn,
  recover_alcoholic_hired=f_recover_alcoholic_hired,
  non_alcoholic_hired=f_non_alcoholic_hired,
  recovered_alc_treat_same=f_recovered_alc_treat_same,
  no_date_hospital_for_alc=f_no_date_hospital_for_alc,
  alc_treatment_intelligent=f_alc_treat_intel,
  alcoholic_trustworthy=f_alcoholic_trustworthy,
  alc_treatment_failure=f_alc_treatment_failure,
  think_less_treated_person=f_think_less_treat_person,
  less_opinion_trtd_person=f_less_opinion_trtd_person,
  group))


                                                    


data<-rbind(patients_score_data,family_score_data)


#recoding alc_treatment_failure variable
data$alc_treatment_failure<-car::recode(data$alc_treatment_failure,"
                                        1='6';2='5';3='4';
                                        4='3';5='2';6='1'")

#recoding recover_alcoholic_chldrn variable
data$recover_alcoholic_chldrn<-car::recode(data$recover_alcoholic_chldrn,"
                                           1='6';2='5';3='4';
                                           4='3';5='2';6='1'")

#recoding think_less_treated_person variable
data$think_less_treated_person<-car::recode(data$think_less_treated_person,"
                                            1='6';2='5';3='4';
                                            4='3';5='2';6='1'")

#recoding think_less_treated_person variable
data$think_less_treated_person<-car::recode(data$think_less_treated_person,"
                                            1='6';2='5';3='4';
                                            4='3';5='2';6='1'")

#recoding non_alcoholic_hired variable
data$non_alcoholic_hired<-car::recode(data$non_alcoholic_hired,"
                                      1='6';2='5';3='4';
                                      4='3';5='2';6='1'")

#recoding no_date_hospital_for_alc variable
data$no_date_hospital_for_alc<-car::recode(data$no_date_hospital_for_alc,"
                                           1='6';2='5';3='4';
                                           4='3';5='2';6='1'")

#recoding less_opinion_trtd_person variable
data$less_opinion_trtd_person<-car::recode(data$less_opinion_trtd_person,"
                                           1='6';2='5';3='4';
                                           4='3';5='2';6='1'")

#Organize scale datasets

#BNI
# stigma_data<-with(data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,think_less_treated_person,
#                                            less_opinion_trtd_person,alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
#                                            recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# data_imputed <- mice(BNI_data, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# BNI_data<-mice::complete(data_imputed,4)

#BNI_Devaluation
# BNI_Devaluation<-with(BNI_data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,
#                                           think_less_treated_person,less_opinion_trtd_person))

# #BNI_Discrimination
# BNI_Discrimination<-with(BNI_data,data.frame(alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
# recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data, seed = 2222, m=5)
imp <- mice(patients_score_data, seed = 2222, m=5)
imp <- mice(family_score_data, seed = 2222, m=5)


# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data<-mice::complete(imp,4)
patients_score_data<-mice::complete(imp,4)
family_score_data<-mice::complete(imp,4)



data_validation<-subset(data[data$redcap_event_name=="enrollment_arm_1",])
#Just to know
#data_validation = is the name you give to the data frame. You can give the name you want.
#subset = the function to access just the lines that you want
#redcap_event_name = you can find this name in codebook,this is the variable that indicates the time it collects was performed. In my case the codebook indicate redcap_event_name
#So this variable has some results, in my case the response enrollment_arm_1 indicate the first data collection = the baseline

cor_data<-cor_auto(data[,-13]) 
cor_data<-cor_auto(patients_score_data[,-13]) 
cor_data<-cor_auto(family_score_data[,-13]) 


psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)

# # fa(cor_data,2,rotate="promax")
# fa(BNI_data,1,fm="pa",rotate="promax")
# fa(BNI_data,2,fm="pa",rotate="promax")
# fa(BNI_data,3,fm="pa",rotate="promax")

#based on a polychoric correlation matrix (TG: wonder what it is about)
fa.poly(BNI_data,1,fm="uls",rotate="oblimin")
fa.poly(data[,-13],2,fm="uls",rotate="oblimin")
fa.poly(BNI_data,3,fm="uls",rotate="oblimin")

# 1 factor model
cfa_model <- '
BNI =~ alc_treatment_intelligent + 
alcoholic_trustworthy +
# alc_treatment_failure +
think_less_treated_person +
# less_opinion_trtd_person + 
alcoholic_close_friend +
recovered_alcoholic_teacher +
# recover_alcoholic_chldrn +
recover_alcoholic_hired +
# non_alcoholic_hired +
recovered_alc_treat_same +
no_date_hospital_for_alc

alc_treatment_intelligent ~~       alcoholic_trustworthy
'

# Neg =~ alc_treatment_failure +
#        # think_less_treated_person +
#        less_opinion_trtd_person + 
#        recover_alcoholic_chldrn +
#        non_alcoholic_hired +
#        no_date_hospital_for_alc
# '

# 2 factor model
cfa_model <- '
BNI_Devaluation =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+
                                         think_less_treated_person+less_opinion_trtd_person
BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
                                              recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
'

fit <- lavaan::cfa(cfa_model,
                   #data = data[,-13],
                   data = data,
                   estimator="WLSMV",
                   ordered=colnames(data)
)

fit <- lavaan::cfa(cfa_model,
                   #data = data[,-13],
                   data = family_score_data,
                   estimator="WLSMV",
                   ordered=colnames(family_score_data)
)
fit <- lavaan::cfa(cfa_model,
                   #data = data[,-13],
                   data = professionals_score_data,
                   estimator="WLSMV",
                   ordered=colnames(professionals_score_data)
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

#Predicted scores

pas_data<-with(data,data.frame(alc_treatment_intelligent, 
                               alcoholic_trustworthy,
                               think_less_treated_person,
                               alcoholic_close_friend,
                               recovered_alcoholic_teacher,
                               recover_alcoholic_hired,
                               recovered_alc_treat_same,
                               no_date_hospital_for_alc))

pas_scores<-lavaan::lavPredict(fit,newdata=pas_data)
pas_scores_scaled<-scales::rescale(as.data.frame(pas_scores)$BNI, 
                                   to = c(0, 100))



# # 1 factor model

# cfa_model <- '
# BNI =~ alc_treatment_intelligent + 
#        alcoholic_trustworthy +
#        alc_treatment_failure +
#        think_less_treated_person +
#        less_opinion_trtd_person + 
#        alcoholic_close_friend +
#        recovered_alcoholic_teacher +
#        recover_alcoholic_chldrn +
#        recover_alcoholic_hired +
#        non_alcoholic_hired +
#        recovered_alc_treat_same +
#        no_date_hospital_for_alc
# '

# # 2 factor model
# # cfa_model <- '
# # BNI_Devaluation =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+
# #                                           think_less_treated_person+less_opinion_trtd_person
# # BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
# #                                                recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
# # '

# fit <- lavaan::cfa(cfa_model,
#                    data = BNI_data,
#                    estimator="WLSMV",
#                    ordered=colnames(BNI_data)
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
# )
# )

# # AIC(fit)
# Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
# subset(Est, op == "=~")
# subset(Est, op == "~~")
# #lavInspect(fit,what="th") 

# ### Modification Indexes
# Mod <- lavaan::modificationIndices(fit)
# subset(Mod, mi > 10)

# #Predicted scores

# pas_data<-with(BNI_data,data.frame(alc_treatment_intelligent, 
#        alcoholic_trustworthy,
#        think_less_treated_person,
#        alcoholic_close_friend,
#        recovered_alcoholic_teacher,
#        recover_alcoholic_hired,
#        recovered_alc_treat_same,
#        no_date_hospital_for_alc))

# pas_scores<-lavaan::lavPredict(fit,newdata=pas_data,method="EBM")
# pas_scores_scaled<-scales::rescale(as.data.frame(pas_scores)$BNI, 
#     to = c(0, 100))
