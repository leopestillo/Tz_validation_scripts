lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)

#MAC
data <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_bnipatients_data.csv", header = TRUE)

#DESKTOP
data <- read.csv("D:/Google Drive/[PES] life discussions/Tz_bnipatient/Tz_bnipatients_data.csv", header = TRUE)

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
BNI_data<-with(data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,think_less_treated_person,
                                           less_opinion_trtd_person,alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                                           recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(BNI_data, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
BNI_data<-mice::complete(data_imputed,4)

#BNI_Devaluation
BNI_Devaluation<-with(BNI_data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,
                                          think_less_treated_person,less_opinion_trtd_person))

#BNI_Discrimination
BNI_Discrimination<-with(BNI_data,data.frame(alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                                               recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))


psych::alpha(BNI_data,n.iter=1000,check.keys=TRUE)

cor_data<-cor_auto(BNI_data) 

# fa(cor_data,2,rotate="promax")
fa(BNI_data,1,fm="pa",rotate="promax")
fa(BNI_data,2,fm="pa",rotate="promax")
fa(BNI_data,3,fm="pa",rotate="promax")

#based on a polychoric correlation matrix
fa.poly(cor_data,1,fm="uls",rotate="oblimin")
fa.poly(cor_data,2,fm="uls",rotate="oblimin")
fa.poly(cor_data,3,fm="uls",rotate="oblimin")

# 1 factor model
cfa_model <- '
BNI =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+think_less_treated_person+
                                           less_opinion_trtd_person+alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
'

# 2 factor model
cfa_model <- '
BNI_Devaluation =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+
                                          think_less_treated_person+less_opinion_trtd_person
BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
                                               recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
'

fit <- lavaan::cfa(cfa_model,
                   data = BNI_data,
                   estimator="WLSMV",
                   ordered=colnames(BNI_data)
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


