######################################################
#rw_phq9validation_data.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step
#
######################################################
#SETTING ENVIRONMENT
######################################################
#install.packages("VIM")
#install.packages("VIMGUI")
#install.packages("miP")
#install.packages("gWidgetsRGtk2")
#install.packages("mi")
# install.packages("tidyverse")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","eRm","mirt","dplyr","devtools","reshape",
         "mice","jsonlite","tidyverse","pROC","Epi"),
       library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

#Mac
data_patients<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/database/Tz_bnipatients_data.csv",sep=',')
data_geral<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/database/Tz_BNIValidationSurvey_DATA_2018-01-16_0940.csv",sep=',')

#desktop
data_patients<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_bnipatients_data.csv",sep=',')
data_geral<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_BNIValidationSurvey_DATA_2018-01-16_0940.csv",sep=',')

#Organize scale datasets
data_patients <- subset(data_patients, consumption > "0")
data_geral <- subset(data_geral, how_often_drink > "0")

#audit_pc_patients
audit_pc_patients<-with(data_patients,data.frame(how_often_drink,
                                                 number_drinks_day,
                                                 how_often_cant_stop_drinking,
                                                 fail_expectation_bc_drinking,
                                                 others_concerned_your_drinking))


summary(audit_pc_patients)

#audit_pc_geral
audit_pc_geral<-with(data_geral,data.frame(how_often_drink,
                                          number_drinks_day,
                                          how_often_cant_stop_drinking=how_often_cant_stop,
                                          fail_expectation_bc_drinking=fail_expectation_bc_drink,
                                          others_concerned_your_drinking=other_concerned_your_drink))

summary(audit_pc_geral)

#create variable
#to create a nominal variable, has to use "XX"
audit_pc_patients$subject <- c("patients")
audit_pc_geral$subject <- c("geral")

# Mergind datasets
library("plyr")
audit_pc_data<-rbind(audit_pc_patients,audit_pc_geral)

#DATA IMPUTATION - TO CREATE A FUNCTION (ANY NAME) AND RECODE ALL VECTORS IN THE
#SAME TIME

#NAto0<-function(x){
#    car::recode(x,"NA=0")
#    }

#audit_data_NAto0<-lapply(audit_data,NAto0)
#audit_data_NAto0<-as.data.frame(audit_data_NAto0)

#summary(audit_data_NAto0)

audit_pc_imputed<-function(x){
  car::recode(x,"NA='0'")
}

audit_pc_imputed01<-lapply(audit_pc_data,audit_pc_imputed)
audit_pc_data_imputed<-as.data.frame(audit_pc_imputed01)

summary(audit_pc_data_imputed)
describe(audit_pc_data_imputed)

#IMPUT DATA SEPARETED
#patients

audit_pc_imputed_p<-function(x){
  car::recode(x,"NA='0'")
}

audit_pc_imputed02<-lapply(audit_pc_patients,audit_pc_imputed_p)
audit_pc_patients_imputed<-as.data.frame(audit_pc_imputed02)

summary(audit_pc_patients_imputed)


#geral
audit_pc_imputed_g<-function(x){
  car::recode(x,"NA='0'")
}

audit_pc_imputed03<-lapply(audit_pc_geral,audit_pc_imputed_g)
audit_pc_geral_imputed<-as.data.frame(audit_pc_imputed03)

summary(audit_pc_geral_imputed)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
#data_imputed <- mice(audit_patients, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
#audit_patients<-mice::complete(data_imputed,4)


# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
#data_imputed <- mice(audit_geral, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
#audit_geral<-mice::complete(data_imputed,4)


#recoding variables
#audit_data$how_often_drink<-car::recode(audit_data$how_often_drink,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$number_drinks_day<-car::recode(audit_data$number_drinks_day,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$how_often_6_more_drinks<-car::recode(audit_data$how_often_6_more_drinks,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$how_often_cant_stop_drinking<-car::recode(audit_data$how_often_cant_stop_drinking,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$fail_expectation_bc_drinking<-car::recode(audit_data$fail_expectation_bc_drinking,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$how_often_drink_morning<-car::recode(audit_data$how_often_drink_morning,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$how_often_guilt_postdrinking<-car::recode(audit_data$how_often_guilt_postdrinking,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$how_often_no_memory_postdrinking<-car::recode(audit_data$how_often_no_memory_postdrinking,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$drinking_injured_you_or_someone<-car::recode(audit_data$drinking_injured_you_or_someone,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#audit_data$others_concerned_your_drinking<-car::recode(audit_data$others_concerned_your_drinking,"
#                                     0='1';1='2';2='3';
#                                     3='4';4='5'")

#RELIABILITY
##############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)

#audit 3
#not applicable just 1 item

psych::alpha(audit_pc_data_imputed,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_pc_patients_imputed,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_pc_geral_imputed,n.iter=1000,check.keys=TRUE)

##############################################################
#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# AUDIT

# 1 factor model
cfa_model <- '
audit_pc =~ how_often_drink +
            number_drinks_day +
            how_often_cant_stop_drinking +
            fail_expectation_bc_drinking +
            others_concerned_your_drinking
'


#OVERALL
fit <- lavaan::cfa(cfa_model,
                   data = audit_pc_data_imputed,
                   estimator="WLSMV",
                   ordered=colnames(audit_pc_data_imputed)
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
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th")
#install.packages("lavInspect")
#library(lavInspect)

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)


#Composite Reliabilty
sum(Est$std.all[1:5])^2/(sum(Est$std.all[1:5])^2+sum(Est$std.all[24:28]))
#Average Extracted Variance
sum(Est$std.all[1:5]^2)/length(Est$std.all[1:5])
#Thresholds
by(Est$std.all[1:5],Est$lhs[1:5],mean)

#PATIENTS

fit <- lavaan::cfa(cfa_model,
                   data = audit_pc_patients_imputed,
                   estimator="WLSMV",
                   ordered=colnames(audit_pc_patients_imputed)
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
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th")
#install.packages("lavInspect")
#library(lavInspect)

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)


#Composite Reliabilty
#AUDIT 4
sum(Est$std.all[1:5])^2/(sum(Est$std.all[1:5])^2+sum(Est$std.all[24:28]))
#Average Extracted Variance
sum(Est$std.all[1:5]^2)/length(Est$std.all[1:5])
#Thresholds
by(Est$std.all[1:5],Est$lhs[1:5],mean)


#GERAL

fit <- lavaan::cfa(cfa_model,
                   data = audit_pc_geral_imputed,
                   estimator="WLSMV",
                   ordered=colnames(audit_pc_geral_imputed)
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
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th")
#install.packages("lavInspect")
#library(lavInspect)

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)


#Composite Reliabilty
#AUDIT 4
sum(Est$std.all[1:5])^2/(sum(Est$std.all[1:5])^2+sum(Est$std.all[23:27]))
#Average Extracted Variance
sum(Est$std.all[1:5]^2)/length(Est$std.all[1:5])
#Thresholds
by(Est$std.all[1:5],Est$lhs[1:5],mean)


#DSM V CRITERIA TO ALCOHOL USE DISORDER

#mac
data_patients<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_bnipatients_data.csv",sep=',')
data_geral<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_BNIValidationSurvey_DATA_2018-01-16_0940.csv",sep=',')

#Desktop
data_patients<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_bnipatients_data.csv",sep=',')
data_geral<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_BNIValidationSurvey_DATA_2018-01-16_0940.csv",sep=',')

data_patients <- subset(data_patients, consumption > "0")
data_geral <- subset(data_geral, how_often_drink > "0")
#data_patients_male<-subset(data_patients,female=0)
#data_patients_female<-subset(data_patients,female=1)
#data_geral_male<-subset(data_geral,female=0)
#data_geral_female<-subset(data_geral,female=1)

summary(data_patients)
describe(data_patients)

#Organize scale datasets

#dsm_patients
dsm_patients<-with(data_patients,data.frame(drink_more_times_want,
                                            cant_stop,
                                            spent_days_drinking,
                                            strong_desire_drink,
                                            drinking_interferes,
                                            drinking_arguments,
                                            gave_up_bc_drink,
                                            could_get_hurt,
                                            continue_drink_w_prblm,
                                            needed_more_drink,
                                            had_withdrawl))

summary(dsm_patients)
describe(dsm_patients)


#dsm_geral
dsm_geral<-with(data_geral,data.frame(drink_more_times_want=aud_drinkmore,
                                      cant_stop=aud_notstop,
                                      spent_days_drinking=aud_time,
                                      strong_desire_drink=aud_cravings,
                                      drinking_interferes=aud_interfere,
                                      drinking_arguments=aud_keepdrink,
                                      gave_up_bc_drink=aud_stopactivities,
                                      could_get_hurt=aud_hurt,
                                      continue_drink_w_prblm=aud_depress,
                                      needed_more_drink=aud_effect,
                                      had_withdrawl=aud_wd))

summary(dsm_geral)
describe(dsm_geral)


#create variable
#to create a nominal variable, has to use "XX"
dsm_patients$subject <- c("patients")
dsm_geral$subject <- c("geral")


# Mergind datasets
library("dplyr")
dsm_data<-rbind(dsm_patients,dsm_geral)
dsm_male<-rbind(data_patients_male,data_geral_male)

#recode NA
dsm_recode<-function(x){
  car::recode(x,"NA='0';5='0';8='0';9='0'")
}

dsm_recode01<-lapply(dsm_data,dsm_recode)
dsm_data_final<-as.data.frame(dsm_recode01)

summary(dsm_data_final)

#PATIENTS
dsm_recode<-function(x){
  car::recode(x,"NA='0';5='0';8='0';9='0'")
}

dsm_recode02<-lapply(dsm_patients,dsm_recode)
dsm_patients_final<-as.data.frame(dsm_recode02)

summary(dsm_patients_final)

#GERAL
dsm_recode<-function(x){
  car::recode(x,"NA='0';5='0';8='0';9='0'")
}

dsm_recode03<-lapply(dsm_geral,dsm_recode)
dsm_geral_final<-as.data.frame(dsm_recode03)

summary(dsm_geral_final)




#SUM ALL ITEMS
dsm_data_final$dsm_sum <- rowSums(dsm_data_final[ , 1:11])
dsm_patients_final$dsm_sum <- rowSums(dsm_patients_final[ , 1:11])
dsm_geral_final$dsm_sum <- rowSums(dsm_geral_final[ , 1:11])

#create variable - DSM CLASSIFICATION TO AUD
#ABSENCE
#MILD
#MODERATE
#SEVERE
#to create a nominal variable, has to use "XX"

#dsm_recode$mild <-  car::recode(dsm_recode$dsm_sum,"NA='0';0='0';1='0';2='1';3='1';4='1';5='1';6='1';7='1';8='1';9='1';10='1';11='1'")
#dsm_recode$moderate <-  car::recode(dsm_recode$dsm_sum,"0='0';1='0';2='0';3='0';4='1';5='1';6='1';7='1';8='1';9='1';10='1';11='1'")
#dsm_recode$severe <-  car::recode(dsm_recode$dsm_sum,"0='0';1='0';2='0';3='0';4='0';5='0';6='1';7='1';8='1';9='1';10='1';11='1'")


# necessary to change label (moderate)
dsm_data_final$alcoholuse<-car::recode(dsm_data_final$dsm_sum,"0='0';1='0';2='0';3='0';4='1';5='1';6='1';7='1';8='1';9='1';10='1';11='1'")
dsm_patients_final$alcoholuse_patients<-car::recode(dsm_patients_final$dsm_sum,"0='0';1='0';2='0';3='0';4='1';5='1';6='1';7='1';8='1';9='1';10='1';11='1'")
dsm_geral_final$alcoholuse_general<-car::recode(dsm_geral_final$dsm_sum,"0='0';1='0';2='0';3='0';4='1';5='1';6='1';7='1';8='1';9='1';10='1';11='1'")


#table<-with(dsm_recode,table(mild))
#table

table<-with(dsm_recode,table(moderate))
table

#table<-with(dsm_recode,table(severe))
#table


#SUM AUDIT pc - ITEMS
audit_pc_data_imputed$audit_pc_sum_all <- rowSums(audit_pc_data_imputed[ , 1:5])

#PATIENTS
audit_pc_patients_imputed$audit_pc_sum_p <- rowSums(audit_pc_patients_imputed[ , 1:5])

#GERAL
audit_pc_geral_imputed$audit_pc_sum_g <- rowSums(audit_pc_geral_imputed[ , 1:5])

# Mergind datasets
library("plyr")
alcohol_datapc_all<-data.frame(audit_pc_data_imputed,dsm_data_final)

#PATIENTS
#library("plyr")
alcohol_datapc_p<-data.frame(audit_pc_patients_imputed,dsm_patients_final)

#GERAL
#library("plyr")
alcohol_datapc_g<-data.frame(audit_pc_geral_imputed,dsm_geral_final)

########################################################
#ROC Plot with Sensitivity and Specificity
########################################################
# with(data_mcid2,by(change_score,change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC2,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC2,summary))

library(Epi)
#ROC(form=mild~audit_pc_sum_all, data=alcohol_datapc)
#ROC(form=mild~audit_pc_sum_p, data=alcohol_datapc)
#ROC(form=mild~audit_pc_sum_g, data=alcohol_datapc)

library(OptimalCutpoints)
#optimal.cutpoint.Youden <- optimal.cutpoints(X = "audit_pc_sum_all",
 #                                            #X = "audit_pc_sum_p",
  #                                           #X = "audit_pc_sum_g",
   #                                          status = "mild", 
    #                                         tag.healthy = "0",
     #                                        methods = "Youden", 
      #                                       data = alcohol_datapc, 
       #                                      pop.prev = NULL, 
        #                                     categorical.cov = NULL, #"gender",
         #                                    control = control.cutpoints("generalized.Youden = TRUE"), 
          #                                   ci.fit = FALSE, 
           #                                  conf.level = 0.95, 
            #                                 trace = FALSE)

#summary(optimal.cutpoint.Youden)
#plot(optimal.cutpoint.Youden)


#ROC(form=moderate~audit_pc_sum_all, data=alcohol_datapc_all)
#ROC(form=moderate~audit_pc_sum_p, data=alcohol_datapc_p)
#ROC(form=moderate~audit_pc_sum_g, data=alcohol_datapc_g)

#optimal.cutpoint.Youden <- optimal.cutpoints(X = "audit_pc_sum_all",
                                             #X = "audit_pc_sum_p",
                                             #X = "audit_pc_sum_g",
 #                                            status = "moderate", 
  #                                           tag.healthy = "0",
   #                                          methods = "Youden", 
    #                                         data = alcohol_datapc_all,
                        #                     data = alcohol_datapc_p,
                         #                    data = alcohol_datapc_g,
     #                                        pop.prev = NULL, 
      #                                       categorical.cov = NULL, #"gender",
       #                                      control = control.cutpoints("generalized.Youden = TRUE"), 
        #                                     ci.fit = FALSE, 
         #                                    conf.level = 0.95, 
          #                                   trace = FALSE)

#summary(optimal.cutpoint.Youden)
#plot(optimal.cutpoint.Youden)


#ROC(form=severe~audit_pc_sum_all, data=alcohol_datapc)
#ROC(form=severe~audit_pc_sum_p, data=alcohol_datapc)
#ROC(form=severe~audit_pc_sum_g, data=alcohol_datapc)

#optimal.cutpoint.Youden <- optimal.cutpoints(X = "audit_pc_sum_all",
 #                                            #X = "audit_pc_sum_p",
  #                                           #X = "audit_pc_sum_g",
   #                                          status = "severe", 
    #                                         tag.healthy = "0",
     #                                        methods = "Youden", 
      #                                       data = alcohol_datapc, 
       #                                      pop.prev = NULL, 
        #                                     categorical.cov = NULL, #"gender",
         #                                    control = control.cutpoints("generalized.Youden = TRUE"), 
          #                                   ci.fit = FALSE, 
           #                                  conf.level = 0.95, 
            #                                 trace = FALSE)

#summary(optimal.cutpoint.Youden)
#plot(optimal.cutpoint.Youden)

ROC(form=alcoholuse~audit_pc_sum_all, data=alcohol_datapc_all)
ROC(form=alcoholuse_patients~audit_pc_sum_p, data=alcohol_datapc_p)
ROC(form=alcoholuse_general~audit_pc_sum_g, data=alcohol_datapc_g)

optimal.cutpoint.Youden <- optimal.cutpoints(#X = "audit_pc_sum_all",
                                           #  X = "audit_pc_sum_p", 
                                             X = "audit_pc_sum_g", 
                                          #   status = "alcoholuse",
                                         #    status = "alcoholuse_patients", 
                                             status = "alcoholuse_general", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                        #     data = alcohol_datapc_all,
                                        #   data = alcohol_datapc_p,
                                           data = alcohol_datapc_g,
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints("generalized.Youden = TRUE"), 
                                             ci.fit = FALSE, 
                                             conf.level = 0.95, 
                                             trace = FALSE)

summary(optimal.cutpoint.Youden)
plot(optimal.cutpoint.Youden)


#GENDER
data_patients<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/Tz_bnipatients_data.csv",sep=',')


#