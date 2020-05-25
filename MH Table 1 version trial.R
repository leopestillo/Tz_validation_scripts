## MH TABLE 1 and LPA

#the bigger the scores the badder the outcome, EXCEPT for FIM (high score, high functioning)
#############################
## SETTING THE ENVIRONMENT ##
#############################



#library(poLCA)
library(ggplot2)
library(devtools)
#install_github("ricardo-bion/ggradar")
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(mclust)
library(fmsb)
library(scales)
library(reshape)

####################
## IMPORTING DATA ##
####################

df<- read.csv("/Users/Loren/Desktop/Thesis/tz_TBIregistryANDmh_data.csv")
audit_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/audit_factorscores.csv")[,-1]
sf8_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/sf8_factorscores.csv")[,-1]
kessler_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/kessler_factorscores.csv")[,-1]
phq9_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/phq9_factorscores.csv")[,-1]
fim_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/FIM_30_factorscores.csv")[,-1]
moca_fscore <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/moca_factorscores.csv")[,-1]

# fim_fscore_3mos <-read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/FIM_3mos_factorscores.csv")[-1]
# moca_fscore_3mos <-read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/MOCA_3mos_factorscores.csv")[-1]
# kessler_fscore_3mos <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/kessler_3mos_factorscores.csv")[-1]
# sf8_fscore_3mos <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/sf8_3mos_factorscores.csv")[-1]
# phq9_fscore_3mos <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/phq9_3mos_factorscores.csv")[-1]
# audit_fscore_3mos <- read.csv("/Users/Loren/Desktop/Thesis/Factor Scores/audit_3mos_factorscores.csv")[-1]

#LEO DATA
df<-read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/database/Tz_MHpostTBI_data.csv",sep=',')
audit_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/audit_factorscores.csv")[,-1]
sf8_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/sf8_factorscores.csv")[,-1]
kessler_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/kessler_factorscores.csv")[,-1]
phq9_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/phq9_factorscores.csv")[,-1]
fim_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/FIM_30_factorscores.csv")[,-1]
moca_fscore <- read.csv("/Users/LeoPestillo/Google Drive/[PES] life discussions/Tz_Validations/network_mentalhealth/moca_factorscores.csv")[,-1]


#############################
## CREATING THE DATA FRAME ##
#############################
df0<-subset(df,redcap_event_name=="enrollment_arm_1")
df2 <- subset(df, redcap_event_name!="month_6_followup_arm_1")
df3<-subset(df,redcap_event_name=="month_3_followup_arm_1")

#####################
## DATA MANAGEMENT ##
#####################

 # df0$education_cat<-reshapecar::recode(df0$education,"0:7=‘Some primary’;8:13=‘Some secondary’;
 #                                           14:16=‘Some university’;89=NA")

# AGGREGATE SCORES -- BASELINE

# sf8_b1 - sf8_b8
df0$sf8_score <- df0$sf8_b1 + df0$sf8_b2 + df0$sf8_b3 + df0$sf8_b4 + 
                df0$sf8_b5 + df0$sf8_b6 + df0$sf8_b7 + df0$sf8_b8 
#SF8 - mental

# SF8 - physcial

# phq9_b11 - phq9_b19
df0$phq9_score <- df0$phq9_b11 + df0$phq9_b12 + df0$phq9_b13 + df0$phq9_b14 + df0$phq9_b15 + 
                df0$phq9_b16 + df0$phq9_b17 + df0$phq9_b18 + df0$phq9_b19 

# kes d1 - d10
df0$kessler_score <- df0$d1 + df0$d2 + df0$d3 + df0$d4 + df0$d5 + 
                df0$d6 + df0$d7 + df0$d8 + df0$d9 + df0$d10 

df0$depression_score <- df0$d1 +df0$d4 + df0$d7 +df0$d8 + df0$d9 + df0$d10
df0$anxiety_score <- df0$d2 +df0$d3 + df0$d5 +df0$d6

# ces e1 - e20
df0$ces_score <- df0$e1 + df0$e2 + df0$e3 + df0$e4 + df0$e5 + df0$e6 + df0$e7 + df0$e8 + df0$e9 + df0$e10+
                 df0$e11 + df0$e12 + df0$e13 + df0$e14 + df0$e15 + df0$e16 + df0$e17 + df0$e18 + df0$e19 + df0$e20

# f1a - f1e + f2a - f2e + f3 - f16
df0$mmse_score <- df0$f1a + df0$f1b + df0$f1c + df0$f1d + df0$f1e + df0$f2a + df0$f2b + df0$f2c + df0$f2d + df0$f2e +
   df0$f3 + df0$f4 + df0$f5 + df0$f6 + df0$f7 + df0$f8 +
   df0$f9 + df0$f10 + df0$f11 + df0$f15 + df0$f16
#f12 is a checkbox and R doesn't see it? data dictionary skips f13 and f14 


df0$moca_score <- df0$f17+
              df0$f18+
              df0$f19 +
              df0$f20+
              df0$f21+
              df0$f21b+
              df0$f22+
              df0$f23

#FIM  <- g1-g30
df0$fim_score <- df0$g1 + df0$g2 + df0$g3 +df0$g4 + df0$g5 + df0$g6 + df0$g7 + df0$g8 + df0$g9 + df0$g10 +
                df0$g11 + df0$g12 + df0$g13 + df0$g14 + df0$g15 + df0$g16 + df0$g17 + df0$g18 + df0$g19 + df0$g20 +
                df0$g21 + df0$g22 + df0$g23 + df0$g24 + df0$g25 + df0$g26 + df0$g27 + df0$g28 + df0$g29 + df0$g30
#FIM - motor
df0$MF_score <- df0$g1 + df0$g2 + df0$g3 +df0$g4 + df0$g5 + df0$g6 + df0$g7 + df0$g8 + df0$g9 + df0$g10 +
  df0$g11 + df0$g12 + df0$g13 + df0$g14 + df0$g15 + df0$g16
#FIM - cognitive
df0$CF_score <- df0$g17 + df0$g18 + df0$g19 + df0$g20 +
  df0$g21 + df0$g22 + df0$g23 + df0$g24 + df0$g25 + df0$g26 + df0$g27 + df0$g28 + df0$g29 + df0$g30

#audit h1-h14 
df0$audit_score <- df0$h1 + df0$h2 + df0$h3 + df0$h4 + df0$h5 + df0$h6 + df0$h7 + df0$h8 + df0$h9 + df0$h10 +
                  df0$h11 + df0$h12 + df0$h13 + df0$h14
df2$audit_score <- df2$h1 + df2$h2 + df2$h3 + df2$h4 + df2$h5 + df2$h6 + df2$h7 + df2$h8 + df2$h9 + df2$h10 +
  df2$h11 + df2$h12 + df2$h13 + df2$h14

df0$auditCat <- car::recode(df0$audit_score, "0:7=0; 8:100=1")
df2$auditCat <- car::recode(df2$audit_score, "0:7=0; 8:100=1")



# AGGREGATE SCORES  -  Baseline + 3MOS FOLLOW-UP

# sf8_b1 - sf8_b8
df2$sf8_score <- df2$sf8_b1 + df2$sf8_b2 + df2$sf8_b3 + df2$sf8_b4 + 
  df2$sf8_b5 + df2$sf8_b6 + df2$sf8_b7 + df2$sf8_b8 

# phq9_b11 - phq9_b19
df2$phq9_score <- df2$phq9_b11 + df2$phq9_b12 + df2$phq9_b13 + df2$phq9_b14 + df2$phq9_b15 + 
  df2$phq9_b16 + df2$phq9_b17 + df2$phq9_b18 + df2$phq9_b19 

# kes d1 - d10
df2$kessler_score <- df2$d1 + df2$d2 + df2$d3 + df2$d4 + df2$d5 + 
  df2$d6 + df2$d7 + df2$d8 + df2$d9 + df2$d10 

df2$depression_score <- df2$d1 +df2$d4 + df2$d7 +df2$d8 + df2$d9 + df2$d10
df2$anxiety_score <- df2$d2 +df2$d3 + df2$d5 +df2$d6

# ces e1 - e20
df2$ces_score <- df2$e1 + df2$e2 + df2$e3 + df2$e4 + df2$e5 + df2$e6 + df2$e7 + df2$e8 + df2$e9 + df2$e10+
  df2$e11 + df2$e12 + df2$e13 + df2$e14 + df2$e15 + df2$e16 + df2$e17 + df2$e18 + df2$e19 + df2$e20

# f1a - f1e + f2a - f2e + f3 - f16
df2$mmse_score <- df2$f1a + df2$f1b + df2$f1c + df2$f1d + df2$f1e + df2$f2a + df2$f2b + df2$f2c + df2$f2d + df2$f2e +
  df2$f3 + df2$f4 + df2$f5 + df2$f6 + df2$f7 + df2$f8 +
  df2$f9 + df2$f10 + df2$f11 + df2$f15 + df2$f16
#f12 is a checkbox and R doesn't see it? data dictionary skips f13 and f14 


df2$moca_score <- df2$f17+
  df2$f18+
  df2$f19 +
  df2$f20+
  df2$f21+
  df2$f21b+
  df2$f22+
  df2$f23

#FIM  <- g1-g30
df2$fim_score <- df2$g1 + df2$g2 + df2$g3 +df2$g4 + df2$g5 + df2$g6 + df2$g7 + df2$g8 + df2$g9 + df2$g10 +
  df2$g11 + df2$g12 + df2$g13 + df2$g14 + df2$g15 + df2$g16 + df2$g17 + df2$g18 + df2$g19 + df2$g20 +
  df2$g21 + df2$g22 + df2$g23 + df2$g24 + df2$g25 + df2$g26 + df2$g27 + df2$g28 + df2$g29 + df2$g30 +0

#FIM - motor
df2$MF_score <- df2$g1 + df2$g2 + df2$g3 +df2$g4 + df2$g5 + df2$g6 + df2$g7 + df2$g8 + df2$g9 + df2$g10 +
  df2$g11 + df2$g12 + df2$g13 + df2$g14 + df2$g15 + df2$g16
#FIM - cognitive
df2$CF_score <- df2$g17 + df2$g18 + df2$g19 + df2$g20 +
  df2$g21 + df2$g22 + df2$g23 + df2$g24 + df2$g25 + df2$g26 + df2$g27 + df2$g28 + df2$g29 + df2$g30


#audit h1-h14 
df2$audit_score <- df2$h1 + df2$h2 + df2$h3 + df2$h4 + df2$h5 + df2$h6 + df2$h7 + df2$h8 + df2$h9 + df2$h10 +
  df2$h11 + df2$h12 + df2$h13 + df2$h14
df2$audit_score <- df2$h1 + df2$h2 + df2$h3 + df2$h4 + df2$h5 + df2$h6 + df2$h7 + df2$h8 + df2$h9 + df2$h10 +
  df2$h11 + df2$h12 + df2$h13 + df2$h14
df2$auditCat <- car::recode(df2$audit_score, "0:7=0; 8:100=1")
df2$auditCat <- car::recode(df2$audit_score, "0:7=0; 8:100=1")

# ##############################################
# # TABLE 1                                   #
# #############################################
# 
# #why does age have age.x and age.y ?
# summary(df0$age.x)
# summary(df0$personal_income)
# sum(df0$female=="0", na.rm=TRUE)
# round(((sum(df0$female == "0", na.rm=TRUE) / sum(df0$female == "1" | df0$female == "0", na.rm=TRUE))*100), 2)
# df0$married_2cat<-car::recode(df0$married, "1=0; 3=1; 4=1; 5=1")
# sum(df0$married_2cat=="0", na.rm=TRUE)
# round(((sum(df0$married_2cat == "0", na.rm=TRUE) / sum(df0$married_2cat == "1" | df0$married_2cat == "0", na.rm=TRUE))*100), 2)
# df0$occupation_2cat<-car::recode(df0$occupation, "1=0; 2=0; 3=0; 4=0; 5=0; 6=0; 7=0; 89=0; 8=1")
# sum(df0$occupation=="8", na.rm=TRUE)
# round(((sum(df0$occupation_2cat == "1", na.rm=TRUE) / sum(df0$occupation_2cat == "1" | df0$occupation_2cat == "0", na.rm=TRUE))*100), 2)
# 
# summary(df0$phq_score)
# summary(df0$kes_score)
# summary(df0$audit_score)
# summary(df0$moca_score)
# summary(df0$sf8_score)
# summary(df0$fim_score)

##################################################
# LATENT PROFILE ANALYSIS                         #
#################################################
# summed score profiles
# BASELINE
MH_class <- with(df0, data.frame(#phq9_score, 
                                 sf8_score,
                                 kessler_score,
                                 #ces_score,
                                 #mmse_score,
                                 moca_score,
                                 MF_score,
                                 CF_score,
                                 audit_score))

data_imputed <- mice(MH_class, seed = 2222, m=10)
MH_class<-mice::complete(data_imputed,4)


#MH_class<-na.omit(MH_class)
fit <- mclustBIC(MH_class, G = 4)
cl<-mclustModel(MH_class,fit)

summary(cl) # display the best model
cl$parameters$pro
cl$parameters$mean
cl$bic
cl$loglik

groupCat <- vector("integer", 192)
groupCat <- vector("integer", 89)

i=1
while (i < 193) {
  v1 = cl$z[i,1:4]
 # v1 = cl$z
  a=order(-v1)[1] 
  groupCat[[i]] <- a
  i=i+1
}
df0$groupCat <- as.factor(groupCat)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(cl$parameters$pro) # Class proportions
error_post <- mean(apply(cl$z, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy


## BASELINE + FOLLOWUP
MH_class <- with(df2, data.frame(
  sf8_score,
  kessler_score,
  moca_score,
  MF_score,
  CF_score,
  audit_score))
data_imputed <- mice(MH_class, seed = 2222, m=10)
MH_class<-mice::complete(data_imputed,4)
#MH_class<-na.omit(MH_class)
fit <- mclustBIC(MH_class, G = 5)
cl<-mclustModel(MH_class,fit)

summary(cl) # display the best model
cl$parameters$pro
cl$parameters$mean
cl$bic
cl$loglik

groupCat <- vector("integer", 268)
i=1
while (i < 269) {
  v1 = cl$z[i,1:5]
  a=order(-v1)[1] 
  groupCat[[i]] <- a
  i=i+1
}
df2$groupCat <- as.factor(groupCat)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(cl$P) # Class proportions
error_post <- mean(apply(cl$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# > cl$parameters$pro
# [1] 0.07509907 0.07752630 0.14962414 0.22691592 0.02251391 0.02697828 0.11307676
# [8] 0.10140107 0.20686455
# > cl$parameters$mean
# [,1]       [,2]       [,3]         [,4]        [,5]        [,6]
# sf8_score       3.721229   9.226071   1.973800 5.883347e-12   0.6002295   0.5009009
# kessler_score  11.691517  13.103126  10.182969 1.036140e+01  10.7999260  10.0000000
# moca_score     11.349085  10.930500   8.884535 7.879612e+00  15.2000765   7.8354407
# fim_score     194.187873 207.618798 168.210468 1.947170e+02 208.7995560 208.1670722
# audit_score     9.722874   3.346118   2.409937 1.012767e+01   0.4001480   2.4952831
# [,7]         [,8]        [,9]
# sf8_score       9.266132 1.485047e-05   0.0000000
# kessler_score  17.294955 1.000000e+01  10.0867626
# moca_score      8.718972 7.047185e+00  11.0226186
# fim_score     172.461676 1.939588e+02 210.0000000
# audit_score     3.839429 6.238805e-01   0.9952582
# > cl$bic
# [1] -5861.243
# > cl$loglik
# [1] -2509.212



# MH_class <- with(df2, data.frame(#phq9_score, 
#   sf8_score,
#   kessler_score,
#   #ces_fscore,
#   #mmse_fscore,
#   moca_score,
#   fim_score,
#   audit_fscore))
# MH_class<-na.omit(MH_class)
# fit <- mclustBIC(MH_class)
# cl<-mclustModel(MH_class,fit)
# 
# summary(cl) # display the best model
# cl$parameters$pro
# cl$parameters$mean
# cl$bic
# cl$loglik

#factor scores WITH AUDIT -- BASELINE
MH_class <- with(df0, data.frame(phq9_fscore, 
                                 sf8_fscore,
                                 kessler_fscore,
                                 #ces_fscore,
                                 #mmse_fscore,
                                 moca_fscore,
                                 fim_fscore,
                                 audit_fscore))
MH_class<-na.omit(MH_class)
fit <- mclustBIC(MH_class)
#plot(fit)

## Best Fit - 4 class
cl<-mclustModel(MH_class,fit)

summary(cl) # display the best model
cl$parameters$pro
cl$parameters$mean
cl$bic
cl$loglik

# > cl$parameters$pro
# [1] 0.3052633 0.1785130 0.2394887 0.1667715 0.1099636
# > cl$parameters$mean
# [,1]        [,2]        [,3]         [,4]        [,5]
# phq9_fscore   0.661347674 -0.08412778 -0.08412778 -0.084127783 -0.08412778
# sf8_fscore    0.474548670 -0.09999149  0.15252124 -0.009307727 -0.17854949
# Depression    0.675560033 -0.11859874 -0.11859874  0.035639852 -0.11859874
# Anxiety       0.831808174 -0.14332966 -0.14332966  0.061282701 -0.14332966
# moca_fscore  -0.006253985  0.07579551  0.17853088 -0.546440241  0.27126032
# MF           -0.244960860  0.46602547 -0.53224647 -0.334861060  0.41284472
# CF           -0.403591306  0.51382748 -0.33775008 -0.784788387  0.41557024
# audit_fscore  0.145823896 -0.47632374 -0.15590048  0.111654113  0.91307696

# ## WITH AUDIT -- 3 MONTH FOLLOWUP
# MH_class2 <- with(df2, data.frame(phq9_fscore_3mos, 
#                                  sf8_fscore_3mos,
#                                  kessler_fscore_3mos,
#                                  moca_fscore_3mos,
#                                  fim_fscore_3mos,
#                                  audit_fscore_3mos))
# MH_class2<-na.omit(MH_class2)
# fit <- mclustBIC(MH_class2)
#plot(fit)

## Best Fit 
cl2<-mclustModel(MH_class2,fit)

summary(cl2) # display the best model
cl2$parameters$pro
cl2$parameters$mean
cl2$bic
cl2$loglik

# > cl$parameters$pro
# [1] 0.1000032 0.1577711 0.6682840 0.0739417
# > cl$parameters$mean
#               [,1]        [,2]         [,3]       [,4]
# phq9_fscore  1.3143224 -0.08412778 -0.076668821 1.03475514
# sf8_fscore   0.9546842  0.27397212 -0.038954321 0.40152720
# Depression   0.6769769  0.70709603 -0.118598745 0.67012024
# Anxiety      0.8440746  0.88300369 -0.143329664 0.81861665
# moca_fscore -0.2024971  0.04032614  0.008624111 0.01623324
# MF          -0.7946299 -0.24783690 -0.057522250 0.37202116
# CF          -0.9312451 -0.43954407 -0.068831501 0.14777922


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## The values of the output of parameters$mean are Z scores
## Positive and negative Z scores reveal the number of standard deviations 
## that the score is either above or below the mean
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


##############################################################
## CLASSIFY INTO GROUPS 
##############################################################
## BASELINE
groupCat <- vector("integer", 192)

i=1
while (i < 191) {
  v1 = cl$z[i,1:5]
  a=order(-v1)[1] 
  groupCat[[i]] <- a
  i=i+1
}
groupCat[191]=3
groupCat[192]=5

df0$groupCat <- as.factor(groupCat)
#df0$groupCat <- car::recode(df0$groupCat, "1='BDepressive';2='CLowFunc';3='AHealthy';4='DMentallyIll'")
#df0$groupCat <- car::recode(df0$groupCat, "1='BDepressive';2='AHealthy';3='CCognitiveIssues';4='DPoorQOL'; 5='EMentallyIll'")
#head(df0$groupCat)


##
## Radial Graph of Profiles 
##

radialplot_data<-as.data.frame(pnorm(cl$parameters$mean))

radialplot_data <- as.data.frame(rescale(cl$parameters$mean, to = c(0,1), from = range(cl$parameters$mean, na.rm = T, finite = T)))
radialplot_data<-as.data.frame(t(radialplot_data))
# I used pnorm on the Z scores to get rid of negative values and give probabilities
# is that right??

radialplot_data
rownames(radialplot_data)<-c("group 1",
                             "group 2",
                             "group 3",
                             "group 4",
                             "group 5")

colnames(radialplot_data)<-c("PHQ9","SF8","Depression"
                             ,"Anxiety", "Moca", "MF", "CF", "AUDIT")

radialplot_data %>%
  rownames_to_column( var = "group" ) -> radialplot_data2

ggradar(radialplot_data2,
        font.radar="sans",
        grid.label.size=7,
        axis.label.size=5,
        axis.labels=colnames(radialplot_data),
        legend.text.size=10) 


###################################
## gg spider plot
##################################

# FOR AGGREGATE SCORES

df0$phq9_rscore <- rescale(df0$phq9_score, to = c(0, 100), from = range(df0$phq9_score, na.rm = TRUE, finite = TRUE))
df0$sf8_rscore <- rescale(df0$sf8_score, to = c(0, 100), from = range(df0$sf8_score, na.rm = TRUE, finite = TRUE))
df0$anxiety_rscore <- rescale(df0$anxiety_score, to = c(0, 100), from = range(df0$anxiety_score, na.rm = TRUE, finite = TRUE))
df0$depression_rscore <- rescale(df0$depression_score, to = c(0, 100), from = range(df0$depression_score, na.rm = TRUE, finite = TRUE))
df0$moca_rscore <- rescale(df0$moca_score, to = c(0, 100), from = range(df0$moca_score, na.rm = TRUE, finite = TRUE))
df0$MF_rscore <- rescale(df0$MF_score, to = c(0, 100), from = range(df0$MF_score, na.rm = TRUE, finite = TRUE))
df0$CF_rscore <- rescale(df0$CF_score, to = c(0, 100), from = range(df0$CF_score, na.rm = TRUE, finite = TRUE))
df0$audit_rscore <- rescale(df0$audit_score, to = c(0, 100), from = range(df0$audit_score, na.rm = TRUE, finite = TRUE))

# added the mean of each aggregate score  for each groupCat after 100, 1
# summary(df0$audit_rscore[df0$groupCat=="1"])
df_radial <- NULL
df_radial$phq9 <- c(100,1, 7.637, 0.8913, 0.6536, 0.1838, 0.2674)
df_radial<-as.data.frame(df_radial)

df_radial$sf8 <- c(100,1, 14.79, 1.754, 6.979, 4.77, 1.754)
df_radial$depression <- c(100, 1, 8.19, 1.136, 0.7246, 1.302, 0)
df_radial$anxiety <- c(100,1, 6.897, 0, 0.2717, 0.9766, 0)
df_radial$moca <- c(100,1, 56.36, 50, 61.92, 41.88, 59.69)
df_radial$MF <- c(100, 1, 83.807, 90.186, 73.458, 70.49, 99.03)
df_radial$CF <- c(100, 1, 91.94, 92.01, 93.89, 78.87, 96.92)
df_radial$audit <- c(100,1, 16.491, 7.792, 12.733, 11.786, 23.25)

names(df_radial) <- c("PHQ-9", "SF-8", "Depression", "Anxiety", "MoCA", "Motor FIM", "Cognitive FIM", "AUDIT")

# FOR Z SCORES
z_rscores <- as.data.frame(scales::rescale(cl$parameters$mean, to = c(0,100), from = range(cl$parameters$mean, na.rm = T, finite = T)))
z_rscores<-as.data.frame(t(z_rscores))
z_rdata <- as.data.frame(z_rscores)
z_rdata[1,]=100 
z_rdata[2,]=1
z_rdata[3,]=z_rscores[1,]
z_rdata[4,]=z_rscores[2,]
z_rdata[5,]=z_rscores[3,]
z_rdata[6,]=z_rscores[4,]
z_rdata[7,]=z_rscores[5,]

#setting the border color for each group
colors_border = c("#660000",
                  "#0080FF",
                  "#33FF99",
                  "#808080",
                  "#CC99FF")
#setting the shade in color for each group. Online there is a converter from #000000 format to rgb
colors_in = c(rgb(0, 0, 102, max = 255, alpha = 125),
              rgb(0, 128, 255, max = 255, alpha = 125),
              rgb(51, 225, 153, max = 255, alpha = 150),
              rgb(128, 128, 128, max = 255, alpha = 100),
              rgb(204, 153, 255, max = 255, alpha = 100))

par(mar = c(4, 0, 0, 0))
radarchart(z_rdata, axistype = 1,
           pcol=colors_border, plwd = 2, plty = 1,
           pfcol=colors_in,
           cglcol = "grey", cglty=1, axislabcol = "grey",
           caxislabels = seq(0, 100, 25), cglwd = .7)
legend("bottom",
       inset = c(0,-.18),
       #legend = row.names(classes_prob3[-c(1,2),]),
       legend = c("Class 1",
                  "Class 2",
                  "Class 3",
                  "Class 4",
                  "Class 5"),
       bty="o",
       box.lwd = .2,
       x.intersp=0.5,
       text.width = (.3),
       pch=20,
       col=colors_border,
       text.col = "black",
       cex = .9,
       pt.cex = 2,
       horiz = TRUE,
       xpd = TRUE)


#################################
# COMPARE AUDIT SCORES
##################################

## FACTOR CATEGORICAL LOGISTIC REGRESSION -- BASELINE

comparison <- glm(auditCat ~ 
                groupCat,
              data = df0,family = binomial(link="logit"))
#Here I use auditCat which is dichotomized , values 8 and above being 1 for heavy drinker
summary(comparison)
round(exp(cbind(Odds=coef(comparison),confint(comparison,level=0.95))), 3)
dope<-as.data.frame(summary(comparison)$coefficients)

tmp<-data.frame(cbind(exp(coef(comparison)), exp(confint(comparison))))
odds<-tmp[-1,]
names(odds)<-c("OR", "lower", "upper")
rownames(odds) <- c("Depressive",
                    "Low Functionality",
                    "Mentally Ill"
)
odds$vars<-row.names(odds)
dope2<-dope[2:4,]
odds$pval<-dope2$`Pr(>|z|)`
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y=OR , x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), color=ifelse(odds$pval <.05, "orange", "grey"), size=ifelse (odds$pval <.05, 1.2, .9), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  labs(x = "Profile Classification", y = "Odds", title = "Odds for Profile Association with Harmful Alcohol Use") +
  theme_bw()

## NUMERIC CONTINUOUS LINEAR REGRESSION -- BASELINE
df0$auditF <- audit_fscore
comparison <- glm(auditF ~ 
                    groupCat,
                  data = df0,family = gaussian(link="identity"))

# is identity the right link for gaussian family? if not it doesn't like values outside of 0 and 1

summary(comparison)
round(exp(cbind(Odds=coef(comparison),confint(comparison,level=0.95))), 3)
dope<-as.data.frame(summary(comparison)$coefficients)

tmp<-data.frame(cbind(exp(coef(comparison)), exp(confint(comparison))))
odds<-tmp[-1,]
names(odds)<-c("OR", "lower", "upper")
rownames(odds) <- c("Depressive","Low Functionality","Mentally Ill")
odds$vars<-row.names(odds)
dope2<-dope[2:4,]
odds$pval<-dope2$`Pr(>|z|)`
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y=OR , x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), color="grey", size=.9, width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  labs(x = "Profile Classification", y = "Odds", title = "Odds for Profile Association with Harmful Alcohol Use") +
  theme_bw()


#geom_errorbar(aes(ymin=lower, ymax=upper), color="grey", size=0.9, width=.2)


## 3 MONTH FOLLOW UP
groupCat2 <- vector("integer", 79)

i=1
while (i < 79) {
  v2 = cl2$z[i,1:4]
  a2=order(-v1)[1] 
  groupCat2[[i]] <- a2
  i=i+1
}

df2$groupCat2 <- as.factor(groupCat2)
df2$groupCat2 <- car::recode(df2$groupCat2, "1='BDepressive';2='CLowFunc';3='AHealthy';4='DMentallyIll'")
#df0$groupCat <- car::recode(df0$groupCat, "1='BDepressive';2='AHealthy';3='CCognitiveIssues';4='DPoorQOL'; 5='EMentallyIll'")
head(df2$groupCat2)

## FACTOR CATEGORICAL LOGISTIC REGRESSION -- 3 MONTH FOLLOWUP

comparison <- glm(auditCat ~ 
                    groupCat2,
                  data = df2,family = binomial(link="logit"))
# Warning messages:glm.fit: fitted probabilities numerically 0 or 1 occurred
summary(comparison)
round(exp(cbind(Odds=coef(comparison),confint(comparison,level=0.95))), 3)

## NUMERIC CONTINUOUS LINEAR REGRESSION -- 3 MONTH FOLLOWUP
df2$auditF <- audit_fscore
#audit_fscore has 192 rows -- I think that it is only for baseline...
comparison2 <- glm(auditF ~ 
                    groupCat2,
                  data = df2,family = gaussian(link="identity"))


##DESCRIPTIVE STATS

Depressivedata <- subset(df0, groupCat=="BDepressive")
LowFuncdata <- subset(df0, groupCat=="CLowFunc")
Healthydata <- subset(df0, groupCat=="AHealthy")
MentallyIlldata <- subset(df0, groupCat=="DMentallyIll")
mean(Depressivedata$audit_score, na.rm = T)
sd(Depressivedata$audit_score, na.rm = T)
mean(LowFuncdata$audit_score, na.rm = T)
sd(LowFuncdata$audit_score, na.rm = T)
mean(Healthydata$audit_score, na.rm = T)
sd(Healthydata$audit_score, na.rm = T)
mean(MentallyIlldata$audit_score, na.rm = T)
sd(MentallyIlldata$audit_score, na.rm = T)

audit_fscore <- read.csv("/Users/Loren/Desktop/audit_factorscores.csv")
audit_fscore$groupCat <- df0$groupCat
Depressivedata <- subset(audit_fscore, groupCat=="BDepressive")
LowFuncdata <- subset(audit_fscore, groupCat=="CLowFunc")
Healthydata <- subset(audit_fscore, groupCat=="AHealthy")
MentallyIlldata <- subset(audit_fscore, groupCat=="DMentallyIll")
mean(Depressivedata$Audit, na.rm = T)
sd(Depressivedata$Audit, na.rm = T)
mean(LowFuncdata$Audit, na.rm = T)
sd(LowFuncdata$Audit, na.rm = T)
mean(Healthydata$Audit, na.rm = T)
sd(Healthydata$Audit, na.rm = T)
mean(MentallyIlldata$Audit, na.rm = T)
sd(MentallyIlldata$Audit, na.rm = T)
