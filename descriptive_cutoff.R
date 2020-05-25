data_validation<-subset(data[data$redcap_event_name=="enrollment_arm_1",])

kessler_data1<-with(data_validation,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
data_imputed1 <- mice(kessler_data1, seed = 2222, m=10)
kessler_data<-mice::complete(data_imputed1,4)

kessler_data$Sum_data <- rowSums(kessler_data[ , 1:10]) 
kessler_data$Sum_data<-car::recode(kessler_data$Sum_data,"10:20='no';21:50='yes'")

table<-with(kessler_data,table(Sum_data))
table
prop.table(table)


phq9<-with(data_validation,data.frame(phq9_b11,phq9_b12,phq9_b13,phq9_b14,
                                      phq9_b15,phq9_b16,phq9_b17,phq9_b18,phq9_b19))
data_imputed1 <- mice(phq9, seed = 2222, m=10)
phq9<-mice::complete(data_imputed1,4)

phq9$Sum_data <- rowSums(phq9[ , 1:9]) 
phq9$Sum_data<-car::recode(phq9$Sum_data,"0:7.9='no';8:21='yes'")

table<-with(phq9,table(Sum_data))
table
prop.table(table)

audit_data<-with(data_validation,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
data_imputed1 <- mice(audit_data, seed = 2222, m=10)
audit_data<-mice::complete(data_imputed1,4)

audit_data$Sum_data <- rowSums(audit_data[ , 1:9]) 
audit_data$Sum_data<-car::recode(audit_data$Sum_data,"0:7.9='no';8:35='yes'")

table<-with(audit_data,table(Sum_data))
table
prop.table(table)


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
sf8_physical<-with(sf8_data,data.frame(sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5))
sf8_mental<-with(sf8_data,data.frame(sf8_b5,sf8_b6,sf8_b7,sf8_b8))

sf8_physical$Sum_data <- rowSums(sf8_physical[ , 1:4]) 
sf8_physical$Sum_data<-car::recode(sf8_physical$Sum_data,"00:49.9='no';50:100='yes'")
sf8_mental$Sum_data <- rowSums(sf8_mental[ , 1:4]) 
sf8_mental$Sum_data<-car::recode(sf8_mental$Sum_data,"0:49.9='no';50:100='yes'")

table<-with(sf8_physical,table(Sum_data))
table
prop.table(table)

table<-with(sf8_mental,table(Sum_data))
table
prop.table(table)

data_mhregistry$phq9_cat<-car::recode(data_mhregistry$phq9_sum,"0:7.9='no';8:21='yes'")
data_mhregistry$kessler_score_cat<-car::recode(data_mhregistry$kessler_sum,"10:20='no';21:50='yes'")
data_mhregistry$audit_cat<-car::recode(data_mhregistry$audit_sum,"0:7.9='no';8:35='yes'")
data_mhregistry$fimPC_cat<-car::recode(data_mhregistry$fim_scores_CF,"0:49.999='yes';50:100='no'")
data_mhregistry$fimMC_cat<-car::recode(data_mhregistry$fim_scores_MF,"0:49.999='yes';50:100='no'")
data_mhregistry$sf8MC_cat<-car::recode(data_mhregistry$sf8_scores_MF,"0:49.9='no';50:100='yes'")
data_mhregistry$sf8PC_cat<-car::recode(data_mhregistry$sf8_scores_CF,"0:49.9='no';50:100='yes'")
data_mhregistry$moca_cat<-car::recode(data_mhregistry$moca_sum,"0:25.9='yes';26:30='no'")
