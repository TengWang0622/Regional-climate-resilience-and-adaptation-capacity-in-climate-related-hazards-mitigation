##########################################################################################################
#
#                                    P   R   O   J   E   C   T
#                                                of
#   Assessing Regional Climate Resilience and Adaption Capacity to Climate-related Hazards in China: 
#                  Cause-specific Hospitalizations and Related burdens
#
###########################################################################################################

# Developed by Teng Wang, Hanxu Shi, Zhenyu Zhang

# Contact: wang.teng19@alumni.imperial.ac.uk
#          shx@bjmu.edu.cn

# Version - 20240325

# Description: Main Script

############################################
#             Preparation
############################################

library(readxl)
library(tidyverse)
#library(dlnm)
#library(splines)
#library(survival)
#library(mvmeta)
library(dplyr)
library(magrittr)
library(Matrix)

library(foreach)
library(doParallel)
library(progress)

library(lme4)
#library(lmerTest)

library(ape)

library(MatchIt)
library(WeightIt)
library(MASS)
library(cli)
library(readxl)
library(openxlsx)

library(networkD3)
library(dplyr)
library(htmlwidgets)

# Variable containers


InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','PartReim_Over_All','Self_After_Reim','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
DiseaseALL=c('Malnutrition','Infectious','Injuries','Respiratory','Mental','Neoplasms','Endocrine','Blood','Neurology','CVD','Genitourinary')
EventALL=c('Storm', 'Flood', 'Cyclone', 'WinterStorm', 'Drought', 'Heatwave', 'Sand')

##############################################################################
#                           Extract the key info  
##############################################################################

# Burden_Eco, Burden_Ecomin, Burden_Ecomax             -       Total Economic burden

# Eco_paid_by_NHSA, Eco_paid_by_NHSAmin, Eco_paid_by_NHSAmax   NHSA afforded burden, total, CNY
# NHSA_USD_AVE, NHSA_USDmin_AVE, NHSA_USDmax_AVE               NHSA afforded burden, annual, USD
# NHSA_Payment                                                 NHSA afforded burden and the proportion of climate hazard induced additional burdens on NHSA

# PAF_China                                                    Overall PAF
# PAF_Event                                                    Event specific PAF
# PAF_Disease                                                  Disease specific PAF

# Prop_Event_Eco                                               Event specific burden and contribution
# Prop_Disease_Eco                                             Disease specific burden and contribution
# Prop_Insurance_Eco                                           Insurance specific burden and contribution
# Prop_Age_Eco                                                 Age specific burden and contribution

# Prop_Event_DALYs                                             Event specific burden and contribution
# Prop_Disease_DALYs                                           Disease specific burden and contribution
# Prop_Age_DALYs                                               Age specific burden and contribution


#====================== Economic burden ==================================
# Load the prepared Burden dataset (Prepared through script - Resilience_Visualization.R)
Sankey_Eco_Data=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_Data.xlsx")


# Pre-setting variables
InflationRate=5.88*0.01
PPP=1/4.208 # purchasing power parties when transfer CNY to USD
Ratio=PPP*(1+InflationRate)
YearLength=8

# ---------------------------------- Event ---------------------------------- 
Prop_Event_Eco=aggregate(Sankey_Eco_Data[, c("Eco","Ecomin","Ecomax")], by = Sankey_Eco_Data[, c("Event")], FUN = sum)

Burden_Eco=sum(Prop_Event_Eco$Eco)
Burden_Ecomin=sum(Prop_Event_Eco$Ecomin)
Burden_Ecomax=sum(Prop_Event_Eco$Ecomax)

Burden_EcoUSD_AVE=Burden_Eco

Prop_Event_Eco$Prop=round(Prop_Event_Eco$Eco/Burden_Eco*100,2)
Prop_Event_Eco$Propmin=round(Prop_Event_Eco$Ecomin/Burden_Eco*100,2)
Prop_Event_Eco$Propmax=round(Prop_Event_Eco$Ecomax/Burden_Eco*100,2)

Prop_Event_Eco$EcoUSD_AVE=Prop_Event_Eco$Eco*Ratio
Prop_Event_Eco$EcominUSD_AVE=Prop_Event_Eco$Ecomin*Ratio
Prop_Event_Eco$EcomaxUSD_AVE=Prop_Event_Eco$Ecomax*Ratio

write.xlsx(Prop_Event_Eco, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Event_Eco.xlsx', rowNames = FALSE)


# ---------------------------------- Disease ---------------------------------- 
Prop_Disease_Eco=aggregate(Sankey_Eco_Data[, c("Eco","Ecomin","Ecomax")], by = Sankey_Eco_Data[, c("Disease")], FUN = sum)

Prop_Disease_Eco$Prop=round(Prop_Disease_Eco$Eco/Burden_Eco*100,2)
Prop_Disease_Eco$Propmin=round(Prop_Disease_Eco$Ecomin/Burden_Eco*100,2)
Prop_Disease_Eco$Propmax=round(Prop_Disease_Eco$Ecomax/Burden_Eco*100,2)

Prop_Disease_Eco$EcoUSD_AVE=Prop_Disease_Eco$Eco*Ratio
Prop_Disease_Eco$EcominUSD_AVE=Prop_Disease_Eco$Ecomin*Ratio
Prop_Disease_Eco$EcomaxUSD_AVE=Prop_Disease_Eco$Ecomax*Ratio

write.xlsx(Prop_Disease_Eco, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Disease_Eco.xlsx', rowNames = FALSE)


# ---------------------------------- Insurance ---------------------------------- 
Prop_Insurance_Eco=aggregate(Sankey_Eco_Data[, c("Eco","Ecomin","Ecomax")], by = Sankey_Eco_Data[, c("Insurance")], FUN = sum)

Prop_Insurance_Eco$Prop=round(Prop_Insurance_Eco$Eco/Burden_Eco*100,2)
Prop_Insurance_Eco$Propmin=round(Prop_Insurance_Eco$Ecomin/Burden_Eco*100,2)
Prop_Insurance_Eco$Propmax=round(Prop_Insurance_Eco$Ecomax/Burden_Eco*100,2)

Prop_Insurance_Eco$EcoUSD_AVE=Prop_Insurance_Eco$Eco*Ratio
Prop_Insurance_Eco$EcominUSD_AVE=Prop_Insurance_Eco$Ecomin*Ratio
Prop_Insurance_Eco$EcomaxUSD_AVE=Prop_Insurance_Eco$Ecomax*Ratio

write.xlsx(Prop_Insurance_Eco, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Insurance_Eco.xlsx', rowNames = FALSE)

# National Healthcare Security Administration, NHSA
Eco_paid_by_NHSA=sum(Prop_Insurance_Eco[which(Prop_Insurance_Eco$Insurance %in% c('Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins5')),"Eco"])
Eco_paid_by_NHSAmin=sum(Prop_Insurance_Eco[which(Prop_Insurance_Eco$Insurance %in% c('Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins5')),"Ecomin"])
Eco_paid_by_NHSAmax=sum(Prop_Insurance_Eco[which(Prop_Insurance_Eco$Insurance %in% c('Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins5')),"Ecomax"])

NHSA_USD_AVE=Eco_paid_by_NHSA*Ratio
NHSA_USDmin_AVE=Eco_paid_by_NHSAmin*Ratio
NHSA_USDmax_AVE=Eco_paid_by_NHSAmax*Ratio

NHSA_Ins=data.frame('Year'=c('2016','2017','2018','2019','2020','2021','2022','2023'),'Ins'=100000000*c(8088,14422,17822,20854,21032,24043.1,24597.2,28140.3))
NHSA_InsALL=sum(NHSA_Ins$Ins)/YearLength

NHSA_Payment=data.frame("Item"=c('Eco','Ecomin','Ecomax','USD_AVE','USDmin_AVE','USDmax_AVE','NHSA_Prop','NHSA_Propmin','NHSA_Propmax'),
                        "Val"=c(Eco_paid_by_NHSA,Eco_paid_by_NHSAmin,Eco_paid_by_NHSAmax,NHSA_USD_AVE,NHSA_USDmin_AVE,NHSA_USDmax_AVE,
                                round(Eco_paid_by_NHSA/NHSA_InsALL*100,2),round(Eco_paid_by_NHSAmin/NHSA_InsALL*100,2),round(Eco_paid_by_NHSAmax/NHSA_InsALL*100,2)))

print(paste('The AF of climate hazard on NHSA expenditure = ',round(Eco_paid_by_NHSA/NHSA_InsALL*100,2),'% [',round(Eco_paid_by_NHSAmin/NHSA_InsALL*100,2),'%, ',round(Eco_paid_by_NHSAmax/NHSA_InsALL*100,2),'%]',sep=""))

write.xlsx(NHSA_Payment, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/NHSA_Payment.xlsx', rowNames = FALSE)


# ---------------------------------- Age ---------------------------------- 
Prop_Age_Eco=aggregate(Sankey_Eco_Data[, c("Eco","Ecomin","Ecomax")], by = Sankey_Eco_Data[, c("AgeGroup")], FUN = sum)

Prop_Age_Eco$Prop=round(Prop_Age_Eco$Eco/Burden_Eco*100,2)
Prop_Age_Eco$Propmin=round(Prop_Age_Eco$Ecomin/Burden_Eco*100,2)
Prop_Age_Eco$Propmax=round(Prop_Age_Eco$Ecomax/Burden_Eco*100,2)

Prop_Age_Eco$EcoUSD_AVE=Prop_Age_Eco$Eco*Ratio
Prop_Age_Eco$EcominUSD_AVE=Prop_Age_Eco$Ecomin*Ratio
Prop_Age_Eco$EcomaxUSD_AVE=Prop_Age_Eco$Ecomax*Ratio

# Population
Prop_Age=data.frame("AgeGroup"=c("Age18","Age19_44","Age45_64","Age65"),"Pop"=c(Pop18,Pop19_44,Pop45_64,Pop65))
Prop_Age$Prop=round(Prop_Age$Pop/sum(Prop_Age$Pop)*100,2)

Prop_Age_Eco$Population=Prop_Age$Pop
Prop_Age_Eco$Population_Prop=Prop_Age$Prop

Prop_Age_Eco$EcoUSD_AVEpop=Prop_Age_Eco$EcoUSD_AVE/Prop_Age_Eco$Population
Prop_Age_Eco$EcominUSD_AVEpop=Prop_Age_Eco$EcominUSD_AVE/Prop_Age_Eco$Population
Prop_Age_Eco$EcomaxUSD_AVEpop=Prop_Age_Eco$EcomaxUSD_AVE/Prop_Age_Eco$Population

write.xlsx(Prop_Age_Eco, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Age_Eco.xlsx', rowNames = FALSE)




#====================== Disease burden ==================================
# Load the prepared Burden dataset (Prepared through script - Resilience_Visualization.R)
Sankey_DALYs_Data=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_DALYs_Data.xlsx")

# ------------------------ Event ------------------------ 
Prop_Event_DALYs=aggregate(Sankey_DALYs_Data[, c("DALYs","DALYsmin","DALYsmax")], by = Sankey_DALYs_Data[, c("Event")], FUN = sum)

Burden_DALYs=sum(Prop_Event_DALYs$DALYs)
Burden_DALYsmin=sum(Prop_Event_DALYs$DALYsmin)
Burden_DALYsmax=sum(Prop_Event_DALYs$DALYsmax)

Prop_Event_DALYs$Prop=round(Prop_Event_DALYs$DALYs/Burden_DALYs*100,2)

Prop_Event_DALYs$DALYs_AVE=Prop_Event_DALYs$DALYs
Prop_Event_DALYs$DALYsmin_AVE=Prop_Event_DALYs$DALYsmin
Prop_Event_DALYs$DALYsmax_AVE=Prop_Event_DALYs$DALYsmax

write.xlsx(Prop_Event_DALYs, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Event_DALYs.xlsx', rowNames = FALSE)

# ------------------------ Disease ------------------------ 
Prop_Disease_DALYs=aggregate(Sankey_DALYs_Data[, c("DALYs","DALYsmin","DALYsmax")], by = Sankey_DALYs_Data[, c("Disease")], FUN = sum)
Prop_Disease_DALYs$Prop=round(Prop_Disease_DALYs$DALYs/Burden_DALYs*100,2)

write.xlsx(Prop_Disease_DALYs, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Disease_DALYs.xlsx', rowNames = FALSE)

# ------------------------ Age ------------------------ 
Prop_Age_DALYs=aggregate(Sankey_DALYs_Data[, c("DALYs","DALYsmin","DALYsmax")], by = Sankey_DALYs_Data[, c("AgeGroup")], FUN = sum)
Prop_Age_DALYs$Prop=round(Prop_Age_DALYs$DALYs/Burden_DALYs*100,2)

Prop_Age=data.frame("AgeGroup"=c("Age18","Age19_44","Age45_64","Age65"),"Pop"=c(Pop18,Pop19_44,Pop45_64,Pop65))
Prop_Age$Prop=round(Prop_Age$Pop/sum(Prop_Age$Pop)*100,2)

Prop_Age_DALYs$Population=Prop_Age$Pop
Prop_Age_DALYs$PopulationProp=Prop_Age$Prop

Prop_Age_DALYs$DALYs_AVEpop=Prop_Age_DALYs$DALYs/Prop_Age_DALYs$Population
Prop_Age_DALYs$DALYsmin_AVEpop=Prop_Age_DALYs$DALYsmin/Prop_Age_DALYs$Population
Prop_Age_DALYs$DALYsmax_AVEpop=Prop_Age_DALYs$DALYsmax/Prop_Age_DALYs$Population

write.xlsx(Prop_Age_DALYs, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Prop_Age_DALYs.xlsx', rowNames = FALSE)


# ======================== PAF =======================

# Compute hazard-specific PAF
PAF_Event=data.frame()
for(k in 1:length(EventALL)){
  Event_type=EventALL[k]
  eval(parse(text = paste('PAF_',Event_type,'=sum(Summary_China_Event_ALL$PAF_',Event_type,'[Summary_China_Event_ALL$Days_',Event_type,' !=0]*Summary_China_Event_ALL$Pop[Summary_China_Event_ALL$Days_',Event_type,' !=0])/sum(Summary_China_Event_ALL$Pop[Summary_China_Event_ALL$Days_',Event_type,' !=0])',sep="")))
  eval(parse(text = paste('Temp_PAF_Event=cbind(Event_type,"PAF"=PAF_',Event_type,')',sep="")))
  PAF_Event=rbind(PAF_Event,Temp_PAF_Event)
  #eval(parse(text = paste('print(PAF_',Event_type,')',sep="")))
}
write.xlsx(PAF_Event, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/PAF_Event.xlsx', rowNames = FALSE)


# Compute disease-specific PAF
PAF_Disease=data.frame()
for(k in 1:length(DiseaseALL)){
  Disease=DiseaseALL[k]
  eval(parse(text = paste('PAF_',Disease,'=sum(Summary_China_Event_ALL$PAF_',Disease,'*Summary_China_Event_ALL$Pop)/sum(Summary_China_Event_ALL$Pop)',sep="")))
  eval(parse(text = paste('Temp_PAF_Disease=cbind(Disease,"PAF"=PAF_',Disease,')',sep="")))
  PAF_Disease=rbind(PAF_Disease,Temp_PAF_Disease)
  #eval(parse(text = paste('print(PAF_',Disease,')',sep="")))
}
write.xlsx(PAF_Disease, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/PAF_Disease.xlsx', rowNames = FALSE)


# ------------------------ Print -----------------------------------------
print(paste('The overall PAF in China = ',PAF_China,sep=""))
print(paste('The total yearly averaged DALYs in China = ',sum(Summary_China_Event_ALL$DALYs),' [',sum(Summary_China_Event_ALL$DALYsmin),', ',sum(Summary_China_Event_ALL$DALYsmax),']',sep=""))
print(paste('The total yearly averaged Eco in China (USD)= ',sum(Summary_China_Event_ALL$Eco)*Ratio,' [',sum(Summary_China_Event_ALL$Ecomin)*Ratio,', ',sum(Summary_China_Event_ALL$Ecomax)*Ratio,']',sep=""))

print(paste('The climate hazard induced additional burden taken by NHSA = ',round(Eco_paid_by_NHSA/NHSA_InsALL*100,2),'% [',round(Eco_paid_by_NHSAmin/NHSA_InsALL*100,2),'%, ',round(Eco_paid_by_NHSAmax/NHSA_InsALL*100,2),'%]',sep=""))

print(PAF_Event)
print(PAF_Disease)

print(Prop_Event_Eco)
print(Prop_Disease_Eco)
print(Prop_Insurance_Eco)
print(Prop_Age_Eco)

print(Prop_Event_DALYs)
print(Prop_Disease_DALYs)
print(Prop_Age_DALYs)

# ------------------------------------------------------------------------

