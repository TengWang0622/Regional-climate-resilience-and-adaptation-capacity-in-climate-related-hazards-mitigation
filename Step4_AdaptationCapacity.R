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

InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','PartReim_Over_All','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
DiseaseALL=c('Malnutrition','Infectious','Injuries','Respiratory','Mental','Neoplasms','Endocrine','Blood','Neurology','CVD','Genitourinary')
EventALL=c('Storm', 'Flood', 'Cyclone', 'WinterStorm', 'Drought', 'Heatwave', 'Sand')

###########################################################################
#
#                        Adaptation capacity
#
###########################################################################

# NOTE: key variables
# AdaptationCapacity - contains the analysed adaptation capacity
# RR_Adpt_Obs        - the risk form at specified AAdpt_Obs= Warning, Enduring, Post, After, RTH, 

# Loading files

# Risk # Saved in Adaptation folder!
RR_Enduring=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_Enduring.xlsx")
RR_Post=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_Post.xlsx")
RR_After=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_After.xlsx")
RR_RTH=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_RTH.xlsx")


IncidenceALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Updated.xlsx")

# Obs windows
Summary_Days_Event=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Adaptation/Summary_Days_Event.xlsx")

# Select the windows
Adpt_ObsALL=c('Enduring','Post','After','RTH')

########################################################################################
#                    Adaptation Capacity: All Resilience Results 
########################################################################################

# Copy and clean the risk form
Adpt_Form=RR_Post
Adpt_Capacity=RR_Post
AdaptationCapacity=data.frame()

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  eval(parse(text = paste('Adpt_Form$',Event_type,'=0',sep="")))
  eval(parse(text = paste('Adpt_Capacity$',Event_type,'=0',sep="")))
}

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  for(j in 1:length(Adpt_ObsALL)){
    Adpt_Obs=Adpt_ObsALL[j]
    eval(parse(text = paste('Adpt_Form$',Event_type,'=Adpt_Form$',Event_type,'+abs(RR_',Adpt_Obs,'$',Event_type,'*as.numeric(Summary_Days_Event[which(Summary_Days_Event$Event=="',Event_type,'"),"',Adpt_Obs,'"]))',sep="")))
  }
}

for(i in 1:length(DiseaseALL)){
  Disease=DiseaseALL[i]
  
  SubGroup=Adpt_Form[which(Adpt_Form$Disease==Disease),]
  SubCapacity=Adpt_Capacity[which(Adpt_Capacity$Disease==Disease),]
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    
    eval(parse(text = paste('RiskRef=SubGroup[which(SubGroup$Resilience==3),"',Event_type,'"]',sep="")))
    eval(parse(text = paste('SubCapacity$',Event_type,'=(as.numeric(RiskRef)-SubGroup$',Event_type,')/as.numeric(RiskRef)',sep="")))
    
  }
  AdaptationCapacity=rbind(AdaptationCapacity,SubCapacity)
  
}

AdaptationCapacity$SeqNo=1:nrow(AdaptationCapacity)

AdaptationCapacity=merge(AdaptationCapacity, IncidenceALL[,c('Disease','Overall')], by = "Disease", all.x = TRUE)
colnames(AdaptationCapacity)[which(names(AdaptationCapacity) == "Overall")]="IncidenceRate"

AdaptationCapacity=AdaptationCapacity[order(AdaptationCapacity$SeqNo),]

# Save the Adaptation Capacity form
eval(parse(text = paste('write.xlsx(AdaptationCapacity, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/AdaptationCapacity.xlsx")',sep="")))



########################################################################################
#                    Overall Adaptation Capacity Estimation 
########################################################################################

# Climate-sensitive disease that taken into account
KeyDiseaseALL=DiseaseALL[!grepl("Blood|Malnutrition", DiseaseALL)]

AdaptationCapacity=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/AdaptationCapacity.xlsx")

# ======================= High Resilience Result ======================= 

# Pre-settings
Resilience_Level=1

# Calculation
KeyAdaptationCapacity=AdaptationCapacity[AdaptationCapacity$Disease %in% KeyDiseaseALL & AdaptationCapacity$Resilience == Resilience_Level, ]
KeyRiskForm=Adpt_Form[Adpt_Form$Disease %in% KeyDiseaseALL & Adpt_Form$Resilience == Resilience_Level, ] # Note: the KeyRiskForm does not exactly contains the cumRR increase, instead the cum(RR*Exposure days). However, the comuputation still make sense as the Exposure days simultaneously occur at both numerator and the denominator of the fraction

Overall_AC=data.frame()
for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  
  eval(parse(text = paste('Temp_Overall=sum(KeyAdaptationCapacity$',Event_type,'*KeyAdaptationCapacity$IncidenceRate*KeyRiskForm$',Event_type,')/sum(KeyAdaptationCapacity$IncidenceRate*KeyRiskForm$',Event_type,'*1)',sep="")))
  TempGroup=c("Event"=Event_type,"Overall"=as.numeric(Temp_Overall))
  Overall_AC=rbind(Overall_AC,TempGroup)
}

colnames(Overall_AC)=c('Event','Overall_Adaptation_Capcity')

# Print
Disease_AC_Resilience_High=KeyAdaptationCapacity
Overall_AC_Resilience_High=Overall_AC

print(Disease_AC_Resilience_High)
print(Overall_AC_Resilience_High)


# ======================= Moderate Resilience Result ======================= 

# Pre-settings
Resilience_Level=2

# Calculation
KeyAdaptationCapacity=AdaptationCapacity[AdaptationCapacity$Disease %in% KeyDiseaseALL & AdaptationCapacity$Resilience == Resilience_Level, ]
KeyRiskForm=Adpt_Form[Adpt_Form$Disease %in% KeyDiseaseALL & Adpt_Form$Resilience == Resilience_Level, ] # Note: the KeyRiskForm does not exactly contains the cumRR increase, instead the cum(RR*Exposure days). However, the comuputation still make sense as the Exposure days simultaneously occur at both numerator and the denominator of the fraction

Overall_AC=data.frame()
for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  
  eval(parse(text = paste('Temp_Overall=sum(KeyAdaptationCapacity$',Event_type,'*KeyAdaptationCapacity$IncidenceRate*KeyRiskForm$',Event_type,')/sum(KeyAdaptationCapacity$IncidenceRate*KeyRiskForm$',Event_type,'*1)',sep="")))
  TempGroup=c("Event"=Event_type,"Overall"=as.numeric(Temp_Overall))
  Overall_AC=rbind(Overall_AC,TempGroup)
}

colnames(Overall_AC)=c('Event','Overall_Adaptation_Capcity')

# Print
Disease_AC_Resilience_Moderate=KeyAdaptationCapacity
Overall_AC_Resilience_Moderate=Overall_AC

print(Disease_AC_Resilience_Moderate)
print(Overall_AC_Resilience_Moderate)




