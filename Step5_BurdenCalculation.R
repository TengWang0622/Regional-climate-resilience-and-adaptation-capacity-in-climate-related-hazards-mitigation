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

# ============================ Loading files ============================

# Loading the GDP, Pop, Age & Gender populations
City_GDP_Pop=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/City_GDP_Pop.rds")

# Loading Disasters
TypeDisaster_province_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_province_Formal.xlsx")
TypeDisaster_city_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_city_Formal.xlsx")
TypeDisaster_county_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_county_Formal.xlsx")
HeatwaveEvent=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/HW_Sum_Loc90.xlsx")

# Loading incidence rates
# (1) Old
#IncidenceRate=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceRate.xlsx")
# (2) Updated
IncidenceRate=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceRate_Updated.xlsx")

# Loading DALYs
DALYs=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/DALYs.xlsx")

# Loading Medical Expenditure and Reimbursement
ReimbursementAll=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/ReimbursementAll.xlsx")

# Loading population weighted Incidence rate matrix
# (1) Org Respiratory 40
#IncidenceALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Org.xlsx")
# (2) Updated Respiratory 0.2
IncidenceALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Updated.xlsx")

# Loading DALYs matrix
DALYsALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/DALYsSummary.xlsx")

# Loading Risk matrix (Note: Risk*0.01)
RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary.xlsx")

# Loading Province-City-County structure and Tier groups (from the MAIN script)

CityT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT0.rds")
CityT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT1.rds")
CityT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT2.rds")
CityT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT3.rds")

CountyT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT0.rds")
CountyT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT1.rds")
CountyT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT2.rds")
CountyT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT3.rds")

GDP=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/GDP.rds")


# ========================= Event summary table preparation ========================= 

China_Event=City_GDP_Pop

df_Province=TypeDisaster_province_Formal
df_City=TypeDisaster_city_Formal
df_County=TypeDisaster_county_Formal

China_Event$City_Tier=3

is_Tier1=China_Event$City %in% CityT1
is_Tier2=China_Event$City %in% CityT2
is_Tier3=China_Event$City %in% CityT3

China_Event$City_Tier=ifelse(is_Tier1, 1, China_Event$City_Tier) # 1 for High resilience
China_Event$City_Tier=ifelse(is_Tier2, 2, China_Event$City_Tier) # 2 for Moderate resilience

China_Event$No_Storm=0
China_Event$No_Flood=0
China_Event$No_Cyclone=0
China_Event$No_Drought=0
China_Event$No_Heatwave=0
China_Event$No_WinterStorm=0
China_Event$No_Sand=0

China_Event$Days_Storm=0
China_Event$Days_Flood=0
China_Event$Days_Cyclone=0
China_Event$Days_Drought=0
China_Event$Days_Heatwave=0
China_Event$Days_WinterStorm=0
China_Event$Days_Sand=0

# ============ Input the number of events by disaster type ============= --------------------------------------------------------------------

Event_ALL=c('Storm', 'Flood', 'Cyclone', 'Drought', 'WinterStorm', 'Sand')
Event_level="ALL"    # all level - ALL; Billion - B; Million - M

for(k in 1:length(Event_ALL)){
  Event_type=Event_ALL[k]
  
  # Event filter
  df_EProvince=df_Province[df_Province$Disaster==Event_type,]
  df_ECity=df_City[df_City$Disaster==Event_type,]
  df_ECounty=df_County[df_County$Disaster==Event_type,]
  
  # ========= Province =========
  
  No_Event=length(unique(df_EProvince$Events))
  
  if(No_Event>0){
    Judgement_Province=1
    ID=unique(df_EProvince$Events)
    
    Event_info=data.frame()
    for(i in 1:No_Event){
      
      Loc_row=which(df_EProvince$Events==ID[i])
      Group=df_EProvince[Loc_row,]
      Loc=df_EProvince$Province
      StartT=df_EProvince$Begin[Loc_row]
      EndT=df_EProvince$End[Loc_row]
      No_days=difftime(EndT, StartT, units = "days")+1
      Damage=df_EProvince$`Total Damage, Adjusted ('000 US$)`[Loc_row]
      
      
      No_subLoc=nrow(Group)
      
      LocLIST=data.frame()
      for(j in 1:No_subLoc){
        subLoc=Group$Province[j]
        
        AllLoc_row=which(GDP$Province==subLoc)
        subGroup=GDP[AllLoc_row,]
        
        LocLIST_temp=data.frame(EventCity=unique(subGroup$City))
        
        #AllLoc=GDP$County[AllLoc_row]
        #AllGDP=GDP$GDP_t[AllLoc_row]
        
        
        #LocLIST_temp=data.frame(AllLoc,AllGDP)
        LocLIST=rbind(LocLIST,LocLIST_temp)
      }
      
      #LocLIST=na.omit(LocLIST)
      
      #LocLIST$EventID=i
      #LocLIST$Start=StartT[1]
      #LocLIST$End=EndT[1]
      LocLIST$No_days=No_days[1]
      #LocLIST$DamageTotal=Damage[1]
      #LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
      
      Event_info=rbind(Event_info,LocLIST) # Produce the Event information
    }
    
    Event_Province=Event_info
  } else{
    Judgement_Province=0
  }
  
  
  # ========= City =========
  
  No_Event=length(unique(df_ECity$Events))
  
  if(No_Event>0){
    Judgement_City=1
    ID=unique(df_ECity$Events)
    
    Event_info=data.frame()
    for(i in 1:No_Event){
      
      Loc_row=which(df_ECity$Events==ID[i])
      Group=df_ECity[Loc_row,]
      Loc=df_ECity$City
      StartT=df_ECity$Begin[Loc_row]
      EndT=df_ECity$End[Loc_row]
      No_days=difftime(EndT, StartT, units = "days")+1
      Damage=df_ECity$`Total Damage, Adjusted ('000 US$)`[Loc_row]
      
      No_subLoc=nrow(Group)
      
      LocLIST=data.frame()
      for(j in 1:No_subLoc){
        subLoc=Group$City[j]
        
        AllLoc_row=which(GDP$City==subLoc)
        subGroup=GDP[AllLoc_row,]
        AllLoc=GDP$County[AllLoc_row]
        AllGDP=GDP$GDP_t[AllLoc_row]
        
        LocLIST_temp=data.frame(EventCity=unique(subGroup$City))
        
        #LocLIST_temp=data.frame(AllLoc,AllGDP)
        LocLIST=rbind(LocLIST,LocLIST_temp)
      }
      
      #LocLIST=na.omit(LocLIST)
      
      #LocLIST$EventID=i
      #LocLIST$Start=StartT[1]
      #LocLIST$End=EndT[1]
      LocLIST$No_days=No_days[1]
      #LocLIST$DamageTotal=Damage[1]
      #LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
      
      Event_info=rbind(Event_info,LocLIST) # Produce the Event information
    } 
    
    Event_City=Event_info
  } else{
    Judgement_City=0
  }
  
  
  # Combine all the counties
  
  Event_info=data.frame()
  if(Judgement_Province==1){
    Event_info=rbind(Event_info,Event_Province)
  }
  if(Judgement_City==1){
    Event_info=rbind(Event_info,Event_City)
  }  
  #if(Judgement_County==1){
  #  Event_info=rbind(Event_info,Event_County)
  #}  
  
  #quantile(Event_info$DamageCounty,seq(0,1,by=0.1)) # ------------------CHECK
  
  #Event_info_ALL=Event_info
  
  #if(Event_level=="ALL"){
  #  Event_info=Event_info_ALL
  #}
  #if(Event_level=="M"){
  #  Event_info_M=Event_info[Event_info$DamageCounty<=1000,]
  #  Event_info=Event_info[Event_info$DamageCounty<=1000,]
  #  if(nrow(Event_info_M)==0){
  #    print("No M-level event is found!")
  #  }
  #  if(nrow(Event_info_M)>0){
  #    RatioEvent=round(nrow(Event_info_M)/nrow(Event_info_ALL),2)
  #    print(c("The proportion of M-level events",Event_type,RatioEvent))
  #  }
  #}
  #if(Event_level=="B"){
  #  Event_info_B=Event_info[Event_info$DamageCounty>1000,]
  #  Event_info=Event_info[Event_info$DamageCounty>1000,]
  #  if(nrow(Event_info_B)==0){
  #    print("No B-level event is found!")
  #  }
  #  if(nrow(Event_info_B)>0){
  #    RatioEvent=round(nrow(Event_info_B)/nrow(Event_info_ALL),2)
  #    print(c("The proportion of B-level events",Event_type,RatioEvent))
  #  }
  #}
  
  Event_info=na.omit((Event_info))
  
  for(i in 1:nrow(Event_info)){
    NoRow=which(China_Event$City==Event_info$EventCity[i])
    eval(parse(text = paste('China_Event$No_',Event_type,'[NoRow]=China_Event$No_',Event_type,'[NoRow]+1',sep="")))
    eval(parse(text = paste('China_Event$Days_',Event_type,'[NoRow]=China_Event$No_',Event_type,'[NoRow]+Event_info$No_days',sep="")))
  }
}

# Add the heatwave info into the China_Event data frame

China_Event=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.rds") # This file has already been computed by the upper scripts
HeatwaveEvent=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/HW_Sum_Loc90.xlsx") # This file is obtained through the supplementary script - National_Scale_HW_Identification_Loc.R - see README for help

China_Event$No_Heatwave=0

HeatwaveEvent=na.omit(HeatwaveEvent)
for(i in 1:nrow(HeatwaveEvent)){
  China_Event$No_Heatwave[which(China_Event$City==HeatwaveEvent$County[i])]=HeatwaveEvent$NoEvent[i]
  China_Event$Days_Heatwave[which(China_Event$City==HeatwaveEvent$County[i])]=HeatwaveEvent$Duration[i]
}

China_Event$No_Sum=China_Event$No_Storm+China_Event$No_Flood+China_Event$No_Cyclone+China_Event$No_Drought+China_Event$No_Heatwave+China_Event$No_WinterStorm+China_Event$No_Sand
China_Event$No_Frequency=China_Event$No_Sum/8

China_Event$Days_Sum=China_Event$Days_Storm+China_Event$Days_Flood+China_Event$Days_Cyclone+China_Event$Days_Drought+China_Event$Days_Heatwave+China_Event$Days_WinterStorm+China_Event$Days_Sand
China_Event$Days_AVE=China_Event$Days_Sum/8

saveRDS(China_Event,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.rds')
write.xlsx(China_Event, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.xlsx', rowNames = FALSE)

###############################################################################
#              Build the population group weighted Metrices
###############################################################################

# NOTE ----------------------------------------------------------
# Incidence rate          - IncidenceALL
# DALYs                   - DALYsALL
# DRG-aided economic info - ReimbursementAll
# ---------------------------------------------------------------

# ======================== Population groups ======================== 

# Summarize population age groups
Pop_total=14.11*100000000

Pop18=(4.31+6.29+6.36+5.51)/100*Pop_total
Pop19_44=(5.1+5.89+8.09+7.61+6.81)/100*Pop_total
Pop45_64=(7.2+8.84+8.14+4.98)/100*Pop_total
Pop65=Pop_total-(Pop18+Pop19_44+Pop45_64)

# ======================== Build the incidence rate matrix ======================== 

DiseaseALL=unique(IncidenceRate$Disease)

IncidenceALL=data.frame()
for(i in 1:length(DiseaseALL)){
  Temp_Disease=DiseaseALL[i]
  NoRow=which(IncidenceRate$Disease==Temp_Disease)
  
  Age18=mean(IncidenceRate$In19[NoRow])
  Age18min=mean(IncidenceRate$Lower19[NoRow])
  Age18max=max(IncidenceRate$Upper19[NoRow])
  
  Age19_44=mean(rowMeans(IncidenceRate[NoRow,c("In20_24","In25_29","In30_34","In35_39","In40_44")]))
  Age19_44min=mean(rowMeans(IncidenceRate[NoRow,c("Lower20_24","Lower25_29","Lower30_34","Lower35_39","Lower40_44")]))
  Age19_44max=mean(rowMeans(IncidenceRate[NoRow,c("Upper20_24","Upper25_29","Upper30_34","Upper35_39","Upper40_44")]))
  
  Age45_64=mean(rowMeans(IncidenceRate[NoRow,c("In45_49","In50_54","In55_59","In60_64")]))
  Age45_64min=mean(rowMeans(IncidenceRate[NoRow,c("Lower45_49","Lower50_54","Lower55_59","Lower60_64")]))
  Age45_64max=mean(rowMeans(IncidenceRate[NoRow,c("Upper45_49","Upper50_54","Upper55_59","Upper60_64")]))
  
  Age65=mean(rowMeans(IncidenceRate[NoRow,c("In65_69","In70")]))
  Age65min=mean(rowMeans(IncidenceRate[NoRow,c("Lower65_69","Lower70")]))
  Age65max=mean(rowMeans(IncidenceRate[NoRow,c("Upper65_69","Upper70")]))
  
  Temp_Incidence=data.frame(Disease=Temp_Disease,Age18,Age18min,Age18max,Age19_44,Age19_44min,Age19_44max,Age45_64,Age45_64min,Age45_64max,Age65,Age65min,Age65max)
  IncidenceALL=rbind(IncidenceALL,Temp_Incidence)
}

# Supplementary
IncidenceALL$Overall=(IncidenceALL$Age18*Pop18+IncidenceALL$Age19_44*Pop19_44+IncidenceALL$Age45_64*Pop45_64+IncidenceALL$Age65*Pop65)/Pop_total
IncidenceALL$Overallmin=(IncidenceALL$Age18min*Pop18+IncidenceALL$Age19_44min*Pop19_44+IncidenceALL$Age45_64min*Pop45_64+IncidenceALL$Age65min*Pop65)/Pop_total
IncidenceALL$Overallmax=(IncidenceALL$Age18max*Pop18+IncidenceALL$Age19_44max*Pop19_44+IncidenceALL$Age45_64max*Pop45_64+IncidenceALL$Age65max*Pop65)/Pop_total

saveRDS(IncidenceALL,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Updated.rds')
write.xlsx(IncidenceALL, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Updated.xlsx', rowNames = FALSE)


# ======================== Build the DALYs matrix ======================== 

DiseaseALL=unique(DALYs$Disease)

DALYsALL=data.frame()
for(i in 1:length(DiseaseALL)){
  Temp_Disease=DiseaseALL[i]
  NoRow=which(DALYs$Disease==Temp_Disease)
  
  NoRow18=which(DALYs$Disease==Temp_Disease & DALYs$Age=="<20 years")
  NoRow19_44=which(DALYs$Disease==Temp_Disease & (DALYs$Age=="20-24 years" | DALYs$Age=="25-29 years" | DALYs$Age=="30-34 years" | DALYs$Age=="35-39 years" | DALYs$Age=="40-44 years"))
  NoRow45_64=which(DALYs$Disease==Temp_Disease & (DALYs$Age=="45-49 years" | DALYs$Age=="50-54 years" | DALYs$Age=="55-59 years" | DALYs$Age=="60-64 years"))
  NoRow65=which(DALYs$Disease==Temp_Disease & (DALYs$Age=="65-69 years" | DALYs$Age=="70+ years"))
  
  Age18=mean(DALYs$DALYs[NoRow18])/Pop18
  Age18min=mean(DALYs$lower[NoRow18])/Pop18
  Age18max=mean(DALYs$upper[NoRow18])/Pop18
  
  Age19_44=mean(DALYs$DALYs[NoRow19_44])/Pop19_44
  Age19_44min=mean(DALYs$lower[NoRow19_44])/Pop19_44
  Age19_44max=mean(DALYs$upper[NoRow19_44])/Pop19_44
  
  Age45_64=mean(DALYs$DALYs[NoRow45_64])/Pop45_64
  Age45_64min=mean(DALYs$lower[NoRow45_64])/Pop45_64
  Age45_64max=mean(DALYs$upper[NoRow45_64])/Pop45_64
  
  Age65=mean(DALYs$DALYs[NoRow65])/Pop65
  Age65min=mean(DALYs$lower[NoRow65])/Pop65
  Age65max=mean(DALYs$upper[NoRow65])/Pop65
  
  Temp_DALYs=data.frame(Disease=Temp_Disease,Age18,Age18min,Age18max,Age19_44,Age19_44min,Age19_44max,Age45_64,Age45_64min,Age45_64max,Age65,Age65min,Age65max)
  DALYsALL=rbind(DALYsALL,Temp_DALYs)
}

saveRDS(DALYsALL,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/DALYsSummary.rds')
write.xlsx(DALYsALL, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/DALYsSummary.xlsx', rowNames = FALSE)

# ======================== Build the medical expenditure and reimbursement matrix ======================== 

# Note: The Reimbursement Matrix is computed in the SUPPLEMENTARY script - ReimbursementMatrix.R, with outcome serving as input in this script
#       Now load the result for Burden computation

# Loading files

Case_individual=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Case_individual.rds")

# ---------------- Stastics ----------------

Case_all=Case_individual

AgeALL=c('Age18', 'Age19_44', 'Age45_64', 'Age65','ALL')
DiseaseALL=c('Blood', 'CVD', 'Endocrine', 'Genitourinary', 'Infectious', 'Injuries', 'Malnutrition', 'Mental', 'Neoplasms', 'Neuro', 'Respiratory', 'Others')

ReimbursementAll=data.frame()

for(j in 1:length(AgeALL)){
  Age=AgeALL[j]
  
  cli_progress_bar('Processing O3 dataset', total=length(DiseaseALL), type='tasks',
                   format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")
  
  for(i in 1:length(DiseaseALL)){
    Disease=DiseaseALL[i]
    
    if(Age=='Age18'){
      eval(parse(text = paste('Case_all=Case_individual[which(Case_individual$age=="1. <18" & Case_individual$"',Disease,'"=="1"),]',sep="")))
    } else if(Age=='Age19_44'){
      eval(parse(text = paste('Case_all=Case_individual[which(Case_individual$age=="2. <45" & Case_individual$"',Disease,'"=="1"),]',sep="")))
    } else if(Age=='Age45_64'){
      eval(parse(text = paste('Case_all=Case_individual[which((Case_individual$age=="3. <55" | Case_individual$age=="4. <65") & Case_individual$"',Disease,'"=="1"),]',sep="")))
    } else if(Age=='Age65'){
      eval(parse(text = paste('Case_all=Case_individual[which((Case_individual$age=="5. <75" | Case_individual$age=="6. >=75") & Case_individual$"',Disease,'"=="1"),]',sep="")))
    } else if(Age=='ALL'){
      eval(parse(text = paste('Case_all=Case_individual[which(Case_individual$"',Disease,'"=="1"),]',sep="")))
    }
    
    Pop_All=nrow(Case_all)
    
    Pop_Blood=sum(as.numeric(Case_all$Blood))
    Pop_CVD=sum(as.numeric(Case_all$CVD))
    Pop_Endocrine=sum(as.numeric(Case_all$Endocrine))
    Pop_Genitourinary=sum(as.numeric(Case_all$Genitourinary))
    Pop_Infectious=sum(as.numeric(Case_all$Infectious))
    Pop_Injuries=sum(as.numeric(Case_all$Injuries))
    Pop_Malnutrition=sum(as.numeric(Case_all$Malnutrition))
    Pop_Mental=sum(as.numeric(Case_all$Mental))
    Pop_Neoplasms=sum(as.numeric(Case_all$Neoplasms))
    Pop_Neuro=sum(as.numeric(Case_all$Neuro))
    Pop_Respiratory=sum(as.numeric(Case_all$Respiratory))
    Pop_Others=sum(as.numeric(Case_all$Others))
    
    print(paste('Pop_All = ',Pop_All))
    print(paste('Pop_Blood = ',Pop_Blood))
    print(paste('Pop_CVD = ',Pop_CVD))
    print(paste('Pop_Endocrine = ',Pop_Endocrine))
    print(paste('Pop_Genitourinary = ',Pop_Genitourinary))
    print(paste('Pop_Infectious = ',Pop_Infectious))
    print(paste('Pop_Injuries = ',Pop_Injuries))
    print(paste('Pop_Malnutrition = ',Pop_Malnutrition))
    print(paste('Pop_Mental = ',Pop_Mental))
    print(paste('Pop_Neoplasms = ',Pop_Neoplasms))
    print(paste('Pop_Neuro = ',Pop_Neuro))
    print(paste('Pop_Respiratory = ',Pop_Respiratory))
    print(paste('Pop_Others = ',Pop_Others))
    
    Total_cost=sum(Case_all$zfy,na.rm = TRUE)
    No_individual=nrow(Case_all)
    
    Total_N=sum(Case_all$num)
    Temp_mean_val=Total_cost/Total_N
    
    Total_cost=sum(Case_all$zfy)
    Mean_cost=Total_cost/Total_N
    
    Sum_DRG=sum(Case_all$num*Case_all$DRG_mod)
    
    ReimbursementRatio=1.1
    Unit=ReimbursementRatio*Mean_cost
    
    # ----------------- The contribution of each type of insurance ----------------- 
    
    Sum_Ins0=sum(Case_all$Insurance_payment[Case_all$Insurance==0])  # 自费
    Sum_Ins1=sum(Case_all$Insurance_payment[Case_all$Insurance==1])  # 城镇居民基本医疗保险
    Sum_Ins2=sum(Case_all$Insurance_payment[Case_all$Insurance==2])  # 新型农村合作医疗
    Sum_Ins3=sum(Case_all$Insurance_payment[Case_all$Insurance==3])  # 贫困救助
    Sum_Ins4=sum(Case_all$Insurance_payment[Case_all$Insurance==4])  # 商业医疗保险
    Sum_Ins5=sum(Case_all$Insurance_payment[Case_all$Insurance==5])  # 全公费
    
    Sum_Ins7=sum(Case_all$Insurance_payment[Case_all$Insurance==7])  # 其他社会保险
    
    Percent_Ins1=round(Sum_Ins1/Total_cost,4)
    Percent_Ins2=round(Sum_Ins2/Total_cost,4)
    Percent_Ins3=round(Sum_Ins3/Total_cost,4)
    Percent_Ins4=round(Sum_Ins4/Total_cost,4)
    Percent_Ins5=round(Sum_Ins5/Total_cost,4)
    
    Percent_Ins7=round(Sum_Ins7/Total_cost,4)
    
    Percent_Ins0=1-Percent_Ins1-Percent_Ins2-Percent_Ins3-Percent_Ins4-Percent_Ins5-Percent_Ins7
    
    print(c(c('Number of patient)'),No_individual))
    print(c(c('Medical expenditure (All patient)'),round(Total_cost,2)))
    
    print(c('个人支付占所有医疗费用的比例',round(Percent_Ins0,4)))
    print(c('医保支付占所有医疗费用的比例',round(1-Percent_Ins0,4)))
    
    print(c('纯自费占所有医疗费用的比例',round(sum(Case_all$zfy[Case_all$Insurance==0])/Total_cost,4)))
    print(c('医保部分占所有医疗费用的比例',round(sum(Case_all$zfy[Case_all$Insurance>0])/Total_cost,4)))
    
    print(c('使用医保患者的医保报销比例',round(sum(Case_all$Insurance_payment[Case_all$Insurance>0])/sum(Case_all$zfy[Case_all$Insurance>0]),4)))
    
    print(rbind(c('自费','城镇居民基本医疗保险','新型农村合作医疗','贫困救助','商业医疗保险','全公费','其他社会保险'),c(Percent_Ins0,Percent_Ins1,Percent_Ins2,Percent_Ins3,Percent_Ins4,Percent_Ins5,Percent_Ins7)))
    
    #quantile(Case_all$DRG_mod,seq(0,1,by=0.05))
    #quantile(Case_all$zfy,seq(0,1,by=0.05))
    #quantile(Case_all$Self_payment,seq(0,1,by=0.05))
    
    print(c(c('Medical expenditure (per patient)'),round(Total_cost/No_individual,2)))
    print(c(c('Medical expenditure (per hospitalization'),round(Total_cost/Total_N,2)))
    print(c(c('Averaged hospitalization per patient'),round(Total_N/No_individual,2)))
    print(c(c('Medical credit per hospitalization'),round(Sum_DRG/Total_N,2)))
    print(c(c('Medical insurance credit (Ref)'),round(Unit,2)))
    
    Temp_Reimbursement=data.frame(Age,Disease,Pop=No_individual,Total_cost,Self_Over_All=round(Percent_Ins0,4),Reim_Over_All=round(1-Percent_Ins0,4),
                                  PureSelf_Over_All=round(sum(Case_all$zfy[Case_all$Insurance==0])/Total_cost,4),PartReim_Over_All=round(sum(Case_all$zfy[Case_all$Insurance>0])/Total_cost,4),
                                  Reim_Over_PartReim=round(sum(Case_all$Insurance_payment[Case_all$Insurance>0])/sum(Case_all$zfy[Case_all$Insurance>0]),4),
                                  Percent_Ins0,Percent_Ins1,Percent_Ins2,Percent_Ins3,Percent_Ins4,Percent_Ins5,Percent_Ins7,
                                  CostPerPatient=round(Total_cost/No_individual,2),CostPerHosp=round(Total_cost/Total_N,2),HospPerPatient=round(Total_N/No_individual,2),Credit=round(Sum_DRG/Total_N,2),CostPerCredit=round(Unit,2))
    ReimbursementAll=rbind(ReimbursementAll,Temp_Reimbursement)
    cli_progress_update()
  }
  
}

saveRDS(ReimbursementAll,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/ReimbursementAll.rds')
write.xlsx(ReimbursementAll, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/ReimbursementAll.xlsx', rowNames = FALSE)



################################################################################################
#                        Population attributable fraction (PAF)
################################################################################################

# Load the event data frame
China_Event=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.rds") # This file has already been computed by the upper scripts

# Loading Risk matrix (Note: Risk*0.01)

ObsWindow='Enduring'       # Warning, Enduring, Post, After, RTH

if(ObsWindow=='Warning'){
  for(i in 1:length(EventALL)){
    Event=EventALL[i]
    eval(parse(text = paste('China_Event$Days_',Event,'=China_Event$No_',Event,'*7',sep="")))
  }
  RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_Warning.xlsx")
  
} else if(ObsWindow=='Enduring'){
  RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_Enduring.xlsx")
  
} else if(ObsWindow=='Post'){
  China_Event$Days_Drought=China_Event$No_Drought*14
  RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_Post.xlsx")
  
} else if(ObsWindow=='After'){
  China_Event$Days_Drought=China_Event$No_Drought*14
  RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_After.xlsx")
  
} else if(ObsWindow=='RTH'){
  China_Event$Days_Drought=China_Event$No_Drought*14
  RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/RiskSummary_RTH.xlsx")
  
}


Days_Total=as.numeric(difftime(as.Date("2023-12-31"), as.Date("2016-01-01"), units = "days"))


InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','PartReim_Over_All','Self_After_Reim','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
DiseaseALL=c('Malnutrition','Infectious','Injuries','Respiratory','Mental','Neoplasms','Endocrine','Blood','Neurology','CVD','Genitourinary')
EventALL=c('Storm', 'Flood', 'Cyclone', 'WinterStorm', 'Drought', 'Heatwave', 'Sand')


# ============================ Candidate columns ============================ 
# Build zero candidatte columns for further saving PAF results with respect to Event_type and Disease

China_Event$PAF=0

for(i in 1:length(DiseaseALL)){
  Disease=DiseaseALL[i]
  eval(parse(text = paste('China_Event$PAF_',Disease,'=0',sep="")))
}

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  eval(parse(text = paste('China_Event$PAF_',Event_type,'=0',sep="")))
}

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  for(j in 1:length(DiseaseALL)){
    Disease=DiseaseALL[j]
    eval(parse(text = paste('China_Event$PAF_',Event_type,'_',Disease,'=0',sep="")))
  }
  #eval(parse(text = paste('China_Event$PAF_',Event_type,'=0',sep="")))
}

# ============================ Compute the PAFs ============================ 

for(i in 1:nrow(China_Event)){
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      eval(parse(text = paste('China_Event$PAF_',Event_type,'_',Disease,'[i]=China_Event$Days_',Event_type,'[i]/Days_Total*RiskALL[which(RiskALL$Disease=="',Disease,'" & RiskALL$Resilience==China_Event$City_Tier[i]),"',Event_type,'"]/(China_Event$PAF_',Event_type,'_',Disease,'[i]=China_Event$Days_',Event_type,'[i]/Days_Total*RiskALL[which(RiskALL$Disease=="',Disease,'" & RiskALL$Resilience==China_Event$City_Tier[i]),"',Event_type,'"]+1)',sep="")))
      eval(parse(text = paste('China_Event$PAF_',Event_type,'[i]=China_Event$PAF_',Event_type,'[i]+as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])',sep="")))
      eval(parse(text = paste('China_Event$PAF_',Disease,'[i]=China_Event$PAF_',Disease,'[i]+as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])',sep="")))
      
    }
    eval(parse(text = paste('China_Event$PAF[i]=China_Event$PAF[i]+China_Event$PAF_',Event_type,'[i]',sep="")))
  }
}

################################################################################################
#                            Disease and Economic Burdens
################################################################################################

# ============================ Candidate columns ============================ 

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  for(j in 1:length(DiseaseALL)){
    Disease=DiseaseALL[j]
    
    # ----------- Disease burden columns -------------
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18min=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18max=0',sep="")))
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44min=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44max=0',sep="")))
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64min=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64max=0',sep="")))
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65min=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65max=0',sep="")))
    
    # Summation
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALL=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin=0',sep="")))
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax=0',sep="")))
    
    # ----------- Economic burden columns -------------
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18min=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18max=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44min=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44max=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64min=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64max=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65min=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65max=0',sep="")))
    
    # Summation
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALL=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALLmin=0',sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALLmax=0',sep="")))
    
    # - - - - NOTE- - - - - - - Ins0-7 & Overall Reimbursment amount - - - - - - - - - 
    
    # Ins0 - Individual payment excluding reimbursement
    # Ins1 - Basic medical insurance for urban and rural residents
    # Ins2 - New rural cooperative medical care system
    # Ins3 - Poverty assistance
    # Ins4 - Commercial medical insurance
    # Ins5 - Medical subsidy for civil servants
    # Ins7 - Other social insurance
    
    # Reim_Over_All - sum(Ins1-7) 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','PartReim_Over_All','Self_After_Reim','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
    
    for(s in 1:length(InsuranceALL)){
      Ins_Type=InsuranceALL[s]
      
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18min=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18max=0',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44min=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44max=0',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64min=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64max=0',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65min=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65max=0',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALL=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALLmin=0',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALLmax=0',sep="")))
      
    }
  }
}

# ============================ Compute the burdens ============================ 

Cost=ReimbursementAll

cli_progress_bar('Processing O3 dataset', total=nrow(China_Event), type='tasks',
                 format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")

for(i in 1:nrow(China_Event)){
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      # ------------ Disease Burden ------------ 
      
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age18"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18min[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age18min"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age18max[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age18max"])',sep="")))
      
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age19_44"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44min[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age19_44min"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44max[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age19_44max"])',sep="")))
      
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age45_64"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64min[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age45_64min"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64max[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age45_64max"])',sep="")))
      
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age65"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65min[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age65min"])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_Age65max[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(DALYsALL[which(DALYsALL$Disease=="',Disease,'"),"Age65max"])',sep="")))
      
      # Summation
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALL[i]=as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65[i])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin[i]=as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18min[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44min[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64min[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65min[i])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax[i]=as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18max[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44max[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64max[i])','+as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65max[i])',sep="")))
      
      
      
      # ------------ Economic Burden ------------ 
      
      # CostPerHosp ----------Total climate-induced additional Expenditure
      
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18min[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age18max[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44min[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age19_44max[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64min[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age45_64max[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65min[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_Age65max[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])',sep="")))
      
      # Summation
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALL[i]=as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65[i])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALLmin[i]=as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18min[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44min[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64min[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65min[i])',sep="")))
      eval(parse(text = paste('China_Event$Eco_',Event_type,'_',Disease,'_ALLmax[i]=as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18max[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44max[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64max[i])','+as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65max[i])',sep="")))
      
      
      # - - - - NOTE- - - - - - - Ins0-7 & Overall Reimbursment amount - - - - - - - - - 
      
      # Ins0 - Individual payment excluding reimbursement
      # Ins1 - Basic medical insurance for urban and rural residents
      # Ins2 - New rural cooperative medical care system
      # Ins3 - Poverty assistance
      # Ins4 - Commercial medical insurance
      # Ins5 - Medical subsidy for civil servants
      # Ins7 - Other social insurance
      
      # Reim_Over_All - sum(Ins1-7) 
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      
      #InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','PartReim_Over_All','Self_After_Reim','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
      
      for(s in 1:length(InsuranceALL)){
        Ins_Type=InsuranceALL[s]
        
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18min[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18max[i]=China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age18" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44min[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44max[i]=China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age19_44" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64min[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64max[i]=China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age45_64" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65min[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65min"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65max[i]=China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65max"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[i])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"CostPerHosp"])*as.numeric(Cost[which(Cost$Age=="Age65" & Cost$Disease=="',Disease,'"),"',Ins_Type,'"])',sep="")))
        
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALL[i]=as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65[i])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALLmin[i]=as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18min[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44min[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64min[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65min[i])',sep="")))
        eval(parse(text = paste('China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_ALLmax[i]=as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age18max[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age19_44max[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age45_64max[i])','+as.numeric(China_Event$Eco_',Ins_Type,'_',Event_type,'_',Disease,'_Age65max[i])',sep="")))
        
      }
    }
  }
  cli_progress_update()
}

#######################################################################

# NOTE: Structure of the data frame

# ---------------------------- PAF ---------------------------- 
# China_Event$PAF                                           - The overall PAF for each city
# China_Event$PAF_Event_type                                - The disaster specified PAF after summation of diseases for each city
# China_Event$PAF_Disease                                   - The disease specified PAF after summation of hazards for each city
# China_Event$PAF_Event_type_Disease                        - The disaster and disease specified PAF for each city

# ---------------------------- Disease burden ---------------------------- 
# China_Event$DALYs                                         - The overall DALYs for each city
# China_Event$DALYs_Event_type                              - The disaster specified DISEASE BURDEN after summation of diseases for each city    
# China_Event$DALYs_Disease                                 - The disease specified DISEASE BURDEN after summation of hazards for each city      
# China_Event$DALYs_Event_type_Disease                      - The disaster and disease specified DISEASE BURDEN for each city          

# China_Event$DALYs_Event_type_Disease_Age18                - The disaster, disease and age group specified expected DISEASE BURDEN for each city
# China_Event$DALYs_Event_type_Disease_Age18min             - The disaster, disease and age group specified lower DISEASE BURDEN for each city
# China_Event$DALYs_Event_type_Disease_Age18max             - The disaster, disease and age group specified upper DISEASE BURDEN for each city

# China_Event$DALYs_Event_type_Disease_ALL                  - The TOTAL disaster, disease and age group specified expected DISEASE BURDEN for each city
# China_Event$DALYs_Event_type_Disease_ALLmin               - The TOTAL disaster, disease and age group specified lower DISEASE BURDEN for each city
# China_Event$DALYs_Event_type_Disease_ALLmax               - The TOTAL disaster, disease and age group specified upper DISEASE BURDEN for each city

# ---------------------------- Economic burden ---------------------------- 
# China_Event$Eco                                           - The overall ECONOMIC BURDEN for each city 
# China_Event$Eco_Event_type                                - The disaster specified ECONOMIC BURDEN after summation of diseases for each city  
# China_Event$Eco_Disease                                   - The disease specified ECONOMIC BURDEN after summation of hazards for each city   
# China_Event$Eco_Event_type_Disease                        - The disaster and disease specified ECONOMIC BURDEN for each city
# China_Event$Eco_Age                                       - The age group specified ECONOMIC BURDEN for each city
# China_Event$Eco_Agemin                                    - The age group specified ECONOMIC BURDEN for each city 
# China_Event$Eco_Agemax                                    - The age group specified ECONOMIC BURDEN for each city 

# China_Event$Eco_Event_type_Disease_Age18                  - The disaster, disease and age group specified expected ECONOMIC BURDEN for each city
# China_Event$Eco_Event_type_Disease_Age18min               - The disaster, disease and age group specified lower ECONOMIC BURDEN for each city
# China_Event$Eco_Event_type_Disease_Age18max               - The disaster, disease and age group specified upper ECONOMIC BURDEN for each city

# China_Event$Eco_Event_type_Disease_ALL                    - The TOTAL disaster, disease and age group specified expected ECONOMIC BURDEN for each city
# China_Event$Eco_Event_type_Disease_ALLmin                 - The TOTAL disaster, disease and age group specified lower ECONOMIC BURDEN for each city
# China_Event$Eco_Event_type_Disease_ALLmax                 - The TOTAL disaster, disease and age group specified upper ECONOMIC BURDEN for each city

# China_Event$Eco_Ins_Type_Event_type_Disease_Age18         - The disaster, disease and age group specified expected SPECIFIC INSURANCE BURDEN for each city
# China_Event$Eco_Ins_Type_Event_type_Disease_Age18min      - The disaster, disease and age group specified lower SPECIFIC INSURANCE BURDEN for each city
# China_Event$Eco_Ins_Type_Event_type_Disease_Age18max      - The disaster, disease and age group specified upper SPECIFIC INSURANCE BURDEN for each city

# China_Event$Eco_Ins_Type_Event_type_Disease_ALL           - The TOTAL disaster, disease and age group specified expected SPECIFIC INSURANCE BURDEN for each city
# China_Event$Eco_Ins_Type_Event_type_Disease_ALLmin        - The TOTAL disaster, disease and age group specified lower SPECIFIC INSURANCE BURDEN for each city
# China_Event$Eco_Ins_Type_Event_type_Disease_ALLmax        - The TOTAL disaster, disease and age group specified upper SPECIFIC INSURANCE BURDEN for each city

#######################################################################

# Manipulation

#saveRDS(China_Event,"~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Temp/China_Event.rds")
#write.xlsx(China_Event, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Temp/China_Event.xlsx")

#TempCE0609=China_Event
#China_Event[is.na(China_Event)]=0
#China_Event[,na_cols]=0

China_Event$DALYs=0
China_Event$DALYsmin=0
China_Event$DALYsmax=0

China_Event$Eco=0
China_Event$Ecomin=0
China_Event$Ecomax=0

for(j in 1:length(EventALL)){
  Event_type=EventALL[j]
  for(k in 1:length(DiseaseALL)){
    Disease=DiseaseALL[k]
    
    eval(parse(text = paste('China_Event$DALYs=China_Event$DALYs+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
    eval(parse(text = paste('China_Event$DALYsmin=China_Event$DALYsmin+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
    eval(parse(text = paste('China_Event$DALYsmax=China_Event$DALYsmax+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
    
    eval(parse(text = paste('China_Event$Eco=China_Event$Eco+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
    eval(parse(text = paste('China_Event$Ecomin=China_Event$Ecomin+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
    eval(parse(text = paste('China_Event$Ecomax=China_Event$Ecomax+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
    
  }
}



China_Burden=cbind(City=China_Event$City,City_Tier=China_Event$City_Tier,Pop=China_Event$Pop,Event_Sum=China_Event$No_Sum,
                   DALYs_person_event=China_Event$DALYs/China_Event$Pop/China_Event$No_Sum,Eco_person_event=China_Event$Eco/China_Event$Pop/China_Event$No_Sum,
                   DALYs=China_Event$DALYs,DALYsmin=China_Event$DALYsmin,DALYsmax=China_Event$DALYsmax,
                   Eco=China_Event$Eco,Ecomin=China_Event$Ecomin,Ecomax=China_Event$Ecomax)


# ====================== Disease Burden data frame ======================  

df_DiseaseBurden=data.frame(Type="DiseaseBurden")

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  
  DiseaseGroup=data.frame()
  for(j in 1:length(DiseaseALL)){
    Disease=DiseaseALL[j]
    eval(parse(text = paste('Group=rbind(sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_ALL)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65)))',sep="")))
    eval(parse(text = paste('Groupmin=rbind(sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18min)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44min)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64min)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65min)))',sep="")))
    eval(parse(text = paste('Groupmax=rbind(sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age18max)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age19_44max)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age45_64max)),sum(as.numeric(China_Event$DALYs_',Event_type,'_',Disease,'_Age65max)))',sep="")))
    
    tempRowNames=c('ALL','Age18','Age19_44','Age45_64','Age65')
    
    eval(parse(text = paste('DiseaseRowNames=rep("',Disease,'",length(tempRowNames))',sep="")))
    
    temp_DiseaseGroup=cbind(Disease=DiseaseRowNames,AgeGroup=tempRowNames,DALYs=Group,DALYsmin=Groupmin,DALYsmax=Groupmax)
    eval(parse(text = paste('colnames(temp_DiseaseGroup)=c("Disease","AgeGroup","',Event_type,'_DALYs","',Event_type,'_DALYsmin","',Event_type,'_DALYsmax")',sep="")))
    
    DiseaseGroup=rbind(DiseaseGroup,temp_DiseaseGroup)
  }
  df_DiseaseBurden=cbind(df_DiseaseBurden,DiseaseGroup)
  
}


# ====================== Economic Burden data frame ======================  

df_EconomicBurden=data.frame(Type="EconomicBurden")

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  
  EcoGroup=data.frame()
  for(j in 1:length(DiseaseALL)){
    Disease=DiseaseALL[j]
    
    eval(parse(text = paste('Group=rbind(sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_ALL)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65)))',sep="")))
    eval(parse(text = paste('Groupmin=rbind(sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_ALLmin)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18min)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44min)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64min)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65min)))',sep="")))
    eval(parse(text = paste('Groupmax=rbind(sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_ALLmax)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age18max)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age19_44max)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age45_64max)),sum(as.numeric(China_Event$Eco_',Event_type,'_',Disease,'_Age65max)))',sep="")))
    
    tempRowNames=c('ALL','Age18','Age19_44','Age45_64','Age65')
    
    eval(parse(text = paste('DiseaseRowNames=rep("',Disease,'",length(tempRowNames))',sep="")))
    
    temp_EcoGroup=cbind(Disease=DiseaseRowNames,AgeGroup=tempRowNames,DALYs=Group,DALYsmin=Groupmin,DALYsmax=Groupmax)
    eval(parse(text = paste('colnames(temp_EcoGroup)=c("Disease","AgeGroup","',Event_type,'_Eco","',Event_type,'_Ecomin","',Event_type,'_Ecomax")',sep="")))
    
    EcoGroup=rbind(EcoGroup,temp_EcoGroup)
  }
  df_EconomicBurden=cbind(df_EconomicBurden,EcoGroup)
  
}

# ====================== Insurance Burden data frame ======================  

df_InsuranceBurden=data.frame(Type="InsuranceBurden")
df_EventInsuranceBurden=data.frame(Type="InsuranceBurden")

for(i in 1:length(EventALL)){
  Event_type=EventALL[i]
  
  InsGroup=data.frame()
  EventGroup=data.frame()
  for(j in 1:length(InsuranceALL)){
    Ins_type=InsuranceALL[j]
    
    DiseaseGroup=data.frame()
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      eval(parse(text = paste('Group=rbind(sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_ALL)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age18)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age19_44)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age45_64)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age65)))',sep="")))
      eval(parse(text = paste('Groupmin=rbind(sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_ALLmin)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age18min)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age19_44min)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age45_64min)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age65min)))',sep="")))
      eval(parse(text = paste('Groupmax=rbind(sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_ALLmax)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age18max)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age19_44max)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age45_64max)),sum(as.numeric(China_Event$Eco_',Ins_type,'_',Event_type,'_',Disease,'_Age65max)))',sep="")))
      
      tempRowNames=c('ALL','Age18','Age19_44','Age45_64','Age65')
      
      eval(parse(text = paste('DiseaseRowNames=rep("',Disease,'",length(tempRowNames))',sep="")))
      
      temp_InsGroup=cbind(Insurance=Ins_type,Disease=DiseaseRowNames,AgeGroup=tempRowNames,Ins=Group,Inssmin=Groupmin,Insmax=Groupmax)
      eval(parse(text = paste('colnames(temp_InsGroup)=c("Insurance","Disease","AgeGroup","',Event_type,'_Ins","',Event_type,'_Insmin","',Event_type,'_Insmax")',sep="")))
      
      DiseaseGroup=rbind(DiseaseGroup,temp_InsGroup)
    }
    
    EventGroup=rbind(EventGroup,DiseaseGroup)
    
    
    Ins_SumALL=DiseaseGroup[which(DiseaseGroup$AgeGroup=='ALL'),]
    Ins_SumAge18=DiseaseGroup[which(DiseaseGroup$AgeGroup=='Age18'),]
    Ins_SumAge19_44=DiseaseGroup[which(DiseaseGroup$AgeGroup=='Age19_44'),]
    Ins_SumAge45_64=DiseaseGroup[which(DiseaseGroup$AgeGroup=='Age45_64'),]
    Ins_SumAge65=DiseaseGroup[which(DiseaseGroup$AgeGroup=='Age65'),]
    
    eval(parse(text = paste('temp_Ins_SumALL=cbind(Insurance="',Ins_type,'",Disease="ALL",AgeGroup="ALL",',Event_type,'_Ins=sum(as.numeric(Ins_SumALL$',Event_type,'_Ins)),',Event_type,'_Insmin=sum(as.numeric(Ins_SumALL$',Event_type,'_Insmin)),',Event_type,'_Insmax=sum(as.numeric(Ins_SumALL$',Event_type,'_Insmax)))',sep="")))
    eval(parse(text = paste('temp_Ins_SumAge18=cbind(Insurance="',Ins_type,'",Disease="ALL",AgeGroup="Age18",',Event_type,'_Ins=sum(as.numeric(Ins_SumAge18$',Event_type,'_Ins)),',Event_type,'_Insmin=sum(as.numeric(Ins_SumAge18$',Event_type,'_Insmin)),',Event_type,'_Insmax=sum(as.numeric(Ins_SumAge18$',Event_type,'_Insmax)))',sep="")))
    eval(parse(text = paste('temp_Ins_SumAge19_44=cbind(Insurance="',Ins_type,'",Disease="ALL",AgeGroup="Age19_44",',Event_type,'_Ins=sum(as.numeric(Ins_SumAge19_44$',Event_type,'_Ins)),',Event_type,'_Insmin=sum(as.numeric(Ins_SumAge19_44$',Event_type,'_Insmin)),',Event_type,'_Insmax=sum(as.numeric(Ins_SumAge19_44$',Event_type,'_Insmax)))',sep="")))
    eval(parse(text = paste('temp_Ins_SumAge45_64=cbind(Insurance="',Ins_type,'",Disease="ALL",AgeGroup="Age45_64",',Event_type,'_Ins=sum(as.numeric(Ins_SumAge45_64$',Event_type,'_Ins)),',Event_type,'_Insmin=sum(as.numeric(Ins_SumAge45_64$',Event_type,'_Insmin)),',Event_type,'_Insmax=sum(as.numeric(Ins_SumAge45_64$',Event_type,'_Insmax)))',sep="")))
    eval(parse(text = paste('temp_Ins_SumAge65=cbind(Insurance="',Ins_type,'",Disease="ALL",AgeGroup="Age65",',Event_type,'_Ins=sum(as.numeric(Ins_SumAge65$',Event_type,'_Ins)),',Event_type,'_Insmin=sum(as.numeric(Ins_SumAge65$',Event_type,'_Insmin)),',Event_type,'_Insmax=sum(as.numeric(Ins_SumAge65$',Event_type,'_Insmax)))',sep="")))
    
    temp_SumIns=rbind(temp_Ins_SumALL,temp_Ins_SumAge18,temp_Ins_SumAge19_44,temp_Ins_SumAge45_64,temp_Ins_SumAge65)
    
    InsGroup=rbind(InsGroup,temp_SumIns)
    
  }
  df_InsuranceBurden=cbind(df_InsuranceBurden,InsGroup)
  df_EventInsuranceBurden=cbind(df_EventInsuranceBurden,EventGroup)
}



# ===================== Tabulate the data for figures ===================== 

Burden_Disease_ALL=df_DiseaseBurden[which(df_DiseaseBurden$AgeGroup=='ALL'),]

for(i in 1:length(EventALL)){
  Event=EventALL[i]
  eval(parse(text = paste('Burden_Disease_ALL$',Event,'_DALYs=round(as.numeric(Burden_Disease_ALL$',Event,'_DALYs),2)',sep="")))
  eval(parse(text = paste('Burden_Disease_ALL$',Event,'_DALYsmin=round(as.numeric(Burden_Disease_ALL$',Event,'_DALYsmin),2)',sep="")))
  eval(parse(text = paste('Burden_Disease_ALL$',Event,'_DALYsmax=round(as.numeric(Burden_Disease_ALL$',Event,'_DALYsmax),2)',sep="")))
}

Burden_Disease_ALL$ALL_DALYs=round(as.numeric(Burden_Disease_ALL$Storm_DALYs)+as.numeric(Burden_Disease_ALL$Flood_DALYs)+as.numeric(Burden_Disease_ALL$Cyclone_DALYs)+as.numeric(Burden_Disease_ALL$WinterStorm_DALYs)+as.numeric(Burden_Disease_ALL$Drought_DALYs)+as.numeric(Burden_Disease_ALL$Heatwave_DALYs)+as.numeric(Burden_Disease_ALL$Sand_DALYs),2)
Burden_Disease_ALL$ALL_DALYsmin=round(as.numeric(Burden_Disease_ALL$Storm_DALYsmin)+as.numeric(Burden_Disease_ALL$Flood_DALYsmin)+as.numeric(Burden_Disease_ALL$Cyclone_DALYsmin)+as.numeric(Burden_Disease_ALL$WinterStorm_DALYsmin)+as.numeric(Burden_Disease_ALL$Drought_DALYsmin)+as.numeric(Burden_Disease_ALL$Heatwave_DALYsmin)+as.numeric(Burden_Disease_ALL$Sand_DALYsmin),2)
Burden_Disease_ALL$ALL_DALYsmax=round(as.numeric(Burden_Disease_ALL$Storm_DALYsmax)+as.numeric(Burden_Disease_ALL$Flood_DALYsmax)+as.numeric(Burden_Disease_ALL$Cyclone_DALYsmax)+as.numeric(Burden_Disease_ALL$WinterStorm_DALYsmax)+as.numeric(Burden_Disease_ALL$Drought_DALYsmax)+as.numeric(Burden_Disease_ALL$Heatwave_DALYsmax)+as.numeric(Burden_Disease_ALL$Sand_DALYsmax),2)


Burden_Disease_AgeGroup=df_DiseaseBurden[which(!df_DiseaseBurden$AgeGroup=='ALL'),]

for(i in 1:length(EventALL)){
  Event=EventALL[i]
  eval(parse(text = paste('Burden_Disease_AgeGroup$',Event,'_DALYs=round(as.numeric(Burden_Disease_AgeGroup$',Event,'_DALYs),2)',sep="")))
  eval(parse(text = paste('Burden_Disease_AgeGroup$',Event,'_DALYsmin=round(as.numeric(Burden_Disease_AgeGroup$',Event,'_DALYsmin),2)',sep="")))
  eval(parse(text = paste('Burden_Disease_AgeGroup$',Event,'_DALYsmax=round(as.numeric(Burden_Disease_AgeGroup$',Event,'_DALYsmax),2)',sep="")))
}

Burden_Disease_AgeGroup$ALL_DALYs=round(as.numeric(Burden_Disease_AgeGroup$Storm_DALYs)+as.numeric(Burden_Disease_AgeGroup$Flood_DALYs)+as.numeric(Burden_Disease_AgeGroup$Cyclone_DALYs)+as.numeric(Burden_Disease_AgeGroup$WinterStorm_DALYs)+as.numeric(Burden_Disease_AgeGroup$Drought_DALYs)+as.numeric(Burden_Disease_AgeGroup$Heatwave_DALYs)+as.numeric(Burden_Disease_AgeGroup$Sand_DALYs),2)
Burden_Disease_AgeGroup$ALL_DALYsmin=round(as.numeric(Burden_Disease_AgeGroup$Storm_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$Flood_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$Cyclone_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$WinterStorm_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$Drought_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$Heatwave_DALYsmin)+as.numeric(Burden_Disease_AgeGroup$Sand_DALYsmin),2)
Burden_Disease_AgeGroup$ALL_DALYsmax=round(as.numeric(Burden_Disease_AgeGroup$Storm_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$Flood_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$Cyclone_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$WinterStorm_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$Drought_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$Heatwave_DALYsmax)+as.numeric(Burden_Disease_AgeGroup$Sand_DALYsmax),2)


# Save
eval(parse(text = paste('write.xlsx(Burden_Disease_ALL, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Burden_Disease_ALL_',ObsWindow,'.xlsx")',sep="")))
eval(parse(text = paste('write.xlsx(Burden_Disease_AgeGroup, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Burden_Disease_AgeGroup_',ObsWindow,'.xlsx")',sep="")))


# ====================================================
#               SAVE BURDEN outcomes
# ====================================================

eval(parse(text = paste('write.xlsx(China_Event, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_',ObsWindow,'.xlsx")',sep="")))

eval(parse(text = paste('write.xlsx(df_DiseaseBurden, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/df_DiseaseBurden_',ObsWindow,'.xlsx")',sep="")))
eval(parse(text = paste('write.xlsx(df_EconomicBurden, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/df_EconomicBurden_',ObsWindow,'.xlsx")',sep="")))
eval(parse(text = paste('write.xlsx(df_EventInsuranceBurden, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/df_EventInsuranceBurden_',ObsWindow,'.xlsx")',sep="")))
eval(parse(text = paste('write.xlsx(df_InsuranceBurden, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/df_InsuranceBurden_',ObsWindow,'.xlsx")',sep="")))


