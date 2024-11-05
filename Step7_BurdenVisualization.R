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

###########################################################################
#
#                        Visualization
#
###########################################################################

# Loading files

# All info, including but not limited to Eco and Disease burdens at each regions

ChinaEvent_Enduring=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_Enduring.xlsx")
ChinaEvent_Post=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_Post.xlsx")
ChinaEvent_After=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_After.xlsx")
ChinaEvent_RTH=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_RTH.xlsx")

# Select the windows
Adpt_ObsALL=c('Enduring','Post','After','RTH')



###################################################
#                Eco burdens
###################################################

# Prepare the observation window specific burden forms
for(m in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[m]
  
  eval(parse(text = paste('China_Event=ChinaEvent_',Adpt_Obs,sep="")))
  
  df_InsuranceBurden=data.frame(Type="InsuranceBurden")
  
  df_EventInsuranceBurden=data.frame()
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
        eval(parse(text = paste('EventRowNames=rep("',Event_type,'",length(tempRowNames))',sep="")))
        
        temp_InsGroup=cbind(Event=EventRowNames,Disease=DiseaseRowNames,Insurance=Ins_type,AgeGroup=tempRowNames,Ins=Group,Inssmin=Groupmin,Insmax=Groupmax)
        eval(parse(text = paste('colnames(temp_InsGroup)=c("Event","Disease","Insurance","AgeGroup","Eco","Ecomin","Ecomax")',sep="")))
        
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
    df_EventInsuranceBurden=rbind(df_EventInsuranceBurden,EventGroup)
  }
  eval(parse(text = paste('Sankey_Eco_',Adpt_Obs,'=df_EventInsuranceBurden',sep="")))
  
}

# Summation
Sankey_Eco_ALL=Sankey_Eco_Enduring[,c("Event","Disease","Insurance","AgeGroup")]

Sankey_Eco_ALL$Eco=0
Sankey_Eco_ALL$Ecomin=0
Sankey_Eco_ALL$Ecomax=0

for(i in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[i]
  
  eval(parse(text = paste('Sankey_Eco_ALL$Eco=Sankey_Eco_ALL$Eco+as.numeric(Sankey_Eco_',Adpt_Obs,'$Eco)',sep="")))
  eval(parse(text = paste('Sankey_Eco_ALL$Ecomin=Sankey_Eco_ALL$Ecomin+as.numeric(Sankey_Eco_',Adpt_Obs,'$Ecomin)',sep="")))
  eval(parse(text = paste('Sankey_Eco_ALL$Ecomax=Sankey_Eco_ALL$Ecomax+as.numeric(Sankey_Eco_',Adpt_Obs,'$Ecomax)',sep="")))
}



# Filter
InsALL=c("PureSelf_Over_All","Self_After_Reim","Percent_Ins1","Percent_Ins2","Percent_Ins3","Percent_Ins4","Percent_Ins5","Percent_Ins7")

# Sankey dataset - 4 var
Sankey_Eco_Data=Sankey_Eco_ALL[which(!Sankey_Eco_ALL$AgeGroup=="ALL" & Sankey_Eco_ALL$Insurance %in% InsALL),]

write.xlsx(Sankey_Eco_Data, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_Data.xlsx', rowNames = FALSE)


# Prepare the Sankey 2 var dataset

# =============== Event - Disease ===============
Sankey_2var=Sankey_Eco_Data[,c("Event","Disease","Eco")]
Sankey_2var$Eco=as.numeric(Sankey_2var$Eco)
Sankey_2var=aggregate(Sankey_2var[, c("Eco")], by = Sankey_2var[, c("Event", "Disease")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","Eco")

Sankey_Eco_E_D=Sankey_2var
write.xlsx(Sankey_Eco_E_D, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_E_D.xlsx', rowNames = FALSE)


# =============== Event - Insurance ===============
Sankey_2var=Sankey_Eco_Data[,c("Event","Insurance","Eco")]
Sankey_2var$Eco=as.numeric(Sankey_2var$Eco)
Sankey_2var=aggregate(Sankey_2var[, c("Eco")], by = Sankey_2var[, c("Event", "Insurance")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","Eco")

Sankey_Eco_E_I=Sankey_2var
write.xlsx(Sankey_Eco_E_I, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_E_I.xlsx', rowNames = FALSE)


# =============== Disease - Insurance ===============
Sankey_2var=Sankey_Eco_Data[,c("Disease","Insurance","Eco")]
Sankey_2var$Eco=as.numeric(Sankey_2var$Eco)
Sankey_2var=aggregate(Sankey_2var[, c("Eco")], by = Sankey_2var[, c("Disease", "Insurance")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","Eco")

Sankey_Eco_D_I=Sankey_2var
write.xlsx(Sankey_Eco_D_I, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_D_I.xlsx', rowNames = FALSE)


# =============== Disease - Insurance ===============
Sankey_2var=Sankey_Eco_Data[,c("Disease","Insurance","Eco")]
Sankey_2var$Eco=as.numeric(Sankey_2var$Eco)
Sankey_2var=aggregate(Sankey_2var[, c("Eco")], by = Sankey_2var[, c("Disease", "Insurance")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","Eco")

Sankey_Eco_D_I=Sankey_2var
write.xlsx(Sankey_Eco_D_I, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_D_I.xlsx', rowNames = FALSE)


# =============== Disease - Age ===============
Sankey_2var=Sankey_Eco_Data[,c("Disease","AgeGroup","Eco")]
Sankey_2var$Eco=as.numeric(Sankey_2var$Eco)
Sankey_2var=aggregate(Sankey_2var[, c("Eco")], by = Sankey_2var[, c("Disease", "AgeGroup")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","Eco")

Sankey_Eco_D_A=Sankey_2var
write.xlsx(Sankey_Eco_D_A, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_D_A.xlsx', rowNames = FALSE)

# =============== Disease - Insurance - AgeGroup =============
#Sankey_Eco_D_I_A=rbind(Sankey_Eco_D_I,Sankey_Eco_I_A)
#write.xlsx(Sankey_Eco_D_I_A, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_D_I_A.xlsx', rowNames = FALSE)

# =============== Event - Disease - Insurance - AgeGroup =============
#Sankey_Eco_E_D_I_A=rbind(Sankey_Eco_E_D,Sankey_Eco_D_I,Sankey_Eco_I_A)
#write.xlsx(Sankey_Eco_E_D_I_A, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_Eco_E_D_I_A.xlsx', rowNames = FALSE)


###################################################
#                Disease burdens
###################################################

# Prepare the observation window specific burden forms
for(m in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[m]
  
  eval(parse(text = paste('China_Event=ChinaEvent_',Adpt_Obs,sep="")))
  
  df_DiseaseBurden=data.frame()
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
      eval(parse(text = paste('EventRowNames=rep("',Event_type,'",length(tempRowNames))',sep="")))
      
      temp_DiseaseGroup=cbind(Event=EventRowNames,Disease=DiseaseRowNames,AgeGroup=tempRowNames,DALYs=Group,DALYsmin=Groupmin,DALYsmax=Groupmax)
      eval(parse(text = paste('colnames(temp_DiseaseGroup)=c("Event","Disease","AgeGroup","DALYs","DALYsmin","DALYsmax")',sep="")))
      
      DiseaseGroup=rbind(DiseaseGroup,temp_DiseaseGroup)
    }
    df_DiseaseBurden=rbind(df_DiseaseBurden,DiseaseGroup)
    
  }
  eval(parse(text = paste('Sankey_DALYs_',Adpt_Obs,'=df_DiseaseBurden',sep="")))
}


# Summation
Sankey_DALYs_ALL=Sankey_DALYs_Enduring[,c("Event","Disease","AgeGroup")]

Sankey_DALYs_ALL$DALYs=0
Sankey_DALYs_ALL$DALYsmin=0
Sankey_DALYs_ALL$DALYsmax=0

for(i in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[i]
  
  eval(parse(text = paste('Sankey_DALYs_ALL$DALYs=Sankey_DALYs_ALL$DALYs+as.numeric(Sankey_DALYs_',Adpt_Obs,'$DALYs)',sep="")))
  eval(parse(text = paste('Sankey_DALYs_ALL$DALYsmin=Sankey_DALYs_ALL$DALYsmin+as.numeric(Sankey_DALYs_',Adpt_Obs,'$DALYsmin)',sep="")))
  eval(parse(text = paste('Sankey_DALYs_ALL$DALYsmax=Sankey_DALYs_ALL$DALYsmax+as.numeric(Sankey_DALYs_',Adpt_Obs,'$DALYsmax)',sep="")))
}

# Filter

# Sankey dataset - 3 var
Sankey_DALYs_Data=Sankey_DALYs_ALL[which(!Sankey_DALYs_ALL$AgeGroup=="ALL"),]

write.xlsx(Sankey_DALYs_Data, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_DALYs_Data.xlsx', rowNames = FALSE)


# Prepare the Sankey 2 var dataset

# =============== Event - Disease ===============
Sankey_2var=Sankey_DALYs_Data[,c("Event","Disease","DALYs")]
Sankey_2var$DALYs=as.numeric(Sankey_2var$DALYs)
Sankey_2var=aggregate(Sankey_2var[, c("DALYs")], by = Sankey_2var[, c("Event", "Disease")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","DALYs")

Sankey_DALYs_E_D=Sankey_2var
write.xlsx(Sankey_DALYs_E_D, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_DALYs_E_D.xlsx', rowNames = FALSE)


# =============== Disease - Age ===============
Sankey_2var=Sankey_DALYs_Data[,c("Disease","AgeGroup","DALYs")]
Sankey_2var$DALYs=as.numeric(Sankey_2var$DALYs)
Sankey_2var=aggregate(Sankey_2var[, c("DALYs")], by = Sankey_2var[, c("Disease", "AgeGroup")], FUN = sum)

colnames(Sankey_2var)=c("Src","Target","DALYs")

Sankey_DALYs_D_A=Sankey_2var
write.xlsx(Sankey_DALYs_D_A, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_DALYs_D_A.xlsx', rowNames = FALSE)


# =============== Event - Disease - Insurance - AgeGroup =============
Sankey_DALYs_E_D_A=rbind(Sankey_DALYs_E_D,Sankey_DALYs_D_A)
write.xlsx(Sankey_DALYs_E_D_A, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Sankey/Sankey_DALYs_E_D_A.xlsx', rowNames = FALSE)


