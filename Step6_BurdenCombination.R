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


InsuranceALL=c('Self_Over_All','Reim_Over_All','PureSelf_Over_All','Self_After_Reim','PartReim_Over_All','Percent_Ins0','Percent_Ins1','Percent_Ins2','Percent_Ins3','Percent_Ins4','Percent_Ins5','Percent_Ins7','Reim_Over_All')
DiseaseALL=c('Malnutrition','Infectious','Injuries','Respiratory','Mental','Neoplasms','Endocrine','Blood','Neurology','CVD','Genitourinary')
EventALL=c('Storm', 'Flood', 'Cyclone', 'WinterStorm', 'Drought', 'Heatwave', 'Sand')

########################################################################################################
#                           Combine all the Burden data frames, PAF estimation
########################################################################################################

# Loading files

# All info, including but not limited to Eco and Disease burdens at each regions
ChinaEvent_Enduring=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_Enduring.xlsx")
ChinaEvent_Post=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_Post.xlsx")
ChinaEvent_After=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_After.xlsx")
ChinaEvent_RTH=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/ChinaEvent_RTH.xlsx")

# Select the windows
Adpt_ObsALL=c('Enduring','Post','After','RTH')

for(m in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[m]
  
  eval(parse(text = paste('China_Event=ChinaEvent_',Adpt_Obs,sep="")))
  
  # ==========================================================
  #                   Fill the Burden forms 
  # ==========================================================
  
  
  China_Event$DALYs=0
  China_Event$DALYsmin=0
  China_Event$DALYsmax=0
  
  China_Event$Eco=0
  China_Event$Ecomin=0
  China_Event$Ecomax=0
  
  China_Event$PAF=0
  
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
      
      eval(parse(text = paste('China_Event$PAF=China_Event$PAF+as.numeric(China_Event$PAF_',Event_type,'_',Disease,')',sep="")))
    }
  }
  
  # Compute the hazard- and disease- specific Disease and Economic burdens
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmin_',Event_type,'=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmax_',Event_type,'=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomin_',Event_type,'=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomax_',Event_type,'=0',sep="")))
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmin_',Event_type,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmax_',Event_type,'_pd=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomin_',Event_type,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomax_',Event_type,'_pd=0',sep="")))
  }
  
  for(j in 1:length(DiseaseALL)){
    Disease=DiseaseALL[j]
    eval(parse(text = paste('China_Event$DALYs_',Disease,'=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmin_',Disease,'=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmax_',Disease,'=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Disease,'=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomin_',Disease,'=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomax_',Disease,'=0',sep="")))
    
    eval(parse(text = paste('China_Event$DALYs_',Disease,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmin_',Disease,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$DALYsmax_',Disease,'_pd=0',sep="")))
    
    eval(parse(text = paste('China_Event$Eco_',Disease,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomin_',Disease,'_pd=0',sep="")))
    eval(parse(text = paste('China_Event$Ecomax_',Disease,'_pd=0',sep="")))
  }
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      eval(parse(text = paste('China_Event$DALYs_',Event_type,'=China_Event$DALYs_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('China_Event$DALYsmin_',Event_type,'=China_Event$DALYsmin_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('China_Event$DALYsmax_',Event_type,'=China_Event$DALYsmax_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Event_type,'=China_Event$Eco_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('China_Event$Ecomin_',Event_type,'=China_Event$Ecomin_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('China_Event$Ecomax_',Event_type,'=China_Event$Ecomax_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('China_Event$DALYs_',Disease,'=China_Event$DALYs_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('China_Event$DALYsmin_',Disease,'=China_Event$DALYsmin_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('China_Event$DALYsmax_',Disease,'=China_Event$DALYsmax_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('China_Event$Eco_',Disease,'=China_Event$Eco_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('China_Event$Ecomin_',Disease,'=China_Event$Ecomin_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('China_Event$Ecomax_',Disease,'=China_Event$Ecomax_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
    }
  }
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    
    eval(parse(text = paste('China_Event$DALYs_',Event_type,'_pd=China_Event$DALYs_',Event_type,'/China_Event$Pop/China_Event$Days_',Event_type,sep="")))
    eval(parse(text = paste('China_Event$Eco_',Event_type,'_pd=China_Event$Eco_',Event_type,'/China_Event$Pop/China_Event$Days_',Event_type,sep="")))
  }
  
  # ================================================================================================================
  #                       Compute the sensitive disease overall PAF for each sub region 
  # ================================================================================================================
  
  # Note: the Hazard- and Disease- specific PAF considering the population size and groups are already computed at the PAF section
  #       The codes below estimate the overall PAF for all climate sensitive disease, with local population size and age distribution and specific incidence rate considered
  China_Event$PAF=0
  
  for(i in 1:nrow(China_Event)){
    
    Temp_PopIncPAF=0
    Temp_PopInc=0
    
    for(l in 1:length(DiseaseALL)){
      Disease=DiseaseALL[l]
      
      eval(parse(text = paste('Temp_PopIncPAF=Temp_PopIncPAF+China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*China_Event$PAF_',Disease,'[i]+
                            China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*China_Event$PAF_',Disease,'[i]+
                            China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*China_Event$PAF_',Disease,'[i]+
                            China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*China_Event$PAF_',Disease,'[i]',sep="")))
      eval(parse(text = paste('Temp_PopInc=Temp_PopInc+China_Event$Age18[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])+
                            China_Event$Age19_44[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])+
                            China_Event$Age45_64[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])+
                            China_Event$Age65[i]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])',sep="")))
    }
    
    China_Event$PAF[i]=Temp_PopIncPAF/Temp_PopInc
  }
  
  # ===================================================================
  #       Select the key info and save in Burden_Overview 
  #====================================================================
  Burden_Overview=cbind(City=China_Event$City,City_Tier=China_Event$City_Tier,Pop=China_Event$Pop,Event_Sum=China_Event$No_Sum,Days_Sum=China_Event$Days_Sum,
                        DALYs_person_event=China_Event$DALYs/China_Event$Pop/China_Event$No_Sum,Eco_person_event=China_Event$Eco/China_Event$Pop/China_Event$No_Sum,
                        DALYs_person_day=China_Event$DALYs/China_Event$Pop/China_Event$Days_Sum,Eco_person_day=China_Event$Eco/China_Event$Pop/China_Event$Days_Sum,
                        DALYs=China_Event$DALYs,DALYsmin=China_Event$DALYsmin,DALYsmax=China_Event$DALYsmax,
                        Eco=China_Event$Eco,Ecomin=China_Event$Ecomin,Ecomax=China_Event$Ecomax)
  
  Burden_Overview=as.data.frame(Burden_Overview)
  
  
  # =========== SAVE =========== 
  
  # Note: The Summary_China_Event_Adpt_Obs contains the advanced info at each region for each observation window
  
  eval(parse(text = paste('write.xlsx(China_Event, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_',Adpt_Obs,'.xlsx")',sep="")))
  eval(parse(text = paste('write.xlsx(Burden_Overview, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Burden_Overview_',Adpt_Obs,'.xlsx")',sep="")))
}

# =======================================================================
#                         Burden combination
# =======================================================================

# Combine all the analysed Burden summary files -- Summary_China_Event_Adpt_Obs, where Adpt_Obs='Enudring','Post','After','RTH'

# Loading files
Summary_China_Event_Enduring=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_Enduring.xlsx")
Summary_China_Event_Post=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_Post.xlsx")
Summary_China_Event_After=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_After.xlsx")
Summary_China_Event_RTH=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_RTH.xlsx")


# Take the Enduring form as a template and update the values accordingly
Summary_China_Event_ALL=Summary_China_Event_Enduring[,1:9]

Summary_China_Event_ALL$PAF=0

Summary_China_Event_ALL$DALYs=0
Summary_China_Event_ALL$DALYsmin=0
Summary_China_Event_ALL$DALYsmax=0

Summary_China_Event_ALL$Eco=0
Summary_China_Event_ALL$Ecomin=0
Summary_China_Event_ALL$Ecomax=0


Summary_China_Event_ALL$Days_Sum=0
for(j in 1:length(EventALL)){
  Event_type=EventALL[j]
  eval(parse(text = paste('Summary_China_Event_ALL$Days_',Event_type,'=0',sep="")))
  
}

for(j in 1:length(EventALL)){
  Event_type=EventALL[j]
  
  eval(parse(text = paste('Summary_China_Event_ALL$PAF_',Event_type,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Event_type,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Event_type,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Event_type,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Event_type,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Event_type,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Event_type,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Event_type,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Event_type,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Event_type,'_pd=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Event_type,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Event_type,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Event_type,'_pd=0',sep="")))
}

for(j in 1:length(DiseaseALL)){
  Disease=DiseaseALL[j]
  
  eval(parse(text = paste('Summary_China_Event_ALL$PAF_',Disease,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Disease,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Disease,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Disease,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Disease,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Disease,'=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Disease,'=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Disease,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Disease,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Disease,'_pd=0',sep="")))
  
  eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Disease,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Disease,'_pd=0',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Disease,'_pd=0',sep="")))
}


# Upadate the data frame
for(i in 1:length(Adpt_ObsALL)){
  Adpt_Obs=Adpt_ObsALL[i]
  
  eval(parse(text = paste('China_Event=Summary_China_Event_',Adpt_Obs,sep="")))
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    
    eval(parse(text = paste('Summary_China_Event_ALL$Days_',Event_type,'=Summary_China_Event_ALL$Days_',Event_type,'+Summary_China_Event_',Adpt_Obs,'$Days_',Event_type,sep="")))
    
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      eval(parse(text = paste('Summary_China_Event_ALL$DALYs=Summary_China_Event_ALL$DALYs+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin=Summary_China_Event_ALL$DALYsmin+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax=Summary_China_Event_ALL$DALYsmax+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('Summary_China_Event_ALL$Eco=Summary_China_Event_ALL$Eco+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomin=Summary_China_Event_ALL$Ecomin+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomax=Summary_China_Event_ALL$Ecomax+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      #eval(parse(text = paste('Summary_China_Event_ALL$PAF=Summary_China_Event_ALL$PAF+as.numeric(China_Event$PAF_',Event_type,'_',Disease,')',sep="")))
    }
  }
  
  
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Event_type,'=Summary_China_Event_ALL$DALYs_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Event_type,'=Summary_China_Event_ALL$DALYsmin_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Event_type,'=Summary_China_Event_ALL$DALYsmax_',Event_type,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Event_type,'=Summary_China_Event_ALL$Eco_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Event_type,'=Summary_China_Event_ALL$Ecomin_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Event_type,'=Summary_China_Event_ALL$Ecomax_',Event_type,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Disease,'=Summary_China_Event_ALL$DALYs_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Disease,'=Summary_China_Event_ALL$DALYsmin_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Disease,'=Summary_China_Event_ALL$DALYsmax_',Disease,'+China_Event$DALYs_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
      eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Disease,'=Summary_China_Event_ALL$Eco_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALL',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Disease,'=Summary_China_Event_ALL$Ecomin_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmin',sep="")))
      eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Disease,'=Summary_China_Event_ALL$Ecomax_',Disease,'+China_Event$Eco_',Event_type,'_',Disease,'_ALLmax',sep="")))
      
    }
  }
  
  
  # -------------------------------- China_Event$PAF_Event_type -----------------------------------------
  for(l in 1:nrow(China_Event)){
    for(j in 1:length(EventALL))
      Event_type=EventALL[j]
    
    eval(parse(text = paste('China_Event$PAF_',Event_type,'[l]=0',sep="")))
    
    
    Temp_PopIncPAF_acc=0
    Temp_PopInc_acc=0
    
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      eval(parse(text = paste('Temp_PopIncPAF_acc=Temp_PopIncPAF_acc+China_Event$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[l])+
                                                                      +China_Event$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[l])+
                                                                      +China_Event$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[l])+
                                                                      +China_Event$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*as.numeric(China_Event$PAF_',Event_type,'_',Disease,'[l])',sep="")))
      
      eval(parse(text = paste('Temp_PopInc_acc=Temp_PopInc_acc+China_Event$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])+
                                       +China_Event$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])+
                                       +China_Event$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])+
                                       +China_Event$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])',sep="")))
      
    }
    
    
    eval(parse(text = paste('China_Event$PAF_',Event_type,'[l]=Temp_PopIncPAF_acc/Temp_PopInc_acc',sep="")))
    
    
  }
  
  
  # -------------------------------- Summary_China_Event_ALL$PAF_Disease/Event_type = sum(China_Event$PAF_Disease/Event_type) -----------------------------------------
  for(j in 1:length(EventALL)){
    Event_type=EventALL[j]
    eval(parse(text = paste('Summary_China_Event_ALL$PAF_',Event_type,'=Summary_China_Event_ALL$PAF_',Event_type,'+China_Event$PAF_',Event_type,sep="")))
  }
  
  for(k in 1:length(DiseaseALL)){
    Disease=DiseaseALL[k]
    eval(parse(text = paste('Summary_China_Event_ALL$PAF_',Disease,'=Summary_China_Event_ALL$PAF_',Disease,'+China_Event$PAF_',Disease,sep="")))
  }
  
  # -------------------------------- China_Event$PAF -----------------------------------------
  for(l in 1:nrow(China_Event)){
    
    Temp_PopIncPAF_acc=0
    Temp_PopInc_acc=0
    
    for(k in 1:length(DiseaseALL)){
      Disease=DiseaseALL[k]
      
      eval(parse(text = paste('Temp_PopIncPAF_acc=Temp_PopIncPAF_acc+China_Event$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*as.numeric(China_Event$PAF_',Disease,'[l])+
        +China_Event$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*as.numeric(China_Event$PAF_',Disease,'[l])+
        +China_Event$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*as.numeric(China_Event$PAF_',Disease,'[l])+
        +China_Event$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*as.numeric(China_Event$PAF_',Disease,'[l])',sep="")))
      
      eval(parse(text = paste('Temp_PopInc_acc=Temp_PopInc_acc+China_Event$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])+
        +China_Event$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])+
        +China_Event$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])+
        +China_Event$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])',sep="")))
    }
    China_Event$PAF[l]=Temp_PopIncPAF_acc/Temp_PopInc_acc
  }
  
  #eval(parse(text = paste('Summary_China_Event_ALL$PAF=Summary_China_Event_ALL$PAF+Summary_China_Event_',Adpt_Obs,'$PAF',sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$PAF=Summary_China_Event_ALL$PAF+China_Event$PAF',sep="")))
}

Summary_China_Event_ALL$Days_Sum=Summary_China_Event_ALL$Days_Storm+Summary_China_Event_ALL$Days_Flood+Summary_China_Event_ALL$Days_Cyclone+Summary_China_Event_ALL$Days_WinterStorm+
  Summary_China_Event_ALL$Days_Drought+Summary_China_Event_ALL$Days_Heatwave+Summary_China_Event_ALL$Days_Sand


for(j in 1:length(EventALL)){
  Event_type=EventALL[j]
  
  eval(parse(text = paste('Summary_China_Event_ALL$DALYs_',Event_type,'_pd=Summary_China_Event_ALL$DALYs_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmax_',Event_type,'_pd=Summary_China_Event_ALL$DALYsmax_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$DALYsmin_',Event_type,'_pd=Summary_China_Event_ALL$DALYsmin_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  
  
  eval(parse(text = paste('Summary_China_Event_ALL$Eco_',Event_type,'_pd=Summary_China_Event_ALL$Eco_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomax_',Event_type,'_pd=Summary_China_Event_ALL$Ecomax_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  eval(parse(text = paste('Summary_China_Event_ALL$Ecomin_',Event_type,'_pd=Summary_China_Event_ALL$Ecomin_',Event_type,'/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_',Event_type,sep="")))
  
}

# Compute the daily and population averaged Eco and Disease burdens
Summary_China_Event_ALL$DALYs_pd=Summary_China_Event_ALL$DALYs/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_Sum
Summary_China_Event_ALL$Eco_pd=Summary_China_Event_ALL$Eco/Summary_China_Event_ALL$Pop/Summary_China_Event_ALL$Days_Sum

Summary_China_Event_ALL$DALYs_pd_HW_Excluded=(Summary_China_Event_ALL$DALYs-Summary_China_Event_ALL$DALYs_Heatwave)/(Summary_China_Event_ALL$Days_Sum-Summary_China_Event_ALL$Days_Heatwave)/Summary_China_Event_ALL$Pop
Summary_China_Event_ALL$Eco_pd_HW_Excluded=(Summary_China_Event_ALL$Eco-Summary_China_Event_ALL$Eco_Heatwave)/(Summary_China_Event_ALL$Days_Sum-Summary_China_Event_ALL$Days_Heatwave)/Summary_China_Event_ALL$Pop

# ====================================================================
#         Compute the overall PAF for the whole country - China
# ====================================================================

Temp_PopIncPAF=0
Temp_PopInc=0

for(i in 1:nrow(Summary_China_Event_ALL)){
  
  for(l in 1:length(DiseaseALL)){
    Disease=DiseaseALL[l]
    
    eval(parse(text = paste('Temp_PopIncPAF=Temp_PopIncPAF+Summary_China_Event_ALL$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])*Summary_China_Event_ALL$PAF_',Disease,'[l]+
                            Summary_China_Event_ALL$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])*Summary_China_Event_ALL$PAF_',Disease,'[l]+
                            Summary_China_Event_ALL$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])*Summary_China_Event_ALL$PAF_',Disease,'[l]+
                            Summary_China_Event_ALL$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])*Summary_China_Event_ALL$PAF_',Disease,'[l]',sep="")))
    eval(parse(text = paste('Temp_PopInc=Temp_PopInc+Summary_China_Event_ALL$Age18[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age18"])+
                            Summary_China_Event_ALL$Age19_44[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age19_44"])+
                            Summary_China_Event_ALL$Age45_64[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age45_64"])+
                            Summary_China_Event_ALL$Age65[l]*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="',Disease,'"),"Age65"])',sep="")))
  }
}

PAF_China=Temp_PopIncPAF/Temp_PopInc
print(paste('The overall PAF in China = ',PAF_China,sep=""))   # -------------------------------------------- OVERALL PAF !

# ------- SAVE -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Note: the Summary_China_Event_ALL contains the ultimate version of all the regional info in China
#       All event types, all disease types, all population size and groups, all the insurance details, are saved

eval(parse(text = paste('write.xlsx(Summary_China_Event_ALL, "~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_ALL.xlsx")',sep="")))

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------










