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

# Description: Visualization Script

############################################
#             Preparation
############################################

library(readxl)
library(MatchIt)
library(cobalt)
library(ggplot2)

# ============== Manipulation ======================

OrderPSMdata=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_PSM/Flood/PSM_ProfileT0_Supplementary.xlsx")

MatchC=OrderPSMdata
MatchC$Tag=0

MatchDataset=rbind(OrderPSMdata,MatchC)

#================================== PSM ==================================

# --------------- NOTE ------------------------------------------------------------------------------------------
# Confounding factors: gdp, GRP_Growth_Rate, Household_Saving,           # gdp - population averaged
#                      Population, urban, prate, AgeRatio, EDURatio, work_rate,     # Population - population density
#                      GreenRatio, Dim_Env_B, AQI, PM25, PM10, SO2, CO, NO2, O3, TempC_Target, tempmax_Target, humidity_Target,precip_Target
#                      Dim_Hea_A, Health_Cost
#     Eq= gdp+GRP_Growth_Rate+Household_Saving+  
#         Population+urban+prate+AgeRatio+EDURatio+work_rate+  
#         GreenRatio+Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+TempC_Target+tempmax_Target+humidity_Target+precip_Target+
#         Dim_Hea_A+Health_Cost

#     Storm:           gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
#     Flood:           gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
#     Cyclone:         gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+Dim_Hea_A+Health_Cost
#     WinterStorm:     gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
#     Sand:            gdp+GRP_Growth_Rate+Household_Saving+    Population+urban+prate+AgeRatio+work_rate+EDURatio   +tempmax_Target  +Dim_Hea_A
#     Drought:         gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+   Dim_Hea_A+Health_Cost
#     Heatwave:        gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+humidity_Target+precip_Target   +Dim_Hea_A+Health_Cost

# ---------------------------------------------------------------------------------------------------------

No_controlCounty=4

m = matchit(Tag~ gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost,
            data=MatchDataset,       # OrderPSMdata, MatchDataset
            distance='logit',
            method='nearest',
            replace=TRUE,
            ratio=No_controlCounty)

summary(m)

No_Control=length(unique(as.numeric(c(m$match.matrix[,1], m$match.matrix[,2], m$match.matrix[,3], m$match.matrix[,4])) %% nrow(OrderPSMdata)))
print(No_Control)

matched_data=match.data(m)

# --------------------- Plot the covariate ----------------------------
love.plot(m, binary = "std", thresholds = c(m = .1))+
  xlab("Standardized mean differences")
v_name=data.frame(old=c("distance","gdp", "GRP_Growth_Rate", "Household_Saving",
                        "Population", "urban", "prate", "AgeRatio", "EDURatio", "work_rate",
                        "GreenRatio", "Dim_Env_B", "AQI", "PM25", "PM10", "SO2", "CO", "NO2", "O3", "TempC_Target", "tempmax_Target", "humidity_Target","precip_Target",
                        "Dim_Hea_A", "Health_Cost"),
                  new=c("Propensity score","GRP","GRP growth rate","Household saving",
                        "Population density","Urbanlization","Percentage male","Percentage the elderly","Education attainment","Employment rate",
                        "Greening ratio","Transportation","AQI","PM2.5","PM10", "SO2", "CO", "NO2", "O3","Mean temperature","Maximum temperature","Humidity","Precipitation",
                        "Healthcare system","Health expenditure"))

love.plot(m, binary = "std", 
          thresholds = c(m = .1),
          var.order = "unadjusted",
          var.names = v_name,
          sample.names = c("Unmatched","Matched"),
          position = "top",
          colors = c('red','blue'),
          alpha = 0.5)+
  xlab("Standardized mean differences")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold",size = 16),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth =1),
    axis.text = element_text(size = 14),
    legend.position = "top"
  )+
  labs(title="")

# Save the PSM figure
eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_PSM/',Event_type,'/CountyT0_SMD_Supplementary.png", width = 8, height = 6, dpi = 600)',sep="")))

# -------------------- Kernel density plot ---------------------
bal.plot(m, var.name = "distance", 
         which = "both", 
         type = "density", 
         mirror = FALSE,
         sample.names = c("Unmatched","Matched"),
         position = "top",
         colors = c('red','blue'),
         alpha = 0.5)+
  xlab("Propensity score") +
  ylab("Kernel density")+
  labs(title="")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold",size = 16),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth =1),
    axis.text = element_text(size = 14),
    legend.position = "top"
  )
# Save the PSM figure
eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_PSM/',Event_type,'/CountyT0_Density_Supplementary.png", width = 8, height = 6, dpi = 600)',sep="")))

# Save the PSM file
eval(parse(text = paste('write.xlsx(OrderPSMdata,"/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_PSM//',Event_type,'/PSM_ProfileT0_Supplementary.xlsx", rowNames = FALSE)',sep="")))




