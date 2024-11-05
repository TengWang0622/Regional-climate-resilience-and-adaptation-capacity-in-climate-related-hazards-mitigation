
#                                   P   R   O   J   E   C   T
#                                               of
#     Assessing Regional Climate Resilience and Adaption Capacity to Climate-related Hazards in China: 
#                      Cause-specific Hospitalizations and Related burdens


Dear Users,

The list below shows the detailed information of the scripts for data prepartion, PSM-DID statistial exmination, risk analysis, result post processing and visualization. The analyses used R version 4.4.1, Matlab R2023a. 

All enquires are welcome. For further collabrations please contact us.


Dr. Teng Wang
wang.teng19@alumni.imperial.ac.uk
School of Public Health, Peking University
Department of Civil Engineering, The University of Hong Kong

Dr. Hanxu Shi
shx@bjmu.edu.cn
School of Public Health, Peking University

Dr. Zhenyu Zhang
zzy@pku.edu.cn
School of Public Health, Peking University


# Main Script:


Step1_ResilienceMetrics.R
Description: Build the regional climate resilience evaluation framework; Monte Carlo simulations
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step1_ResilienceMetrics.R

Step2_PSM-DID_Analysis.R
Description: Build the propensity socre matching aided difference-in-differences framework and conduct the hazard impact simulations 
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step2_PSM-DID_Analysis.R

Step3_ResultOutput.R
Description: Output the PSM-DID estimates; Parallel trend examination 
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step3_ResultOutput.R

Step4_AdaptationCapacity.R
Description: Compute the adaptation capacity
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step4_AdaptationCapacity.R

Step5_BurdenCalculation.R
Description: Compute the population attributable fraction (PAF), economic and disease burdens at each observation period
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step5_BurdenCalculation.R

Step6_BurdenCombination.R
Description: Combine the burdens at each observation window to build the overall summary dataframe
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step6_BurdenCombination.R

Step7_BurdenVisualization.R
Description: Build the particular data frames to visualize the burden components - contributions from climate hazard, disease, and population groups
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step7_BurdenVisualization.R

Step8_BurdenSummary.R
Description: Extract the key infomation of the hazard, disease, age group specific PAF, disease and economic burdens
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Step8_BurdenSummary.R

# Supplementary Visualization Script:

Supplementary_PSM_Examination.R
Description: Propensity score matching examination; Standardized mean differences and kernel density distribution before and after matching
Path: ~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Manuscript/Print/Submit/Code/Supplementary_PSM_Examination.R

Supplementary_Main_Fig4_Burden.m

Supplementary_ED_Fig3_LidarBurden.m

Supplementary_ED_Fig4_AgeBurden.m

Supplementary_ED_Fig5_SankeyBurden

Supplementary_SI_Fig1_MonteCarlo.m

Supplementary_SI_Fig3_4_PSM_Examination.R

# Customized MATLAB functions

Bar95CI.m

ConfidenceEllipse.m



