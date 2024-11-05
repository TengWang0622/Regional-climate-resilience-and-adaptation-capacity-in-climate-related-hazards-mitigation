% =======================================================================================================
%
%                                    P   R   O   J   E   C   T
%                                                of
%   Assessing Regional Climate Resilience and Adaption Capacity to Climate-related Hazards in China: 
%                  Cause-specific Hospitalizations and Related burdens
%
% =======================================================================================================

%% Developed by Teng Wang, Hanxu Shi, Zhenyu Zhang

% Contact: wang.teng19@alumni.imperial.ac.uk
%          shx@bjmu.edu.cn

% Version - 20240325

% Description: Figure Script


% Developed by Teng and Hanxu

% Contact: wang.teng19@alumni.imperial.ac.uk
%          shx@bjmu.edu.cn

% Version: 24-08-08


%% Preparation
clc
clear
close all

% Image settings
figure_fontsize=18;
set(0,'defaultfigurecolor','w')

% Loading files

Path='~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Burden/Final/Summary_China_Event_ALL.xlsx';
Summary_China_Event_ALL=readtable(Path);

% Variables
x=Summary_China_Event_ALL.DALYs_pd_HW_Excluded;
y = Summary_China_Event_ALL.Eco_pd_HW_Excluded;
bubbleSizes = Summary_China_Event_ALL.Pop;
cityTier = Summary_China_Event_ALL.City_Tier;

RowT1=find(cityTier==1);
RowT2=find(cityTier==2);
RowT3=find(cityTier==3);

color1=[255,0,0]/255;
color2=[255,128,0]/255;
color3=[0,128,255]/255;

Val_Alpha=0.4;

PPP=1/4.208;      % Consider currancy transfer - purchasing power parties (PPP)
IR=5.88*0.01;     % Inflation rate
Ratio=(1+IR)*PPP; % considering the PPP and inflation rate, if you want
Ratio=0.14;       % Transfer CNY to USD

%% -------------- 95 Confidence interval for Economic and Disease burdens ---------------

ALL_No_HW=Confidence95(Summary_China_Event_ALL.DALYs_pd_HW_Excluded, Summary_China_Event_ALL.Eco_pd_HW_Excluded*Ratio,RowT1,RowT2,RowT3);

Storm95CI=Confidence95(Summary_China_Event_ALL.DALYs_Storm_pd, Summary_China_Event_ALL.Eco_Storm_pd*Ratio,RowT1,RowT2,RowT3);
Flood95CI=Confidence95(Summary_China_Event_ALL.DALYs_Flood_pd, Summary_China_Event_ALL.Eco_Flood_pd*Ratio,RowT1,RowT2,RowT3);
Cyclone95CI=Confidence95(Summary_China_Event_ALL.DALYs_Cyclone_pd, Summary_China_Event_ALL.Eco_Cyclone_pd*Ratio,RowT1,RowT2,RowT3);
WinterStorm95CI=Confidence95(Summary_China_Event_ALL.DALYs_WinterStorm_pd, Summary_China_Event_ALL.Eco_WinterStorm_pd*Ratio,RowT1,RowT2,RowT3);

Sand95CI=Confidence95(Summary_China_Event_ALL.DALYs_Sand_pd, Summary_China_Event_ALL.Eco_Sand_pd*Ratio,RowT1,RowT2,RowT3);
Drought95CI=Confidence95(Summary_China_Event_ALL.DALYs_Drought_pd, Summary_China_Event_ALL.Eco_Drought_pd*Ratio,RowT1,RowT2,RowT3);
Heatwave95CI=Confidence95(Summary_China_Event_ALL.DALYs_Heatwave_pd, Summary_China_Event_ALL.Eco_Heatwave_pd*Ratio,RowT1,RowT2,RowT3);

ALL95CI=Confidence95(Summary_China_Event_ALL.DALYs_pd, Summary_China_Event_ALL.Eco_pd*Ratio,RowT1,RowT2,RowT3);

Summary_95CI=[ALL95CI,Storm95CI,Flood95CI,Cyclone95CI,WinterStorm95CI,Sand95CI,Drought95CI,Heatwave95CI,ALL_No_HW];



%% --------------- 100,0000 Population-, exposure day- averaged ---------------

% All without HW
ConfidenceEllipse(1,Summary_China_Event_ALL.DALYs_pd_HW_Excluded*100000, ...
    Summary_China_Event_ALL.Eco_pd_HW_Excluded*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.2])
ylim([0,25000])
set(gca,'fontsize',14,'fontname','times new roman')
%set(gca,'fontsize',14,'fontname','Calibri')

% Storm
ConfidenceEllipse(2,Summary_China_Event_ALL.DALYs_Storm_pd*100000, ...
    Summary_China_Event_ALL.Eco_Storm_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,Inf])
ylim([0,Inf])
set(gca,'fontsize',14,'fontname','times new roman')

% Flood
ConfidenceEllipse(3,Summary_China_Event_ALL.DALYs_Flood_pd*100000, ...
    Summary_China_Event_ALL.Eco_Flood_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.25])
ylim([0,25000])
set(gca,'fontsize',14,'fontname','times new roman')

% Cyclone
ConfidenceEllipse(4,Summary_China_Event_ALL.DALYs_Cyclone_pd*100000, ...
    Summary_China_Event_ALL.Eco_Cyclone_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.25])
ylim([0,30000])
set(gca,'fontsize',14,'fontname','times new roman')

% WinterStorm
ConfidenceEllipse(5,Summary_China_Event_ALL.DALYs_WinterStorm_pd*100000, ...
    Summary_China_Event_ALL.Eco_WinterStorm_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.25])
ylim([0,30000])
set(gca,'fontsize',14,'fontname','times new roman')

% Drought
ConfidenceEllipse(6,Summary_China_Event_ALL.DALYs_Drought_pd*100000, ...
    Summary_China_Event_ALL.Eco_Drought_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.25])
ylim([0,30000])
set(gca,'fontsize',14,'fontname','times new roman')

% Heatwave
ConfidenceEllipse(7,Summary_China_Event_ALL.DALYs_Heatwave_pd*100000, ...
    Summary_China_Event_ALL.Eco_Heatwave_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,Inf])
ylim([0,Inf])
set(gca,'fontsize',14,'fontname','times new roman')

% Sand
ConfidenceEllipse(8,Summary_China_Event_ALL.DALYs_Sand_pd*100000, ...
    Summary_China_Event_ALL.Eco_Sand_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.25])
ylim([0,30000])
set(gca,'fontsize',14,'fontname','times new roman')

% ALL
ConfidenceEllipse(9,Summary_China_Event_ALL.DALYs_pd*100000, ...
    Summary_China_Event_ALL.Eco_pd*Ratio*100000, ...
    Summary_China_Event_ALL.Pop, ...
    Summary_China_Event_ALL.City_Tier);
xlim([0,0.2])
ylim([0,25000])
set(gca,'fontsize',14,'fontname','times new roman')




figure(1000)

% Population=10000000, Size=100;
scatter(1,15,300,'filled','MarkerEdgeColor','none','MarkerFaceColor','k','MarkerFaceAlpha',1)
hold on
scatter(1,14,200,'filled','MarkerEdgeColor','none','MarkerFaceColor','k','MarkerFaceAlpha',1)
hold on
scatter(1,13,100,'filled','MarkerEdgeColor','none','MarkerFaceColor','k','MarkerFaceAlpha',1)
hold on

scatter(1,12,200,'filled','MarkerEdgeColor','k','MarkerFaceColor','none','MarkerFaceAlpha',1,'linewidth',2)
hold on

scatter(1,10,200,'filled','MarkerEdgeColor','none','MarkerFaceColor',color1,'MarkerFaceAlpha',Val_Alpha)
hold on
scatter(1,9,200,'filled','MarkerEdgeColor','none','MarkerFaceColor',color2,'MarkerFaceAlpha',Val_Alpha)
hold on
scatter(1,8,200,'filled','MarkerEdgeColor','none','MarkerFaceColor',color3,'MarkerFaceAlpha',Val_Alpha)
hold on

ylim([5,20])





