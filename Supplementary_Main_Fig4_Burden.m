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
set(0,'defaultfigurecolor','w')

%% Main Fig. 4

%Heatwave:    #FF007F  [255 0 127]/255
%Drought:     #FF8000  [255 128 0]/255
%Flood:       #7F00FF  [127 0 255]/255
%Storm:       #0000FF  [0 0 255]/255
%WinterStorm: #0080FF  [0 128 255]/255
%Cyclone:     #008080  [0 128 128]/255
%Sand:        #FFFF00  [255 255 0]/255

%% Default values

MarkerSize=300;
BarWidth=0.4;
Alpha=0.5;

% Color of hazards --------------------------------------

Color_Heatwave=[255 0 0]/255;
Color_Drought=[255 0 0]/255;
Color_Sand=[255,0,0]/255;

Color_Flood=[0 0 255]/255;
Color_Storm=[0 0 255]/255;
Color_WinterStorm=[0 0 255]/255;
Color_Cyclone=[0 0 255]/255;


% Color of disease ---------------------------------------


Color_CVD=[127 0 255]/255;
Color_Injuries=[127 0 255]/255;
Color_Infectious=[127 0 255]/255;
Color_Respiratory=[127 0 255]/255;
Color_Neurology=[127 0 255]/255;
Color_Mental=[127 0 255]/255;
Color_Genitourinary=[127 0 255]/255;
Color_Endocrine=[127 0 255]/255;
Color_Neoplasms=[127 0 255]/255;
Color_Blood=[127 0 255]/255;
Color_Malnutrition=[127 0 255]/255;

%% Alpha

Alpha_Heatwave=0.6;
Alpha_Drought=0.3;
Alpha_Sand=0.1;

Alpha_Flood=0.7;
Alpha_Storm=0.5;
Alpha_WinterStorm=0.3;
Alpha_Cyclone=0.1;

Alpha_CVD=0.8;
Alpha_Injuries=0.7;
Alpha_Infectious=0.6;
Alpha_Respiratory=0.5;
Alpha_Neurology=0.4;
Alpha_Mental=0.35;
Alpha_Genitourinary=0.3;
Alpha_Endocrine=0.25;
Alpha_Neoplasms=0.2;
Alpha_Blood=0.15;
Alpha_Malnutrition=0.1;


%% PAF

Bar95CI(11,37.5,BarWidth,0.77,0.77,0.77,Color_Heatwave,Alpha_Heatwave,MarkerSize,'horizontal')   % Heatwave
Bar95CI(11,35.5,BarWidth,1.40,1.40,1.40,Color_Drought,Alpha_Drought,MarkerSize,'horizontal')   % Drought
Bar95CI(11,33.5,BarWidth,0.014,0.014,0.014,Color_Sand,Alpha_Sand,MarkerSize,'horizontal')% Sand

Bar95CI(11,31.5,BarWidth,0.43,0.43,0.43,Color_Flood,Alpha_Flood,MarkerSize,'horizontal') % Flood
Bar95CI(11,29.5,BarWidth,0.24,0.24,0.24,Color_Storm,Alpha_Storm,MarkerSize,'horizontal')   % Storm
Bar95CI(11,27.5,BarWidth,0.36,0.36,0.36,Color_WinterStorm,Alpha_WinterStorm,MarkerSize,'horizontal')   % WinterStorm
Bar95CI(11,25.5,BarWidth,0.20,0.20,0.20,Color_Cyclone,Alpha_Cyclone,MarkerSize,'horizontal') % Cyclone


Bar95CI(11,21.5,BarWidth,0.37,0.37,0.37, ...
    Color_CVD,Alpha_CVD,MarkerSize,'horizontal') 
Bar95CI(11,19.5,BarWidth,0.24,0.24,0.24, ...
    Color_Injuries,Alpha_Injuries,MarkerSize,'horizontal') 
Bar95CI(11,17.5,BarWidth,0.45,0.45,0.45, ...
    Color_Infectious,Alpha_Infectious,MarkerSize,'horizontal')
Bar95CI(11,15.5,BarWidth,0.58,0.58,0.58, ...
    Color_Respiratory,Alpha_Respiratory,MarkerSize,'horizontal')
Bar95CI(11,13.5,BarWidth,0.11,0.11,0.11, ...
    Color_Neurology,Alpha_Neurology,MarkerSize,'horizontal')   
Bar95CI(11,11.5,BarWidth,0.19,0.19,0.19, ...
    Color_Mental,Alpha_Mental,MarkerSize,'horizontal')   
Bar95CI(11,9.5,BarWidth,0.11,0.11,0.11, ...
    Color_Genitourinary,Alpha_Genitourinary,MarkerSize,'horizontal') 
Bar95CI(11,7.5,BarWidth,0.15,0.15,0.15, ...
    Color_Endocrine,Alpha_Endocrine,MarkerSize,'horizontal') 
Bar95CI(11,5.5,BarWidth,0.10,0.10,0.10, ...
    Color_Neoplasms,Alpha_Neoplasms,MarkerSize,'horizontal') 
Bar95CI(11,3.5,BarWidth,0.03,0.03,0.03, ...
    Color_Blood,Alpha_Blood,MarkerSize,'horizontal') 
Bar95CI(11,1.5,BarWidth,1.69728089481823e-03,1.69728089481823e-03,1.69728089481823e-03, ...
    Color_Malnutrition,Alpha_Malnutrition,MarkerSize,'horizontal') 


xlabel('%')
xlim([0,1.5])
ylim([0,39])
set(gca, 'YTickLabel', []);
box on
set(gca,'fontsize',18,'fontname','Arial')
set(gca, 'YTick', []);
set(gca, 'LineWidth', 2)

%% Economic burden ---------------------------------

MarkerSize=100;
BarWidth=1;


Bar95CI(12,37.5,BarWidth,13113593460,9874436162,17118339066,Color_Heatwave,Alpha_Heatwave,MarkerSize,'horizontal')   % Heatwave
Bar95CI(12,35.5,BarWidth,12121901901,9174090920,15751095198,Color_Drought,Alpha_Drought,MarkerSize,'horizontal')   % Drought
Bar95CI(12,33.5,BarWidth,85889640,65126866,111702947,Color_Sand,Alpha_Sand,MarkerSize,'horizontal')% Sand

Bar95CI(12,31.5,BarWidth,8176944215,6474064265,10291387100,Color_Flood,Alpha_Flood,MarkerSize,'horizontal') % Flood
Bar95CI(12,29.5,BarWidth,4763942080,3764637130,6001823489,Color_Storm,Alpha_Storm,MarkerSize,'horizontal')   % Storm
Bar95CI(12,27.5,BarWidth,4036994798,3147966175,5125773585,Color_WinterStorm,Alpha_WinterStorm,MarkerSize,'horizontal')   % WinterStorm
Bar95CI(12,25.5,BarWidth,2486214912,2486214912,3159534449,Color_Cyclone,Alpha_Cyclone,MarkerSize,'horizontal') % Cyclone


Bar95CI(12,21.5,BarWidth,12875644477,10615235061,15643471342, ...
    Color_CVD,Alpha_CVD,MarkerSize,'horizontal') 
Bar95CI(12,19.5,BarWidth,7534163860,6023912297,9444090449, ...
    Color_Injuries,Alpha_Injuries,MarkerSize,'horizontal') 
Bar95CI(12,17.5,BarWidth,6897601267,5007855933,9067031502, ...
    Color_Infectious,Alpha_Infectious,MarkerSize,'horizontal')
Bar95CI(12,15.5,BarWidth,4037005714,3174789004,5095534894, ...
    Color_Respiratory,Alpha_Respiratory,MarkerSize,'horizontal')
Bar95CI(12,13.5,BarWidth,4031401813,2802557164,5584835095, ...
    Color_Neurology,Alpha_Neurology,MarkerSize,'horizontal')   
Bar95CI(12,11.5,BarWidth,3406814201,2700467451,4262844273, ...
    Color_Mental,Alpha_Mental,MarkerSize,'horizontal')   
Bar95CI(12,9.5,BarWidth,3187832828,2239049015,4329252272, ...
    Color_Genitourinary,Alpha_Genitourinary,MarkerSize,'horizontal') 
Bar95CI(12,7.5,BarWidth,1497924109,860076620,2420326640, ...
    Color_Endocrine,Alpha_Endocrine,MarkerSize,'horizontal') 
Bar95CI(12,5.5,BarWidth,1121372426,853843096,1475088583, ...
    Color_Neoplasms,Alpha_Neoplasms,MarkerSize,'horizontal') 
Bar95CI(12,3.5,BarWidth,150593765,131156515,175340332, ...
    Color_Blood,Alpha_Blood,MarkerSize,'horizontal') 
Bar95CI(12,1.5,BarWidth,45126542,33209448,61840452, ...
    Color_Malnutrition,Alpha_Malnutrition,MarkerSize,'horizontal') 


xlabel('USD')
xlim([0,20*10^9])
ylim([0,39])
set(gca, 'YTickLabel', []);
box on
set(gca,'fontsize',18,'fontname','Arial')
set(gca, 'YTick', []);
set(gca, 'LineWidth', 2)

%% Disease burden 

MarkerSize=100;
BarWidth=1;


Bar95CI(13,37.5,BarWidth,73034.3375,47105.9669,117367.7219,Color_Heatwave,Alpha_Heatwave,MarkerSize,'horizontal')   % Heatwave
Bar95CI(13,35.5,BarWidth,72253.8366,46187.2412,116960.6182,Color_Drought,Alpha_Drought,MarkerSize,'horizontal')   % Drought
Bar95CI(13,33.5,BarWidth,545.1871,354.5225,869.5167,Color_Sand,Alpha_Sand,MarkerSize,'horizontal')% Sand

Bar95CI(13,31.5,BarWidth,65917.9518,46356.5943,94119.5743,Color_Flood,Alpha_Flood,MarkerSize,'horizontal') % Flood
Bar95CI(13,29.5,BarWidth,38682.6940,27059.5585,55410.6236,Color_Storm,Alpha_Storm,MarkerSize,'horizontal')   % Storm
Bar95CI(13,27.5,BarWidth,27224.1672,18982.0394,39174.9477,Color_WinterStorm,Alpha_WinterStorm,MarkerSize,'horizontal')   % WinterStorm
Bar95CI(13,25.5,BarWidth,18936.8999,13082.4020,27843.4745,Color_Cyclone,Alpha_Cyclone,MarkerSize,'horizontal') % Cyclone


Bar95CI(13,21.5,BarWidth,169090.71625,121591.51095,233167.84668, ...
    Color_CVD,Alpha_CVD,MarkerSize,'horizontal') 
Bar95CI(13,19.5,BarWidth,56730.51679,39880.52364,81766.92470, ...
    Color_Injuries,Alpha_Injuries,MarkerSize,'horizontal') 
Bar95CI(13,17.5,BarWidth,3304.62298,1869.99997,5875.58860, ...
    Color_Infectious,Alpha_Infectious,MarkerSize,'horizontal')
Bar95CI(13,15.5,BarWidth,7142.48259,4648.07130,11246.90692, ...
    Color_Respiratory,Alpha_Respiratory,MarkerSize,'horizontal')
Bar95CI(13,13.5,BarWidth,24934.78351,9910.56206,62310.35299, ...
    Color_Neurology,Alpha_Neurology,MarkerSize,'horizontal')   
Bar95CI(13,11.5,BarWidth,19597.50059,11536.92456,31493.97996, ...
    Color_Mental,Alpha_Mental,MarkerSize,'horizontal')   
Bar95CI(13,9.5,BarWidth,3090.19597,1429.54490,6160.91420, ...
    Color_Genitourinary,Alpha_Genitourinary,MarkerSize,'horizontal') 
Bar95CI(13,7.5,BarWidth,970.14344,351.27287,2455.30709, ...
    Color_Endocrine,Alpha_Endocrine,MarkerSize,'horizontal') 
Bar95CI(13,5.5,BarWidth,11674.30414,7872.57980,17165.71147, ...
    Color_Neoplasms,Alpha_Neoplasms,MarkerSize,'horizontal') 
Bar95CI(13,3.5,BarWidth,43.21553,28.82425,70.66874, ...
    Color_Blood,Alpha_Blood,MarkerSize,'horizontal') 
Bar95CI(13,1.5,BarWidth,16.59219,8.51049,32.27542, ...
    Color_Malnutrition,Alpha_Malnutrition,MarkerSize,'horizontal') 

xlabel('DALYs')
xlim([0,2.5*10^5])
ylim([0,39])
set(gca, 'YTickLabel', []);
box on
set(gca,'fontsize',18,'fontname','Arial')
set(gca, 'YTick', []);
set(gca, 'LineWidth', 2)
