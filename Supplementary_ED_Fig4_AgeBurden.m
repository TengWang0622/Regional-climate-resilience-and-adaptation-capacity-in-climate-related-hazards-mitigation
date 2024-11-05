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

%% Economic burden - Annual - Age group

% per 100k population
Bar95CI(1,1.5,1,10.84*100000,8.19*100000,14.76*100000,[0 0 255]/255,0.4,100,'horizontal')
Bar95CI(1,3.5,1,14.55*100000,10.44*100000,19.63*100000,[127 0 255]/255,0.4,100,'horizontal')
Bar95CI(1,5.5,1,38.16*100000,28.05*100000,50.51*100000,[255 128 0]/255,0.4,100,'horizontal')
Bar95CI(1,7.5,1,89.45*100000,73.27*100000,108.77*100000,[255 0 0]/255,0.4,100,'horizontal')

ylim([0,9])
set(gca, 'YTickLabel', []);
box on
set(gca,'fontsize',14,'fontname','times new roman')
set(gca, 'YTick', []);

%% Disease burden - Annual - Age group

% per 100k population
Bar95CI(2,1.5,1,9.55e-5*100000,6.42e-5*100000,15.16e-5*100000,[0 0 255]/255,0.4,100,'horizontal')
Bar95CI(2,3.5,1,4.45e-5*100000,2.59e-5*100000,7.62e-5*100000,[127 0 255]/255,0.4,100,'horizontal')
Bar95CI(2,5.5,1,9.28e-5*100000,5.62e-5*100000,15.19e-5*100000,[255 128 0]/255,0.4,100,'horizontal')
Bar95CI(2,7.5,1,98.71e-5*100000,68.35e-5*100000,145.43e-5*100000,[255 0 0]/255,0.4,100,'horizontal')

ylim([0,9])
set(gca, 'YTickLabel', []);
box on
set(gca,'fontsize',14,'fontname','times new roman')
set(gca, 'YTick', []);





