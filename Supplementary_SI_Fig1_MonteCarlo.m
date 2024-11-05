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


%% Preparation
clc
clear
close all

% Image settings
figure_fontsize=18;
set(0,'defaultfigurecolor','w')

% Loading files

Path='~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/df_MC_sorted.xlsx';
df_MC_sorted=readtable(Path);

RankingORG=df_MC_sorted.Ranking;
Ranking5=df_MC_sorted.lower5;
Ranking95=df_MC_sorted.upper95;

figure(1)

scatter(RankingORG,RankingORG,20,'x','color',[0 0.4470 0.7410],'linewidth',1)
hold on
scatter(RankingORG,Ranking5,5,'k','linewidth',1)
scatter(RankingORG,Ranking95,5,'k','linewidth',1)


for i=1:length(RankingORG)

    x=[RankingORG(i),RankingORG(i)];
    y=[Ranking5(i),Ranking95(i)];

    plot(x,y,'-k','linewidth',0.5)

end

xlabel('Resilience rank given by the regional climate resilience evaluation framework')
ylabel('Resilience rank based on random weights')

box on
set(gca,'fontsize',14,'fontname','times new roman')


figure(10000)

scatter(1,1,100,'x','color',[0 0.4470 0.7410],'linewidth',3)










