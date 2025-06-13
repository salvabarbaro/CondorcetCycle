%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               ReplicateStep1.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Computes tests and preference data that are used as 
%                     inputs in Tables produced in Step 2 of the
%                     replication
%
% Created:            Jun - 2024
%
% Last Modified:      Jul - 2024
%
% Language:           MATLAB
%
% Related References: [1] Zuheir Desai and Tasos Kalandrakis. 2024. "The 
%                     Core of the Party System," Journal of Politics, 
%                     conditionally accepted.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

clear

% Change directory  and add the function folder to path

%cd("/.../Replication/")
addpath(genpath('./Code'),'./Data')

% Load data and execute tests

load('Data.mat')

version=3;
Tests = RunTest15F(Data,1,.75,version,20000,1);
file=strcat('CcoreT',int2str(version));
save(file,'Tests')

Tests = RunTest15F(Data,2,.75,version,20000,1);
file=strcat('CcoreTweight',int2str(version));
save(file,'Tests')

Tests = RunTest15F(Data,3,.75,version,20000,1);
file=strcat('WcoreT',int2str(version));
save(file,'Tests')

Tests = RunTest15F(Data,4,.75,version,20000,1);
file=strcat('WcoreTweight',int2str(version));
save(file,'Tests')

% Create cell of country names to loop over

Countries={'ALB','ARG','AUS','AUT','BELF','BELW','BGR','BLR','BRA','CAN'...
    ,'CHE','CHL','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC'...
    ,'HKG','HRV','HUN','IRL','ISL','ISR','ITA','JPN','KEN','KGZ','KOR'...
    ,'LVA','LTU','MEX','MNE','NLD','NOR','NZL','PER','PHL','POL','PRT'...
    ,'ROU','RUS','SRB','SVK','SVN','SWE','THA','TUR','TWN','UKR','USA'...
    ,'URY','ZAF'};

% load all core test results and extract results

TestC3 = load('CcoreT3.mat','Tests');
TestC3w = load('CcoreTweight3.mat','Tests');

C3results = ExtractResults(TestC3.Tests,Countries,'CoreT3', ...
    'CorepvalT3','FCoreT3','FCorepvalT3');
C3wresults = ExtractResults(TestC3w.Tests,Countries,'CoreWT3', ...
    'CorepvalWT3','FCoreWT3','FCorepvalWT3');

TestW3 = load('WcoreT3.mat','Tests');
TestW3w = load('WcoreTweight3.mat','Tests');

W3results = ExtractResults(TestW3.Tests,Countries,  'CWinT3', ...
    'CwinpvalT3','FCWinT3','FCwinpvalT3');
W3wresults = ExtractResults(TestW3w.Tests,Countries,'CWinWT3', ...
    'CwinpvalWT3','FCWinWT3','FCwinpvalWT3');

% Combine all results in a table and write to and excel sheet

Results = {C3results,C3wresults,W3results,W3wresults};
        
tableold = C3results;
for i = 1:numel(Results)-1
    temp = Results{i+1}(:,4:7);
    Allresults = horzcat(tableold,temp);
    tableold = Allresults;
    clear temp
end
clear tableold

% fix the country names for Germany 2002 and Greece 2015
cdeu = [string(repmat('DEU1',8,1));...
    string(repmat('DEU2',8,1))];
cgrc = [string(repmat('GRC1',7,1));...
    string(repmat('GRC2',8,1))];

Allresults.country(Allresults.country == "DEU" & ...
    Allresults.year == 2002) = cellstr(cdeu);
Allresults.country(Allresults.country == "GRC" & ...
    Allresults.year == 2015) = cellstr(cgrc);

writetable(Allresults,'TestCoreResults.csv')

% load preference data
load('Data.mat')

% get variables used for base preference analysis

opts = detectImportOptions('Fulldata_minustest.csv');
C3 = readtable('Fulldata_minustest.csv',opts);
C3 = C3(:,{'country', 'year','id','Party','pres','lhseat',...
    'majsurvey','post_gov','remove_s','voterlr'});

C3.country(C3.country == "DEU" & C3.year == 2002) = cellstr(cdeu);
C3.country(C3.country == "GRC" & C3.year == 2015) = cellstr(cgrc);

C3.country = string(C3.country);
C3.Party = string(C3.Party);

% Create core categories and merge test outcomes

Tests = Allresults(:,{'country','year','Party','CoreT3','FCoreT3'});

Tests.fullcore = Tests.CoreT3 .* Tests.FCoreT3;
Tests.notcore = 1 - (Tests.CoreT3);

Tests = Tests(:,{'country','year','Party','fullcore','notcore'});

C3new = join(C3,Tests);

% Base preferences for core parties, government parties

Tablecore1 = BasePref(Countries,Data,C3new,1,1);
Tablegov1 = BasePref(Countries,Data,C3new,1,0);

writetable(Tablecore1,'Tablecore_top1.xlsx')
writetable(Tablegov1,'Tablegov_top1.xlsx')

% Legislative preferences for core and non-core parties

Tablecoal1 = BaseCoal(Countries,Data,C3new,1);

writetable(Tablecoal1,'Tablebase_top1.xlsx')

% Individual level indifference data

[LTableindiff ]= Indiff(Countries,C3new,Data);

writetable(LTableindiff,'Indiff_ind.csv')
