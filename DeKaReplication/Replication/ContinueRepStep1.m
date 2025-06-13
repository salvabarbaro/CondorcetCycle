%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               ContinueReplicateStep1.m
% Description:        Setzt die Replikation ab dem Punkt nach den .mat-Dateien fort
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
addpath(genpath('./Code'), './Data')

% Define country codes
Countries = {'ALB','ARG','AUS','AUT','BELF','BELW','BGR','BLR','BRA','CAN',...
    'CHE','CHL','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC',...
    'HKG','HRV','HUN','IRL','ISL','ISR','ITA','JPN','KEN','KGZ','KOR',...
    'LVA','LTU','MEX','MNE','NLD','NOR','NZL','PER','PHL','POL','PRT',...
    'ROU','RUS','SRB','SVK','SVN','SWE','THA','TUR','TWN','UKR','USA',...
    'URY','ZAF'};

% use already existing *mat and *csv files
TestC3  = load('CcoreT3.mat','Tests');
TestC3w = load('CcoreTweight3.mat','Tests');
TestW3  = load('WcoreT3.mat','Tests');
TestW3w = load('WcoreTweight3.mat','Tests');

% Extract results
C3results  = ExtractResults(TestC3.Tests,  Countries, 'CoreT3',   'CorepvalT3',   'FCoreT3',   'FCorepvalT3');
C3wresults = ExtractResults(TestC3w.Tests, Countries, 'CoreWT3',  'CorepvalWT3',  'FCoreWT3',  'FCorepvalWT3');
W3results  = ExtractResults(TestW3.Tests,  Countries, 'CWinT3',   'CwinpvalT3',   'FCWinT3',   'FCwinpvalT3');
W3wresults = ExtractResults(TestW3w.Tests, Countries, 'CWinWT3',  'CwinpvalWT3',  'FCWinWT3',  'FCwinpvalWT3');

% Combine
Results = {C3results, C3wresults, W3results, W3wresults};
tableold = C3results;
for i = 1:numel(Results)-1
    temp = Results{i+1}(:,4:7);
    Allresults = horzcat(tableold, temp);
    tableold = Allresults;
end

% Country-specific amendment
cdeu = [string(repmat('DEU1',8,1)); string(repmat('DEU2',8,1))];
cgrc = [string(repmat('GRC1',7,1)); string(repmat('GRC2',8,1))];
Allresults.country(Allresults.country == "DEU" & Allresults.year == 2002) = cellstr(cdeu);
Allresults.country(Allresults.country == "GRC" & Allresults.year == 2015) = cellstr(cgrc);

% Save
writetable(Allresults, 'TestCoreResults.csv');

% Load preference data
load('Data.mat')
opts = detectImportOptions('Fulldata_minustest.csv');
C3 = readtable('Fulldata_minustest.csv', opts);
C3 = C3(:, {'country','year','id','Party','pres','lhseat','majsurvey','post_gov','remove_s','voterlr'});

C3.country(C3.country == "DEU" & C3.year == 2002) = cellstr(cdeu);
C3.country(C3.country == "GRC" & C3.year == 2015) = cellstr(cgrc);
C3.country = string(C3.country); C3.Party = string(C3.Party);

% Merge results
Tests = Allresults(:, {'country','year','Party','CoreT3','FCoreT3'});
Tests.fullcore = Tests.CoreT3 .* Tests.FCoreT3;
Tests.notcore  = 1 - Tests.CoreT3;
Tests = Tests(:, {'country','year','Party','fullcore','notcore'});

C3new = join(C3, Tests);

% Generate tables
Tablecore1 = BasePref(Countries, Data, C3new, 1, 1);
Tablegov1  = BasePref(Countries, Data, C3new, 1, 0);
Tablecoal1 = BaseCoal(Countries, Data, C3new, 1);
[LTableindiff] = Indiff(Countries, C3new, Data);

% save-up the results
writetable(Tablecore1,   'Tablecore_top1.xlsx');
writetable(Tablegov1,    'Tablegov_top1.xlsx');
writetable(Tablecoal1,   'Tablebase_top1.xlsx');
writetable(LTableindiff, 'Indiff_ind.csv');
