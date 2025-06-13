function [Tablefinal] = ExtractResults(Tests,Countries,name1,name2,name3,name4)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               ExtractResults.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Extracts test results to export in an Excel sheet
%                     
%
% Created:            Nov - 2023
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


for i = 1:numel(Countries)
    ctry = Countries{i};
    roundnames = fieldnames(Tests.(ctry));
    for j = 1:numel(roundnames)
        round = roundnames{j};
        partynames = fieldnames(Tests.(ctry).(round));
        country = repmat(ctry,numel(partynames),1);
        country = string(country);
        if strlength(round) < 4
            number = str2double(extractAfter(round,"R"));
        else
            number = str2double(round(3:end));
        end
        if number < 90
            year = repmat(str2double(strcat('20',round(end-1:end)))...
                ,numel(partynames),1);
        else
            year = repmat(str2double(strcat('19',round(end-1:end)))...
                ,numel(partynames),1);
        end
        Party = string(partynames);
        % partyno = (1:1:length(year))';
        Tabletemp = table(country,year,Party);
        coretest = zeros(numel(partynames),1);
        fcoretest = zeros(numel(partynames),1);
        corepval = zeros(numel(partynames),1);
        fcorepval = zeros(numel(partynames),1);
        for k = 1:numel(partynames)
            party = partynames{k};
            tempdata = Tests.(ctry).(round).(party);
            tempnan = findnanfields(tempdata);
            tempdata = struct2cell(tempdata);
            if length(tempnan) == length(tempdata)
                coretest(k) = NaN;
                corepval(k) = NaN;
                fcoretest(k) = NaN;
                fcorepval(k) = NaN;
            else
                coretest(k) = 1 - tempdata{1};
                corepval(k) = tempdata{end};
                fcoretest(k) = tempdata{5};
                fcorepval(k) = tempdata{6};
            end
            clear tempdata tempnan
        end
        Tabletemp.Core = coretest;
        Tabletemp.Pval = corepval;
        Tabletemp.FCore = fcoretest;
        Tabletemp.FPval = fcorepval;
        Tabletemp.Properties.VariableNames{'Core'} = name1;
        Tabletemp.Properties.VariableNames{'Pval'} = name2;
        Tabletemp.Properties.VariableNames{'FCore'} = name3;
        Tabletemp.Properties.VariableNames{'FPval'} = name4;
        if i == 1
            Tablenew = Tabletemp;
        else
            Tableold = Tablenew;
            Tablenew = [Tableold;Tabletemp];
        end
    end
end
Tablefinal = Tablenew;
end
