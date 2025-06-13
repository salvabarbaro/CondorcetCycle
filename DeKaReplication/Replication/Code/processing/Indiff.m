function [LTablefinal] = Indiff(Countries,C3,Data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               Indiff.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Computes individual level statistics about
%                     indifferent comparisons
%
% Created:            Nov - 2023
%
% Last Modified:      Jun - 2024
%
% Language:           MATLAB
%
% Related References: [1] Zuheir Desai and Tasos Kalandrakis. 2024. "The 
%                     Core of the Party System," Journal of Politics, 
%                     conditionally accepted.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Copyright (c) 2024, Zuheir Desai and Tasos Kalandrakis
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
%
% 1. Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
%
%
% 2. Redistributions in binary form must reproduce the above copyright
% notice, this list of conditions and the following disclaimer in the
% documentation and/or other materials provided with the distribution.
%
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

% INPUT     Countries: List of countries
% INPUT     C3: seat share and test data of parties in each survey
% INPUT     Data: Preference data
% OUTPUT    LTablefinal: Table of data about individual-level indifferences

for i = 1:length(Countries)

    countrydata = Data.(Countries{i});
    years = unique(cellfun(@(x) x(2:end),fieldnames(countrydata),...
        'UniformOutput', false));
    ctry = string(Countries{i});

    for k = 1:length(years)

        tempyear = years{k};
        round = strcat("R",years{k});
        deleteMe = isletter(tempyear); % Detect which ones are letters
        tempyear(deleteMe) = [];       % Delete the letters
        if str2double(tempyear) < 90
            year = str2double(strcat('20',tempyear));
        else
            year = str2double(strcat('19',tempyear));
        end

        if ctry == "DEU" && round == "Ra02"
            C3sub = C3(C3.country == "DEU1" & C3.year == 2002,:);
        elseif ctry == "DEU" && round == "Rb02"
            C3sub = C3(C3.country == "DEU2" & C3.year == 2002,:);
        elseif ctry == "GRC" && round == "Ra15"
            C3sub = C3(C3.country == "GRC1" & C3.year == 2015,:);
        elseif ctry == "GRC" && round == "Rb15"
            C3sub = C3(C3.country == "GRC2" & C3.year == 2015,:);
        else
            C3sub = C3(C3.country == ctry & C3.year == year,:);
        end

        majsurvey = mean(C3sub.majsurvey);
        pres = mean(C3sub.pres);
        id = C3sub.id(1);
        
        % Figure out which comparisons are between core, possibly core, and
        % non core
        core = C3sub.fullcore;
        ccore = 1 - C3sub.fullcore;
        ncore = C3sub.notcore;
        
        % Figure out which columns correspond to comparisons between
        % parties in relevant categories

        coreoncore= kron(core,core.');
        coreoncore = coreoncore(~eye(size(coreoncore)));
        ncoreoncore= kron(ncore,ncore.');
        ncoreoncore = ncoreoncore(~eye(size(ncoreoncore)));
        coreonccore = kron(core,ccore');
        ccoreoncore = kron(ccore,core');
        coreonccore = coreonccore + ccoreoncore;
        coreonccore = coreonccore(~eye(size(coreonccore)));
        ccoreonccore = kron(ccore,ccore');
        ccoreonccore = ccoreonccore(~eye(size(ccoreonccore)));
        
        % Generate data about indifferences
        Sname = strcat('M',years{k});
        S = countrydata.(Sname);
        Pname = strcat('P',years{k});
        P = countrydata.(Pname);
        indiffdata = PrefI(S,P);

        % Remove duplicate columns and create subsets of indifferences for
        % core-on-core etc.

        index = ones(1,size(indiffdata,2));
        index = tril(reshape(index,length(P)-1,length(P)));
        index = logical(index(:)');
        indiffdata = indiffdata(:,index);
        
        coreoncore = coreoncore(index);
        indiffcore = indiffdata;
        indiffcore(:,coreoncore == 0 | isnan(coreoncore)) = 0;
        ncoreoncore = ncoreoncore(index);
        indiffncore = indiffdata;
        indiffncore(:,ncoreoncore == 0 | isnan(ncoreoncore)) = 0;
        coreonccore = coreonccore(index);
        indiffcorec = indiffdata;
        indiffcorec(:,coreonccore == 0 | isnan(coreonccore)) = 0;
        ccoreonccore = ccoreonccore(index);
        indiffccore = indiffdata;
        indiffccore(:,ccoreonccore == 0 | isnan(ccoreonccore)) = 0;

        % Remove the parties for which we did not carry out tests

        indiffdata(:,isnan(coreoncore)) = NaN;

        % Generate individual counts of total comparisons and indifferences
        
        indiffcount = sum(indiffdata,2,'omitnan');
        totalpairs = sum(~isnan(indiffdata),2);

        % make sure to drop respondents that rated no parties or only 1 party
        % and generate indifference counts and proportion for all parties,  
        % core, possible core, non-core, complement of core

        indiffcount(totalpairs == 0) = NaN;
        totalpairs(totalpairs == 0) = NaN;
        propindiff = indiffcount./totalpairs;

        indiffcountcore = sum(indiffcore,2,'omitnan');
        propindiffcore = indiffcountcore./indiffcount;
        propindiffcore(indiffcount == 0) = 0;

        indiffcountncore = sum(indiffncore,2,'omitnan');
        propindiffncore = indiffcountncore./indiffcount;
        propindiffncore(indiffcount == 0) = 0;

        indiffcountcorec = sum(indiffcorec,2,'omitnan');
        propindiffcorec = indiffcountcorec./indiffcount;
        propindiffcorec(indiffcount == 0) = 0;

        indiffcountccore = sum(indiffccore,2,'omitnan');
        propindiffccore = indiffcountccore./indiffcount;
        propindiffccore(indiffcount == 0) = 0;

        % Figure out which individuals are indifferent between all parties
        % and all but their top rated parties, make sure to attribute NaNs
        % wherever appropriate

        indiffall = double(sum(indiffdata,2,'omitnan') == sum(~isnan(indiffdata),2));
        indiffall(all(isnan(indiffdata),2)) = NaN;

        indiffallbutone = double((sum(S==min(S,[],2),2) == sum(~isnan(S),2) - 1));
        indiffallbutone((sum(isnan(S),2) == length(P) - 1) | ...
            (sum(isnan(S),2) == length(P) - 2)) = 0;
        indiffallbutone(all(isnan(S),2)) = NaN;
        indiffallbutone(all(isnan(indiffdata),2)) = NaN;
        indiffnone = double((propindiff == 0));
        indiffnone(isnan(indiffall)) = NaN;
        indiffsome = 1 - indiffall - indiffallbutone - indiffnone;
        
        N = length(indiffall);

        % Start populating the table
   
        LTabletemp = table(indiffcount,totalpairs,propindiff,...
            indiffcountcore, indiffcountcorec,indiffcountccore, ...
            indiffcountncore,propindiffcore,propindiffcorec,...
            propindiffccore,propindiffncore, ...
            indiffall,indiffallbutone,indiffsome,indiffnone);

        if ctry == "DEU" && round == "Ra02"
           LTabletemp.id = repmat("DEU_2002a",N,1);
        elseif ctry == "DEU" && round == "Rb02"
           LTabletemp.id = repmat("DEU_2002b",N,1);
        elseif ctry == "GRC" && round == "Ra15"
           LTabletemp.id = repmat("GRC_2015a",N,1);
        elseif ctry == "GRC" && round == "Rb15"
           LTabletemp.id = repmat("GRC_2015b",N,1);
        elseif ctry == "BELF" || ctry == "BELW"
           LTabletemp.id = repmat(strcat(ctry,string(year)),N,1);
        else
           LTabletemp.id = repmat(strcat(ctry,"_",string(year)),N,1);
        end

        if ~ismember(ctry,["BELF","BELW","BLR","KGZ","HKG","FRA"]) && ...
           ~ismember(id,["JPN_1996","JPN_2004","JPN_2007",...
                    "JPN_2013","LTU_1997","ROU_2009","ROU_2014","RUS_2000",...
                    "RUS_2004","THA_2001","TWN_2004","TWN_2008","TWN_2012",...
                    "TWN_2016","TUR_2015","TUR_2018","RUS_1999","UKR_1998",...
                    "DEU_2002b","THA_2007","TWN_1996"]) && ...
           pres == 0 && majsurvey == 0
           LTabletemp.regob = ones(N,1);
        else
           LTabletemp.regob = zeros(N,1);
        end

        LTabletemp = [LTabletemp(:,end-2),LTabletemp(:,1:end-3),LTabletemp(:,end-1:end)];
        if i == 1
            LTablenew = LTabletemp;
        else
            LTableold = LTablenew;
            LTablenew = [LTableold;LTabletemp];
        end
    end
end
LTablefinal = LTablenew;
end