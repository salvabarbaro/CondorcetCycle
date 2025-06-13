function [Tablenew] = BaseCoal(Countries,Data,C3,top)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               BaseCoal.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Computes for each pair of core and non-core parties
%                     in a survey aggregate legislative preferences for
%                     each party
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
% INPUT     Data: Preference data
% INPUT     C3: seat share and test data of parties in each survey
% INPUT     top: version of base, top 1 or top 2
% OUTPUT    Tablenew: Table of legislative preference for each
%           core-non-core pair of parties

Tablenew = [];

% First, loop over countries

for i = 1:numel(Countries)
    
    ctry = Countries{i};
    roundnames = unique(cellfun(@(x) x(2:end),fieldnames(Data.(Countries{i})),...
        'UniformOutput', false));
    country = string(ctry);
    
    % and then, over the elections
    for j = 1:numel(roundnames)
        
        round = char(strcat("R",roundnames{j}));
        Pname = strrep(round,'R','P');
        partynames = Data.(ctry).(Pname);
        if strlength(round) < 4
            number = str2double(extractAfter(round,"R"));
        else
            number = str2double(extractAfter(round,2));
        end
        if number < 90
            year = str2double(strcat('20',round(end-1:end)));
        else
            year = str2double(strcat('19',round(end-1:end)));
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
            C3sub = C3(C3.country == country & C3.year == year,:);
        end

        pres = mean(C3sub.pres);
        majsurvey = mean(C3sub.majsurvey);

        tests = C3sub.fullcore;

        remove_s = C3sub.remove_s;

        lhseat = C3sub.lhseat;
        post_gov = C3sub.post_gov;
        voterlr = C3sub.voterlr;

        parties = C3sub.Party(C3sub.fullcore == 1);
        nonparties = C3sub.Party(C3sub.notcore == 1);
%        comb = table2array(combinations(parties,nonparties));
        comb = all_combinations(parties, nonparties);
        Sname = strrep(round,'R','M');
        Prefdata = Data.(ctry).(Sname);
        maxutil = max(Prefdata,[],2);
        indexk = double((maxutil(:) == Prefdata(:,:)));
        bases = cell(1,numel(partynames));
        
        % For each combination of core and non-core...
        for k = 1:size(comb,1)
            pair = append(comb(k,1),' ',comb(k,2));
            p1 = find(strcmp(partynames,comb{k,1}));
            p2 = find(strcmp(partynames,comb{k,2}));

            missing = ~isnan(tests(p1)) & ~isnan(tests(p2)) & ...
                         ~isnan(lhseat(p1)) & ~isnan(lhseat(p2)) & ...
                         ~isnan(post_gov(p1)) & ~isnan(post_gov(p2)) & ...
                         ~isnan(voterlr(p1)) & ~isnan(voterlr(p2)) &...
                         (remove_s(p1) == 0) & (remove_s(p2) == 0);

            supportc = cell(2,numel(partynames));
            supportn = cell(2,numel(partynames));
            coalcr = zeros(size(partynames));
            coalcp = zeros(size(partynames));
            coalnr = zeros(size(partynames));
            coalnp = zeros(size(partynames));
            
            % Look at all parties in survey and see whether base prefers
            for l = 1:numel(partynames)
                colnum = find(strcmp(partynames,partynames(l)));
                % define base for each party in election
                bases{l} = Prefdata(sum(indexk,2) <= top & ...
                            indexk(:,colnum) == 1,:);
                % check the number of people that weakly and strictly
                % prefer p1 over p2 and vice versa
                supportc{1,l} = double(bases{l}(:,p1) >= bases{l}(:,p2));
                supportc{1,l}(isnan(bases{l}(:,p1)) | ...
                            isnan(bases{l}(:,p2))) = NaN;
                supportc{2,l} = double(bases{l}(:,p1) > bases{l}(:,p2));
                supportc{2,l}(isnan(bases{l}(:,p1)) | ...
                            isnan(bases{l}(:,p2))) = NaN;
                % Weak and strict majority base preference for core
                coalcr(l) = (sum(supportc{1,l},"omitnan")/...
                            sum(~isnan(supportc{1,l})) >= 0.5);
                coalcp(l) = (sum(supportc{2,l},"omitnan")/...
                            sum(~isnan(supportc{2,l})) > 0.5);
                supportn{1,l} = double(bases{l}(:,p2) >= bases{l}(:,p1));
                supportn{1,l}(isnan(bases{l}(:,p2)) | ...
                            isnan(bases{l}(:,p1))) = NaN;
                supportn{2,l} = double(bases{l}(:,p2) > bases{l}(:,p1));
                supportn{2,l}(isnan(bases{l}(:,p2)) | ...
                            isnan(bases{l}(:,p1))) = NaN;
                % Weak and strict majority base preference for non-core
                coalnr(l) = (sum(supportn{1,l},"omitnan")/...
                            sum(~isnan(supportn{1,l})) >= 0.5);
                coalnp(l) = (sum(supportn{2,l},"omitnan")/...
                            sum(~isnan(supportn{2,l})) > 0.5);
            end
            
            % Sum of seat share of parties whose bases prefer core and
            % non-core parties
            lhseat = C3sub.lhseat;
            lhseat(isnan(lhseat)) = 0;

            sharecr_seat = dot(coalcr,lhseat);
            sharenr_seat = dot(coalnr,lhseat);
            sharecp_seat = dot(coalcp,lhseat);
            sharenp_seat = dot(coalnp,lhseat);
            
            % Start populating Table
            Tabletemp = table(country,year,pair);
            if ctry == "DEU" && round == "Ra02"
                Tabletemp.id = "DEU_2002a";
                elseif ctry == "DEU" && round == "Rb02"
                Tabletemp.id = "DEU_2002b";
                elseif ctry == "GRC" && round == "Ra15"
                Tabletemp.id = "GRC_2015a";
                elseif ctry == "GRC" && round == "Rb15"
                Tabletemp.id = "GRC_2015b";
                elseif ctry == "BELF" || ctry == "BELW"
                Tabletemp.id = strcat(country,string(year));
                else
                Tabletemp.id = strcat(country,"_",string(year));
            end

            Tabletemp.corer_seat = sharecr_seat;
            Tabletemp.corep_seat = sharecp_seat;
            Tabletemp.ncorer_seat = sharenr_seat;
            Tabletemp.ncorep_seat = sharenp_seat;

            Tabletemp.majsurvey = majsurvey;

            if ~ismember(country,["BELF","BELW","BLR","CHE","KGZ","HKG","FRA"]) && ...
               ~ismember(Tabletemp.id,["JPN_1996","JPN_2004","JPN_2007",...
                        "JPN_2013","LTU_1997","ROU_2009","ROU_2014","RUS_2000",...
                        "RUS_2004","THA_2001","TWN_2004","TWN_2008","TWN_2012",...
                        "TWN_2016","TUR_2015","TUR_2018","RUS_1999","UKR_1998",...
                        "DEU_2002b","THA_2007","TWN_1996"]) && ...
               pres == 0 && majsurvey == 0 && missing == 1
               Tabletemp.regob = 1;
            else
               Tabletemp.regob = 0;
            end

            Tabletemp.missing = 1 - missing;

            if isempty(Tablenew)
               Tablenew = Tabletemp;
            else
               Tableold = Tablenew;
               Tablenew = [Tableold;Tabletemp];
            end
        end
    end
end
end
