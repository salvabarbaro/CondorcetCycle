function [Tablenew] = BasePref(Countries,Data,C3,top,coreorgov)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               BasePref.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Computes for a party triplet in a survey the share of
%                     party base that prefers a core or government party to 
%                     a non-core or non-government party
%                     
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
% INPUT     C3: test data of parties in each survey
% INPUT     top:  version of base, top 1 or top 2
% INPUT     coreorgov:  if 1 do the analysis for core parties, else gov
% OUTPUT    Tablenew:  Is a table of base preferences for core or gov v.
%           non-core or non-gov parties

if coreorgov == 1
    vname1 = 'CoreParty';
    vname2 = 'OtherCore';
    vname3 = 'NonCore';
    vname4 = 'PropPrefCR';
    vname5 = 'PropPrefnCR';
    vname6 = 'PropPrefCP';
    vname7 = 'PropPrefnCP';
    vname8 = 'WmajC';
    vname9 = 'WmajNC';
    vname10 = 'SmajC';
    vname11 = 'SmajNC';
else
    vname1 = 'GovParty';
    vname2 = 'OtherGov';
    vname3 = 'NonGov';
    vname4 = 'PropPrefGR';
    vname5 = 'PropPrefnGR';
    vname6 = 'PropPrefGP';
    vname7 = 'PropPrefnGP';
    vname8 = 'WmajG';
    vname9 = 'WmajNG';
    vname10 = 'SmajG';
    vname11 = 'SmajNG';
end

Tablenew = [];

for i = 1:numel(Countries)

    ctry = Countries{i};
    roundnames = unique(cellfun(@(x) x(2:end),fieldnames(Data.(Countries{i})),...
        'UniformOutput', false));
    country = string(ctry);

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

        if sum(C3sub.fullcore,"omitnan") > 1 || sum(C3sub.post_gov,"omitnan") > 1

           if coreorgov == 1
               parties = C3sub.Party(C3sub.fullcore == 1);
               nonparties = C3sub.Party(C3sub.notcore == 1);
           else
               parties = C3sub.Party(C3sub.post_gov == 1);
               nonparties = C3sub.Party(C3sub.post_gov == 0);
           end
           
           Sname = strrep(round,'R','M');
           Prefdata = Data.(ctry).(Sname);
           maxutil = max(Prefdata,[],2);

           for k = 1:numel(parties)

               colnum = find(strcmp(partynames,parties(k)));
               indexk = double((maxutil(:) == Prefdata(:,:)));
               basek = Prefdata(sum(indexk,2) <= top & indexk(:,colnum) == 1,:);

               otherparties = setdiff(parties,parties(k));

 %              comb = combinations(otherparties,nonparties);
                comb = all_combinations(otherparties, nonparties);

               for l = 1:size(comb,1)

                   p1 = find(strcmp(partynames,comb{l,1}));
                   p2 = find(strcmp(partynames,comb{l,2}));

                   missing = ~isnan(tests(p1)) & ~isnan(tests(p2)) & ~isnan(tests(colnum)) & ...
                            ~isnan(lhseat(p1)) & ~isnan(lhseat(p2)) & ~isnan(lhseat(colnum)) & ...
                            ~isnan(post_gov(p1)) & ~isnan(post_gov(p2)) & ~isnan(post_gov(colnum)) & ...
                            ~isnan(voterlr(p1)) & ~isnan(voterlr(p2)) & ~isnan(voterlr(colnum)) & ...
                            (remove_s(p1) == 0) & (remove_s(p2) == 0) & (remove_s(colnum) == 0);

                   support{1} = double(basek(:,p1) >= basek(:,p2));
                   support{1}(isnan(basek(:,p1)) | isnan(basek(:,p2))) = NaN;
                   propsupp(1) = sum(support{1},"omitnan")/sum(~isnan(support{1}));
                   supportn{1} = double(basek(:,p2) >= basek(:,p1));
                   supportn{1}(isnan(basek(:,p1)) | isnan(basek(:,p2))) = NaN;
                   propnsupp(1) = sum(supportn{1},"omitnan")/sum(~isnan(supportn{1}));
                    
                   support{2} = double(basek(:,p1) > basek(:,p2));
                   support{2}(isnan(basek(:,p1)) | isnan(basek(:,p2))) = NaN;
                   propsupp(2) = sum(support{2},"omitnan")/sum(~isnan(support{2}));
                   supportn{2} = double(basek(:,p2) > basek(:,p1));
                   supportn{2}(isnan(basek(:,p1)) | isnan(basek(:,p2))) = NaN;
                   propnsupp(2) = sum(supportn{2},"omitnan")/sum(~isnan(supportn{2}));

                   Tabletemp = table(country,year);

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

                   Tabletemp.(vname1) = parties(k);
                   Tabletemp.(vname2) = comb{l,1};
                   Tabletemp.(vname3) = comb{l,2};
                   Tabletemp.(vname4) = propsupp(1);
                   Tabletemp.(vname5) = propnsupp(1);
                   Tabletemp.(vname6) = propsupp(2);
                   Tabletemp.(vname7) = propnsupp(2);
                   
                   if propsupp(1) > 0.5
                       Tabletemp.(vname8) = 1;
                   else
                       Tabletemp.(vname8) = 0;
                   end

                   if propnsupp(1) > 0.5
                       Tabletemp.(vname9) = 1;
                   else
                       Tabletemp.(vname9) = 0;
                   end

                   if propsupp(2) > 0.5
                       Tabletemp.(vname10) = 1;
                   else
                       Tabletemp.(vname10) = 0;
                   end

                   if propnsupp(2) > 0.5
                       Tabletemp.(vname11) = 1;
                   else
                       Tabletemp.(vname11) = 0;
                   end

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
                   
                   Tabletemp.missing = 1-missing;

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
end
end
