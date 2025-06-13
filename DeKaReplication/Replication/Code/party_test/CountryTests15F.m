function Ttemp=CountryTests15F(CData,type,ExclCut,version,B,pval)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               CountryTests15F.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Executed inside the parfor loop of RunTest15F.m, 
%                     takes country specific data as input and returns the 
%                     resulting tests for each survey and each party                    
%
% Created:            Jan - 2020
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

% INPUT CData: Country specific component of WData.mat or CData.mat, etc.,
% structure
% INPUT type: Type of test as follows
%                 -- 1: Core and Not Core tests unweighted
%                 -- 2: Core and Not Core tests weighted
%                 -- 3: CW and Not CW tests unweighted
%                 -- 4: CW and Not CW tests weighted
% INPUT     ExclCut: percentage of response as a fraction of maximal
%           response below which party is excluded form analysis
% INPUT version:  version of Romano et al. (2014) test -- values 1, 2, or 3
% INPUT B:  number of bootstrap samples for test
% INPUT pval:  takes value 1 to compute p values.
% OUTPUT Ttemp:  Is a country-specific structure with all test results

if nargin<6
    pval=0;
end
years = unique(cellfun(@(x) x(2:end),fieldnames(CData),'UniformOutput', false));
for k = 1:length(years)
    Dname = strcat('M',years{k});
    S = CData.(Dname);
    Pname = strcat('P',years{k});
    P = CData.(Pname);
    Wname = strcat('W',years{k});
    W = CData.(Wname);
    Rname = strcat('R',years{k});
    [N,J]=size(S);
    Rates= (N-sum(isnan(S)))/N;
    Rates=Rates/max(Rates);
    Excl=(Rates<ExclCut);
    
    for j=1:J
        if Excl(j)
            Pname=P{j};
            Ttemp.(Rname).(Pname).('reject') = NaN;
            Ttemp.(Rname).(Pname).('stat') = NaN;
            Ttemp.(Rname).(Pname).('critVal') = NaN;
            Ttemp.(Rname).(Pname).('muNull') = NaN;
        end
    end
    Sexcl=S(:,not(Excl));
    Pexcl=P(:,not(Excl));
    Jexcl=J-sum(Excl);
    if type==1
        D=PrefC(Sexcl,Pexcl);
    elseif type==2
        D=PrefC(Sexcl,Pexcl,W);
    elseif type==3
        D=PrefW(Sexcl,Pexcl);
    elseif type==4
        D=PrefW(Sexcl,Pexcl,W);
    end
    for j = 1:Jexcl
        M = MTestj(D,Pexcl,j);
        Pname=Pexcl{j};
        [reject,stat,critVal,muNull] = TestTwoStep15F(M,0.05,version,B);
        [Freject,FPvalue,FPvector] = FlipIUT(-M,0.05);
        Ttemp.(Rname).(Pname).('reject') = reject;
        Ttemp.(Rname).(Pname).('stat') = stat;
        Ttemp.(Rname).(Pname).('critVal') = critVal;
        Ttemp.(Rname).(Pname).('muNull') = muNull;
        Ttemp.(Rname).(Pname).('Freject') = Freject;
        Ttemp.(Rname).(Pname).('FPvalue') = FPvalue;
        Ttemp.(Rname).(Pname).('FPvector') = FPvector;
        if pval==1
            Ttemp.(Rname).(Pname).('Pvalue') = NaN;
            if reject==1
                alpha0=0;
                alpha1=0.05;
                alpha=(alpha0+alpha1)/2;
            elseif reject==0
                alpha0=0.05;
                alpha1=1;
                alpha=(alpha0+alpha1)/2;
            else
                alpha1=0;
                alpha0=0;
                alpha=NaN;
            end
            while alpha1-alpha0>0.005
                [preject,~,~,~] = TestTwoStep15F(M,alpha,version,B);               
                if  preject==1
                    alpha1=alpha;
                elseif preject==0
                    alpha0=alpha;
                else
                    break
                end
                alpha=(alpha0+alpha1)/2;
                if alpha1-alpha0<=0.005
                    Ttemp.(Rname).(Pname).('Pvalue') = alpha;
                end
            end
            
        end
    end
end