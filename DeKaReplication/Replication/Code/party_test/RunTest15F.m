function [T] = RunTest15F(Data,type,ExclCut,version,B,pval)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               RunTest15F.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Runs party tests for all countries, given sympathy 
%                     score data                     
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

% INPUT     Data: Complete data sith sympathy scores and weights (a N x 1
%           structure) 
% INPUT     type: Type of test as follows
%                 -- 1: Core and Not Core tests unweighted
%                 -- 2: Core and Not Core tests weighted
%                 -- 3: CW and Not CW tests unweighted
%                 -- 4: CW and Not CW tests weighted
% INPUT     ExclCut: percentage of response as a fraction of maximal
%           response below which party is excluded form analysis
% INPUT     version: version of Romano et al. (2014) test -- values 1, 2,
%           or 3 
% INPUT     B:  number of bootstrap samples for test
% INPUT     pval:  takes value 1 to compute p values.

% OUTPUT    T: Test results for all countries(a N x 1 structure)         

% Loop over all countries, all survey-years and all parties. Carry out test
% for each party and store in a structure.

if nargin<6
    pval=0;
end

Countries=fieldnames(Data);
K=length(Countries);
Ttemp=cell(1,K);
Data=struct2cell(Data);
parfor i = 1:K
    Ttemp{i}=CountryTests15F(Data{i},type,ExclCut,version,B,pval);
end
for i=1:K
    T.(Countries{i})=Ttemp{i};
end
end