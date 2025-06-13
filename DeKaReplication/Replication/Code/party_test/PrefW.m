function [M] = PrefW(S,P,W)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File:               PrefW.m
%
% Authors:            Zuheir Desai and Tasos Kalandrakis
%
% Description:        Outputs a matrix of individual strict individual 
%                     preferences for party j over other parties h                     
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

% INPUT     S: Sympathy score matrix (a N x J matrix)
% INPUT     P: Party names (a 1 x J cell)
% INPUT     W(optional): Weight vector (a N x 1 matrix)
% OUTPUT    M: Matrix of individual preference of Party j over other
%               parties (a N x (J-1) matrix)

% For all parties j in P:
%   First split sympathy score matrix into a vector of party j scores and a
%   matrix of all party scores except for j. Check if a weight vector was
%   provided. If not, then if individual i strictly prefers j over h, then
%   m_C(j,s_i) = 1, else -1. If a weight vector is provided, then if
%   individual i strictly prefers j over h, then m_C(j,s_i) = W(i)*phi,
%   where phi = (total number of available comparisons)/(sum of weights of
%   those individuals), else -W(i)*phi. Repeat for all j.

J = length(P);
N = length(S);
M = zeros(N,J*(J-1));
for j=1:J
    Sj = S(:,j);
    Sh = S(:,setdiff(1:length(P),j));
    if nargin<3
        Temp = double((Sj(:) <= Sh(:,:))-(Sj(:) > Sh(:,:)));
        Temp(isnan(Sj),:) = NaN;
        Temp(isnan(Sh)) = NaN;
        M(:,(j+(j-1)*(J-2)):(j*(J-1)))= Temp;
    else
        M1 = double((Sj(:) <= Sh(:,:))-(Sj(:) > Sh(:,:)));
        M1(isnan(Sj),:) = NaN;
        M1(isnan(Sh)) = NaN;
        M2 = repmat(W,1,length(P)-1);
        M2(isnan(M1)) = NaN;
        N = sum(not(isnan(M1)));
        phi = N./nansum(M2);
        M2 = M2.*phi;
        Temp = M1.*M2;
        Temp(isnan(M1)) = NaN;
        M(:,(j+(j-1)*(J-2)):(j*(J-1)))= Temp;
    end
end
end