function [reject,stat,critVal,muNull] = TestTwoStep15F(wMat,alpha,version,B,beta)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modification written by Tasos Kalandrakis, 1/2020
% Last updated: 2/19/2021
%
% This is a modified version of the code made available online by Michael
% Wolf implementing the two-step tests of moment inequalities detailed in
% Romano et al. (2014).  It includes two main changes:
% -- A modified S function is used that computes inverses using gaussian
%    elimination.
% -- Allows for missingnes in the moment data wMat
% -- Sets the random generation seed to a fixed value to allow replication
%    of results
%    and ensure consistency in computation of p-values
% -- Full usage of (15) (page 1984 of Romano et al. (2014)) to avoid
%    computation of the second step when the first step confidence set is
%    contained in the third quadrant.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% THE REST OF THE COMMENTS ARE AS IN THE ORIGINAL RELEASE
%
% PURPOSE: carries out two-step test
% ------------------------------------------------------------------------
% INPUTS: - w = data matrix; dimension is n x k
%         - alpha = significance level of the test;
%           the default is alpha = 0.05
%         - version = variable to denote version of the test statistic
%                     1 for MMM of Andrews and Soares (2010, ETCA)
%                     2 for QLR of Andrews and Barwick (2012, ETCA)
%                     3 for MAX as in (6) of Romano et al. (2014, ETCA)
%           the default is version = 2
%         - B = number of bootstrap repetions, taken to be same in both steps;
%           the default is B = 5000
%         - beta = 1 - confidence level for first-step confidence region;
%           the default is beta = alpha/10
% ------------------------------------------------------------------------
% RETURNS: - reject = decision of the test (0 for not reject, 1 for reject)
%          - s = test statistic
%          - critVal = critical value of the test
%          - muNull = the null vector obtained in the first step
% ------------------------------------------------------------------------
% NOTES:   need Optimization Toolbox of Matlab for version 2 of test statistic
%          need Statistics Toolbox to carry out bootstrap
% ------------------------------------------------------------------------

% written by: Michael Wolf
% CREATED  05/2014
% UPDATED  01/2017


% Copyright (c) 2014, Joseph P. Romano, Azeem M. Shaikh and Michael Wolf
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


if nargin < 5
    beta = alpha/10;
end
if nargin < 4
    B = 5000;
end
if nargin < 3
    version = 2;
end
if nargin < 2
    alpha = 0.05;
end

if (alpha-beta < 0.0001)
    error('alpha-beta < 0.0001');
end

[n,k] = size(wMat);
rootn = sqrt(n);
Sigma = cov(wMat,'partialrows');
sd = sqrt(diag(Sigma));
wMeans = mean(wMat,'omitnan')';

% compute multipler d of first-step confidence region
maxBoot = ones(B,1);
%sets the seed to maintain the same bootstrap sequence across tests
rng(132435);
for b = 1:B
    wMatBoot = wMat(datasample(1:n,n),:);
    tBoot = rootn*(wMeans-mean(wMatBoot,'omitnan')')./sqrt(diag(cov(wMatBoot,'partialrows')));
    maxBoot(b) = max(tBoot);
end
d = quantile(maxBoot,1-beta);

% intersect resulting confidence region with third quadrant to get "null vector"
muNull = bsxfun(@min,zeros(k,1),wMeans+d*sd/rootn);

%compute the second step only if thefirst step confidence set is not
%contained in the third quadrant
maxConf=max(wMeans+d*sd/rootn);

if maxConf > 0
    % compute centered bootstrap test statistics
    sBoot = ones(B,1);
    centerVec = (-wMeans+muNull)';
    for b = 1:B
        wMatBootCenter = wMat(datasample(1:n,n),:)+centerVec(ones(n,1),:);
        sBoot(b) = Smod(wMatBootCenter,version);
    end
    
    % compute critcal value of two-step test
    critVal = quantile(sBoot,1-alpha+beta);
    
    % compute test statistic and decision of two-step test
    stat = Smod(wMat,version);
    
    reject = 0;
    if (stat > critVal)
        reject = 1;
    end
else
    reject = 0;
    stat = [];
    critVal=[];
end


end

