function [s] = Smod(wMat,version)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modification written by Tasos Kalandrakis, 6/2018
% Last updated: 1/19/2021 
%
% This is a modified version of the code made available online by Michael
% Wolf implementing the two-step tests of moment inequalities detailed in
% Romano et al. (2014).  It includes two main changes: 
% -- Inverses are computed using Gaussian elimination.
% -- Allows for missingnes in the moment data wMat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% THE REST OF THE COMMENTS ARE AS IN THE ORIGINAL RELEASE
%
% PURPOSE: compute test statistic s
% ------------------------------------------------------------------------
% INPUTS: - w = data matrix; dimension is n x k
%         - version = variable to denote version of the test statistic
%                     1 for MMM of Andrews and Soares (2010, ETCA)
%                     2 for QLR of Andrews and Barwick (2012, ETCA)
%                     3 for MAX as in (6) of Romano et al. (2014, ETCA)
%           the default is 2
% ------------------------------------------------------------------------
% RETURNS: - s = the computed test statistic
% ------------------------------------------------------------------------
% NOTES:   need Optimization Toolbox of Matlab to compute version 2
% ------------------------------------------------------------------------

% written by: Michael Wolf
% CREATED  05/14
% UPDATED


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


if nargin < 2
    version = 2;
end

[n,k] = size(wMat);
Sigma = cov(wMat,'partialrows');
wMeans = mean(wMat,'omitnan')';

switch version
    case 1 % MMM statistic
        wMean_j_greater_zero = wMeans > 0;
        s = n*sum(((wMeans.^2)./diag(Sigma)).*wMean_j_greater_zero);
    case 2 % QLR statistic
%        SigmaInv = inv(Sigma);
        SigmaInv = Sigma\eye(k);
        SigmaInv=(SigmaInv+SigmaInv')/2;
        m = sqrt(n)*wMeans;
        f = -m'*SigmaInv;
        f = f';
        ub = zeros(k,1);
        t0 = bsxfun(@min,m,-0.001*ones(k,1));
%        opts = optimset('Display','off');
        opts = optimoptions(@quadprog,'Display','off','ConstraintTolerance',1e-14,'OptimalityTolerance',1e-14,'StepTolerance',1e-14); 
        if not((isnan(sum(sum(Sigma))))) && not((isnan(sum(sum(SigmaInv)))))
            [t,fval] = quadprog(SigmaInv,f,[],[],[],[],[],ub,t0,opts);
            s = 2*fval + m'*SigmaInv*m;
        else
            s=NaN;
        end
    case 3 % MAX statistic
        s = sqrt(n)*max(wMeans./(sqrt(diag(Sigma))));
    otherwise
        error('the argument "version" must equal 1, 2, or 3');
end

end