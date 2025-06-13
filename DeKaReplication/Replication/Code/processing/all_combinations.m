function comb = all_combinations(A, B)
% cartesian product

if isstring(A), A = cellstr(A); end
if isstring(B), B = cellstr(B); end

nA = numel(A);
nB = numel(B);
comb = cell(nA * nB, 2);

idx = 1;
for i = 1:nA
    for j = 1:nB
        comb{idx, 1} = A{i};
        comb{idx, 2} = B{j};
        idx = idx + 1;
    end
end
end
