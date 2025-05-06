library(dplyr)
library(vote)

## v.sd = .1, delta <- 0.37
######################
setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/RScripts/")
set.seed(55234)
v.sd <- .2

df1 <- data.frame(voter = 1:18,
                  A = c(0.8 + rnorm(6, mean = 0, sd = v.sd),
                        0.4 + rnorm(6, mean = 0, sd = v.sd),
                        0.6 + rnorm(6, mean = 0, sd = v.sd)),
                  B = c(0.6 + rnorm(6, mean = 0, sd = v.sd),
                        0.8 + rnorm(6, mean = 0, sd = v.sd),
                        0.4 + rnorm(6, mean = 0, sd = v.sd)),
                  C = c(0.4 + rnorm(6, mean = 0, sd = v.sd),
                        0.6 + rnorm(6, mean = 0, sd = v.sd),
                        0.8 + rnorm(6, mean = 0, sd = v.sd))
                  )

df <- df1 %>% select(., -c("voter"))
df.rank <- t(apply(-df, 1, function(x) rank(x, ties.method = "max")))
condorcet(df.rank)

###############################################################################
delta <- .37   #.37 --> CW, CL
ind.fun <- function(scores, delta) {
  # Pairwise comparisons
  diffs <- outer(scores, scores, "-")
  # Build a logical matrix of "score i ≥ score j + delta"
  pref_mat <- diffs > delta
  # Count how many times each item is "strictly better"
  strict_counts <- rowSums(pref_mat)
  # Group tied items together
  # Items with same number of strict wins are tied
  ranks <- rank(-strict_counts, ties.method = "min")
  return(ranks)
}


df.rank.indiff <- t(apply(df, 1, ind.fun, delta = delta)) %>%
  as.data.frame(.) %>% setNames(colnames(df))

condorcet(df.rank.indiff)

######################################################################
# Run paired t-tests (UIT) for all pairs:
v.alpha <- 0.05
pairs <- combn(colnames(df), 2, simplify = FALSE)

test_results <- lapply(pairs, function(p) {
  t.test(df[[p[1]]], df[[p[2]]], 
         alternative = "greater", 
         paired = TRUE, 
         conf.level = 1-v.alpha)
})
names(test_results) <- sapply(pairs, function(p) paste(p[1], ">", p[2]))
## Extract p-values
pvals <- sapply(test_results, function(t) t$p.value)
pvals_adj <- p.adjust(pvals, method = "holm")  #holm
## Is there a Condorcet-winner?
# Pairwise info
alts <- colnames(df)
n <- length(alts)
win_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(alts, alts))

# Fill win_matrix: if adjusted p-value < 0.05 → row beats column
for (i in seq_along(pairs)) {
  p <- pairs[[i]]
  if (pvals_adj[i] < v.alpha) {
    win_matrix[p[1], p[2]] <- 1
  }
}
rowSums(win_matrix)
