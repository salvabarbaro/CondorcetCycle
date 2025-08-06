#!/usr/bin/env Rscript
#
library(parallel)
library(dplyr)
library(readr)  # for write_csv
library(tidyr)
library(vote)
library(stringr)
#
########################################################################
load("Crankinglist.RData")  # consists of ranking.list (Candidate elections)

set.seed(55234)
R <- 10000

classify.fun <- function(vote.obj) {
  if (!is.null(vote.obj$elected)) {
    return(vote.obj$elected[1])
  } else {
    totals.df <- vote.obj$totals %>% as.data.frame(.) %>% mutate(wins = rowSums(.))
    win_counts <- table(totals.df$wins)
    res <- case_when(
      any(win_counts == 3) ~ "Cycle",
      any(win_counts == 2) ~ "Tie",
      TRUE                 ~ "?"
    )
    return(res)
  }
}
# Define a function for one case
bootstrap_and_classify <- function(df, R = 10000, cores = 38) {
  n <- nrow(df)
  bootstrap_list <-  replicate(R, df[sample(1:n, size = n, replace = TRUE), ], simplify = FALSE)
  vote.obj.list  <-  mclapply(bootstrap_list, condorcet, quiet = TRUE, runoff = FALSE, mc.cores = cores)
  classified.list <- mclapply(vote.obj.list, classify.fun, mc.cores = cores)
  return(unlist(classified.list))  # return vector of labels
}

# Apply to all items in ranking.list
results.list <- lapply(ranking.list, bootstrap_and_classify, R = R, cores = 38)

# Combine into data.frame (columns = cases, rows = bootstrap replications)
results.df <- as.data.frame(results.list) %>%
  mutate(across(everything(), ~ str_replace(., "^candidate_rating_", "")))

# Save to CSV
write_csv(results.df, "resultsC.csv")

# Transpose to long format for easier summarizing
results_long <- results.df %>%
  mutate(replication = row_number()) %>%
  tidyr::pivot_longer(
    cols = -replication,
    names_to = "case",
    values_to = "result"
  )



# Summarize counts per case
summary_table <- results_long %>%
  group_by(case) %>%
  summarise(
    count_candidate = sum(result %in% LETTERS[1:9], na.rm = TRUE),
    count_tie       = sum(result == "Tie", na.rm = TRUE),
    count_cycle     = sum(result == "Cycle", na.rm = TRUE),
    .groups = "drop"
  )

# make csv
write_csv(summary_table, "summarytableP.csv")

summary_table %>% filter(., count_cycle > 0)
