#!/usr/bin/env Rscript
#
# associated sh-file: jobbootW.sh
library(dplyr)  # standard
library(vote)   # for condorcet()
library(parallel)  # for mclapply (suitable for UNIX/LINUX only!)
library(data.table)  # For frank()
library(stringr)  # for renaming items
############################################################
#setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/mogon/")
load("d.RData")
Repl <- 5000   # bootstrap replications
nb.cores <- 40 # number of cores to use in lapply() !---! be carefully!
############################################################
##############  FUNCTIONS ##################################
# Convert ratings to orderings, consider the weights
trank.fun_Parties_w <- function(case) {
  dat <- d %>% 
    filter(case_ID == case) %>%
    select(starts_with("party_rating_"), starts_with("IMD5000_"), IMD1010_3)  # include weights
  include <- which(dat[1, grep("IMD5000_", names(dat))] != 9999999)
  if (case == "Switzerland_2007")     include <- setdiff(include, 7:8)
  if (case == "Spain_2004")           include <- setdiff(include, 4:9)
  if (case == "Great Britain_1997")   include <- setdiff(include, 4:5)
  if (case == "Great Britain_2005")   include <- setdiff(include, 4:5)
  if (case == "Great Britain_2015")   include <- setdiff(include, c(5, 7))
  keep_cols <- names(dat)[grep("IMD5000_", names(dat))[include]]
  party_cols <- sub("IMD5000_", "party_rating_", keep_cols)
  if (length(party_cols) == 0) return(NULL)
  # Subset data to only valid party ratings
  ratings <- dat %>%
    select(all_of(party_cols)) %>%
    select(where(~ !all(is.na(.)))) %>%
    rename_with(~ str_replace(., "party_rating_", ""))  # rename A, B, C...
  # Return NULL if no valid rating columns
  if (ncol(ratings) == 0) return(NULL)
  # Compute row-wise rankings 
  ranked <- as.data.frame(t(apply(ratings, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE))))
  colnames(ranked) <- colnames(ratings)
  # Add weights as an extra column
  ranked$weight <- dat$IMD1010_3
  return(ranked)
}
# 2. Now we use the weights to consider in the generation of replica
weighted_replications.fun <- function(df, R = Repl) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!"weight" %in% names(df)) stop("No weight column found.")
  # Keep only rows with non-negative and finite weights
  df <- df %>% filter(!is.na(weight) & weight > 0 & is.finite(weight))
  n <- nrow(df)
  if (n == 0) {
    warning("All rows had invalid weights. Skipping case.")
    return(NULL)
  }
  w <- df$weight
  replicate(R, {
    sample_indices <- sample(seq_len(n), 
                             size = n, 
                             replace = TRUE, 
                             prob = w)  # here is there weights are considered!
    df[sample_indices, ]
  }, simplify = FALSE)
}
#3. Run on each boostraped replication a condorcet() and classify .$elected == NULL
smr.bootstrap.classified <- function(bootstrap_list) {
if (is.null(bootstrap_list)) return(NULL)

lapply(bootstrap_list, function(df) {
  df_clean <- df[, setdiff(names(df), "weight")]
  
  result <- tryCatch(
    vote::condorcet(df_clean, quiet = TRUE, runoff = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    return("Error")
  } else if (!is.null(result$elected)) {
    return(result$elected[1])
  } else {
    totals.df <- result$totals %>% as.data.frame() %>% mutate(wins = rowSums(.))
    win_counts <- table(totals.df$wins)
    res <- dplyr::case_when(
      any(win_counts == 3) ~ "Cycle",
      any(win_counts == 2) ~ "Tie",
      TRUE                 ~ "?"
    )
    return(res)
  }
})
}
##################################################################
########  Analysis ###############################################
# A1: In a first step, we select the case_ID for whom PBW are available.
pbw.cases <- d %>% group_by(case_ID) %>%
  reframe(PBW.var = var(IMD1010_3)) %>%
  filter(., PBW.var > 0) %>%
  filter(., !case_ID %in% c("B_FL_1999", "B_FL_2019", 
                            "B_WA_1999", "B_WA_2019"))
caseid.party <- as.list(pbw.cases$case_ID)
#
set.seed(55234)
# A2: We apply trank.fun_w to generate ranking.lists
ranking.list <- mclapply(caseid.party, 
                         trank.fun_Parties_w, 
                         mc.cores = nb.cores)  
names(ranking.list) <- caseid.party
print("ranking.list done")
# A3: We take the ranking list (one ranking for each election) and 
#     apply the
bootstrap.list <- mclapply(ranking.list, 
                           weighted_replications.fun, 
                           R = Repl, 
                           mc.cores = nb.cores)
rm(ranking.list); gc()
print("bootstrap.list done")
# A4: We take the ranking.list and apply the the condorcet() and classify-functions upon.
classified.results <- lapply(bootstrap.list, smr.bootstrap.classified)
rm(bootstrap.list); gc()
print("classified.results done")
#
results.df <- as.data.frame(
  lapply(classified.results, unlist), 
  stringsAsFactors = FALSE)
rm(classified.results); gc()

write.csv(results.df, "PartyWeightsBootstrap.csv", row.names = F)

print("Candidate completet")
#################################################################
#load("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/mogon/NEWBOOT/dp.RData")
load("dp.RData")
#Candidate
trank.fun_Candidate_w <- function(case) {
  dat <- d %>% 
    filter(case_ID == case) %>%
    select(starts_with("candidate_rating_"), starts_with("IMD5000_"), IMD1010_3)  # include weights
  include <- which(dat[1, grep("IMD5000_", names(dat))] != 9999999)
  if (case == "Switzerland_2007") { include <- setdiff(include, 7:8) }         #
  if (case == "Spain_2004") { include <- setdiff(include, 4:9) }               #
  if (case == "Great Britain_1997") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2005") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2015") { include <- setdiff(include, c(5,7)) }    #
  keep_cols <- names(dat)[grep("IMD5000_", names(dat))[include]]
  party_cols <- sub("IMD5000_", "candidate_rating_", keep_cols)
  if (length(party_cols) == 0) return(NULL)
  # Subset data to only valid party ratings
  ratings <- dat %>%
    select(all_of(party_cols)) %>%
    select(where(~ !all(is.na(.)))) %>%
    rename_with(~ str_replace(., "party_rating_", ""))  # rename A, B, C...
  # Return NULL if no valid rating columns
  if (ncol(ratings) == 0) return(NULL)
  # Compute row-wise rankings 
  ranked <- as.data.frame(t(apply(ratings, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE))))
  colnames(ranked) <- colnames(ratings)
  # Add weights as an extra column
  ranked$weight <- dat$IMD1010_3
  return(ranked)
}
#
## Analysis
pbw.cases <- dp %>% group_by(case_ID) %>%
  reframe(PBW.var = var(IMD1010_3)) %>%
  filter(., PBW.var > 0) 
caseid.cand <- as.list(pbw.cases$case_ID)
#
ranking.list <- mclapply(caseid.cand, 
                         trank.fun_Candidate_w, 
                         mc.cores = nb.cores)  
names(ranking.list) <- caseid.cand
bootstrap.list <- mclapply(ranking.list, 
                           weighted_replications.fun, 
                           R = Repl, 
                           mc.cores = nb.cores)
rm(ranking.list); gc()
classified.results <- lapply(bootstrap.list, smr.bootstrap.classified)
rm(bootstrap.list); gc()
#
resultsC.df <- as.data.frame(
  lapply(classified.results, unlist), 
  stringsAsFactors = FALSE) %>%
  mutate(across(everything(), ~ str_replace(., "^candidate_rating_", "")))
write.csv(resultsC.df, "CandidateWeightsBootstrap.csv", row.names = F)
###
rm(list =ls())
