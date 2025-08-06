#!/usr/bin/env Rscript
#
library(dplyr)  # standard
library(vote)   # for condorcet()
library(parallel)  # for mclapply (suitable for UNIX/LINUX only!)
library(data.table)  # For frank()
library(stringr)  # for renaming items

#setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/mogon/")
load("d.RData")
#
##################################################################################################
ns.lvl <- 1.1          # Define noise level
r <- 10000              # Number of replications
nb.cores <- 38         # Number of cores for parallel processing
# Define functions
# Function 1: add_noise adds a random noise to a set of ratings
add_noise <- function(data, ns.lvl) {
  data %>%
    mutate(across(everything(), ~ . + runif(n = n(), min = -ns.lvl, max = ns.lvl)))
}
# Function 2: converts a set of ratings to a profile
ranking.fun <- function(dat) {
  as.data.frame(t(apply(dat, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE)))) %>%
    setNames(colnames(dat))
}
# Function 3: if a Condorcet winner exists: CW. If not, check whether a tie or actually a cycle is present.
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
#
######################################################################################
# Main Script
## Party Rating Evaluation
D <- d
removed.party <- c("Switzerland_2007", "Spain_2004", 
                   "Great Britain_1997", "Great Britain_2005", 
                   "Great Britain_2015", "Great Britain_2017", 
                   "Great Britain_2019", "India_2019",                    # Special Cases, see below
                   "Russian Federation_2000", "Russian Federation_2004")  # Russia remains removed.
d <- D %>% filter(., !case_ID %in% removed.party)
case_IDs <- unique(d$case_ID)  
#results_list <- list()         
set.seed(55234)
#
############################################################
## Main Function
RandomNoise.fun <- function(case){
case.data <- d %>%
  filter(case_ID == case) %>%
  select(starts_with("party_rating_")) %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list <- replicate(r, add_noise(case.data, ns.lvl), simplify = FALSE)
ranked.profiles <- lapply(profile.list, ranking.fun)
rm(profile.list) 
gc()
condorcet.list  <- lapply(ranked.profiles, condorcet, quiet = T, runoff = F)
rm(ranked.profiles) 
gc()
classified.list <- lapply(condorcet.list, classify.fun)
rm(condorcet.list)
gc()
return(classified.list)
}

RandomNoise.resultlist <- mclapply(case_IDs, RandomNoise.fun, mc.cores = nb.cores)
names(RandomNoise.resultlist) <- case_IDs
###############################################################################
results.df <- do.call(cbind, RandomNoise.resultlist) %>% 
  as.data.frame(.) %>% 
  setNames(., case_IDs) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))

write.csv(results.df, "NEWNoiseParty110.csv", row.names = F)

#################################################################################
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
write.csv(summary_table, "RNsummarytableP.csv")
##############################################################
### END OF PART 1: PARTIES
##############################################################
### START OF PART 2: CANDIDATES
included.candidates <- c("Argentina_2015", "Belarus_2001", "Brazil_2002", "Brazil_2006", "Brazil_2018",
                         "Chile_2009", "Chile_2017", "Costa Rica_2018", "El Salvador_2019", "France_2012",
                         "France_2017", "Kenya_2013", "Lithuania_1997", "Mexico_2000", "Mexico_2006", 
                         "Mexico_2012", "Mexico_2018", "Peru_2000", "Peru_2001", "Peru_2011", "Peru_2016",
                         "Peru_2021", "Philippines_2010", "Philippines_2016", "Romania_1996", "Romania_2009", 
                         "Romania_2014", "Serbia_2012", "Taiwan_1996", "Taiwan_2008", "Taiwan_2012", "Taiwan_2016",
                         "Taiwan_2020", "Tunisia_2019", "Turkey_2018", "Uruguay_2009", "Uruguay_2019")
## to consider later: Chile_1999, France_2002, Taiwan_2004
d <- D %>% filter(., case_ID %in% included.candidates)
case_IDs <- unique(d$case_ID)  
#
## Main Function (Candidates)
RandomNoiseC.fun <- function(case){
  case.data <- d %>%
    filter(case_ID == case) %>%
    select(starts_with("candidate_rating_")) %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  profile.list <- replicate(r, add_noise(case.data, ns.lvl), simplify = FALSE)
#  ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
#  condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
#  classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)
ranked.profiles <- lapply(profile.list, ranking.fun)
rm(profile.list)
gc()
condorcet.list  <- lapply(ranked.profiles, condorcet, quiet = T, runoff = F)
rm(ranked.profiles)
gc()
classified.list <- lapply(condorcet.list, classify.fun)
rm(condorcet.list)
gc()
  return(classified.list)
}

RandomNoiseC.resultlist <- mclapply(case_IDs, RandomNoiseC.fun, mc.cores = nb.cores)
names(RandomNoiseC.resultlist) <- case_IDs
gc()
###############################################################################
results.df <- do.call(cbind, RandomNoiseC.resultlist) %>% 
  as.data.frame(.) %>% 
  setNames(., case_IDs) %>%
  mutate(across(everything(), ~ str_replace(., "^candidate_rating_", "")))

write.csv(results.df, "NEWNoiseCand110.csv", row.names = F)

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
write.csv(summary_table, "RNsummarytableC.csv")
rm(list = ls())
## Special cases (because of distinct data pattern)
gb1997 <- D %>% filter(., case_ID == "Great Britain_1997") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(gb1997, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

gb1997.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Great Britain_1997")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(gb1997, classified.list, condorcet.list)
#
gb2005 <- D %>% filter(., case_ID == "Great Britain_2005") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(gb2005, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)
gb2005.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Great Britain_2005")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(gb2005, condorcet.list, classified.list)
#
gb2015 <- D %>% filter(., case_ID == "Great Britain_2015") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", 
              "party_rating_D", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(gb2015, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

gb2015.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Great Britain_2015")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(gb2015, condorcet.list, classified.list)
# #### end of script)
gb2017 <- D %>% filter(., case_ID == "Great Britain_2017") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", 
              "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(gb2017, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

gb2017.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Great Britain_2017")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(gb2017, condorcet.list, classified.list)
#
gb2019 <- D %>% filter(., case_ID == "Great Britain_2019") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", 
              "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(gb2019, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

gb2019.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Great Britain_2019")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(gb2019, condorcet.list, classified.list)
#
es2004 <- D %>% filter(., case_ID == "Spain_2004") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(es2004, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

es2004.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Spain_2004")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(es2004, condorcet.list, classified.list)
#
ch2007 <- D %>% filter(., case_ID == "Switzerland_2007") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", 
              "party_rating_D", "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile.list    <- replicate(r, add_noise(ch2007, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

ch2007.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("Switzerland_2007")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(ch2007, condorcet.list, classified.list)
#
## India 2019
in2019 <- D %>% filter(., case_ID == "India_2019") %>%
  select(c("party_rating_A", "party_rating_B", "party_rating_F")) 
profile.list    <- replicate(r, add_noise(in2019, ns.lvl), simplify = FALSE)
ranked.profiles <- mclapply(profile.list, ranking.fun, mc.cores = nb.cores)
rm(profile.list)
gc()
condorcet.list  <- mclapply(ranked.profiles, condorcet, quiet = T, runoff = F, mc.cores = nb.cores)
rm(ranked.profiles)
gc()
classified.list <- mclapply(condorcet.list, classify.fun, mc.cores = nb.cores)

in2019.cw <- unlist(classified.list) %>%
  as.data.frame(.) %>%
  setNames(., c("India_2019")) %>%
  mutate(across(everything(), ~ str_replace(., "^party_rating_", "")))
rm(in2019, condorcet.list, classified.list)
#
specialparty.df <- data.frame("Great Britain_1997" = gb1997.cw,
                              "Great Britain_2005" = gb2005.cw,
                              "Great Britain_2015" = gb2015.cw,
                              "Great Britain_2017" = gb2017.cw,
                              "Great Britain_2019" = gb2019.cw,
                              "Switzerland_2007" = ch2007.cw,
                              "Spain_2004" = es2004.cw,
                              "India_2019" = in2019.cw)
#rm(ch2007.cw, es2004.cw, gb1997.cw, gb2005.cw, gb2015.cw, in2019.cw, gb2017.cw, gb2019.cw)
write.csv(specialparty.df, "NoiseSpecial110.csv", row.names = F)
specialparty.df <- read.csv("NoiseSpecial110.csv", header = T)
#
results_long <- specialparty.df %>%
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

write.csv(summary_table, "SpecialSummaryTableP.csv", row.names = F)
rm(list = ls())
