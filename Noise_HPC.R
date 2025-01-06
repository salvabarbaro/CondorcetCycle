#!/usr/bin/env Rscript
#
load("d.RData")
#
library(dplyr)
library(vote)
library(parallel)
library(data.table)  # For frank()
##################################################################################################
ns.lvl <- 1.1          # Define noise level
r <- 10000           # Number of replications
nb.cores <- 38        # Number of cores for parallel processing
# Define functions
add_noise <- function(data, ns.lvl) {
  data %>%
    mutate(across(everything(), ~ . + runif(n = n(), min = -ns.lvl, max = ns.lvl)))
}
###########################
ranking.fun <- function(dat) {
  as.data.frame(t(apply(dat, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE)))) %>%
    setNames(colnames(dat))
}
############################
condorcetTieCycle.fun <- function(profile) {
  result <- tryCatch(
    condorcet(profile, quiet = TRUE), 
    error = function(e) return(NULL)  # Return NULL if there's an error
  )
  if (is.null(result) || is.null(result$elected)) {
    if (!is.null(result[["totals"]])) {
      totals_df <- result[["totals"]] %>%
        as.data.frame() %>%
        mutate(wins = rowSums(.))
      
      win_counts <- table(totals_df$wins)
      
      if (any(win_counts == 2)) {
        return("Tie")
      } else if (any(win_counts == 3)) {
        return("Cycle")
      } else {
        return("Tie")
      }
    } else {
      return("nototals")
    }
  } else {
    return(result$elected[1])
  }
}
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
results_list <- list()         
set.seed(55234)
#
for (case in case_IDs) {
  case_data <- d %>%
    filter(case_ID == case) %>%
    select(starts_with("party_rating_")) %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  profile_list <- replicate(r, add_noise(case_data, ns.lvl), simplify = FALSE)
  ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
  smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
  results_list[[case]] <- unlist(smr.list)
}

results.df <- do.call(cbind, results_list)
colnames(results.df) <- case_IDs
results.df <- as.data.frame(results.df)

write.csv(results.df, "NoiseParty110.csv", row.names = F)

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
results_list <- list()         
#
for (case in case_IDs) {
  case_data <- d %>%
    filter(case_ID == case) %>%
    select(starts_with("candidate_rating_")) %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  profile_list <- replicate(r, add_noise(case_data, ns.lvl), simplify = FALSE)
  ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
  smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
  results_list[[case]] <- unlist(smr.list)
}

results.df <- do.call(cbind, results_list)
colnames(results.df) <- case_IDs
results.df <- as.data.frame(results.df)

write.csv(results.df, "NoiseCandidate110.csv", row.names = F)
############################################################################################
### Special Cases
## Great Britain_1997
gb1997 <- D %>% filter(., case_ID == "Great Britain_1997") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(gb1997, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
gb1997.cw <- unlist(smr.list)
rm(gb1997)

## Great Britain_2005
gb2005 <- D %>% filter(., case_ID == "Great Britain_2005") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(gb2005, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
gb2005.cw <- unlist(smr.list)
rm(gb2005)

## Great Britain_2015
gb2015 <- D %>% filter(., case_ID == "Great Britain_2015") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", "party_rating_D", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(gb2015, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
gb2015.cw <- unlist(smr.list)
rm(gb2015)
#
## Great Britain_2017
gb2017 <- D %>% filter(., case_ID == "Great Britain_2017") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(gb2017, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
gb2017.cw <- unlist(smr.list)
rm(gb2017)
#
## Great Britain_2019
gb2019 <- D %>% filter(., case_ID == "Great Britain_2019") %>%
#    select(starts_with("party_rating_")) #
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(gb2019, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
gb2019.cw <- unlist(smr.list)
rm(gb2019)
#
## Spain_2004
es2004 <- D %>% filter(., case_ID == "Spain_2004") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(es2004, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
es2004.cw <- unlist(smr.list)
rm(es2004)

## Switzerland_2007
ch2007 <- D %>% filter(., case_ID == "Switzerland_2007") %>%
  select(., c("party_rating_A", "party_rating_B", "party_rating_C", 
              "party_rating_D", "party_rating_E", "party_rating_F")) %>%
  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(ch2007, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
ch2007.cw <- unlist(smr.list)
rm(ch2007)
##
## India 2019
in2019 <- D %>% filter(., case_ID == "India_2019") %>%
      select(starts_with("party_rating_")) #%>%
#  select(., c("party_rating_A", "party_rating_B", "party_rating_C", "party_rating_E", "party_rating_F")) %>%
#  filter(if_all(everything(), ~ !is.na(.)))
profile_list <- replicate(r, add_noise(in2019, ns.lvl), simplify = FALSE)
ranked_profiles <- mclapply(profile_list, ranking.fun, mc.cores = nb.cores)
smr.list <- mclapply(ranked_profiles, condorcetTieCycle.fun, mc.cores = nb.cores)
in2019.cw <- unlist(smr.list)
rm(in2019)



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
rm(list = ls())
#### end of script)
