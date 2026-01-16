#### Barbaro & Kurella: On the prevalence of Condorcet's paradox
library(vote)
library(dplyr)
library(parallel)
library(data.table)
library(stringr)
library(tidyr)
library(hutils)
##
#setwd("~")
###
# Load CSES data 
#load("Data/cses_imd.rdata") as cses_imd
load("d.RData")
###################################################
#####   C H A P T E R  1: Base Inference ##########
# The purpose of this chapter is to generate a    #
# list of rankings derived from the thermometer-  #
# style data. Further, we generate a file d.Rdata #
# that consists basically of the CSES data after  #
# out adjustments made in the following lines.    #
###################################################
# Chapter I.1: Parliamentary Elections            #
###################################################
d <- cses_imd %>%
#  weight2rows(., "IMD1010_3") %>%
  mutate(cntry = ifelse(IMD1003 %in% c(5611999, 5612019), "B_FL", 
                 ifelse(IMD1003 %in% c(5621999, 5622019), "B_WA", 
                 IMD1006_NAM))) %>%   # we bind togehter Belgium
  mutate(presidential = ifelse(IMD1009==20, 1, 0)) %>%  # Dummy for presidential elections
  mutate(both = ifelse(IMD1009==12, 1, 0)) %>%
  mutate(
    year = as.character(IMD1008_YEAR), # election year
    case_ID = paste(cntry, year, sep = "_") 
  ) %>%   # we generate a variable consisting of Country_Year
  filter(!is.na(case_ID)) %>%    # omit case_ID == NA
  # Generate individual party ratings, where 0 = strongly dislike, 10 = strongly like
  mutate(across(
    starts_with("IMD3008_"),  
    ~ ifelse(. < 11, ., NA),
    .names = "party_rating_{substr(.col, 9, 9)}"
  )) %>% 
  group_by(case_ID) %>%
  mutate(across(
    starts_with("party_rating_"),
    ~ ifelse(. == 11, NA, .)
  )) %>%
  filter(!if_all(everything(), is.na)) %>%
  ungroup() %>%
  # Generate candidate rating
  mutate(across(
    starts_with("IMD3009_"),
    ~ ifelse(. < 11, ., NA),
    .names = "candidate_rating_{substr(.col, 9, 9)}"
  )) %>%
  group_by(case_ID) %>%
  mutate(across(
    starts_with("candidate_rating_"), 
    ~ ifelse(. == 11, NA, .)
  )) %>%
  filter(!if_all(everything(), is.na)) %>%
  ungroup() %>%
  mutate(across(    # Add the left-right ideological position of the parties / alternatives
    starts_with("IMD3007_"),
    ~ ifelse(. <= 10, ., NA),
    .names = "leri_party_{substr(.col, 9, 9)}"
  )) %>%
  filter(., !case_ID == "Switzerland_2019") %>%
  filter(., !cntry == "Belgium")

### merge CDU and CSU to one party: start
## Approach: if participants live in Bavaria, use the CSU rating, CDU otherwise.
# Ger 1998
d$party_rating_B[d$case_ID=="Germany_1998" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_C[d$case_ID=="Germany_1998" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_C[d$case_ID=="Germany_1998"] <- NA
# Ger 2002
d$party_rating_B[d$case_ID=="Germany_2002" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_C[d$case_ID=="Germany_2002" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_C[d$case_ID=="Germany_2002"] <- NA
# Ger 2005
d$party_rating_B[d$case_ID=="Germany_2005" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_F[d$case_ID=="Germany_2005" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_F[d$case_ID=="Germany_2005"] <- NA
# Ger 2009
d$party_rating_A[d$case_ID=="Germany_2009" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_F[d$case_ID=="Germany_2009" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_F[d$case_ID=="Germany_2009"] <- NA
# Ger 2013
d$party_rating_A[d$case_ID=="Germany_2013" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_E[d$case_ID=="Germany_2013" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_E[d$case_ID=="Germany_2013"] <- NA
# Ger 2017
d$party_rating_A[d$case_ID=="Germany_2017" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_G[d$case_ID=="Germany_2017" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_G[d$case_ID=="Germany_2017"] <- NA
# Ger 2021
d$party_rating_A[d$case_ID=="Germany_2021" & d$IMD2008==2276 & !is.na(d$IMD2008)] <- d$party_rating_G[d$case_ID=="Germany_2021" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_G[d$case_ID=="Germany_2021"] <- NA

#### end 
rm(cses_imd)  # no longer required.
gc()
######## 
## save d as d.RData in order to simplify the boostrap / RN replications.
#obs.p <- d %>% group_by(case_ID) %>% reframe(obs = n())
##############################################################################################
## We define our function to transform thermometer-style data to
## orderings. 
trank.fun <- function(case) {
  dat <- d %>% 
    filter(case_ID == case) %>%
    select(starts_with("party_rating_"), starts_with("IMD5000_"))  
  # Identify valid IMD5000_* columns and map to corresponding party_rating_* columns
  include <- which(dat[1, grep("IMD5000_", names(dat))] != 9999999)
  # Apply case-specific adjustments to the 'include' vector by excluding specific indices
  if (case == "Switzerland_2007") { include <- setdiff(include, 7:8) }         #
  if (case == "Spain_2004") { include <- setdiff(include, 4:9) }               #
  if (case == "Great Britain_1997") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2005") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2015") { include <- setdiff(include, c(5,7)) }    #
  ###############################################################################
  keep_cols <- names(dat)[grep("IMD5000_", names(dat))[include]]
  party_cols <- sub("IMD5000_", "party_rating_", keep_cols)
  # Check if there are any valid party_rating_* columns to process
  if (length(party_cols) == 0) {
    return(NULL)  # Return NULL if no valid columns are found
  }
  # Subset dat to include only relevant party_rating columns, removing NA-only columns
  dat <- dat %>%
    select(all_of(party_cols)) %>%
    select_if(~ !all(is.na(.))) %>%
    rename_with(~ str_replace(., "party_rating_", ""))  
  # Ensure dat still has columns after removing NA-only columns
  if (ncol(dat) == 0) {
    return(NULL)  # Return NULL if no columns remain after filtering
  }
  # Perform row-wise ranking
  ranked.df <- as.data.frame(t(apply(dat, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE)))) %>%
    setNames(colnames(dat))
} ## Note: frank is more efficient, but you can also use the simple rank()-fun.

# Run the function over unique case_ID values
caseid.party <- as.list(unique(d$case_ID))
#caseid.party <- as.list(c("France_2007", "Italy_2018", "Germany_1998"))
## List of ranking tables
ranking.list <- lapply(caseid.party, trank.fun)  # takes a few minutes, much faster with mclapply()
names(ranking.list) <- caseid.party
#rm(d)
################ save ranking.list 
## save.image(....)
##########################################################################
## optional: by running res.party, you get a list of Condorcet elections.
## this function is the dplyr-equivalent to the main replication file.
#res.party <- mclapply(ranking.list, FUN = condorcet, runoff = F, quiet = T, mc.cores = 14)  
#names(res.party) <- caseid.party

#smr.result.fun <- function(case, result) {
#  data.frame(
#    case_ID = case,
#    elected = ifelse(!is.null(result$elected), result$elected, NA),
#    loser = ifelse(!is.null(result$loser), result$loser, NA),
#    num_parties = ifelse(!is.null(result$totals), ncol(result$totals), NA)
#  )
#}
#results.df <- do.call(rbind, Map(smr.result.fun, as.list(names(res.party)), res.party))  %>%
#  filter(., num_parties > 2) %>% filter(!if_all(everything(), is.na))


###################################################
# Chapter I.1: Presidential Elections             #
###################################################
## Some cases were omitted, mostly because only two viable 
#  candidates (see the paper for other reasons)
omitted.cases <- c("Chile_1999", "Chile_2005", "France_2002", "Kyrgyzstan_2005", "Peru_2006", 
                   "Philippines_2004", "Romania_2004", "Taiwan_2004", 
                   "United States of America_2004", "South Africa_2014")

dp <- d %>% 
  filter(., both == 1) %>% 
  filter(., !case_ID %in% omitted.cases )
## save dp as dp.RData in order to simplify the boostrap / RN replications.
#
#obs.p2 <- dp %>% group_by(case_ID) %>% reframe(obs = n())
#obs <- rbind(obs.p, obs.p2)
### trank.fun2: same as trank.fun but adjusted on candidate_rating
trank.fun2 <- function(case) {
  dat <- dp %>% 
    filter(case_ID == case) %>%
    select(starts_with("candidate_rating_"), starts_with("IMD5000_"))  
  # Identify valid IMD5000_* columns and map to corresponding party_rating_* columns
  include <- which(dat[1, grep("IMD5000_", names(dat))] != 9999999)
  # Apply case-specific adjustments to the 'include' vector by excluding specific indices
  if (case == "Switzerland_2007") { include <- setdiff(include, 7:8) }         #
  if (case == "Spain_2004") { include <- setdiff(include, 4:9) }               #
  if (case == "Great Britain_1997") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2005") { include <- setdiff(include, 4:5) }       #
  if (case == "Great Britain_2015") { include <- setdiff(include, c(5,7)) }    #
  ###############################################################################
  keep_cols <- names(dat)[grep("IMD5000_", names(dat))[include]]
  candidate_cols <- sub("IMD5000_", "candidate_rating_", keep_cols)
  # Check if there are any valid party_rating_* columns to process
  if (length(candidate_cols) == 0) {
    return(NULL)  # Return NULL if no valid columns are found
  }
  # Subset dat to include only relevant party_rating columns, removing NA-only columns
  dat <- dat %>%
    select(all_of(candidate_cols)) %>%
    select_if(~ !all(is.na(.))) %>%
    rename_with(~ str_replace(., "party_rating_", ""))  
  # Ensure dat still has columns after removing NA-only columns
  if (ncol(dat) == 0) {
    return(NULL)  # Return NULL if no columns remain after filtering
  }
  # Perform row-wise ranking
  ranked.df <- as.data.frame(t(apply(dat, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE)))) %>%
    setNames(colnames(dat))
}
#
caseid.candidate <- as.list(unique(dp$case_ID))
## List of ranking tables
Cranking.list <- lapply(caseid.candidate, trank.fun2) # quick!
names(Cranking.list) <- caseid.candidate
## save Cranking.list as Crankinglist.Rdata
# rm(...)
# save.image(Crankinglist.RData)

####################################################################
###################################################
#####   C H A P T E R  2: PB-Weights     ##########
# The purpose of this chapter is to generate a    #
# list of rankings derived from the thermometer-  #
# style data for the cases where Political-Beh-   #
# aviour Weights are available.                   #
#                                                 #
###################################################
# Chapter I.1: Parliamentary Elections            #
###################################################
# In a first step, we select the case_ID for whom PBW are available.
pbw.cases <- d %>% group_by(case_ID) %>%
  reframe(PBW.var = var(IMD1010_3)) %>%
  filter(., PBW.var > 0) %>%
  filter(., !case_ID %in% c("B_FL_1999", "B_FL_2019", 
                            "B_WA_1999", "B_WA_2019"))


d_w <- cses_imd %>%
    weight2rows(., "IMD1010_3") %>%
  mutate(cntry = ifelse(IMD1003 %in% c(5611999, 5612019), "B_FL", 
                 ifelse(IMD1003 %in% c(5621999, 5622019), "B_WA", 
                 IMD1006_NAM))) %>%   # we bind togehter Belgium
  mutate(presidential = ifelse(IMD1009==20, 1, 0)) %>%  # Dummy for presidential elections
  mutate(both = ifelse(IMD1009==12, 1, 0)) %>%
  mutate(
    year = as.character(IMD1008_YEAR), # election year
    case_ID = paste(cntry, year, sep = "_") 
  ) %>%   # we generate a variable consisting of Country_Year
  filter(!is.na(case_ID)) %>%    # omit case_ID == NA
  # Generate individual party ratings, where 0 = strongly dislike, 10 = strongly like
  mutate(across(
    starts_with("IMD3008_"),  
    ~ ifelse(. < 11, ., NA),
    .names = "party_rating_{substr(.col, 9, 9)}"
  )) %>% 
  group_by(case_ID) %>%
  mutate(across(
    starts_with("party_rating_"),
    ~ ifelse(. == 11, NA, .)
  )) %>%
  filter(!if_all(everything(), is.na)) %>%
  ungroup() %>%
  # Generate candidate rating
  mutate(across(
    starts_with("IMD3009_"),
    ~ ifelse(. < 11, ., NA),
    .names = "candidate_rating_{substr(.col, 9, 9)}"
  )) %>%
  group_by(case_ID) %>%
  mutate(across(
    starts_with("candidate_rating_"), 
    ~ ifelse(. == 11, NA, .)
  )) %>%
  filter(!if_all(everything(), is.na)) %>%
  ungroup() %>%
  mutate(across(    # Add the left-right ideological position of the parties / alternatives
    starts_with("IMD3007_"),
    ~ ifelse(. <= 10, ., NA),
    .names = "leri_party_{substr(.col, 9, 9)}"
  )) %>%
  filter(., !case_ID == "Switzerland_2019") %>%
  filter(., !cntry == "Belgium") %>%
  filter(., case_ID %in% pbw.cases$case_ID)



###### new approach: consider the weights in the ranking-function
trank.fun_w <- function(case) {
  dat <- d %>% 
    filter(case_ID == case) %>%
    select(starts_with("party_rating_"), starts_with("IMD5000_"), IMD1010_3)  # include weights
  # Identify valid IMD5000_* columns and map to corresponding party_rating_* columns
  include <- which(dat[1, grep("IMD5000_", names(dat))] != 9999999)
  
  # Apply case-specific adjustments to the 'include' vector
  if (case == "Switzerland_2007")     include <- setdiff(include, 7:8)
  if (case == "Spain_2004")           include <- setdiff(include, 4:9)
  if (case == "Great Britain_1997")   include <- setdiff(include, 4:5)
  if (case == "Great Britain_2005")   include <- setdiff(include, 4:5)
  if (case == "Great Britain_2015")   include <- setdiff(include, c(5, 7))
  
  # Get valid column names
  keep_cols <- names(dat)[grep("IMD5000_", names(dat))[include]]
  party_cols <- sub("IMD5000_", "party_rating_", keep_cols)
  
  # Return NULL if no valid party columns found
  if (length(party_cols) == 0) return(NULL)
  
  # Subset data to only valid party ratings
  ratings <- dat %>%
    select(all_of(party_cols)) %>%
    select(where(~ !all(is.na(.)))) %>%
    rename_with(~ str_replace(., "party_rating_", ""))  # rename A, B, C...
  
  # Return NULL if no valid rating columns
  if (ncol(ratings) == 0) return(NULL)
  
  # Compute row-wise rankings (higher rating = better rank)
  ranked <- as.data.frame(t(apply(ratings, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE))))
  colnames(ranked) <- colnames(ratings)
  
  # Add weights as an extra column
  ranked$weight <- dat$IMD1010_3
  
  return(ranked)
}

caseid.party <- as.list(pbw.cases$case_ID)
#caseid.party <- as.list(c("France_2007", "Italy_2018", "Germany_1998"))
## List of ranking tables
ranking.list <- mclapply(caseid.party, trank.fun_w, mc.cores = 12)  # takes a few minutes, much faster with mclapply()
names(ranking.list) <- caseid.party

# Next we use the ranking list (with weight information) to generate 
# bootstrap replication of them.

weighted_replications.fun <- function(df, R = 100) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!"weight" %in% names(df)) stop("No weight column found.")
  
  # Keep only rows with positive and finite weights
  df <- df %>% filter(!is.na(weight) & weight > 0 & is.finite(weight))
  
  n <- nrow(df)
  if (n == 0) {
    warning("All rows had invalid weights. Skipping case.")
    return(NULL)
  }
  
  w <- df$weight
  
  replicate(R, {
    sample_indices <- sample(seq_len(n), size = n, replace = TRUE, prob = w)
    df[sample_indices, ]
  }, simplify = FALSE)
}


set.seed(55234)  # for reproducibility

bootstrap.list <- mclapply(ranking.list, 
                           weighted_replications.fun, 
                           R = 100, 
                           mc.cores = 12)
rm(ranking.list); gc()
library(vote)

smr.bootstrap.w <- function(bootstrap_list) {
  if (is.null(bootstrap_list)) return(NULL)
  
  lapply(bootstrap_list, function(df) {
    # Remove the weight column
    df_clean <- df[, setdiff(names(df), "weight")]
    tryCatch(
      condorcet(df_clean, quiet = TRUE, runoff = F),
      error = function(e) NULL  # Handle cases where condorcet() fails
    )
  })
}

smr.res <- mclapply(bootstrap.list, smr.bootstrap.w, mc.cores = 4)

rm(bootstrap.list); gc()

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

results.austria <- lapply(smr.res$Austria_2017, classify.fun) # %>% unlist(.)
results.austria <- lapply(smr.res[[1]], classify.fun)  %>% unlist(.)
results <- lapply(1:44, 
                  FUN = function(i){lapply(smr.res[[i]], 
                                           classify.fun)})



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

classified.results <- lapply(bootstrap.list, smr.bootstrap.classified)

results.df <- as.data.frame(
  lapply(classified.results, unlist), 
  stringsAsFactors = FALSE)


