#### Barbaro & Kurella: On the prevalence of Condorcet's paradox
library(vote)
library(dplyr)
library(parallel)
library(data.table)
library(stringr)
library(tidyr)
library(hutils)
##
#setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/")
setwd("~")
###
# Load CSES data (no. 5)
#load("Data/cses5.rdata")
load("Data/cses_imd.rdata")

d <- cses_imd %>%
  weight2rows(., "IMD1010_3") %>%
  mutate(cntry = ifelse(IMD1003 %in% c(5611999, 5612019), "B_FL", 
                        ifelse(IMD1003 %in% c(5621999, 5622019), "B_WA", 
                               IMD1006_NAM))) %>%
  mutate(presidential = ifelse(IMD1009==20, 1, 0)) %>%
  mutate(both = ifelse(IMD1009==12, 1, 0)) %>%
  mutate(
    year = as.character(IMD1008_YEAR), # election year
    case_ID = paste(cntry, year, sep = "_") 
  ) %>%
  # omit case_ID == NA
  filter(!is.na(case_ID)) %>%
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

### merge CDU and CSU: start
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
#obs.p <- d %>% group_by(case_ID) %>% reframe(obs = n())
##############################################################################################
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
}

# Run the function over unique case_ID values
caseid.party <- as.list(unique(d$case_ID))
## List of ranking tables
ranking.list <- mclapply(caseid.party, trank.fun, mc.cores = 8)
names(ranking.list) <- caseid.party
res.partyW <- mclapply(ranking.list, FUN = condorcet, runoff = F, quiet = T, mc.cores = 14)  
names(res.partyW) <- caseid.party

smr.result.fun <- function(case, result) {
  data.frame(
    case_ID = case,
    elected = ifelse(!is.null(result$elected), result$elected, NA),
    loser = ifelse(!is.null(result$loser), result$loser, NA),
    num_parties = ifelse(!is.null(result$totals), ncol(result$totals), NA)
  )
}

results.df <- do.call(rbind, Map(smr.result.fun, as.list(names(res.partyW)), res.partyW))  %>%
  filter(., num_parties > 2) %>% filter(!if_all(everything(), is.na))

results.df %>% filter(., is.na(elected) == T)
results.df %>% filter(., is.na(loser) == T)
################################################################################
res.borda <- mclapply(ranking.list, 
                      FUN = score, nseats = 1, larger.wins = F, quiet = T, 
                      mc.cores = 8)
names(res.borda) <- caseid.party
#
bordaresults.df <- data.frame(
  case_ID = names(res.borda),
  elected = sapply(res.borda, function(x) if (!is.null(x$elected)) x$elected else NA)
)
####################################################################################
res.stv <- mclapply(ranking.list, 
                    FUN = stv, nseats = 1, quiet = T, equal.ranking = TRUE, 
                    mc.cores = 8)
names(res.stv) <- caseid.party
#
stvresults.df <- data.frame(
  case_ID = names(res.stv),
  elected = sapply(res.stv, function(x) if (!is.null(x$elected)) x$elected else NA)
)
######################################################################################
binary_ranking.list <- lapply(ranking.list, function(mat) {
  ifelse(mat == 1, 1, 0)  # Convert values to 1 if they are 1, otherwise 0
})
res.plurality <- mclapply(binary_ranking.list,
                          FUN = plurality, nseats = 1, quiet = TRUE ,
                          mc.cores = 8)
names(res.plurality) <- caseid.party
#
pluralityresults.df <- data.frame(
  case_ID = names(res.plurality),
  elected = sapply(res.plurality, function(x) if (!is.null(x$elected)) x$elected else NA)
)


compare.df <- results.df %>% rename(Condorcet = elected) %>% select(., c("case_ID", "Condorcet")) %>%
  left_join(x = .,
            y = bordaresults.df %>% rename(Borda = elected),
            by = "case_ID") %>%
  left_join(x = .,
            y = stvresults.df %>% rename(STV = elected), 
            by = "case_ID") %>%
  left_join(x = .,
            y = pluralityresults.df %>% rename(Plurality = elected), 
            by = "case_ID")
rm(pluralityresults.df, res.borda, res.plurality, res.stv, stvresults.df, binary_ranking.list, bordaresults.df)
## Set Party Names to compare.df
csespartycsv <- read.csv("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/RScripts/csespartycsv.csv")
partyname.fun <- function(case){
  partycodes <- d %>% filter(., case_ID == case) %>% 
    select(c("case_ID", starts_with("IMD5000_"))) %>%
    distinct() %>%
    separate(case_ID, into = c("Countryname", "Year"), sep = "_", remove = FALSE) %>%
    pivot_longer(cols = starts_with("IMD5000"), names_to = "Letter", values_to = "Value") %>%
    mutate(Letter = LETTERS[1:n()]) %>% rename(partyID = Value)
  csescode <- csespartycsv %>% filter(., Country %in% partycodes$Countryname) 
  merged.df <- left_join(x = partycodes,
                         y = csescode,
                         by = "partyID")
  return(merged.df)
}
#
partynames.list <- lapply(caseid.party, partyname.fun)
partyname.df <- bind_rows(partynames.list) %>% filter(., is.na(Country) == F)
rm(partynames.list)
#
compare2.df <- compare.df %>%
  left_join(x = .,
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>%
              rename(Condorcet = Letter),
            by = c("case_ID", "Condorcet")) %>%
  rename(Cwinner_Name = partyNAME) %>%
  left_join(x = ., 
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(Borda = Letter),
            by = c("case_ID", "Borda"), relationship = "many-to-many") %>%
  rename(Borda_Name = partyNAME) %>%
  left_join(x = ., 
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(STV = Letter),
            by = c("case_ID", "STV"), relationship = "many-to-many") %>%
  rename(STV_Name = partyNAME) %>%
  left_join(x = ., 
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(Plurality = Letter),
            by = c("case_ID", "Plurality"), relationship = "many-to-many") %>%
  rename(Plurality_Name = partyNAME) 
#####################################################################
results2.df <- results.df %>%
  left_join(x = .,
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(elected = Letter, Cwinner_party = partyNAME),
            by = c("case_ID", "elected")) %>%
  left_join(x = ., 
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(loser = Letter, Closer_party = partyNAME),
            by = c("case_ID", "loser"))

#####################################################################
compare3.df <- compare2.df %>%
  left_join(x = ., 
            y = results2.df %>% select(., c("case_ID", "loser", "Closer_party")),
            by = "case_ID")
bordaparadox <- compare3.df %>% filter(., Plurality == loser)  # two occurences of the Borda paradox, both in Switzerland.
#######################################################################
########################################################################
### Candidate elections

omitted.cases <- c("Chile_1999", "Chile_2005", "France_2002", "Kyrgyzstan_2005", "Peru_2006", 
                   "Philippines_2004", "Romania_2004", "Taiwan_2004", 
                   "United States of America_2004", "South Africa_2014")

dp <- d %>% filter(., both == 1) %>% filter(., !case_ID %in% omitted.cases )
#
#obs.p2 <- dp %>% group_by(case_ID) %>% reframe(obs = n())
#obs <- rbind(obs.p, obs.p2)
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
ranking.list <- mclapply(caseid.candidate, trank.fun2, mc.cores = 8)
names(ranking.list) <- caseid.candidate
res.candidateW <- mclapply(ranking.list, FUN = condorcet, runoff = F, quiet = T, mc.cores = 12)  
names(res.candidateW) <- caseid.candidate
#
results_candidates.df <- do.call(rbind, 
                                 Map(smr.result.fun, 
                                     as.list(names(res.candidateW)), res.candidateW))  %>%
  filter(., num_parties > 2) %>% filter(!if_all(everything(), is.na)) 
######  END CANDIDATE PART ####################################################
###############################################################################

## Agreement proportion matrix
agreement_matrix <- function(data) {
  methods <- data[, 2:5]  
  n_methods <- ncol(methods)
  
  agreement <- matrix(NA, n_methods, n_methods)
  colnames(agreement) <- rownames(agreement) <- colnames(methods)
  
  for (i in 1:n_methods) {
    for (j in i:n_methods) {
      agreement[i, j] <- mean(methods[, i] == methods[, j], na.rm = TRUE)
      agreement[j, i] <- agreement[i, j]  # Symmetric matrix
    }
  }
  return(as.data.frame(agreement))
}
#
agreement_df <- agreement_matrix(compare.df)
print(agreement_df)
c <- corrplot::corrplot(as.matrix(agreement_df), method = 'pie', type = "upper", bg = "gray80")

ggsave("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/corrplot.pdf", plot = c)
############# Fleiss Kappa
library(DescTools)
ratings <- compare.df %>% select(., -c("case_ID"))
ratings.table <- ratings  %>%
  rowwise() %>%
  mutate(counts = list(table(factor(c_across(everything()), levels = unique(unlist(ratings)))))) %>%
  tidyr::unnest_wider(counts, names_sep = "_") %>%
  dplyr::select(starts_with("counts_")) %>%
  as.matrix()

# Calculate Fleiss' Kappa
kappa_result <- KappaM(ratings.table, method = "Fleiss", conf.level = 0.95)
print(kappa_result)

### Kendall's tau - correlation
ratings.factor <- ratings %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))
kendall_tau_matrix <- cor(ratings.factor, method = "kendall", use = "pairwise.complete.obs")

# Print the Kendall's Tau correlation matrix
print(kendall_tau_matrix)
#corrplot::corrplot(kendall_tau_matrix, method = "pie", type = "upper")


to.keep <- c("res.candidateW", "res.partyW")
rm(list = setdiff(ls(), to.keep))
save.image("resW.RData")

