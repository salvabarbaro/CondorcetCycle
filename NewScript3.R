#### Barbaro & Kurella: On the prevalence of Condorcet's paradox
library(vote)
library(dplyr)
library(parallel)
library(data.table)
library(stringr)
##
setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/")
#setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetCycle/")
###
# Load CSES data (no. 5)
load("Data/cses5.rdata")
load("Data/cses_imd.rdata")

d <- cses_imd %>%
  mutate(
    cntry = IMD1006_NAM,  # country name
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
  filter(., !case_ID == "Switzerland_2019")

rm(cses_imd)  # no longer required.
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
res.party <- mclapply(ranking.list, FUN = condorcet, runoff = F, quiet = T, mc.cores = 8)  
names(res.party) <- caseid.party

smr.result.fun <- function(case, result) {
  data.frame(
    case_ID = case,
    elected = ifelse(!is.null(result$elected), result$elected, NA),
    loser = ifelse(!is.null(result$loser), result$loser, NA),
    num_parties = ifelse(!is.null(result$totals), ncol(result$totals), NA)
  )
}

results.df <- do.call(rbind, Map(smr.result.fun, as.list(names(res.party)), res.party))  %>%
  filter(., num_parties > 2) %>% filter(!if_all(everything(), is.na))

results.df %>% filter(., is.na(elected) == T)
results.df %>% filter(., is.na(loser) == T)


#explaining the no-Condorcet-winner party cases
results.df %>% filter(., is.na(elected) == T) %>% select("case_ID")
# Belgium_1999
bel99 <- res.party$Belgium_1999
bel99.tot <- bel99[["totals"]] %>% as.data.frame(.) %>% mutate(wins = rowSums(.))
bel99.dat <- bel99[["data"]] %>% as.data.frame(.)
nrow(bel99.dat %>% filter(., A < B)) - nrow(bel99.dat %>% filter(., B < A))
nrow(bel99.dat %>% filter(., C < A)) - nrow(bel99.dat %>% filter(., A < C))
nrow(bel99.dat %>% filter(., C < B)) - nrow(bel99.dat %>% filter(., B < C))
# Hong Kong_2012
hk12 <- res.party$`Hong Kong_2012`
hk12.tot <- hk12[["totals"]] %>% as.data.frame(.) %>% mutate(wins = rowSums(.))
hk12.dat <- hk12[["data"]] %>% as.data.frame(.)
nrow(hk12.dat %>% filter(., C < E)) - nrow(hk12.dat %>% filter(., E < C))
## Slovenia_1996
slo96 <- res.party$Slovenia_1996
slo96.tot <- slo96[["totals"]] %>% as.data.frame(.) %>% mutate(wins = rowSums(.))
slo96.dat <- slo96[["data"]] %>% as.data.frame(.)
nrow(slo96.dat %>% filter(., A < B)) - nrow(slo96.dat %>% filter(., B < A))
#
rm(list = ls(pattern = "^(slo|hk|bel)"))
###################################################################################
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
corrplot::corrplot(as.matrix(agreement_df), method = 'pie', type = "upper", bg = "gray80")
ggplot2::ggsave("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/corrplot.pdf")
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
corrplot::corrplot(kendall_tau_matrix, method = "pie", type = "upper")
