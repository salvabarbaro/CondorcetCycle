library(dplyr)
library(ggplot2)
library(purrr)
##################
setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/")
###
# Load CSES data (no. 5)
load("Data/cses5.rdata")
load("Data/cses_imd.rdata")
d <- cses_imd %>% 
  mutate(cntry = IMD1006_NAM,
         year = as.character(IMD1008_YEAR),
         case_ID = paste(cntry, year, sep="_"), 
         .before = IMD1001) %>%
  mutate(across(starts_with("IMD3008_"), ~ ifelse(. < 11, ., NA), 
                .names = "party_rating_{tolower(substr(.col, 9, 9))}")) 

####################################################

# Define a function to set ratings to NA if condition is met
set_na_if_condition <- function(rating) {
  ifelse(all(is.na(rating) | rating == 11), NA, rating)
}
# Apply the function to each party_rating column by case_ID group
d <- d %>%
  group_by(case_ID) %>%
  mutate(across(starts_with("party_rating"), set_na_if_condition)) %>%
  ungroup()
############################################################
# Individual Candidate ratings # 0=strongly dislike # 10=strongly like
d <- d %>%
  mutate(across(starts_with("IMD3009_"), ~ ifelse(. < 11, ., NA), .names = "candidate_rating_{col}"))
# Apply the function to each candidate_rating column by case_ID group
d <- d %>%
  group_by(case_ID) %>%
  mutate(across(starts_with("candidate_rating"), set_na_if_condition)) %>%
  ungroup()

################################################################


d <- d %>%
  rename_with(~ sub("IMD3007_", "leri_party_", .), starts_with("IMD3007_"))

d <- d %>%
  mutate(across(starts_with("leri_party"), ~ ifelse(. > 10, NA, .)))

final <- d %>%
  filter(!is.na(case_ID)) %>%
  distinct(case_ID) %>%
  as.data.frame()

#############################################################################
final <- d %>%
  group_by(case_ID) %>%
  summarize(
    cntry = first(cntry),
    year = first(year),
    polityIV = first(IMD5051_1),
    system = first(IMD5013),
    n_eff_parties = first(IMD5058_2)
  )

party_columns <- paste0("party_rating_", letters[1:9])
imd_columns <- paste0("IMD5000_", toupper(letters[1:9]))

# Filter parties based on inclusion criteria using mutate and rowwise

