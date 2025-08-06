## In case you want to skip running the replication files, here are the respective outputs and the script to generate 
## the overview tables in the SM

library(dplyr)
#setwd("~")

## Basic Bootstrap
partysummary.df <-   read.csv("summarytableP.csv", header = T)
candsummary.df  <-   read.csv("summarytableC.csv", header = T)
##################################################################################
## Basic Random Noise
regularcases.df <- read.csv("RNsummarytableP.csv", header = T) %>% select(., -c("X"))
specialcases.df <- read.csv("SpecialSummaryTableP.csv", header = T)
RNsummaryP.df <- rbind(regularcases.df, specialcases.df)
RNsummaryC.df <- read.csv("RNsummarytableC.csv", header = T)
##################################################################################
## Weighed Cases
WsummaryP.df <- read.csv("WsummaryP.csv", header = T)
WsummaryC.df <- read.csv("WsummaryC.csv", header = T)  # to be added (special cases)
##################################################################################
## Imputation
IMPsummaryP.df <- read.csv("IMPsummaryP.csv", header = T)
IMPsummaryC.df <- read.csv("IMPsummaryC.csv", header = T)

###################################################################################
partyinference.df <- partysummary.df %>% 
  select(., c("case", "count_cycle")) %>%
  rename(Base = count_cycle) %>%
  left_join(x = ., 
            y = WsummaryP.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(PBW = count_cycle), 
              by = "case") %>%
  left_join(x = ., 
            y = RNsummaryP.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(RN = count_cycle),
            by = "case") %>%
  left_join(x = ., 
            y = IMPsummaryP.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(IMP = count_cycle),
            by = "case") %>%
  filter(., !case %in% c("B_FL_1999", "B_FL_2019", "B_WA_1999", "B_WA_2019")) %>%
  filter(., rowSums(.[,2:5], na.rm = T) > 0) %>%
  mutate(share.Base = scales::percent(Base/10000),
         share.PBW  = scales::percent(PBW/10000), 
         share.RN   = scales::percent(RN/10000),
         share.IMP  = scales::percent(IMP/10000)) %>%
  select(., c("case", starts_with("share."))) %>%
  setNames(c("case_ID", "Bootstrap", "PBW", "RandomNoise", "Imputation")) %>%
  mutate(RandomNoise = ifelse(is.na(RandomNoise), 0, RandomNoise),
         Imputation  = ifelse(is.na(Imputation),  0, Imputation ))


rm("candsummary.df",  "IMPsummaryC.df",  "IMPsummaryP.df",  "partysummary.df", 
   "regularcases.df", "RNsummaryC.df",   "RNsummaryP.df",   "specialcases.df", 
   "WsummaryC.df", "WsummaryP.df")

stargazer::stargazer(partyinference.df, 
                     summary = F,
                     type = "latex", rownames = F)


candidateinference.df <- candsummary.df %>% 
  select(., c("case", "count_cycle")) %>%
  rename(Base = count_cycle) %>%
  left_join(x = ., 
            y = WsummaryC.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(PBW = count_cycle), 
            by = "case") %>%
  left_join(x = ., 
            y = RNsummaryC.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(RN = count_cycle),
            by = "case") %>%
  left_join(x = ., 
            y = IMPsummaryC.df %>% 
              select(., c("case", "count_cycle")) %>%
              rename(IMP = count_cycle),
            by = "case") %>%
#  filter(., !case %in% c("B_FL_1999", "B_FL_2019", "B_WA_1999", "B_WA_2019")) %>%
  filter(., rowSums(.[,2:5], na.rm = T) > 0) %>%
  mutate(share.Base = scales::percent(Base/10000),
         share.PBW  = scales::percent(PBW/10000), 
         share.RN   = scales::percent(RN/10000),
         share.IMP  = scales::percent(IMP/10000)) %>%
  select(., c("case", starts_with("share."))) %>%
  setNames(c("case_ID", "Bootstrap", "PBW", "RandomNoise", "Imputation")) %>%
  mutate(RandomNoise = ifelse(is.na(RandomNoise), 0, RandomNoise),
         Imputation  = ifelse(is.na(Imputation),  0, Imputation ))


stargazer::stargazer(candidateinference.df, 
                     summary = F,
                     type = "latex", rownames = F)

rm(ls=())




