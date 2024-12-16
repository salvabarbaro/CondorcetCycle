# assume that d is already present (from CondocetCycle_Revision.R or NewScript3.R)

## Parties in government / cabinet after elections.
cabinet.df <- d %>% select(starts_with("IMD5031"))
# Cabinet posts
# N of cabinet posts by party
d$minis_party_a <- d$IMD5031_A
d$minis_party_b <- d$IMD5031_B
d$minis_party_c <- d$IMD5031_C
d$minis_party_d <- d$IMD5031_D
d$minis_party_e <- d$IMD5031_E
d$minis_party_f <- d$IMD5031_F
d$minis_party_g <- d$IMD5031_G
d$minis_party_h <- d$IMD5031_H
d$minis_party_i <- d$IMD5031_I

d$minis_party_a[d$minis_party_a>47] <- NA
d$minis_party_b[d$minis_party_b>47] <- NA
d$minis_party_c[d$minis_party_c>47] <- NA
d$minis_party_d[d$minis_party_d>47] <- NA
d$minis_party_e[d$minis_party_e>47] <- NA
d$minis_party_f[d$minis_party_f>47] <- NA
d$minis_party_g[d$minis_party_g>47] <- NA
d$minis_party_h[d$minis_party_h>47] <- NA
d$minis_party_i[d$minis_party_i>47] <- NA

d$minis_party_a <- d$minis_party_a / d$IMD5030
d$minis_party_b <- d$minis_party_b / d$IMD5030
d$minis_party_c <- d$minis_party_c / d$IMD5030
d$minis_party_d <- d$minis_party_d / d$IMD5030
d$minis_party_e <- d$minis_party_e / d$IMD5030
d$minis_party_f <- d$minis_party_f / d$IMD5030
d$minis_party_g <- d$minis_party_g / d$IMD5030
d$minis_party_h <- d$minis_party_h / d$IMD5030
d$minis_party_i <- d$minis_party_i / d$IMD5030

#deu21 <- d %>% filter(., case_ID == "Germany_2021")

party_columns <- grep("^minis_party_", names(d), value = TRUE)

# Group by case_ID and determine parties in the government
cabinet <- d %>%
  group_by(case_ID) %>%
  summarise(
    gov.parties = list(
      names(pick(all_of(party_columns)))[
        sapply(pick(all_of(party_columns)), function(col) any(col > 0))
      ] %>% 
        sub("^minis_party_", "", .) %>%  # Extract the party letter
        toupper()  # Convert to uppercase
    ),
    .groups = "drop"  # Ungroup after summarising
  ) 

# Adjust manually missings:
cabinet$gov.parties[cabinet$case_ID == "Australia_1996"] <- list(c("B", "C"))
cabinet$gov.parties[cabinet$case_ID == "Canada_1997"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Chile_2009"] <- list(c("A", "B"))
cabinet$gov.parties[cabinet$case_ID == "Czech Republic/Czechia_1996"] <- list(c("A", "E", "F"))
cabinet$gov.parties[cabinet$case_ID == "Denmark_1998"] <- list(c("A", "B"))
cabinet$gov.parties[cabinet$case_ID == "El Salvador_2019"] <- list(c("A", "G"))
cabinet$gov.parties[cabinet$case_ID == "France_2002"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Germany_1998"] <- list(c("A", "D"))
cabinet$gov.parties[cabinet$case_ID == "Great Britain_2017"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Hong Kong_1998"] <- list(c("B"))
cabinet$gov.parties[cabinet$case_ID == "Hong Kong_2000"] <- list(c("B"))
cabinet$gov.parties[cabinet$case_ID == "Hungary_1998"] <- list(c("B", "C", "G"))
cabinet$gov.parties[cabinet$case_ID == "Iceland_1999"] <- list(c("A", "C"))
cabinet$gov.parties[cabinet$case_ID == "Israel_1996"] <- list(c("B", "C"))
cabinet$gov.parties[cabinet$case_ID == "Japan_1996"] <- list(c("A", "E"))
cabinet$gov.parties[cabinet$case_ID == "Mexico_1997"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Mexico_2000"] <- list(c("A", "E"))
cabinet$gov.parties[cabinet$case_ID == "Netherlands_1998"] <- list(c("A", "B", "D"))
cabinet$gov.parties[cabinet$case_ID == "New Zealand_1996"] <- list(c("A", "C"))
cabinet$gov.parties[cabinet$case_ID == "Norway_1997"] <- list(c("D", "E"))
cabinet$gov.parties[cabinet$case_ID == "Peru_2001"] <- list(c("A", "D"))
cabinet$gov.parties[cabinet$case_ID == "Poland_1997"] <- list(c("A", "C"))
cabinet$gov.parties[cabinet$case_ID == "Republic of Korea_2000"] <- list(c("B"))
cabinet$gov.parties[cabinet$case_ID == "Romania_1996"] <- list(c("A", "D"))
cabinet$gov.parties[cabinet$case_ID == "Spain_1996"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Spain_2000"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Sweden_1998"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Switzerland_1999"] <- list(c("A", "B", "C", "D"))
cabinet$gov.parties[cabinet$case_ID == "Taiwan_1996"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "Thailand_2001"] <- list(c("A", "C", "D"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_1996"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_2004"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_2008"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_2012"] <- list(c("A"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_2016"] <- list(c("B"))
cabinet$gov.parties[cabinet$case_ID == "United States of America_2020"] <- list(c("A"))

### how often are Condorcet winner part of the cabinet?
##  Use results.df from Main Script
cabinet.efficiency <- results.df %>%
  left_join(x = ., y = cabinet, by = "case_ID") %>%
  filter(gov.parties != "character(0)") %>%
  mutate(gov.parties = sapply(gov.parties, function(parties) {
    non_na_parties <- unlist(strsplit(parties, ", ")) # Split into list
    non_na_parties <- non_na_parties[!is.na(non_na_parties) & non_na_parties != "NA"] # Remove NA
    paste(non_na_parties, collapse = ", ") 
  })) %>%
  mutate(efficiency = ifelse(
    mapply(function(elected, parties) elected %in% unlist(strsplit(parties, ", ")), 
           elected, gov.parties),
    1, 0
  )) %>%
  mutate(BordaParadox = ifelse(
    mapply(function(loser, parties) loser %in% unlist(strsplit(parties, ", ")), 
           loser, gov.parties),
    1, 0
  )) %>%
  filter(., gov.parties != "")
  
# Check for 0
no.df <- cabinet.efficiency %>% 
  filter(., efficiency == 0)

# In 
round(sum(cabinet.efficiency$efficiency)/nrow(cabinet.efficiency)*100,2)
# % of all cases the CW belongs to the cabinet after the resp. election.
