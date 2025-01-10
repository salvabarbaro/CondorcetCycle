#
#   CSES Integrated File
#
#   Frequency of Condorcet Cycles - Main R-Script
#
################################################
rm(list = ls())
# library(foreign)
library(haven)
library(vote)
library(dplyr)
library(data.table)
library(tidyr)

#setwd("")
load("cses_imd.rdata")    # Version 18.11.2024, includes waves 1- 5

d <- cses_imd

na.countries <- c("Switzerland_2019")

d$cntry <- d$IMD1006_NAM

d$cntry[d$IMD1003==5611999] <- "B_FL"
d$cntry[d$IMD1003==5621999] <- "B_WA"

d$cntry[d$IMD1003==5612019] <- "B_FL"
d$cntry[d$IMD1003==5622019] <- "B_WA"

# Generate case identifier=country x year
d$year <- as.character(d$IMD1008_YEAR)
d$case_ID <- paste(d$cntry, d$year, sep="_")
# Individual Party ratings
# 0=strongly dislike
# 10=strongly like
d <- d %>% filter(., !case_ID %in% na.countries)

# Add party names

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
csespartycsv <- read.csv("csespartycsv.csv")
testcases <- as.list(unique(d$case_ID))
testnames.list <- lapply(testcases, partyname.fun)
partyname.df <- bind_rows(testnames.list) %>% filter(., is.na(Country) == F)


d$presidential <- ifelse(d$IMD1009==20, 1, 0)
d$both <- ifelse(d$IMD1009==12, 1, 0)

d$party_rating_a <- ifelse(d$IMD3008_A<11, d$IMD3008_A, NA)
d$party_rating_b <- ifelse(d$IMD3008_B<11, d$IMD3008_B, NA)
d$party_rating_c <- ifelse(d$IMD3008_C<11, d$IMD3008_C, NA)
d$party_rating_d <- ifelse(d$IMD3008_D<11, d$IMD3008_D, NA)
d$party_rating_e <- ifelse(d$IMD3008_E<11, d$IMD3008_E, NA)
d$party_rating_f <- ifelse(d$IMD3008_F<11, d$IMD3008_F, NA)
d$party_rating_g <- ifelse(d$IMD3008_G<11, d$IMD3008_G, NA)
d$party_rating_h <- ifelse(d$IMD3008_H<11, d$IMD3008_H, NA)
d$party_rating_i <- ifelse(d$IMD3008_I<11, d$IMD3008_I, NA)

# set all to missings again when party was not included in feeling thermometer
for(i in unique(d$case_ID)){
  if(length(d$party_rating_a[d$case_ID==i & !is.na(d$party_rating_a)&d$party_rating_a!=11])==0){d$party_rating_a[d$case_ID==i] <- NA}
  if(length(d$party_rating_b[d$case_ID==i & !is.na(d$party_rating_b)&d$party_rating_b!=11])==0){d$party_rating_b[d$case_ID==i] <- NA}
  if(length(d$party_rating_c[d$case_ID==i & !is.na(d$party_rating_c)&d$party_rating_c!=11])==0){d$party_rating_c[d$case_ID==i] <- NA}
  if(length(d$party_rating_d[d$case_ID==i & !is.na(d$party_rating_d)&d$party_rating_d!=11])==0){d$party_rating_d[d$case_ID==i] <- NA}
  if(length(d$party_rating_e[d$case_ID==i & !is.na(d$party_rating_e)&d$party_rating_e!=11])==0){d$party_rating_e[d$case_ID==i] <- NA}
  if(length(d$party_rating_f[d$case_ID==i & !is.na(d$party_rating_f)&d$party_rating_f!=11])==0){d$party_rating_f[d$case_ID==i] <- NA}
  if(length(d$party_rating_g[d$case_ID==i & !is.na(d$party_rating_g)&d$party_rating_g!=11])==0){d$party_rating_g[d$case_ID==i] <- NA}
  if(length(d$party_rating_h[d$case_ID==i & !is.na(d$party_rating_h)&d$party_rating_h!=11])==0){d$party_rating_h[d$case_ID==i] <- NA}
  if(length(d$party_rating_i[d$case_ID==i & !is.na(d$party_rating_i)&d$party_rating_i!=11])==0){d$party_rating_i[d$case_ID==i] <- NA}
}


# merge CDU and CSU ratings in Germany

# Ger 1998
d$party_rating_b[d$case_ID=="Germany_1998" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_c[d$case_ID=="Germany_1998" & d$IMD2008==2276& !is.na(d$IMD2008)]
d$party_rating_c[d$case_ID=="Germany_1998"] <- NA
# Ger 2002
d$party_rating_b[d$case_ID=="Germany_2002" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_c[d$case_ID=="Germany_2002" & d$IMD2008==2276& !is.na(d$IMD2008)]
d$party_rating_c[d$case_ID=="Germany_2002"] <- NA
# Ger 2005
d$party_rating_b[d$case_ID=="Germany_2005" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_f[d$case_ID=="Germany_2005" & d$IMD2008==2276& !is.na(d$IMD2008)]
d$party_rating_f[d$case_ID=="Germany_2005"] <- NA
# Ger 2009
d$party_rating_a[d$case_ID=="Germany_2009" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_f[d$case_ID=="Germany_2009" & d$IMD2008==2276& !is.na(d$IMD2008)]
d$party_rating_f[d$case_ID=="Germany_2009"] <- NA
# Ger 2013
d$party_rating_a[d$case_ID=="Germany_2013" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_e[d$case_ID=="Germany_2013" & d$IMD2008==2276& !is.na(d$IMD2008)]
d$party_rating_e[d$case_ID=="Germany_2013"] <- NA
# Ger 2017
d$party_rating_a[d$case_ID=="Germany_2017" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_g[d$case_ID=="Germany_2017" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_g[d$case_ID=="Germany_2017"] <- NA
# Ger 2021
d$party_rating_a[d$case_ID=="Germany_2021" & d$IMD2008==2276 & !is.na(d$IMD2008)] <-  d$party_rating_g[d$case_ID=="Germany_2021" & d$IMD2008==2276 & !is.na(d$IMD2008)]
d$party_rating_g[d$case_ID=="Germany_2021"] <- NA


# Individual Candidate ratings

# 0=strongly dislike
# 10=strongly like
d$candidate_rating_a <- ifelse(d$IMD3009_A<11, d$IMD3009_A, NA)
d$candidate_rating_b <- ifelse(d$IMD3009_B<11, d$IMD3009_B, NA)
d$candidate_rating_c <- ifelse(d$IMD3009_C<11, d$IMD3009_C, NA)
d$candidate_rating_d <- ifelse(d$IMD3009_D<11, d$IMD3009_D, NA)
d$candidate_rating_e <- ifelse(d$IMD3009_E<11, d$IMD3009_E, NA)
d$candidate_rating_f <- ifelse(d$IMD3009_F<11, d$IMD3009_F, NA)
d$candidate_rating_g <- ifelse(d$IMD3009_G<11, d$IMD3009_G, NA)
d$candidate_rating_h <- ifelse(d$IMD3009_H<11, d$IMD3009_H, NA)
d$candidate_rating_i <- ifelse(d$IMD3009_I<11, d$IMD3009_I, NA)

# set all to missings again when candidate was not included in feeling thermometer
for(i in unique(d$case_ID)){
  if(length(d$candidate_rating_a[d$case_ID==i & !is.na(d$candidate_rating_a)&d$candidate_rating_a!=11])==0){d$candidate_rating_a[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_b[d$case_ID==i & !is.na(d$candidate_rating_b)&d$candidate_rating_b!=11])==0){d$candidate_rating_b[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_c[d$case_ID==i & !is.na(d$candidate_rating_c)&d$candidate_rating_c!=11])==0){d$candidate_rating_c[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_d[d$case_ID==i & !is.na(d$candidate_rating_d)&d$candidate_rating_d!=11])==0){d$candidate_rating_d[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_e[d$case_ID==i & !is.na(d$candidate_rating_e)&d$candidate_rating_e!=11])==0){d$candidate_rating_e[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_f[d$case_ID==i & !is.na(d$candidate_rating_f)&d$candidate_rating_f!=11])==0){d$candidate_rating_f[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_g[d$case_ID==i & !is.na(d$candidate_rating_g)&d$candidate_rating_g!=11])==0){d$candidate_rating_g[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_h[d$case_ID==i & !is.na(d$candidate_rating_h)&d$candidate_rating_h!=11])==0){d$candidate_rating_h[d$case_ID==i] <- NA}
  if(length(d$candidate_rating_i[d$case_ID==i & !is.na(d$candidate_rating_i)&d$candidate_rating_i!=11])==0){d$candidate_rating_i[d$case_ID==i] <- NA}
}

# Replace party ratings with candidate ratings in presidential elections

cond <- d$presidential==1 & d$case_ID!="Chile_1999" & d$case_ID!="Chile_2005" & d$case_ID!="France_2002" &
  d$case_ID!="Kyrgyzstan_2005" & d$case_ID!="Peru_2006" & d$case_ID!="Philippines_2004" & d$case_ID!="Russian Federation_2000" &
  d$case_ID!="Russian Federation_2004" &
  d$case_ID!="Taiwan_2004" & d$case_ID!="United States of America_2004" & d$case_ID!="South Africa_2014"
d$party_rating_a <- ifelse(cond, d$candidate_rating_a, d$party_rating_a)
d$party_rating_b <- ifelse(cond, d$candidate_rating_b, d$party_rating_b)
d$party_rating_c <- ifelse(cond, d$candidate_rating_c, d$party_rating_c)
d$party_rating_d <- ifelse(cond, d$candidate_rating_d, d$party_rating_d)
d$party_rating_e <- ifelse(cond, d$candidate_rating_e, d$party_rating_e)
d$party_rating_f <- ifelse(cond, d$candidate_rating_f, d$party_rating_f)
d$party_rating_g <- ifelse(cond, d$candidate_rating_g, d$party_rating_g)
d$party_rating_h <- ifelse(cond, d$candidate_rating_h, d$party_rating_h)
d$party_rating_i <- ifelse(cond, d$candidate_rating_i, d$party_rating_i)

# Left-right self-placement
d$leri_ego <- ifelse(d$IMD3006<11,d$IMD3006, NA) 

#  left-right placement of parties
d$leri_party_a <- d$IMD3007_A
d$leri_party_b <- d$IMD3007_B
d$leri_party_c <- d$IMD3007_C
d$leri_party_d <- d$IMD3007_D
d$leri_party_e <- d$IMD3007_E
d$leri_party_f <- d$IMD3007_F
d$leri_party_g <- d$IMD3007_G
d$leri_party_h <- d$IMD3007_H
d$leri_party_i <- d$IMD3007_I
############################################
d$leri_party_a[d$leri_party_a>10] <- NA
d$leri_party_b[d$leri_party_b>10] <- NA
d$leri_party_c[d$leri_party_c>10] <- NA
d$leri_party_d[d$leri_party_d>10] <- NA
d$leri_party_e[d$leri_party_e>10] <- NA
d$leri_party_f[d$leri_party_f>10] <- NA
d$leri_party_g[d$leri_party_g>10] <- NA
d$leri_party_h[d$leri_party_h>10] <- NA
d$leri_party_i[d$leri_party_i>10] <- NA

# Party family
d$family_party_a <- d$IMD5011_A
d$family_party_b <- d$IMD5011_B
d$family_party_c <- d$IMD5011_C
d$family_party_d <- d$IMD5011_D
d$family_party_e <- d$IMD5011_E
d$family_party_f <- d$IMD5011_F
d$family_party_g <- d$IMD5011_G
d$family_party_h <- d$IMD5011_H
d$family_party_i <- d$IMD5011_I
#
d$family_party_a[d$family_party_a>22] <- NA
d$family_party_b[d$family_party_b>22] <- NA
d$family_party_c[d$family_party_c>22] <- NA
d$family_party_d[d$family_party_d>22] <- NA
d$family_party_e[d$family_party_e>22] <- NA
d$family_party_f[d$family_party_f>22] <- NA
d$family_party_g[d$family_party_g>22] <- NA
d$family_party_h[d$family_party_h>22] <- NA
d$family_party_i[d$family_party_i>22] <- NA

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

# Party of the prime minister after
d$pm_a <- ifelse(d$IMD5008_2==d$IMD5000_A & d$IMD5008_2<9999989, 1, 0)
d$pm_b <- ifelse(d$IMD5008_2==d$IMD5000_B & d$IMD5008_2<9999989, 1, 0)
d$pm_c <- ifelse(d$IMD5008_2==d$IMD5000_C & d$IMD5008_2<9999989, 1, 0)
d$pm_d <- ifelse(d$IMD5008_2==d$IMD5000_D & d$IMD5008_2<9999989, 1, 0)
d$pm_e <- ifelse(d$IMD5008_2==d$IMD5000_E & d$IMD5008_2<9999989, 1, 0)
d$pm_f <- ifelse(d$IMD5008_2==d$IMD5000_F & d$IMD5008_2<9999989, 1, 0)
d$pm_g <- ifelse(d$IMD5008_2==d$IMD5000_G & d$IMD5008_2<9999989, 1, 0)
d$pm_h <- ifelse(d$IMD5008_2==d$IMD5000_H & d$IMD5008_2<9999989, 1, 0)
d$pm_i <- ifelse(d$IMD5008_2==d$IMD5000_I & d$IMD5008_2<9999989, 1, 0)


d$votes_party_a <- d$IMD5001_A
d$votes_party_b <- d$IMD5001_B
d$votes_party_c <- d$IMD5001_C
d$votes_party_d <- d$IMD5001_D
d$votes_party_e <- d$IMD5001_E
d$votes_party_f <- d$IMD5001_F
d$votes_party_g <- d$IMD5001_G
d$votes_party_h <- d$IMD5001_H
d$votes_party_i <- d$IMD5001_I

# percent presidential candidates in presidential elections
d$votes_party_a[d$presidential==1] <- d$IMD5005_A[d$presidential==1]
d$votes_party_b[d$presidential==1] <- d$IMD5005_B[d$presidential==1]
d$votes_party_c[d$presidential==1] <- d$IMD5005_C[d$presidential==1]
d$votes_party_d[d$presidential==1] <- d$IMD5005_D[d$presidential==1]
d$votes_party_e[d$presidential==1] <- d$IMD5005_E[d$presidential==1]
d$votes_party_f[d$presidential==1] <- d$IMD5005_F[d$presidential==1]
d$votes_party_g[d$presidential==1] <- d$IMD5005_G[d$presidential==1]
d$votes_party_h[d$presidential==1] <- d$IMD5005_H[d$presidential==1]
d$votes_party_i[d$presidential==1] <- d$IMD5005_I[d$presidential==1]

d$votes_party_a[d$votes_party_a>99] <- NA
d$votes_party_b[d$votes_party_b>99] <- NA
d$votes_party_c[d$votes_party_c>99] <- NA
d$votes_party_d[d$votes_party_d>99] <- NA
d$votes_party_e[d$votes_party_e>99] <- NA
d$votes_party_f[d$votes_party_f>99] <- NA
d$votes_party_g[d$votes_party_g>99] <- NA
d$votes_party_h[d$votes_party_h>99] <- NA
d$votes_party_i[d$votes_party_i>99] <- NA
##################################################################
### LOOP START ###################################################
d <- d[!is.na(d$case_ID),]  
final <- as.data.frame(matrix(unique(d$case_ID), ncol=1))
names(final) <- "case_ID"

for(k in unique(d$case_ID)){
  #for(k in unique(final$case_ID)){
  dt <-d[d$case_ID==k,]

  final$cntry[final$case_ID==k]     <- dt$cntry[1]
  final$year[final$case_ID==k]      <- dt$year[1]
  final$polityIV[final$case_ID==k]  <- dt$IMD5051_1[1]
  final$system[final$case_ID==k]    <- dt$IMD5013[1]
  final$n_eff_parties[final$case_ID==k] <- dt$IMD5058_2[1]
  final$presidential[final$case_ID==k] <- ifelse(dt$presidential[1]==1, 1, 0)

  # Party rankings
  # Which parties are included in this case?
  include <- rep(FALSE,9)
  include[1] <-  dt$IMD5000_A[1]!=9999999 & any(!is.na(dt$party_rating_a))
  include[2] <-  dt$IMD5000_B[1]!=9999999 & any(!is.na(dt$party_rating_b))
  include[3]  <- dt$IMD5000_C[1]!=9999999 & any(!is.na(dt$party_rating_c))
  include[4]  <- dt$IMD5000_D[1]!=9999999 & any(!is.na(dt$party_rating_d))
  include[5]  <- dt$IMD5000_E[1]!=9999999 & any(!is.na(dt$party_rating_e))
  include[6]  <- dt$IMD5000_F[1]!=9999999 & any(!is.na(dt$party_rating_f))
  include[7]  <- dt$IMD5000_G[1]!=9999999 & any(!is.na(dt$party_rating_g))
  include[8]  <- dt$IMD5000_H[1]!=9999999 & any(!is.na(dt$party_rating_h))
  include[9]  <- dt$IMD5000_I[1]!=9999999 & any(!is.na(dt$party_rating_i))
  
  parties <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")[include]
  rating_mat <- cbind(dt$party_rating_a,
                      dt$party_rating_b,
                      dt$party_rating_c,
                      dt$party_rating_d,
                      dt$party_rating_e,
                      dt$party_rating_f,
                      dt$party_rating_g,
                      dt$party_rating_h,
                      dt$party_rating_i)
  
  if(k=="Switzerland_2007") {include[7:8] <- FALSE}
  if(k== "Spain_2004") {include[4:9] <- FALSE}
  if(k== "Great Britain_1997") {include[4:5] <- FALSE}
  if(k== "Great Britain_2005") {include[4:5] <- FALSE}
  if(k== "Great Britain_2015") {include[c(5,7)] <- FALSE}

  final$n_parties[final$case_ID==k] <- length(include[include==TRUE])
  
  rating_mat.s <- rating_mat[,include == TRUE] %>% 
    as.data.frame(.) %>%
    filter(!if_all(everything(), is.na))  # removes all rows that consist of NA's only.
  rating_mat <- rating_mat.s
  #
  trank <- as.data.frame(t(apply(rating_mat, 1, 
                                 function(x) frank(-x, 
                                                   ties.method = "max", 
                                                   na.last = TRUE)))) %>%
    setNames(., LETTERS[1:9][include==TRUE]) 
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE)
  if(length(Celection$elected)>0){
    final$Cwinner_party[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_party[final$case_ID==k] <- Celection$loser}
  
  # ideological position of winner and loser party
  mat <- cbind(dt$leri_party_a,
               dt$leri_party_b,
               dt$leri_party_c,
               dt$leri_party_d,
               dt$leri_party_e,
               dt$leri_party_f,
               dt$leri_party_g,
               dt$leri_party_h,
               dt$leri_party_i        )
  
  
  pp <- apply(mat,2,mean,na.rm=TRUE)
  
  
  if(length(Celection$elected)>0){
    final$leri_winner[final$case_ID==k] <- pp[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$leri_loser[final$case_ID==k] <- pp[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  # party family of winner and loser
  fam <- dt %>%select(contains("family"))
  fam <- as.data.frame(fam[1,])
  fam <- as.numeric(fam)
  
  if(length(Celection$elected)>0){
    final$family_winner[final$case_ID==k] <- fam[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$family_loser[final$case_ID==k] <- fam[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  # Percent votes of winner
  
  
  #percent votes of Condorcet Winner
  
  vot <- cbind(dt$votes_party_a,
               dt$votes_party_b,
               dt$votes_party_c,
               dt$votes_party_d,
               dt$votes_party_e,
               dt$votes_party_f,
               dt$votes_party_g,
               dt$votes_party_h,
               dt$votes_party_i        )
  
  if(k=="Japan_2004"){vot[1,] <- c(37.8,30,15.4,7.8,5.4, NA, NA, NA ,NA)}
  if(k=="Japan_2007"){vot[1,] <- c(39.5, 28.1, 13.2, 7.5, 4.5, NA, NA, NA, NA )}
  if(k=="Japan_2013"){vot[1,] <- c(34.7, 13.4, 9.7, 8.9, 11,9, 14.2, NA, NA)}
  
  if(length(Celection$elected)>0 ){
    final$votes_winner[final$case_ID==k] <- vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 ){
    final$votes_loser[final$case_ID==k] <- vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  # vote margin of Condorcet winner
  if(length(Celection$elected)>0 ){
    final$votemargin_winner[final$case_ID==k] <- ifelse(
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]==max(vot[1,], na.rm=TRUE),
      max(vot[1,], na.rm=TRUE)-max(vot[1,-which(vot[1,]==max(vot[1,], na.rm=TRUE))], na.rm=TRUE),
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)] - max(vot[1,], na.rm=TRUE))
  }
  
  
  
  #share of cabinet posts of Condorcet Winner
  
  minis <- cbind(dt$minis_party_a,
                 dt$minis_party_b,
                 dt$minis_party_c,
                 dt$minis_party_d,
                 dt$minis_party_e,
                 dt$minis_party_f,
                 dt$minis_party_g,
                 dt$minis_party_h,
                 dt$minis_party_i        )
  
  
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_winner[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_loser[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_winner[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_loser[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  
  #Party of president after election
  
  pres <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[
    which( c(dt$IMD5000_A[1],
            dt$IMD5000_B[1],
            dt$IMD5000_C[1],
            dt$IMD5000_D[1],
            dt$IMD5000_E[1],
            dt$IMD5000_F[1],
            dt$IMD5000_G[1],
            dt$IMD5000_H[1],
            dt$IMD5000_I[1])==dt$IMD5009_2[1])]
  
  if(length(Celection$elected)>0 & dt$IMD5009_2[1]<9999989 & length(pres)>0){
    final$president_after[final$case_ID==k] <- ifelse(pres==Celection$elected, 1, 0)}
  if(length(Celection$loser)>0 & dt$IMD5009_2[1]<9999989 & length(pres)>0){
    final$president_after_loser[final$case_ID==k] <- ifelse(pres==Celection$loser, 1, 0)}

  
  #Party of prime minister after election
  
  pm <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[
    which( c(dt$IMD5000_A[1],
             dt$IMD5000_B[1],
             dt$IMD5000_C[1],
             dt$IMD5000_D[1],
             dt$IMD5000_E[1],
             dt$IMD5000_F[1],
             dt$IMD5000_G[1],
             dt$IMD5000_H[1],
             dt$IMD5000_I[1])==dt$IMD5008_2[1])]
  
  if(length(Celection$elected)>0 & dt$IMD5008_2[1]<9999989 & length(pm)>0){
    final$pm_after[final$case_ID==k] <- ifelse(pm==Celection$elected, 1, 0)}
  if(length(Celection$loser)>0 & dt$IMD5008_2[1]<9999989 & length(pm)>0){
    final$pm_after_loser[final$case_ID==k] <- ifelse(pm==Celection$loser, 1, 0)}
  
  
  #party family of Condorcet Winner
  
  fam <- cbind(dt$family_party_a,
               dt$family_party_b,
               dt$family_party_c,
               dt$family_party_d,
               dt$family_party_e,
               dt$family_party_f,
               dt$family_party_g,
               dt$family_party_h,
               dt$family_party_i        )
  
  
  if(length(Celection$elected)>0){
    final$family_winner[final$case_ID==k] <- fam[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$family_loser[final$case_ID==k] <- fam[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  # vote margin of Condorcet loser
  if(length(Celection$loser)>0 ){
    final$votemargin_loser[final$case_ID==k] <- ifelse(
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]==max(vot[1,], na.rm=TRUE),
      max(vot[1,], na.rm=TRUE)-max(vot[1,-which(vot[1,]==max(vot[1,], na.rm=TRUE))], na.rm=TRUE),
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)] - max(vot[1,], na.rm=TRUE))
  }
  
  #Is Condorcet Winner part of government?
  
  minis <- cbind(dt$minis_party_a,
                 dt$minis_party_b,
                 dt$minis_party_c,
                 dt$minis_party_d,
                 dt$minis_party_e,
                 dt$minis_party_f,
                 dt$minis_party_g,
                 dt$minis_party_h,
                 dt$minis_party_i        )
  
  
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_winner[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_loser[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  # major or junior government party
  if(length(Celection$elected)>0 ){
    final$largest_gov[final$case_ID==k] <- ifelse(
      minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]==max(minis[1,], na.rm=TRUE),
      1,0)
    final$junior_gov[final$case_ID==k] <- ifelse(
      minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]!=max(minis[1,], na.rm=TRUE) &
        minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]>0  ,
      1,0)
  }
  # Characteristics of largest government party
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    j <- which(minis[1,]==max(minis[1,], na.rm=TRUE))
    final$cab_posts_gov[final$case_ID==k] <- minis[1,j]
    final$leri_gov[final$case_ID==k] <- pp[j]
    final$family_gov[final$case_ID==k] <- fam[1,j]
    final$party_gov[final$case_ID==k] <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[j]
  }
  
  final$N[final$case_ID==k] <- dim(rating_mat)[1]
  
}



final_imd <- final


################ Add presidential elections for mixed surveys #################################

final <- as.data.frame(matrix(unique(d$case_ID[d$both==1 & d$case_ID!="Chile_1999" & d$case_ID!="Chile_2005" & d$case_ID!="France_2002" &
                                                 d$case_ID!="Kyrgyzstan_2005" & d$case_ID!="Peru_2006" & d$case_ID!="Philippines_2004" & d$case_ID!="Romania_2004"& 
                                                 d$case_ID!="Taiwan_2004" & d$case_ID!="United States of America_2004" & d$case_ID!="South Africa_2014"]), ncol=1))
names(final) <- "case_ID"

for(k in unique(d$case_ID[d$both==1 & d$case_ID!="Chile_1999" & d$case_ID!="Chile_2005" & d$case_ID!="France_2002" &
                          d$case_ID!="Kyrgyzstan_2005" & d$case_ID!="Peru_2006" & d$case_ID!="Philippines_2004" & d$case_ID!="Romania_2004"& 
                          d$case_ID!="Taiwan_2004" & d$case_ID!="United States of America_2004" & d$case_ID!="South Africa_2014"])){
  dt <-d[d$case_ID==k,]
  
  
  final$cntry[final$case_ID==k]     <- paste(dt$cntry[1], "presi", sep="_")
  final$year[final$case_ID==k]      <- dt$year[1]
  final$polityIV[final$case_ID==k]  <- dt$IMD5051_1[1]
  final$system[final$case_ID==k]    <- dt$IMD5013[1]
  final$n_eff_parties[final$case_ID==k] <- dt$IMD5058_2[1]
  final$presidential[final$case_ID==k] <- 1
  
  # candidate rankings
  # Which parties are included in this case?
  include <- rep(FALSE,9)
  include[1] <-  dt$IMD5000_A[1]!=9999999 & any(!is.na(dt$candidate_rating_a))
  include[2] <-  dt$IMD5000_B[1]!=9999999 & any(!is.na(dt$candidate_rating_b))
  include[3]  <- dt$IMD5000_C[1]!=9999999 & any(!is.na(dt$candidate_rating_c))
  include[4]  <- dt$IMD5000_D[1]!=9999999 & any(!is.na(dt$candidate_rating_d))
  include[5]  <- dt$IMD5000_E[1]!=9999999 & any(!is.na(dt$candidate_rating_e))
  include[6]  <- dt$IMD5000_F[1]!=9999999 & any(!is.na(dt$candidate_rating_f))
  include[7]  <- dt$IMD5000_G[1]!=9999999 & any(!is.na(dt$candidate_rating_g))
  include[8]  <- dt$IMD5000_H[1]!=9999999 & any(!is.na(dt$candidate_rating_h))
  include[9]  <- dt$IMD5000_I[1]!=9999999 & any(!is.na(dt$candidate_rating_i))
  
  parties <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")[include]
  rating_mat <- cbind(dt$candidate_rating_a,
                      dt$candidate_rating_b,
                      dt$candidate_rating_c,
                      dt$candidate_rating_d,
                      dt$candidate_rating_e,
                      dt$candidate_rating_f,
                      dt$candidate_rating_g,
                      dt$candidate_rating_h,
                      dt$candidate_rating_i)
  
  final$n_parties[final$case_ID==k] <- length(include[include==TRUE])
  
  rating_mat.s <- rating_mat[,include == TRUE] %>% 
    as.data.frame(.) %>%
    filter(!if_all(everything(), is.na))  # removes all rows that consist of NA's only.
  rating_mat <- rating_mat.s
  #
  trank <- as.data.frame(t(apply(rating_mat, 1, 
                                 function(x) frank(-x, 
                                                   ties.method = "max", 
                                                   na.last = TRUE)))) %>%
    setNames(., LETTERS[1:9][include==TRUE]) 
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE)
  if(length(Celection$elected)>0){
    final$Cwinner_party[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_party[final$case_ID==k] <- Celection$loser}
  
  
  # ideological position of winner and loser party
  
  mat <- cbind(dt$leri_party_a,
               dt$leri_party_b,
               dt$leri_party_c,
               dt$leri_party_d,
               dt$leri_party_e,
               dt$leri_party_f,
               dt$leri_party_g,
               dt$leri_party_h,
               dt$leri_party_i        )
  
  
  pp <- apply(mat,2,mean,na.rm=TRUE)
  
  
  if(length(Celection$elected)>0){
    final$leri_winner[final$case_ID==k] <- pp[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$leri_loser[final$case_ID==k] <- pp[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  # party family of winner and loser
  fam <- dt %>%select(contains("family"))
  fam <- as.data.frame(fam[1,])
  fam <- as.numeric(fam)
  
  if(length(Celection$elected)>0){
    final$family_winner[final$case_ID==k] <- fam[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$family_loser[final$case_ID==k] <- fam[which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  # Percent votes of winner
  
  
  #percent votes of Condorcet Winner
  
  vot <- cbind(dt$votes_party_a,
               dt$votes_party_b,
               dt$votes_party_c,
               dt$votes_party_d,
               dt$votes_party_e,
               dt$votes_party_f,
               dt$votes_party_g,
               dt$votes_party_h,
               dt$votes_party_i        )
  
  
  if(length(Celection$elected)>0 ){
    final$votes_winner[final$case_ID==k] <- vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 ){
    final$votes_loser[final$case_ID==k] <- vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  # vote margin of Condorcet winner
  if(length(Celection$elected)>0 ){
    final$votemargin_winner[final$case_ID==k] <- ifelse(
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]==max(vot[1,], na.rm=TRUE),
      max(vot[1,], na.rm=TRUE)-max(vot[1,-which(vot[1,]==max(vot[1,], na.rm=TRUE))], na.rm=TRUE),
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)] - max(vot[1,], na.rm=TRUE))
  }
  
  
  
  #share of cabinet posts of Condorcet Winner
  
  minis <- cbind(dt$minis_party_a,
                 dt$minis_party_b,
                 dt$minis_party_c,
                 dt$minis_party_d,
                 dt$minis_party_e,
                 dt$minis_party_f,
                 dt$minis_party_g,
                 dt$minis_party_h,
                 dt$minis_party_i        )
  
  
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_winner[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_loser[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  
  # vote margin of Condorcet loser
  if(length(Celection$loser)>0 ){
    final$votemargin_loser[final$case_ID==k] <- ifelse(
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]==max(vot[1,], na.rm=TRUE),
      max(vot[1,], na.rm=TRUE)-max(vot[1,-which(vot[1,]==max(vot[1,], na.rm=TRUE))], na.rm=TRUE),
      vot[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)] - max(vot[1,], na.rm=TRUE))
  }
  
  
  
  
  #Party of president after election
  
  pres <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[
    which( c(dt$IMD5000_A[1],
             dt$IMD5000_B[1],
             dt$IMD5000_C[1],
             dt$IMD5000_D[1],
             dt$IMD5000_E[1],
             dt$IMD5000_F[1],
             dt$IMD5000_G[1],
             dt$IMD5000_H[1],
             dt$IMD5000_I[1])==dt$IMD5009_2[1])]
  
  if(length(Celection$elected)>0 & dt$IMD5009_2[1]<9999989 & length(pres)>0){
    final$president_after[final$case_ID==k] <- ifelse(pres==Celection$elected, 1, 0)}
  if(length(Celection$loser)>0 & dt$IMD5009_2[1]<9999989 & length(pres)>0){
      final$president_after_loser[final$case_ID==k] <- ifelse(pres==Celection$loser, 1, 0)}
    
  
  #Party of prime minister after election
  
  pm <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[
    which( c(dt$IMD5000_A[1],
             dt$IMD5000_B[1],
             dt$IMD5000_C[1],
             dt$IMD5000_D[1],
             dt$IMD5000_E[1],
             dt$IMD5000_F[1],
             dt$IMD5000_G[1],
             dt$IMD5000_H[1],
             dt$IMD5000_I[1])==dt$IMD5008_2[1])]
  
  if(length(Celection$elected)>0 & dt$IMD5008_2[1]<9999989 & length(pm)>0){
    final$pm_after[final$case_ID==k] <- ifelse(pm==Celection$elected, 1, 0)}
  if(length(Celection$loser)>0 & dt$IMD5008_2[1]<9999989 & length(pm)>0){
    final$pm_after_loser[final$case_ID==k] <- ifelse(pm==Celection$loser, 1, 0)}
  
  
  
  #party family of Condorcet Winner
  
  fam <- cbind(dt$family_party_a,
               dt$family_party_b,
               dt$family_party_c,
               dt$family_party_d,
               dt$family_party_e,
               dt$family_party_f,
               dt$family_party_g,
               dt$family_party_h,
               dt$family_party_i        )
  
  
  if(length(Celection$elected)>0){
    final$family_winner[final$case_ID==k] <- fam[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0){
    final$family_loser[final$case_ID==k] <- fam[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  
  #Is Condorcet Winner part of government?
  
  minis <- cbind(dt$minis_party_a,
                 dt$minis_party_b,
                 dt$minis_party_c,
                 dt$minis_party_d,
                 dt$minis_party_e,
                 dt$minis_party_f,
                 dt$minis_party_g,
                 dt$minis_party_h,
                 dt$minis_party_i        )
  
  
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_winner[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]}
  if(length(Celection$loser)>0 & any(is.na(minis)==FALSE)){
    final$cab_posts_loser[final$case_ID==k] <- minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$loser)]
  }
  # major or junior government party
  if(length(Celection$elected)>0 ){
    final$largest_gov[final$case_ID==k] <- ifelse(
      minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]==max(minis[1,], na.rm=TRUE),
      1,0)
    final$junior_gov[final$case_ID==k] <- ifelse(
      minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]!=max(minis[1,], na.rm=TRUE) &
        minis[1,which(c("A", "B", "C", "D", "E", "F", "G", "H", "I")==Celection$elected)]>0  ,
      1,0)
  }
  # Characteristics of largest government party
  if(length(Celection$elected)>0 & any(is.na(minis)==FALSE)){
    j <- which(minis[1,]==max(minis[1,], na.rm=TRUE))
    final$cab_posts_gov[final$case_ID==k] <- minis[1,j]
    final$leri_gov[final$case_ID==k] <- pp[j]
    final$family_gov[final$case_ID==k] <- fam[1,j]
    final$party_gov[final$case_ID==k] <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[j]
  }
  
  
  final$N[final$case_ID==k] <- dim(rating_mat)[1]
  
  
}




final_all <- rbind(final_imd, final)



### Manually add party names #####

final_all$elected <- final_all$Cwinner_party
final_all$loser <- final_all$Closer_party

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

testcases <- as.list(unique(d$case_ID))
testnames.list <- lapply(testcases, partyname.fun)
partyname.df <- bind_rows(testnames.list) %>% filter(., is.na(Country) == F)



results2.df <- final_all %>%
  left_join(x = .,
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(elected = Letter, Cwinner_party = partyNAME),
            by = c("case_ID", "elected")) %>%
  left_join(x = ., 
            y = partyname.df %>% select(case_ID, Letter, partyNAME) %>% 
              rename(loser = Letter, Closer_party = partyNAME),
            by = c("case_ID", "loser"))


final_all <- results2.df
### manually add government composition for wave 1 ####
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
cabinet.efficiency <- final_all %>% select(., -c("n_parties")) %>%
  left_join(x = ., y = cabinet, by = "case_ID") %>%
  mutate(efficiency = ifelse(elected %in% gov.parties, 1, 0))

# In 
round(sum(cabinet.efficiency$efficiency)/nrow(cabinet.efficiency)*100,2)
# % of all cases the CW belongs to the cabinet after the resp. election.

final_all$efficiency <- cabinet.efficiency$efficiency

# Special case Slovenia, tie between Condorcet winner parties
final_all$efficiency[final_all$case_ID=="Slovenia_1996"] <- 1  # LDS
final_all$pm_after[final_all$case_ID=="Slovenia_1996"] <- 1  # LDS
final_all$votemargin_winner[final_all$case_ID=="Slovenia_1996"] <- 8  # LDS

# Special case Hong Kong 2012, tie between Condorcet winner parties
final_all$efficiency[final_all$case_ID=="Hong Kong_2012"] <- 0  # Democratic Party
final_all$pm_after[final_all$case_ID=="Hong Kong_2012"] <- 0  # Democratic Party
final_all$votemargin_winner[final_all$case_ID=="Hong Kong_2012"] <- -7  # Democratic Party

for(i in 1:266){
  final_all$cabinet_composition[i] <- paste(unlist(cabinet.efficiency$gov.parties[i]), collapse="")
}

# Manually add presidents

# for presidential elections, update efficiency variable which may deviate if candidate code (A-I) deviates from party codes (A-I)

final_all$case_ID[final_all$presidential==1 & is.na(final_all$president_after)]

final_all$president_after[final_all$case_ID=="Belarus_2001"&final_all$presidential==1] <- 0
final_all$Cwinner_party.y[final_all$case_ID=="Belarus_2001"&final_all$presidential==1] <- "Gajdukkevich"


final_all$president_after[final_all$case_ID=="France_2002"&final_all$presidential==1] <- 1
final_all$Cwinner_party.y[final_all$case_ID=="France_2002"&final_all$presidential==1] <- "Chirac"

final_all$president_after[final_all$case_ID=="Lithuania_1997"&final_all$presidential==1] <- 1
final_all$Cwinner_party.y[final_all$case_ID=="Lithuania_1997"&final_all$presidential==1] <- "Adamkus"

final_all$president_after[final_all$case_ID=="Mexico_2000"&final_all$presidential==1] <- 0
final_all$Cwinner_party.y[final_all$case_ID=="Mexico_2000"&final_all$presidential==1] <- "Solorzano"

final_all$president_after[final_all$case_ID=="Peru_2000"&final_all$presidential==1] <- 1
final_all$Cwinner_party.y[final_all$case_ID=="Peru_2000"&final_all$presidential==1] <- "Fujimori"


final_all$Cwinner_party.y[final_all$case_ID=="Peru_2001"&final_all$presidential==1] <- "Flores Nano"
final_all$Cwinner_party.y[final_all$case_ID=="Peru_2016"&final_all$presidential==1] <- "Fujimori"
final_all$Cwinner_party.y[final_all$case_ID=="Peru_2021"&final_all$presidential==1] <- "De Soto"



final_all$president_after[final_all$case_ID=="Romania_1996"&final_all$presidential==1] <- 1
final_all$Cwinner_party.y[final_all$case_ID=="Romania_1996"&final_all$presidential==1] <- "Constantinescu"
final_all$president_after[final_all$case_ID=="Romania_2009"&final_all$presidential==1] <- 0
final_all$Cwinner_party.y[final_all$case_ID=="Romania_2009"&final_all$presidential==1] <- "Geoana"


final_all$president_after[final_all$case_ID=="Tunisia_2019"&final_all$presidential==1] <- 0
final_all$Cwinner_party.y[final_all$case_ID=="Tunisia_2019"&final_all$presidential==1] <- "Karoubi"

final_all$president_after[final_all$case_ID=="Peru_2016"&final_all$presidential==1] <- 0
final_all$Cwinner_party.y[final_all$case_ID=="Peru_2016"&final_all$presidential==1] <- "Fujimori"

final_all$president_after[final_all$case_ID=="Philippines_2016"&final_all$presidential==1] <- 1
final_all$Cwinner_party.y[final_all$case_ID=="Philippines_2016"&final_all$presidential==1] <- "Duterte"

final_all$Cwinner_party.y[final_all$case_ID=="Argentina_2015"&final_all$presidential==1] <- "Scioli"


# Kyrgyzstan remains NA, as president Kurmanbek Bakiyev ran as independent - not included in our rating matrix which is based on party ratings only
# Russia 2000 and 2004 remain NA, as president Putin ran as independent - not included in our rating matrix which is based on party ratings only

final_all$president_after_loser[final_all$case_ID=="France_2002"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="France_2002"&final_all$presidential==1] <- "LePen"

final_all$president_after_loser[final_all$case_ID=="Belarus_2001"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="Belarus_2001"&final_all$presidential==1] <- "Goncharik"

final_all$president_after_loser[final_all$case_ID=="Lithuania_1997"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="Lithuania_1997"&final_all$presidential==1] <- "Pavilionis"

final_all$president_after_loser[final_all$case_ID=="Mexico_2000"&final_all$presidential==1] <- 1
final_all$Closer_party.y[final_all$case_ID=="Mexico_2000"&final_all$presidential==1] <- "Fox"

final_all$president_after_loser[final_all$case_ID=="Peru_2000"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="Peru_2000"&final_all$presidential==1] <- "Salinas"


final_all$president_after_loser[final_all$case_ID=="Romania_1996"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="Romania_1996"&final_all$presidential==1] <- "Funar"

final_all$president_after_loser[final_all$case_ID=="Tunisia_2019"&final_all$presidential==1] <- 0
final_all$Closer_party.y[final_all$case_ID=="Tunisia_2019"&final_all$presidential==1] <- "Maghzaoui"

# largest party by votes?
  final_all$system[final_all$presidential==1] <- 4

# Is p the largest party?
final_all$largest_party <- ifelse(final_all$votemargin_winner>=0, 1, 0)

final_all$largest_party[final_all$case_ID=="Belarus_2001" & final_all$presidential==1] <- 0  #Lukashenko president, Gajdukkevich CW
final_all$largest_party[final_all$case_ID=="Romania_2014" & final_all$presidential==1] <- 1  #Iohannis
final_all$largest_party[final_all$case_ID=="France_2002" & final_all$presidential==1] <- 1  #Chirac
final_all$largest_party[final_all$case_ID=="Costa Rica_2018" & final_all$presidential==1] <- 1  #Alvarado
final_all$largest_party[final_all$case_ID=="Lithuania_1997" & final_all$presidential==1] <- 1  #Adamkus
final_all$largest_party[final_all$case_ID=="Philippines_2010" & final_all$presidential==1] <- 1  #Aquino III
final_all$largest_party[final_all$case_ID=="Philippines_2016" & final_all$presidential==1] <- 1  #Duterte
final_all$largest_party[final_all$case_ID=="Uruguay_2019" & final_all$presidential==1] <- 1  #Lacalle Pou
final_all$largest_party[final_all$case_ID=="Argentina_2015" & final_all$presidential==1] <- 0  #Macri
final_all$largest_party[final_all$case_ID=="Peru_2016" & final_all$presidential==1] <- 0  #Kuckinsky president, Castillo CW



final_all$efficiency[final_all$cab_posts_winner>0] <- 1

final_all$efficiency[final_all$presidential==1] <- ifelse(final_all$president_after[final_all$presidential==1]==1, 1,0)

# Is CW the prime minister/president

final_all$pm_pres <- ifelse(is.na(final_all$pm_after), final_all$president_after,final_all$pm_after)
final_all$pm_pres[final_all$presidential==1] <- final_all$president_after[final_all$presidential==1]


# Is condorcet loser part of gov?
final_all$loser_turns_winner <- ifelse(final_all$presidential==0 & final_all$cab_posts_loser>0, 1, 0)
final_all$loser_turns_winner[final_all$presidential==1] <- final_all$president_after_loser[final_all$presidential==1]

# Manually add info on Belarus parliamentary election 2000
final_all$largest_party[final_all$case_ID=="Belarus_2001" & final_all$presidential==0] <- 0 # communist party
final_all$pm_pres[final_all$case_ID=="Belarus_2001" & final_all$presidential==0] <- 0 # communist party
final_all$efficiency[final_all$case_ID=="Belarus_2001" & final_all$presidential==0] <- 0 # communist party
###########################################################
# sort 
final_all <- arrange(final_all, case_ID)
final_all <- arrange(final_all, presidential)

final_all <- final_all[final_all$case_ID!="Kyrgyzstan_2005" & 
                         final_all$case_ID!="Russian Federation_2000" &
                         final_all$case_ID!="Russian Federation_2004" ,]
 
write.csv(final_all, file="final_imd.csv", row.names = F)

save(final_all, file="final_imd.rdata")


### END OF SCRIPT ###############################################################

