#
#   CSES Integrated File
#
#   Frequency of Condorcet Cycles
#
################################################
rm(list = ls())
# library(foreign)
library(haven)
library(vote)
library(dplyr)
library(data.table)

setwd("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/RScripts/AnnaScript311024/")
d1 <- read_dta("cses_imd.dta")
d2 <- read_dta("cses5.dta")
# CSES IMD #

d <- d1
rm(d1)
gc()

na.countries <- c("Switzerland_2019")

d$cntry <- d$IMD1006_NAM

# Generate case identifier=country x year
d$year <- as.character(d$IMD1008_YEAR)
d$case_ID <- paste(d$cntry, d$year, sep="_")
# Individual Party ratings
# 0=strongly dislike
# 10=strongly like
d <- d %>% filter(., !case_ID %in% na.countries)


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

# Mean left-right placement of parties
d$leri_party_a <- d$IMD3007_A
d$leri_party_b <- d$IMD3007_B
d$leri_party_c <- d$IMD3007_C
d$leri_party_d <- d$IMD3007_D
d$leri_party_e <- d$IMD3007_E
d$leri_party_f <- d$IMD3007_F
d$leri_party_g <- d$IMD3007_G
d$leri_party_h <- d$IMD3007_H
d$leri_party_i <- d$IMD3007_I

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
  final$presidential[final$case_ID==k] <- ifelse(dt$IMD1009[1]==20, 1, 0)
  
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
  parties <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")[include]  
  
  final$n_parties[final$case_ID==k] <- length(include[include==TRUE])
  
  rating_mat.s <- rating_mat[,include == TRUE] %>% 
    as.data.frame(.) %>%
    filter(!if_all(everything(), is.na))  # removes all rows that consist of NA's only.
  rating_mat <- rating_mat.s
  #
  trank <- as.data.frame(t(apply(rating_mat, 1, 
                                 function(x) frank(-x, 
                                                   ties.method = "max", 
                                                   na.last = TRUE)))) 
  colnames(trank) <- parties
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE, quiet = T)
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
  
  # Candidate rankings
  # Which candidates are included in this case?
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
  
  
  
  if(k=="Switzerland_2007") {include[7:8] <- FALSE}
  if(k== "Spain_2004") {include[4:9] <- FALSE}
  if(k== "Great Britain_1997") {include[4:5] <- FALSE}
  if(k== "Great Britain_2005") {include[4:5] <- FALSE}
  if(k== "Great Britain_2015") {include[c(5,7)] <- FALSE}
  
  
  final$n_candidates[final$case_ID==k] <- length(include[include==TRUE])
  
  rating_mat.s <- rating_mat[,include == TRUE] %>% 
    as.data.frame(.) %>%
    filter(!if_all(everything(), is.na))  # removes all rows that consist of NA's only.
  rating_mat <- rating_mat.s
  ##############################################
  if (any(include)) {
    final$candidate_included[final$case_ID == k] <- TRUE
    trank <- as.data.frame(t(apply(rating_mat, 1, function(x) {
      frank(-x, ties.method = "max", na.last = TRUE)
    }))) %>%
      setNames(., LETTERS[1:length(include[include==TRUE])])
  } else {
    final$candidate_included[final$case_ID == k] <- FALSE
  }
  
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE, quiet = T)
  if(length(Celection$elected)>0){
    final$Cwinner_candidate[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_candidate[final$case_ID==k] <- Celection$loser}
  }



final_imd <- final
#final <- final_imd
write.csv(final, file="finalimd.csv", row.names = F)

#rm(final)






######## CSES 5 ############
d <- d2

d$cntry <- d$E1006_NAM

# Generate case identifier=country x year
d$year <- as.character(d$E1008)
d$case_ID <- paste(d$cntry, d$year, sep="_")



d <- d[d$case_ID!="Switzerland_2019",]   # no feeling thermometer scores available


# Individual Party ratings

# 0=strongly dislike
# 10=strongly like


  d$party_rating_a <- ifelse(d$E3017_A<11, d$E3017_A, NA)
  d$party_rating_b <- ifelse(d$E3017_B<11, d$E3017_B, NA)
  d$party_rating_c <- ifelse(d$E3017_C<11, d$E3017_C, NA)
  d$party_rating_d <- ifelse(d$E3017_D<11, d$E3017_D, NA)
  d$party_rating_e <- ifelse(d$E3017_E<11, d$E3017_E, NA)
  d$party_rating_f <- ifelse(d$E3017_F<11, d$E3017_F, NA)
  d$party_rating_g <- ifelse(d$E3017_G<11, d$E3017_G, NA)
  d$party_rating_h <- ifelse(d$E3017_H<11, d$E3017_H, NA)
  d$party_rating_i <- ifelse(d$E3017_I<11, d$E3017_I, NA)

  # set missings on thermometer scores to minimum value 0
  # d$party_rating_a[is.na(d$party_rating_a)] <- 0
  # d$party_rating_b[is.na(d$party_rating_b)] <- 0
  # d$party_rating_c[is.na(d$party_rating_c)] <- 0
  # d$party_rating_d[is.na(d$party_rating_d)] <- 0
  # d$party_rating_e[is.na(d$party_rating_e)] <- 0
  # d$party_rating_f[is.na(d$party_rating_f)] <- 0
  # d$party_rating_g[is.na(d$party_rating_g)] <- 0
  # d$party_rating_h[is.na(d$party_rating_h)] <- 0
  # d$party_rating_i[is.na(d$party_rating_i)] <- 0

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

  
  # Individual Candidate ratings
  
  # 0=strongly dislike
  # 10=strongly like
  
    d$candidate_rating_a <- ifelse(d$E3018_A<11, d$E3018_A, NA)
    d$candidate_rating_b <- ifelse(d$E3018_B<11, d$E3018_B, NA)
    d$candidate_rating_c <- ifelse(d$E3018_C<11, d$E3018_C, NA)
    d$candidate_rating_d <- ifelse(d$E3018_D<11, d$E3018_D, NA)
    d$candidate_rating_e <- ifelse(d$E3018_E<11, d$E3018_E, NA)
    d$candidate_rating_f <- ifelse(d$E3018_F<11, d$E3018_F, NA)
    d$candidate_rating_g <- ifelse(d$E3018_G<11, d$E3018_G, NA)
    d$candidate_rating_h <- ifelse(d$E3018_H<11, d$E3018_H, NA)
    d$candidate_rating_i <- ifelse(d$E3018_I<11, d$E3018_I, NA)
    
    # set missings on thermometer scores to minimum value 0
    # d$candidate_rating_a[is.na(d$candidate_rating_a)] <- 0
    # d$candidate_rating_b[is.na(d$candidate_rating_b)] <- 0
    # d$candidate_rating_c[is.na(d$candidate_rating_c)] <- 0
    # d$candidate_rating_d[is.na(d$candidate_rating_d)] <- 0
    # d$candidate_rating_e[is.na(d$candidate_rating_e)] <- 0
    # d$candidate_rating_f[is.na(d$candidate_rating_f)] <- 0
    # d$candidate_rating_g[is.na(d$candidate_rating_g)] <- 0
    # d$candidate_rating_h[is.na(d$candidate_rating_h)] <- 0
    # d$candidate_rating_i[is.na(d$candidate_rating_i)] <- 0
    # 
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

  
# Mean left-right placement of parties
  d$leri_party_a <- d$E3019_A
  d$leri_party_b <- d$E3019_B
  d$leri_party_c <- d$E3019_C
  d$leri_party_d <- d$E3019_D
  d$leri_party_e <- d$E3019_E
  d$leri_party_f <- d$E3019_F
  d$leri_party_g <- d$E3019_G
  d$leri_party_h <- d$E3019_H
  d$leri_party_i <- d$E3019_I
  
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
  
  d$family_party_a <- d$E5017_A
  d$family_party_b <- d$E5017_B
  d$family_party_c <- d$E5017_C
  d$family_party_d <- d$E5017_D
  d$family_party_e <- d$E5017_E
  d$family_party_f <- d$E5017_F
  d$family_party_g <- d$E5017_G
  d$family_party_h <- d$E5017_H
  d$family_party_i <- d$E5017_I
  
  d$family_party_a[d$family_party_a>22] <- NA
  d$family_party_b[d$family_party_b>22] <- NA
  d$family_party_c[d$family_party_c>22] <- NA
  d$family_party_d[d$family_party_d>22] <- NA
  d$family_party_e[d$family_party_e>22] <- NA
  d$family_party_f[d$family_party_f>22] <- NA
  d$family_party_g[d$family_party_g>22] <- NA
  d$family_party_h[d$family_party_h>22] <- NA
  d$family_party_i[d$family_party_i>22] <- NA
  
  

  
  # N of cabinet posts by party
  
  d$minis_party_a <- d$E5015_A
  d$minis_party_b <- d$E5015_B
  d$minis_party_c <- d$E5015_C
  d$minis_party_d <- d$E5015_D
  d$minis_party_e <- d$E5015_E
  d$minis_party_f <- d$E5015_F
  d$minis_party_g <- d$E5015_G
  d$minis_party_h <- d$E5015_H
  d$minis_party_i <- d$E5015_I
  
  d$minis_party_a[d$minis_party_a>47] <- NA
  d$minis_party_b[d$minis_party_b>47] <- NA
  d$minis_party_c[d$minis_party_c>47] <- NA
  d$minis_party_d[d$minis_party_d>47] <- NA
  d$minis_party_e[d$minis_party_e>47] <- NA
  d$minis_party_f[d$minis_party_f>47] <- NA
  d$minis_party_g[d$minis_party_g>47] <- NA
  d$minis_party_h[d$minis_party_h>47] <- NA
  d$minis_party_i[d$minis_party_i>47] <- NA
  
  d$minis_party_a <- d$minis_party_a / d$E5016
  d$minis_party_b <- d$minis_party_b / d$E5016
  d$minis_party_c <- d$minis_party_c / d$E5016
  d$minis_party_d <- d$minis_party_d / d$E5016
  d$minis_party_e <- d$minis_party_e / d$E5016
  d$minis_party_f <- d$minis_party_f / d$E5016
  d$minis_party_g <- d$minis_party_g / d$E5016
  d$minis_party_h <- d$minis_party_h / d$E5016
  d$minis_party_i <- d$minis_party_i / d$E5016
  
  
  
  
d <- d[!is.na(d$case_ID),]  
final <- as.data.frame(matrix(unique(d$case_ID), ncol=1))
names(final) <- "case_ID"
#
for(k in unique(final$case_ID)){
  dt <- d[d$case_ID==k,]
  
  final$cntry[final$case_ID==k]     <- dt$cntry[1]
  final$year[final$case_ID==k]      <- dt$year[1]
  final$polityIV[final$case_ID==k]  <- dt$IMD5051_1[1]
  final$system[final$case_ID==k]    <- dt$IMD5013[1]
  final$n_eff_parties[final$case_ID==k] <- dt$IMD5058_2[1]
  final$presidential[final$case_ID==k] <- ifelse(dt$IMD1009[1]==20, 1, 0)
  
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
  rating_mat <- rating_mat[,include]
  # ranking matrix for calculating  seats
  rating_mat <- rating_mat[,include] %>% as.data.frame(.) %>% filter(if_any(everything(), ~ !is.na(.))) 

  if (any(include == TRUE)) {
  trank <- as.data.frame(t(apply(trank, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE)))) %>%
    setNames(., LETTERS[1:ncol(.)])  #   
  }
  
  Celection <- condorcet(trank, runoff = FALSE, quiet = T)
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
  
  # Candidate rankings
  # Which candidates are included in this case?
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
  
  
  
  if(k=="Switzerland_2007") {include[7:8] <- FALSE}
  if(k== "Spain_2004") {include[4:9] <- FALSE}
  if(k== "Great Britain_1997") {include[4:5] <- FALSE}
  if(k== "Great Britain_2005") {include[4:5] <- FALSE}
  if(k== "Great Britain_2015") {include[c(5,7)] <- FALSE}
  
  
  final$n_candidates[final$case_ID==k] <- length(include[include==TRUE])
  
  rating_mat <- rating_mat[,include] %>% as.data.frame(.) %>% filter(if_any(everything(), ~ !is.na(.))) 
  
  trank <- rating_mat
  final$candidate_included[final$case_ID == k] <- any(include == TRUE)
  if (any(include == TRUE)) {
    trank <- as.data.frame(t(apply(trank, 1, function(x) frank(-x, ties.method = "max", na.last = TRUE))))
    colnames(trank) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[include]
  }

  Celection <- condorcet(trank, runoff = FALSE, quiet = T)
  if(length(Celection$elected)>0){
    final$Cwinner_candidate[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_candidate[final$case_ID==k] <- Celection$loser}
}

}  




############################################################
final_all <- rbind(final_imd, final)



  write_dta(final_all, "condorcet_all_NA.dta")
  
  
  
  save.image("condorcet_NA.R")
  
  
  par(mfrow=c(3,1))
  
  plot(density(final_all$leri_winner, na.rm=TRUE), col="magenta", lwd=2, 
       main="Distribution of Condorcet winners' and losers' ideological positions",
       xlab="Left-Right dimension")
  lines(density(final_all$leri_loser, na.rm=TRUE), col="darkblue", lwd=2)
  legend("topleft", col=c("magenta", "darkblue"), lwd=2, legend=c("C. winner", "C. loser"),
         bty="n")
  
  
  plot(density(final_all$leri_winner[final_all$polityIV==9|final_all$polityIV==10], na.rm=TRUE), col="magenta", lwd=2, 
       main="Only democracies",
       xlab="Left-Right dimension")
  lines(density(final_all$leri_loser[final_all$polityIV==9|final_all$polityIV==10], na.rm=TRUE), col="darkblue", lwd=2)
  legend("topleft", col=c("magenta", "darkblue"), lwd=2, legend=c("C. winner", "C. loser"),
         bty="n")
  
  
  plot(density(final_all$leri_loser[final_all$polityIV<9], na.rm=TRUE), col="darkblue", lwd=2, 
       main="Only non-democracies",
       xlab="Left-Right dimension", xlim=c(0,10))
  lines(density(final_all$leri_winner[final_all$polityIV<9], na.rm=TRUE), col="magenta", lwd=2)
  legend("topleft", col=c("magenta", "darkblue"), lwd=2, legend=c("C. winner", "C. loser"),
         bty="n")
  
  
  
  ### Histogram of Condorcet winners and losers party family
  f_win <- table(final_all$family_winner[final_all$family_winner<13])
  f_los <- table(final_all$family_loser[final_all$family_loser<13])
  f <- c(matrix(rbind(f_win, f_los, rep(0,12))))
  
  labs <- c(matrix(rbind(c("Ecological","Communist", "Socialist", "Social Democratic",
                           "Left Liberal", "Liberal", "Right Liberal",
                           "Christ. Dem.", "Consrevative", "National",
                           "Agrarian" ,"Ethnic"), rep("", 12), rep("",12))))

# #################
#   # Alternative approach to show winner and loser by party family:
#   partyfam.labels <- as_factor(d$family_party_a, levels = "both") %>%
#     levels() %>% 
#     as.data.frame() %>%
#     setNames("combined") %>%
#     mutate(
#       bracket_number = stringr::str_extract(combined, "\\[\\d+\\]"),       # Extracts the number in brackets
#       value = as.numeric(stringr::str_extract(combined, "(?<=\\[)\\d+(?=\\])")), # Extracts the number without dot
#       label = stringr::str_trim(stringr::str_replace(combined, ".*\\] \\d+\\.\\s*", ""))  # Extracts the label text
#     ) %>% 
#     select(., c("value", "label"))
# 
# partyfam.plotdata <- final_imd %>% left_join(x = ., 
#                                              y = partyfam.labels %>% rename(family_winner = value), 
#                                              by = "family_winner")  %>%
#   left_join(x = ., 
#             y = partyfam.labels %>% rename(family_loser = value),
#             by = "family_loser") %>%
#   select(., c("case_ID", "family_winner", "family_loser", "label.x", "label.y")) %>%
#   setNames(c("case_ID", "family_winner", "family_loser", "fam.winner", "fam.loser")) %>%
#   tidyr::pivot_longer(cols = c(fam.winner, fam.loser), 
#                names_to = "category", 
#                values_to = "family")
# 
# library(ggplot2)
# ggplot(data = partyfam.plotdata %>% filter(., is.na(family) == F), 
#        aes(x = family, fill = category)) +
#   geom_bar(position = position_dodge2(reverse = TRUE), col = "purple") +
#   scale_fill_manual(values = c("fam.winner" = "forestgreen", "fam.loser" = "orange")) +
#   labs(title = "Frequency of Family Winner and Loser Categories",
#        x = "Family",
#        y = "Count",
#        fill = "Category") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ### End of alternative approach  #############################################################  
  
    
  library(RColorBrewer)
  mycol <- c(brewer.pal(n=5, name="Dark2")[1:2], "white")
  par(mfrow=c(1,1), mar=c(10,5,2,2))
  barplot(f, 
       main="", xlab="", col=mycol,border=mycol,
       names.arg=labs, las=2, legend.text=c("Condorcet Winners", "Condorcet Losers"),
       args.legend=list(x="topright", col=mycol[1:2], bty="n", cex=1.2))
  
  
  # Descriptives for Appendix
  
library(stargazer)
library(xtable)
 
  xtable(table(final_all$cntry, final_all$year))
  
  library(dplyr)
  # sort mtcars by mpg, ascending... use desc(mpg) for descending
 final_all <- arrange(final_all, case_ID)
 
 final_all <- final_all[final_all$n_parties>2,]
  
  xtable(cbind(final_all$cntry, final_all$year,
                         final_all$Cwinner_party, final_all$Closer_party))
 
 
  
  
  d1$cntry <- d1$IMD1006_NAM
  
  # Generate case identifier=country x year
  d1$year <- as.character(d1$IMD1008_YEAR)
  d1$case_ID <- paste(d1$cntry, d1$year, sep="_")
  
  d1 <- arrange(d1, case_ID)
  
  for(i in final_imd$case_ID){
    print(i)
    print(as.character(d1[d1$case_ID==i,166:174][1,]))
  }

  
  d2$cntry <- d2$E1006_NAM
  
  # Generate case identifier=country x year
  d2$year <- as.character(d2$E1008)
  d2$case_ID <- paste(d2$cntry, d2$year, sep="_")
  
  
  for(i in final$case_ID){
    print(i)
    print(as.character(d2[d2$case_ID==i,256:264][1,]))
  }
  
  
  
  # N of cases with no Condorcet winner
  length(final_all$case_ID[is.na(final_all$Cwinner_party)])
  # N
  length(final_all$case_ID)
  
  
  # N of cases with no Condorcet winner candidate
  length(final_all$case_ID[is.na(final_all$Cwinner_candidate) & 
                             final_all$candidate_included==TRUE])
  # N
  length(final_all$case_ID[final_all$candidate_included==TRUE])
  
  
  
  # N of cases where Condorcet winner part of government
  length(final_all$case_ID[!is.na(final_all$Cwinner_party) & 
                             final_all$cab_posts_winner>0])
  
  
