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

#setwd("U:/PaperProjects/CondorcetCycle")
setwd("U:/akurella/PaperProjects/CondorcetCycle")

d1 <- read_dta("U:/akurella/data/cses_imd.dta")

d2 <- read_dta("U:/akurella/data/cses5.dta")



# CSES IMD #

d <- d1

d$cntry <- d$IMD1006_NAM

# Generate case identifier=country x year
d$year <- as.character(d$IMD1008_YEAR)
d$case_ID <- paste(d$cntry, d$year, sep="_")









# Individual Party ratings

# 0=strongly dislike
# 10=strongly like


d$party_rating_a <- ifelse(d$IMD3008_A<11, d$IMD3008_A, NA)
d$party_rating_b <- ifelse(d$IMD3008_B<11, d$IMD3008_B, NA)
d$party_rating_c <- ifelse(d$IMD3008_C<11, d$IMD3008_C, NA)
d$party_rating_d <- ifelse(d$IMD3008_D<11, d$IMD3008_D, NA)
d$party_rating_e <- ifelse(d$IMD3008_E<11, d$IMD3008_E, NA)
d$party_rating_f <- ifelse(d$IMD3008_F<11, d$IMD3008_F, NA)
d$party_rating_g <- ifelse(d$IMD3008_G<11, d$IMD3008_G, NA)
d$party_rating_h <- ifelse(d$IMD3008_H<11, d$IMD3008_H, NA)
d$party_rating_i <- ifelse(d$IMD3008_I<11, d$IMD3008_I, NA)

# set missings on thermometer scores to minimum value 0
d$party_rating_a[is.na(d$party_rating_a)] <- 0
d$party_rating_b[is.na(d$party_rating_b)] <- 0
d$party_rating_c[is.na(d$party_rating_c)] <- 0
d$party_rating_d[is.na(d$party_rating_d)] <- 0
d$party_rating_e[is.na(d$party_rating_e)] <- 0
d$party_rating_f[is.na(d$party_rating_f)] <- 0
d$party_rating_g[is.na(d$party_rating_g)] <- 0
d$party_rating_h[is.na(d$party_rating_h)] <- 0
d$party_rating_i[is.na(d$party_rating_i)] <- 0

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

# set missings on thermometer scores to minimum value 0
d$candidate_rating_a[is.na(d$candidate_rating_a)] <- 0
d$candidate_rating_b[is.na(d$candidate_rating_b)] <- 0
d$candidate_rating_c[is.na(d$candidate_rating_c)] <- 0
d$candidate_rating_d[is.na(d$candidate_rating_d)] <- 0
d$candidate_rating_e[is.na(d$candidate_rating_e)] <- 0
d$candidate_rating_f[is.na(d$candidate_rating_f)] <- 0
d$candidate_rating_g[is.na(d$candidate_rating_g)] <- 0
d$candidate_rating_h[is.na(d$candidate_rating_h)] <- 0
d$candidate_rating_i[is.na(d$candidate_rating_i)] <- 0

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



d <- d[!is.na(d$case_ID),]  

final <- as.data.frame(matrix(unique(d$case_ID), ncol=1))
names(final) <- "case_ID"


for(k in unique(d$case_ID)){
  dt <- d[d$case_ID==k,]
  
  
  final$cntry[final$case_ID==k]     <- dt$cntry[1]
  final$year[final$case_ID==k]      <- dt$year[1]
  final$polityIV[final$case_ID==k]  <- dt$IMD5051_1[1]
  final$system[final$case_ID==k]    <- dt$IMD5013[1]
  final$n_eff_parties[final$case_ID==k] <- dt$IMD5058_2[1]
  
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
  
  rating_mat <- rating_mat[,include]
  
  # reduce to respondents with complete preference profiles (Skalometer)
  #rating_mat <- rating_mat[!is.na(rowSums(rating_mat)),] 
  
  
  # ranking matrix for calculating  seats
  trank <- rating_mat
  
  for(i in 1:length(rating_mat[,1])){
    tmp <- trank[i,]
    tr <- rank(-1*tmp, na.last=TRUE, ties.method="max")
    tr[is.na(tmp)] <- NA
    trank[i,] <- tr
  }
  colnames(trank) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[include]
  
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
  
  rating_mat <- rating_mat[,include]
  
  # reduce to respondents with complete preference profiles (Skalometer)
  #rating_mat <- rating_mat[!is.na(rowSums(rating_mat)),] 
  
  
  # ranking matrix for calculating  seats
  trank <- rating_mat
  
  for(i in 1:length(rating_mat[,1])){
    tmp <- trank[i,]
    tr <- rank(-1*tmp, na.last=TRUE, ties.method="max")
    tr[is.na(tmp)] <- NA
    trank[i,] <- tr
  }
  colnames(trank) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[include]
  
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE)
  if(length(Celection$elected)>0){
    final$Cwinner_candidate[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_candidate[final$case_ID==k] <- Celection$loser}
  
  
  
}


final_imd <- final


rm(final)






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
  d$party_rating_a[is.na(d$party_rating_a)] <- 0
  d$party_rating_b[is.na(d$party_rating_b)] <- 0
  d$party_rating_c[is.na(d$party_rating_c)] <- 0
  d$party_rating_d[is.na(d$party_rating_d)] <- 0
  d$party_rating_e[is.na(d$party_rating_e)] <- 0
  d$party_rating_f[is.na(d$party_rating_f)] <- 0
  d$party_rating_g[is.na(d$party_rating_g)] <- 0
  d$party_rating_h[is.na(d$party_rating_h)] <- 0
  d$party_rating_i[is.na(d$party_rating_i)] <- 0

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
    d$candidate_rating_a[is.na(d$candidate_rating_a)] <- 0
    d$candidate_rating_b[is.na(d$candidate_rating_b)] <- 0
    d$candidate_rating_c[is.na(d$candidate_rating_c)] <- 0
    d$candidate_rating_d[is.na(d$candidate_rating_d)] <- 0
    d$candidate_rating_e[is.na(d$candidate_rating_e)] <- 0
    d$candidate_rating_f[is.na(d$candidate_rating_f)] <- 0
    d$candidate_rating_g[is.na(d$candidate_rating_g)] <- 0
    d$candidate_rating_h[is.na(d$candidate_rating_h)] <- 0
    d$candidate_rating_i[is.na(d$candidate_rating_i)] <- 0
    
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

  
  
d <- d[!is.na(d$case_ID),]  

final <- as.data.frame(matrix(unique(d$case_ID), ncol=1))
names(final) <- "case_ID"
  
  
for(k in unique(d$case_ID)){
  dt <- d[d$case_ID==k,]
  
  
  final$cntry[final$case_ID==k] <- dt$cntry[1]
  final$year[final$case_ID==k] <- dt$year[1]
  final$polityIV[final$case_ID==k]  <- dt$E5091_1[1]
  final$system[final$case_ID==k]    <- dt$E5055[1]
  final$n_eff_parties[final$case_ID==k] <- dt$E5079[1]
  
# Party rankings
  # Which parties are included in this case?
  include <- rep(FALSE,9)
  include[1] <-  dt$E5000_A[1]!=9999999 & any(!is.na(dt$party_rating_a))
  include[2] <-  dt$E5000_B[1]!=9999999 & any(!is.na(dt$party_rating_b))
  include[3]  <- dt$E5000_C[1]!=9999999 & any(!is.na(dt$party_rating_c))
  include[4]  <- dt$E5000_D[1]!=9999999 & any(!is.na(dt$party_rating_d))
  include[5]  <- dt$E5000_E[1]!=9999999 & any(!is.na(dt$party_rating_e))
  include[6]  <- dt$E5000_F[1]!=9999999 & any(!is.na(dt$party_rating_f))
  include[7]  <- dt$E5000_G[1]!=9999999 & any(!is.na(dt$party_rating_g))
  include[8]  <- dt$E5000_H[1]!=9999999 & any(!is.na(dt$party_rating_h))
  include[9]  <- dt$E5000_I[1]!=9999999 & any(!is.na(dt$party_rating_i))
  
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
  
  rating_mat <- rating_mat[,include]
  
  # reduce to respondents with complete preference profiles (Skalometer)
  #rating_mat <- rating_mat[!is.na(rowSums(rating_mat)),] 
  
  
  # ranking matrix for calculating  seats
  trank <- rating_mat
  
  for(i in 1:length(rating_mat[,1])){
    tmp <- trank[i,]
    tr <- rank(-1*tmp, na.last=TRUE, ties.method="max")
    tr[is.na(tmp)] <- NA
    trank[i,] <- tr
  }
  colnames(trank) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[include]
  
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
  
  
  
  # Candidate rankings
  # Which candidates are included in this case?
  include <- rep(FALSE,9)
  include[1] <-  dt$E5000_A[1]!=9999999 & any(!is.na(dt$candidate_rating_a))
  include[2] <-  dt$E5000_B[1]!=9999999 & any(!is.na(dt$candidate_rating_b))
  include[3]  <- dt$E5000_C[1]!=9999999 & any(!is.na(dt$candidate_rating_c))
  include[4]  <- dt$E5000_D[1]!=9999999 & any(!is.na(dt$candidate_rating_d))
  include[5]  <- dt$E5000_E[1]!=9999999 & any(!is.na(dt$candidate_rating_e))
  include[6]  <- dt$E5000_F[1]!=9999999 & any(!is.na(dt$candidate_rating_f))
  include[7]  <- dt$E5000_G[1]!=9999999 & any(!is.na(dt$candidate_rating_g))
  include[8]  <- dt$E5000_H[1]!=9999999 & any(!is.na(dt$candidate_rating_h))
  include[9]  <- dt$E5000_I[1]!=9999999 & any(!is.na(dt$candidate_rating_i))
  
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
  
  rating_mat <- rating_mat[,include]
  
  # reduce to respondents with complete preference profiles (Skalometer)
  #rating_mat <- rating_mat[!is.na(rowSums(rating_mat)),] 
  
  
  # ranking matrix for calculating  seats
  trank <- rating_mat
  
  for(i in 1:length(rating_mat[,1])){
    tmp <- trank[i,]
    tr <- rank(-1*tmp, na.last=TRUE, ties.method="max")
    tr[is.na(tmp)] <- NA
    trank[i,] <- tr
  }
  colnames(trank) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")[include]
  
  # Determine Condorcet winner
  Celection <- condorcet(trank, runoff = FALSE)
  if(length(Celection$elected)>0){
    final$Cwinner_candidate[final$case_ID==k] <- Celection$elected}
  if(length(Celection$loser)>0){
    final$Closer_candidate[final$case_ID==k] <- Celection$loser}
  
    
  
  }
  




final_all <- rbind(final_imd, final)



  write_dta(final_all, "condorcet_all.dta")
  
  
  
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
  
  
  plot(density(final_all$leri_winner[final_all$polityIV<9], na.rm=TRUE), col="magenta", lwd=2, 
       main="Only non-democracies",
       xlab="Left-Right dimension")
  lines(density(final_all$leri_loser[final_all$polityIV<9], na.rm=TRUE), col="darkblue", lwd=2)
  legend("topleft", col=c("magenta", "darkblue"), lwd=2, legend=c("C. winner", "C. loser"),
         bty="n")
  
  
  
  
  
  save.image("condorcet.R")
  
  