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

#setwd()

d1 <- read_dta("cses_imd.dta")    # Version 18.11.2024, includes waves 1- 5


# CSES IMD #

d <- d1


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


# Drop Wallonia in BEL 1999

# Dummy for presidential elections
# 
# for(i in unique(d$case_ID)){
#   d$presidential[d$case_ID==i] <- ifelse(any(c("Argentina_2015", "Belarus_2001", "Brazil_2002", "Brazil_2006", "Brazil_2010", 
#                                                "Brazil_2014", "Chile_1999", "Chile_2005", "Chile_2009", "France_2002", 
#                                                "France_2012", "Kenya_2013", "Kyrgyzstan_2005", "Lithuania_1997", "Mexico_2000", 
#                                                "Mexico_2006", "Mexico_2012", "Peru_2000", "Peru_2001", "Peru_2006", # "Peru_2011",
#                                                "Peru_2016", "Philippines_2004", "Philippines_2010", "Philippines_2016", 
#                                                "Romania_2009", "Romania_2014", "Russia_2000", "Russia_2004", "Taiwan_1996", 
#                                                "Taiwan_2004", "Taiwan_2008", "Taiwan_2012", "United_States_1996", 
#                                                "United States of America_2004", "United States of America_2008", 
#                                                "United States of America_2012", 
#                                                "Uruguay_2009", "Brazil_2018", "Chile_2017", "Costa Rica_2018", 
#                                                "France_2017", "Taiwan_2016", "Taiwan_2020", 
#                                                "Turkey_2018", "United States of America_2016", 
#                                                "United States of America_2020", "Uruguay_2019")==i), 1, 0)}
# 



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
    setNames(., LETTERS[1:length(include[include==TRUE])]) 
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
  
   }



final_imd <- final


### Add presidential elections for mixed surveys ###

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
    setNames(., LETTERS[1:length(include[include==TRUE])]) 
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
  
}




final_all <- rbind(final_imd, final)


write.csv(final, file="final_imd.csv", row.names = F)

#rm(final)




  write_dta(final_all, "condorcet_revision.dta")
  
  
  
  save.image("condorcet_revision.R")

  

  
  # party family
  
  final_all$fam_left <- ifelse(final_all$family_winner<4, 1, 0)
  final_all$fam_socfinal_allem <- ifelse(final_all$family_winner==4, 1, 0)
  final_all$fam_lib <- ifelse(final_all$family_winner>4 & final_all$family_winner<8, 1, 0)
  final_all$fam_cons <- ifelse(final_all$family_winner>7 & final_all$family_winner<10, 1, 0)
  final_all$fam_right <- ifelse(final_all$family_winner==10 | final_all$family_winner==12, 1, 0)
  final_all$fam_other <- ifelse(final_all$family_winner==11 | final_all$family_winner>12, 1, 0)
  
  
  # party family
  
  final_all$fam_l_left <- ifelse(final_all$family_loser<4, 1, 0)
  final_all$fam_l_socfinal_allem <- ifelse(final_all$family_loser==4, 1, 0)
  final_all$fam_l_lib <- ifelse(final_all$family_loser>4 & final_all$family_loser<8, 1, 0)
  final_all$fam_l_cons <- ifelse(final_all$family_loser>7 & final_all$family_loser<10, 1, 0)
  final_all$fam_l_right <- ifelse(final_all$family_loser==10 | final_all$family_loser==12, 1, 0)
  final_all$fam_l_other <- ifelse(final_all$family_loser==11 | final_all$family_loser>12, 1, 0)
  
  
  
  
  
  
  
  ### Histogram of Confinal_allorcet winners anfinal_all losers party family
  f_win <- table(final_all$family_winner[final_all$family_winner<13])
  f_los <- table(final_all$family_loser[final_all$family_loser<13])
  f <- c(matrix(rbind(f_win, f_los, rep(0,12))))
  
  labs <- c(matrix(rbind(c("Ecological","Communist", "Socialist", "Social Democratic",
                           "Left Liberal", "Liberal", "Right Liberal",
                           "Christ. Dem.", "Consrevative", "National",
                           "Agrarian" ,"Ethnic"), rep("", 12), rep("",12))))
  
  library(RColorBrewer)
  mycol <- c(brewer.pal(n=5, name="Dark2")[1:2], "white")
  par(mfrow=c(1,1), mar=c(10,5,2,2))
  barplot(f, ylim=c(0,70),
          main="", xlab="", col=mycol,border=mycol,
          names.arg=labs, las=2, legend.text=c("Condorcet Winners", "Condorcet Losers"),
          args.legend=list(x="topright", col=mycol[1:2], bty="n", cex=1.2))
  box()
  
  
  
  
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
  
  
