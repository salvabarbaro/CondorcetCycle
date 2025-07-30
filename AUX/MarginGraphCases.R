## Cases for Margin Graphs
cases.to.consider <- c("Germany_2013", "France_2007", "Australia_2019",
                       "Great Britain_2015", "Argentina_2015", "Netherlands_2017")


DEU13 <- res.party$Germany_2013
FRA07 <- res.party$France_2007
AUS19 <- res.party$Australia_2019
GRB15 <- res.party$`Great Britain_2015`
NDL17 <- res.party$Netherlands_2017
ARG15 <- res.party$Argentina_2015

## DEU13
# A: CDU/CSU   (CW) 
# B: SPD (2nd)
# D: B90/Green (3rd)
DEU13.data <- DEU13$data %>% as.data.frame(.) %>%
  mutate(AB = ifelse(A < B, 1, 0),
         BA = ifelse(B < A, 1, 0),
         AD = ifelse(A < D, 1, 0),
         DA = ifelse(D < A, 1, 0),
         BD = ifelse(B < D, 1, 0),
         DB = ifelse(D < B, 1, 0))

sapply(DEU13.data[,8:13], sum)
rm(DEU13, DEU13.data)
########################################################
FRA07 <- res.party$France_2007
FRA07.data <- FRA07$data %>% as.data.frame(.) %>%
  mutate(AB = ifelse(A < B, 1, 0),
         BA = ifelse(B < A, 1, 0),
         AC = ifelse(A < C, 1, 0),
         CA = ifelse(C < A, 1, 0),
         BC = ifelse(B < C, 1, 0),
         CB = ifelse(C < B, 1, 0))

sapply(FRA07.data[,8:13], sum)
rm(FRA07, FRA07.data)
########################################################
AUS19 <- res.party$Australia_2019
## A (CW:) Liberal Party, B (2nd): Austr. Labor Party, C (3rd): Austr. Greens
AUS19.data <- AUS19$data %>% as.data.frame(.) %>%
  mutate(AB = ifelse(A < B, 1, 0),
         BA = ifelse(B < A, 1, 0),
         AC = ifelse(A < C, 1, 0),
         CA = ifelse(C < A, 1, 0),
         BC = ifelse(B < C, 1, 0),
         CB = ifelse(C < B, 1, 0))

sapply(AUS19.data[,6:11], sum)
rm(AUS19, AUS19.data)
########################################################
GBR15 <- res.party$`Great Britain_2015`
GBR15$totals
partyname.df %>% filter(., case_ID == "Great Britain_2015")
## A (CW:) Conservatives, B (2nd): Labor, D (3rd): Liberals
GBR15.data <- GBR15$data %>% as.data.frame(.) %>%
  mutate(AB = ifelse(A < B, 1, 0),
         BA = ifelse(B < A, 1, 0),
         AD = ifelse(A < C, 1, 0),
         DA = ifelse(C < A, 1, 0),
         BD = ifelse(B < C, 1, 0),
         DB = ifelse(C < B, 1, 0))

sapply(GBR15.data[,6:11], sum)
rm(GBR15, GBR15.data)
########################################################
NDL17 <- res.party$Netherlands_2017
NDL17$totals
partyname.df %>% filter(., case_ID == "Netherlands_2017")
## D (CW:) D66, A (2nd): VVD, E (3rd): SP (PvdA)
NDL17.data <- NDL17$data %>% as.data.frame(.) %>%
  mutate(AD = ifelse(A < D, 1, 0),
         DA = ifelse(D < A, 1, 0),
         AE = ifelse(A < E, 1, 0),
         EA = ifelse(E < A, 1, 0),
         DE = ifelse(D < E, 1, 0),
         ED = ifelse(E < D, 1, 0))

sapply(NDL17.data[,10:15], sum)
rm(NDL17, NDL17.data)
########################################################
ARG15 <- res.party$Argentina_2015
ARG15$totals
partyname.df %>% filter(., case_ID == "Argentina_2015")
## A (CW:) FPV (left), I (2nd): PJP (Peronsitic, conservative), G (3rd): PRO Propuesta Republicana (market orientated liberal)
ARG15.data <- ARG15$data %>% as.data.frame(.) %>%
  mutate(AI = ifelse(A < I, 1, 0),
         IA = ifelse(I < A, 1, 0),
         AG = ifelse(A < G, 1, 0),
         GA = ifelse(G < A, 1, 0),
         GI = ifelse(G < I, 1, 0),
         IG = ifelse(I < G, 1, 0))

sapply(ARG15.data[,7:12], sum)
rm(NDL17, NDL17.data)
