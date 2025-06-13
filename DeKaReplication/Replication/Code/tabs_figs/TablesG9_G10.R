###### Tables G.9 and G.10: Regressions using Core_0.01 and Condorcet_0.01 #######

regdatas <- regdatas %>% mutate(Corecomb3_a01 = (CoreT3_a01 + FCoreT3_a01)/2, Cwincomb3_a01 = (CWinT3_a01 + FCWinT3_a01)/2)

coef <- c(
  "Core$_{0.05}$",
  "Condorcet$_{0.05}$",
  "LH seats",
  "Seat Plurality",
  "Legislative Median",
  "Distance to median",
  "Incumbent party"
)

coefp <- c(
  "Core$_{0.05}$",
  "Condorcet$_{0.05}$",
  "LH seats",
  "Seat Plurality",
  "Legislative Median",
  "Distance to median",
  "Incumbent premier"
)

party <- c("partycountry","ntparty")

govvars_01 <- c("post_gov","Corecomb3_a01","Cwincomb3_a01","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_01 <- c("pmalt","Corecomb3_a01","Cwincomb3_a01","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")

coef01 <- coef
coef01[1:2] <- c("Core$_{0.01}$","Condorcet$_{0.01}$")
coefp01 <- coefp
coefp01[1:2] <- coef01[1:2]

GovOLS_01 <- CoreRegression(govvars_01, regdatas)
PMOLS_01 <- CoreRegression(pmvars_01, regdatas)
Govparty_01 <- CoreRegression(govvars_01, regdatas, party)
PMparty_01 <- CoreRegression(pmvars_01, regdatas, party)
