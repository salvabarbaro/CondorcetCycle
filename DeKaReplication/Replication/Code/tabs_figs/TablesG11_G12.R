###### Tables G.11 and G.12: Regressions using Core_0.10 and Condorcet_0.10 #######

regdatas <- regdatas %>% mutate(Corecomb3_a10 = (CoreT3_a10 + FCoreT3_a10)/2, Cwincomb3_a10 = (CWinT3_a10 + FCWinT3_a10)/2)

govvars_10 <- c("post_gov","Corecomb3_a10","Cwincomb3_a10","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_10 <- c("pmalt","Corecomb3_a10","Cwincomb3_a10","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")

coef10 <- coef
coef10[1:2] <- c("Core$_{0.10}$","Condorcet$_{0.10}$")
coefp10 <- coefp
coefp10[1:2] <- coef10[1:2]

GovOLS_10 <- CoreRegression(govvars_10, regdatas)
PMOLS_10 <- CoreRegression(pmvars_10, regdatas)
Govparty_10 <- CoreRegression(govvars_10, regdatas, party)
PMparty_10 <- CoreRegression(pmvars_10, regdatas, party)
