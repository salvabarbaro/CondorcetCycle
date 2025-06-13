###### Tables G.15 and G.16: Regressions using Core_FWER and Condorcet_FWER #######

govvars_holm <- c("post_gov","Corecomb3_holm","CWincomb3_holm","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_holm <- c("pmalt","Corecomb3_holm","CWincomb3_holm","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")

coefholm <- coef
coefholm[1:2] <- c("Core$_{FWER}$","Condorcet$_{FWER}$")
coefpholm <- coefp
coefpholm[1:2] <- coefholm[1:2]

GovOLS_holm <- CoreRegression(govvars_holm, regdatas)
PMOLS_holm <- CoreRegression(pmvars_holm, regdatas)
Govparty_holm <- CoreRegression(govvars_holm, regdatas, party)
PMparty_holm <- CoreRegression(pmvars_holm, regdatas, party)
