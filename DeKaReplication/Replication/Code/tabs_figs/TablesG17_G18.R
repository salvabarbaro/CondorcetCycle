###### Tables G.17 and G.18: Regressions using Core_cont and Condorcet_cont #######

govvars_p <- c("post_gov","Corepvalcomb3","Cwinpvalcomb3","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_p <- c("pmalt","Corepvalcomb3","Cwinpvalcomb3","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")

coefcont <- coef
coefcont[1:2] <- c("Core$_{cont}$","Condorcet$_{cont}$")
coefpcont <- coefp
coefpcont[1:2] <- coefcont[1:2]

GovOLS_p <- CoreRegression(govvars_p, regdatas)
PMOLS_p <- CoreRegression(pmvars_p, regdatas)
Govparty_p <- CoreRegression(govvars_p, regdatas, party)
PMparty_p <- CoreRegression(pmvars_p, regdatas, party)
