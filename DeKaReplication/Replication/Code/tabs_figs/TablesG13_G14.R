####### Tables G.13 and G.14: Regressions using Core_FDR and Condorcet_FDR ########

govvars_BY <- c("post_gov","Corecomb3_BY","CWincomb3_BY","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_BY <- c("pmalt","Corecomb3_BY","CWincomb3_BY","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")

coefBY <- coef
coefBY[1:2] <- c("Core$_{FDR}$","Condorcet$_{FDR}$")
coefpBY <- coefp
coefpBY[1:2] <- coefBY[1:2]

GovOLS_BY <- CoreRegression(govvars_BY, regdatas)
PMOLS_BY <- CoreRegression(pmvars_BY, regdatas)
Govparty_BY <- CoreRegression(govvars_BY, regdatas, party)
PMparty_BY <- CoreRegression(pmvars_BY, regdatas, party)
