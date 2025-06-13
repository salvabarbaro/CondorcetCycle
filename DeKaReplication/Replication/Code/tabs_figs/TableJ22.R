########### Table J.22: Specifications removing lower house seat share ###############

govvars_coll <- c("post_gov","Corecomb3","Cwincomb3","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars_coll <- c("pmalt","Corecomb3","Cwincomb3","plu","wmedlrcvr","difmedlrcvr","prepmalt")
party <- c("partycountry","ntparty")

coefm <- c(coef[-3],"Incumbent premier")

collinearity_models <- list(CoreRegression(govvars_coll,regdatas,party)[[4]],CoreRegression(govvars_coll,regdatas,partysurvey)[[4]],CoreRegression(pmvars_coll,regdatas,party)[[4]],CoreRegression(pmvars_coll,regdatas,partysurvey)[[4]])
