############## Table I.21: Including party and survey fixed effects #################

partysurvey <- c("partycountry","id")

Govpartysurvey <- CoreRegression(govvars, regdatas, partysurvey)
PMpartysurvey <- CoreRegression(pmvars, regdatas, partysurvey)

coefsurvey <- c(coef,"Incumbent premier")
