######### Table H.19 and H.20: Regressions on surveys with 90%+coverage ############

govvars <- c("post_gov","Corecomb3","Cwincomb3","lhseat","plu","wmedlrcvr","difmedlrcvr","pre_gov")
pmvars <- c("pmalt","Corecomb3","Cwincomb3","lhseat","plu","wmedlrcvr","difmedlrcvr","prepmalt")
party <- c("partycountry","ntparty")

coveragedata <- regdatas %>% group_by(id) %>%
                    mutate(regseatshare = sum(lhseat, na.rm=TRUE)) %>%
                    filter(regseatshare >= 90)

Gov_full <- CoreRegression(govvars, coveragedata)
PM_full <- CoreRegression(pmvars, coveragedata)
Govparty_full <- CoreRegression(govvars, coveragedata, party)
PMparty_full <- CoreRegression(pmvars, coveragedata, party)
