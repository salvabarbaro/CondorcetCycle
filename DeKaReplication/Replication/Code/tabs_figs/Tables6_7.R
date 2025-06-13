###################### TABLE 6 & 7 (and F.7 & F.8): regressions ###########################

# Subset data

regdatas <- data %>% select(id,partycountry,ntparty,lhvote,
                    pre_gov,prepmalt,lhseat,
                    wmedlrcvr,difmedlrcvr,plu,post_gov,pmalt,
                    starts_with(c("a05","a01")) &
                    !contains(c("Core","CWin","BY","holm")),
                    starts_with(c("Core_a05","CWin_a05")),
                    starts_with(c("Core_a01 ","CWin_a01 ")),
                    Corecomb3,Cwincomb3,Corepvalcomb3,Cwinpvalcomb3,
                    Corecomb3_BY,CWincomb3_BY,CoreT3_a01,CWinT3_a01,
                    FCoreT3_a01,FCWinT3_a01,Corecomb3_holm,CWincomb3_holm,
                    CoreT3_a10,CWinT3_a10,FCoreT3_a10,FCWinT3_a10,
                    regob) %>%
                    filter(regob == 1)

#* Label coefficients

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

govvars <- c("post_gov","Corecomb3","Cwincomb3","lhseat","plu",
            "wmedlrcvr","difmedlrcvr","pre_gov")
pmvars <- c("pmalt","Corecomb3","Cwincomb3","lhseat","plu",
            "wmedlrcvr","difmedlrcvr","prepmalt")
party <- c("partycountry","ntparty")


GovOLS <- CoreRegression(govvars, regdatas)
PMOLS <- CoreRegression(pmvars, regdatas)
Govparty <- CoreRegression(govvars, regdatas, party)
PMparty <- CoreRegression(pmvars, regdatas, party)
