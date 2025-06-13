####################################################################################
#
# File:                 ReplicateStep2.R
#
# Authors:              Zuheir Desai and Tasos Kalandrakis
#
# Description:          Replicates figure 2 and the tables in
#                       ``The Core of the Party System.''
#                       Please see the associated README.txt file for more details.
#
# Created:              Jun - 2024
#
# Last Modified:        Jul - 2024
#
# Language:             R
#
# Related References:   [1] Zuheir Desai and Tasos Kalandrakis. 2024. "The
#                       Core of the Party System," Journal of Politics,
#                       conditionally accepted.
#
####################################################################################

rm(list = ls(all = TRUE))
options("width"=500,"digits" = 3)

# install packages if not already and load libraries

packages.to.install <- c("dplyr","lfe","qwraps2","stargazer","readxl","data.table",
                        "scales","stringr")
not.installed <- packages.to.install[!(packages.to.install %in%
                                    installed.packages()[,"Package"])]
if(length(not.installed)) install.packages(not.installed,
    repos='http://cran.us.r-project.org')

library(dplyr);library(lfe);library(stargazer);library(qwraps2)
library(readxl);library(data.table);library(scales);library(stringr)


# SET WORKING DIRECTORY TO THE FOLDER THAT CONTAINS THIS SCRIPT
setwd("/.../Replication")


#### Read data files, source functions ####

source("./Code/processing/finish_creation.R")
source("./Code/processing/ancillary_functions.R")

data <- readRDS("Finaldata.Rds")

indiff <- fread("Indiff_ind.csv")

base <- read_excel("Tablebase_top1.xlsx")
corebase <- read_excel("Tablecore_top1.xlsx")
govbase <- read_excel("Tablegov_top1.xlsx")

#################################################################################
################################# MAIN PAPER ####################################
#################################################################################

################### FIGURE 2: Party level test p-values ######################

source("./Code/tabs_figs/Figure2.R")

##################### TABLE 1: Party level test results #######################

source("./Code/tabs_figs/Table1.R")

table1 <- fulltable

printsumtable(table1,"Table 1: Party level test results")

############# TABLE 2: Survey-level tests of existence of core/CW #############

source("./Code/tabs_figs/Table2.R")

table2 <- surveytable

printsumtable(table2,"Table 2: Survey-level tests of existence of core/CW")

###################### TABLE 3: Reported Indifferences  #######################

source("./Code/tabs_figs/Table3.R")

table3 <- indiffsumtable

printsumtable(table3,"Table 3: Reported Indifferences")

########### TABLE 4: Reported Indifferences and profile of responses ###########

source("./Code/tabs_figs/Table4.R")

table4 <- indiffcattable

printsumtable(table4,"Table 4: Reported Indifferences and profile of responses")

######################## TABLE 5: Summary statistics ###########################

source("./Code/tabs_figs/Table5.R")

table5 <- partyobsnew

printsumtable(table5,"Table 5: Summary statistics")

######################## TABLE 6 & 7: regressions ###############################

source("./Code/tabs_figs/Tables6_7.R")

printregtable(list(GovOLS,Govparty),coef,"Table 6: Government Participation")

printregtable(list(PMOLS,PMparty),coefp,"Table 7: Premiership")

############# Table 8: Government coalition party base preferences ##############

source("./Code/tabs_figs/Table8.R")

table8 <- govbasesub

printsumtable(table8,"Table 8: Government coalition party base preferences")

##################### Table 9: Core party base preferences #######################

source("./Code/tabs_figs/Table9.R")

table9 <- corebasesub

printsumtable(table9,"Table 9: Core party base preferences")

######## Table 10: Legislative preferences for core v. non-core parties ##########

source("./Code/tabs_figs/Table10.R")

table10 <- basesubsummary

printsumtable(table10,"Table 10: Legislative preferences for core v. non-core parties")

#################################################################################
################################# APPENDIX ######################################
#################################################################################

################## Table B.2: Party level test results ##########################

source("./Code/tabs_figs/TableB2.R")

tableb2 <- fulltable

printsumtable(tableb2,"Table B.2: Party level test results")

############# Table B.3: Survey level tests of existence of core/CW #############

source("./Code/tabs_figs/TableB3.R")

tableb3 <- surveytable

printsumtable(tableb3,"Table B.3: Survey level tests of existence of core/CW")

################ Table C.4: Party level weighted test results ###################

source("./Code/tabs_figs/TableC4.R")

tablec4 <- fulltable

printsumtable(tablec4,"Table C.4: Party level weighted test results")

####################### Table D.5: Country Aggregates ############################

source("./Code/tabs_figs/TableD5.R")

tabled5 <- cbind(T3country1, T3country2)

printsumtable(tabled5,"Table D.5: Country Aggregates")

######## Table E.6: Fraction of seats earned by parties by survey ################

source("./Code/tabs_figs/TableE6.R")

tablee6 <- coveragenum

printsumtable(tablee6,"Table E.6: Fraction of seats earned by parties by survey")

###### Tables G.9 and G.10: Regressions using Core_0.01 and Condorcet_0.01 #######

source("./Code/tabs_figs/TablesG9_G10.R")

printregtable(list(GovOLS_01,Govparty_01),coef01,
    "Table G.9: Government participation using Core$_{0.01}$ and Condorcet$_{0.01}$")

printregtable(list(PMOLS_01,PMparty_01),coefp01,
    "Table G.10: Premiership using Core$_{0.01}$ and Condorcet$_{0.01}$")

###### Tables G.11 and G.12: Regressions using Core_0.10 and Condorcet_0.10 #######

source("./Code/tabs_figs/TablesG11_G12.R")

printregtable(list(GovOLS_10,Govparty_10),coef10,
    "Table G.11: Government participation using Core$_{0.10}$ and Condorcet$_{0.10}$")

printregtable(list(PMOLS_10,PMparty_10),coefp10,
    "Table G.12: Government participation using Core$_{0.10}$ and Condorcet$_{0.10}$")

####### Tables G.13 and G.14: Regressions using Core_FDR and Condorcet_FDR ########

source("./Code/tabs_figs/TablesG13_G14.R")

printregtable(list(GovOLS_BY,Govparty_BY),coefBY,
    "Table G.13: Government participation using Core$_{FDR}$ and Condorcet$_{FDR}$")

printregtable(list(PMOLS_BY,PMparty_BY),coefpBY,
    "Table G.14: Premiership using Core$_{FDR}$ and Condorcet$_{FDR}$")

###### Tables G.15 and G.16: Regressions using Core_FWER and Condorcet_FWER #######

source("./Code/tabs_figs/TablesG15_G16.R")

printregtable(list(GovOLS_holm,Govparty_holm),coefholm,
    "Table G.15: Government participation using Core$_{FWER}$ and Condorcet$_{FWER}$")

printregtable(list(PMOLS_holm,PMparty_holm),coefpholm,
    "Table G.16: Premiership using Core$_{FWER}$ and Condorcet$_{FWER}$")

###### Tables G.17 and G.18: Regressions using Core_cont and Condorcet_cont #######

source("./Code/tabs_figs/TablesG17_G18.R")

printregtable(list(GovOLS_p,Govparty_p),coefcont,
    "Table G.17: Government participation using Core$_{cont}$ and Condorcet$_{cont}$")

printregtable(list(PMOLS_p,PMparty_p),coefpcont,
    "Table G.18: Premiership using Core$_{cont}$ and Condorcet$_{cont}$")

######### Table H.19 and H.20: Regressions on surveys with 90%+coverage ############

source("./Code/tabs_figs/TablesH19_H20.R")

printregtable(list(Gov_full,Govparty_full),coef,
    "Table H.19: Government participation (surveys with $\\geq$ 90\\% seat share coverage")

printregtable(list(PM_full,PMparty_full),coefp,
    "Table H.20: Premimership (surveys with $\\geq$ 90\\% seat share coverage")

############## Table I.21: Including party and survey fixed effects #################

source("./Code/tabs_figs/TableI21.R")

printregtable(list(Govpartysurvey,PMpartysurvey),coefsurvey,
    "Table I.21: Including party and survey fixed effects")

########### Table J.22: Specifications removing lower house seat share ###############

source("./Code/tabs_figs/TableJ22.R")

printregtable(list(collinearity_models),coefm,
    "Table J.22: Specifications removing lower house seat share")
