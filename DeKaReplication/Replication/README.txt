UPDATED VERSION by Salvatore Barbaro

We run the script on a Linux HPC and each *.mat file required more than five hours. In order to want to skip this part,
use the script ContinueRepStep.1

README file for replication of results in
"The Core of the Party System"
by
Zuheir Desai <desai.596@osu.edu> and Tasos Kalandrakis <kalandrakis@rochester.edu>

**** OPERATING SYSTEM ****
macOS Sonoma, version 14.5
** Barbaro: Debian Linux

**** SOFTWARE ****
MATLAB version R_2024a/Apple silicon (should also run on all other R_2024a, R_2023a, R_2023b versions)
** Barbaro: R2021
R version 4.4.1 (2024-06-14) -- "Race for Your Life" (should also run on 4.4.0)
R packages required for analysis:
    data.table, dplyr, lfe, qwraps2, readxl, scales, stargazer, stringr

Please set your working directory to the folder that contains this README file

******************
TABLE OF CONTENTS 
******************

- README.txt                                    This file
- Codebook                                      MS Word file.  Codebook for data in Finaldata.Rds produced during replication
						and used as input in all regression analyses
- party_names.txt                               Text file contains party names by survey/country (auxiliary to codebook/
                                                Finaldata.Rds)
- LogStep1.txt					Log file for Step 1 of replication
- LogStep2.txt					Log file for Step 2 of replication
- ReplicateStep1.m                              MATLAB script
                                                    - Computes tests
                                                    - Creates intermediate inputs
- ReplicateStep2.R                              R script
                                                    - Generates figures and tables in the main text and appendix

-- Data 				Folder. Contains source data files

    - Fulldata_minustest.csv:                   Contains variables sourced from CSES IMD (Integrated Module Dataset)
                                                and CSES Module 5 Second Advance Release (downloaded on 14 May 2020)
    - Data.mat:                                 Contains preference data that are sourced from CSES thermometer data
                                                from CSES IMD (Integrated Module Dataset) and CSES Module 5 Second
                                                Advance Release (downloaded on 14 May 2020)

-- Code 				Folder. Contains codes in three subfolders

    -- party_test 			Folder. Contains functions to carry out party level C, NC, CW, NCW tests

            - CountryTests15F.m                 MATLAB function that is executed inside a loop in RunTest15F.m
            - FlipIUT.m                         MATLAB function that carries out an intersection union test (IUT)
            - MTestj.m                          MATLAB function that locates preference comparisons for party j
            - PrefC.m                           MATLAB function to identify weak preferences in favor of party j
            - PrefW.m                           MATLAB function to identify strict preferences in favor of party j
            - RunTest15F.m                      MATLAB function that runs party tests for all countries in data
            - Smod.m                            MATLAB function that computes the test statistic for the two-step test
            - TestTwoStep15F.m                  MATLAB function that carries out the two-step test from Romano et al. (2014)

    -- processing 			Folder. Contains various functions and scripts to generate and manipulate data

            - ancillary_functions.R             R functions for dataframe manipulation and regressions
            - BaseCoal.m:                       MATLAB function that calculates legislative preferences
            - BasePref.m:                       MATLAB function that calculates base preferences
            - ExtractResults.m:                 MATLAB function to extract party-level test results
            - findnanfields.m:                  MATLAB function to find the names of fields that only have NaN's
            - finish_creation.R:                R script that merges test results to data sourced from CSES,
                                                applies FDR and FWER adjustments to party test p-values, and
                                                creates test outcomes at various significance levels
            - Indiff.m:                         MATLAB function to compute individual level indifference statistics
            - PrefI.m:                          MATLAB function to identify indifferent comparisons

    -- tabs_figs 			Folder. Contains scripts that generate Tables and Figure 2

            - Figure2.R                         R script to build Figure 2
            - Table1.R                          R script to generate Table 1
            - Table2.R                          R script to generate Table 2
            - Table3.R                          R script to generate Table 3
            - Table4.R                          R script to generate Table 4
            - Table5.R                          R script to generate Table 5
            - Tables6_7.R                       R script to generate Tables 6 and 7 (and 'complete' Tables F.7 and F.8 in Appendix)
            - Table8.R                          R script to generate Table 8
            - Table9.R                          R script to generate Table 9
            - Table10.R                         R script to generate Table 10
            - TableB2.R                         R script to generate Table B.2
            - TableB3.R                         R script to generate Table B.3
            - TableC4.R                         R script to generate Table C.4
            - TableD5.R                         R script to generate Table D.5
            - TableE6.R                         R script to generate Table E.6
            - TablesG9_G10.R                    R script to generate Tables G.9 and G.10
            - TablesG11_G12.R                   R script to generate Tables G.11 and G.12
            - TablesG13_G14.R                   R script to generate Tables G.13 and G.14
            - TablesG15_G16.R                   R script to generate Tables G.15 and G.16
            - TablesG17_G18.R                   R script to generate Tables G.17 and G.18
            - TablesH19_H20.R                   R script to generate Tables H.19 and H.20
            - TableI21.R                        R script to generate Table I.21
            - TableJ22.R                        R script to generate Table J.22

    
*************************************************
OUTPUT FILES (written in main replication folder)
*************************************************

    (i) Main dataset

        - Finaldata.Rds:                        Main dataset used to produce most tables in the paper and appendix, observations
                                                are at the party-survey level

    (ii) Intermediate inputs

        - CcoreT3.mat:                          Unweighted C test and NC test results
        - CcoreTweight3.mat:                    Weighted C test and NC test results
        - WcoreT3.mat:                          Unweighted CW test and NCW test results
        - WcoreTweight3.mat:                    Weighted CW test and NCW test results

        - Indiff_ind_final.csv:                 Individual level data on indifferences
        - TestCoreResults.csv:                  Extracted test results from CcoreT3.mat, CcoreTweight3.mat, WcoreT3.mat,
                                                WcoreTweight3.mat

        - Tablebase_top1.xlsx:                  Legislative preferences for core v. non-core parties, each observation is a
                                                pair (core party,non-core party)
        - Tablecore_top1.xlsx:                  Core party base preferences for other core v. non-core parties, each observation is
                                                a triplet of distinct parties (core party,other core party,other non-core party)
        - Tablegov_top1.xlsx:                   Government party base preferences for other government v. non-government parties,
                                                each observation is a triplet of distinct parties
                                                (government party,other government party,other non-government party)

    (iii) Figures

        - Fig2.pdf:                             Figure 2: Party level test p-values


************************
REPLICATION INSTRUCTIONS
************************
1.a On line 27 of `ReplicateStep1.m' change path to the main replication folder (same folder as `ReplicateStep1.m')
1.b Run `ReplicateStep1.m'
2.a On line 40 of `ReplicateStep2.R' set working directory to the main replication folder (same folder as `ReplicateStep2.R')
2.b Run `ReplicateStep2.R'

NOTES:
-- Step 1 takes approximately 10 hours on an M1 MacBook Pro with 32GB of memory and 10 workers in MATLAB's parallel pool
-- Step 2 takes seconds on same machine 
-- All output files are saved on the main 'Replication' folder 
-- All Tables are displayed on the R prompt
-- Regression Tables 6 and 7 are displayed fully on R prompt and only partially in the main paper.  The full Tables are included as Tables F.7 and F.8 and are not reproduced as such in the replication
-- Figure 1 in the main text and Figure A.1 in the Appendix are non-data based art drawn by the authors and are not replicated
