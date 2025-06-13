############# Testing Core dataset creation ###############

# The following script merges the test results to the CSES data,
# applies FDR and FWER adjustments, creates test outcomes

# This script creates the file: Finaldata.Rds

# It needs the following files:  Fulldata_minustest.csv, TestCoreResults.csv

################## Functions #################################

fdr <- function(pval, z, m) {
    # fdr takes as input a matrix of pvalues, applies the FDR p-value
    # adjustment with method m on a combined vector of original and
    # flipped tests if z = 1, and on them separately if z = 0, breaks
    # the adjustment by test, and returns a matrix of p-values from
    # original and flipped tests, along with their adjusted counterparts
    #
    # INPUT:
    #       pval - vector of p-values
    #       z    - 0 if adjustment applied separately, 1 if together
    #       m    - method of p-value adjustment
    # OUTPUT:
    #       data frame of adjusted p-values, ready to be merged in full dataframe

    tests <- pval[,4:length(names(pval))]
    if (z == 1){
        col_odd <- seq_len(ncol(tests)) %% 2
        original <- tests[,col_odd == 1]
        flipped <- tests[,col_odd == 0]
        ntest <- length(original)
        nrows <- nrow(original)
        id <- pval[1:nrows,1:3]
        names(flipped) <- str_sub(names(flipped),start=2)
        combined <- rbind(original,flipped)
        pdata <- combined %>%
                    mutate( across(everything(),function(x) p.adjust(x, method = m),
                            .names = "{paste0({.col},'Adj_',{m})}"),
                            test = ifelse(row_number() <= nrows, "","F")) %>%
                    nest(.by = test) %>%
                    mutate(data = map2(data, test, ~  {
                      t <- .y
                      .x %>% rename_with(~ paste0(t,.), everything())
                      })) %>% {map2(.x = .$data, .y = .$test, ~ .x)} %>% bind_cols()
        fdr <- cbind(id,pdata)
    } else {
        id <- pval[,1:3]
        pdata <- tests %>% mutate(across(everything(),function(x) p.adjust(x, method = m),
                                    .names = "{paste0({.col},'Adj_',{m})}"))
        fdr <- cbind(id,pdata)
    }
}

# Other auxiliary functions for dataframe manipulation

fully <- function(x,y){ifelse(x == 1 & y == 1, 1, 0)}
possibly <- function(x,y){ifelse((x == 1 & y == 0) | (x == 0 & y == 1), 1, 0)}
notc <- function(x,y){ifelse(x == 0 & y == 0, 1, 0)}
matsplitter <- function(m,p) {
    cv <- Map(function(u,v) m[,u:v], seq(1,ncol(m),p), seq(p,ncol(m),p))
    cv
}

##############################################################

data <- read.csv("./Data/Fulldata_minustest.csv")
tests <- read.csv("TestCoreResults.csv")

data <- data %>%
        mutate( country = replace(country, id == "DEU_2002a","DEU1"),
                country = replace(country, id == "DEU_2002b","DEU2"),
                country = replace(country, id == "GRC_2015a","GRC1"),
                country = replace(country, id == "GRC_2015b","GRC2")) %>%
        left_join(tests,by=c("country","year","Party")) %>%
        mutate(
            # First correct Germany and Greece country names
            country = replace(country, country == "DEU1" |
                                country == "DEU2", "DEU")
            ,
            country = replace(country, country == "GRC1" |
                                country == "GRC2", "GRC")
            ,
            # create a party identifier by pasting party and country names together
            partycountry = paste(country, Party)
            ,
            # Make transformation for flipped p-value
            TFCorepvalT3 = 1 - FCorepvalT3,
            TFCwinpvalT3 = 1 - FCwinpvalT3,
            # Create combined categorical variable
            Corecomb3 = (CoreT3 + FCoreT3)/2,
            ,
            Cwincomb3 = (CWinT3 + FCWinT3)/2,
            # Create combined p-value variable
            Corepvalcomb3 = (CorepvalT3 + TFCorepvalT3)/2,
            ,
            Cwinpvalcomb3 = (CwinpvalT3 + TFCwinpvalT3)/2,
            ) %>% group_by(id) %>% mutate(ntparty = sum(!is.na(CoreT3))) %>% ungroup()

remove_survey <- union(unique(data$id[data$pres == 1]),
                    c("JPN_1996","JPN_2004","JPN_2007","JPN_2013","LTU_1997","ROU_2009","ROU_2014",
                        "RUS_2000","RUS_2004","THA_2001","TWN_2004","TWN_2008","TWN_2012","TWN_2016",
                        "TUR_2015","TUR_2018","RUS_1999","UKR_1998","DEU_2002b","THA_2007","TWN_1996"))
remove_country <- c("BELF","BELW","BLR","CHE","KGZ","HKG","FRA")

data$missing <- ifelse(is.na(data$lhseat) | is.na(data$post_gov) | is.na(data$voterlr) | data$remove_s == 1, 1, 0)
data$regob <- ifelse(!data$id %in% remove_survey & !data$country %in% remove_country &
                    !is.na(data$Corecomb3) & data$missing == 0 & data$majsurvey == 0, 1, 0)

#### p-value adjustments, create fully, possibly, and not core/cwin measures ####


originalpval <- data %>% select(c("id","country","partycountry"),(contains("pval"))
                        & !contains("comb") & !contains("TF") & !contains("avg") & !contains("F"))
flippedpval <- data %>% select(c("id","country","partycountry"),(contains("pval"))
                            & !contains("comb") & !contains("TF") & !contains("avg") & contains("F"))

fdr_original_BY <- fdr(originalpval,0,"BY")
fdr_flipped_BY <- fdr(flippedpval,0,"BY")

fdr_original_H <- fdr(originalpval,0,"holm")
fdr_flipped_H <- fdr(flippedpval,0,"holm")

fdr_separate_BY <- left_join(fdr_original_BY,fdr_flipped_BY,by=c("id","country","partycountry"))

fdr_separate_H <- left_join(fdr_original_H,fdr_flipped_H,by=c("id","country","partycountry"))

adjustedpvals_BY <- fdr_separate_BY %>% select(c("id","country","partycountry"),contains("Adj")) %>%
                        rename_with(~ paste0(.,"_s"),ends_with("Adj_BY"))

adjustedpvals_H <- fdr_separate_H %>% select(c("id","country","partycountry"),contains("Adj")) %>%
                        rename_with(~ paste0(.,"_s"),ends_with("Adj_holm"))

adjustedpvals <- adjustedpvals_BY %>% left_join(adjustedpvals_H,by=c("id","country","partycountry"))

tfun <- function(x,y){(x+y)/2}

data <- data %>%
        mutate(
            across(matches("pval.*T3") & !starts_with(c("F","TF")),
                list(a01 = ~ ifelse(.x > 0.01,1,0),
                     a10 = ~ ifelse(.x > 0.1,1,0)),
                .names = "{case_when(
                            grepl('Corepval',{{.col}}) & !grepl('TF',{{.col}})
                             ~ paste0('Core',str_sub({{.col}},start=9),'_',{.fn}),
                            grepl('Cwinpval',{{.col}}) & !grepl('TF',{{.col}})
                             ~ paste0('CWin',str_sub({{.col}},start=9),'_',{.fn}))
                            }"),
            across(matches("pval.*T3") & starts_with("F"),
                list(a01 = ~ ifelse(.x <= 0.01,1,0),
                     a10 = ~ ifelse(.x <= 0.1,1,0)),
                .names = "{case_when(
                            grepl('FCorepval',{{.col}})
                             ~ paste0('FCore',str_sub({{.col}},start=10),'_',{.fn}),
                            grepl('FCwinpval',{{.col}})
                             ~ paste0('FCWin',str_sub({{.col}},start=10),'_',{.fn}))
                            }")
            ) %>%
            left_join(adjustedpvals,by=c("id","country","partycountry")) %>%
            mutate(
            across(matches("Adj") & !starts_with("F"),
                list(a05 = ~ ifelse(.x > 0.05,1,0),
                     a01 = ~ ifelse(.x > 0.01,1,0),
                     a10 = ~ ifelse(.x > 0.1,1,0)),
                .names = "{case_when(
                            grepl('Core.*W.*BY',{{.col}})
                             ~ paste0('Core',str_sub({{.col}},start=-11),paste0('_',{.fn})),
                            grepl('Cwin.*W.*BY',{{.col}})
                             ~ paste0('CWin',str_sub({{.col}},start=-11),paste0('_',{.fn})),
                            grepl('Core.*BY',{{.col}}) & !grepl('W|holm',{{.col}})
                             ~ paste0('Core',str_sub({{.col}},start=-10),paste0('_',{.fn})),
                            grepl('Cwin.*BY',{{.col}}) & !grepl('W|holm',{{.col}})
                             ~ paste0('CWin',str_sub({{.col}},start=-10),paste0('_',{.fn})),
                            grepl('Core.*W.*holm',{{.col}})
                             ~ paste0('Core',str_sub({{.col}},start=-13),paste0('_',{.fn})),
                            grepl('Cwin.*W.*holm',{{.col}})
                             ~ paste0('CWin',str_sub({{.col}},start=-13),paste0('_',{.fn})),
                            grepl('Core.*holm',{{.col}}) & !grepl('W|BY',{{.col}})
                             ~ paste0('Core',str_sub({{.col}},start=-12),paste0('_',{.fn})),
                            grepl('Cwin.*holm',{{.col}}) & !grepl('W|BY',{{.col}})
                             ~ paste0('CWin',str_sub({{.col}},start=-12),paste0('_',{.fn})))
                            }"),
            across(matches("Adj") & starts_with("F"),
                list(a05 = ~ ifelse(.x <= 0.05,1,0),
                     a01 = ~ ifelse(.x <= 0.01,1,0),
                     a10 = ~ ifelse(.x <= 0.1,1,0)),
                .names = "{case_when(
                            grepl('Core.*W.*BY',{{.col}})
                             ~ paste0('FCore',str_sub({{.col}},start=-11),paste0('_',{.fn})),
                            grepl('Cwin.*W.*BY',{{.col}})
                             ~ paste0('FCWin',str_sub({{.col}},start=-11),paste0('_',{.fn})),
                            grepl('Core.*BY',{{.col}}) & !grepl('W|holm',{{.col}})
                             ~ paste0('FCore',str_sub({{.col}},start=-10),paste0('_',{.fn})),
                            grepl('Cwin.*BY',{{.col}}) & !grepl('W|holm',{{.col}})
                             ~ paste0('FCWin',str_sub({{.col}},start=-10),paste0('_',{.fn})),
                            grepl('Core.*W.*holm',{{.col}})
                             ~ paste0('FCore',str_sub({{.col}},start=-13),paste0('_',{.fn})),
                            grepl('Cwin.*W.*holm',{{.col}})
                             ~ paste0('FCWin',str_sub({{.col}},start=-13),paste0('_',{.fn})),
                            grepl('Core.*holm',{{.col}}) & !grepl('W|BY',{{.col}})
                             ~ paste0('FCore',str_sub({{.col}},start=-12),paste0('_',{.fn})),
                            grepl('Cwin.*holm',{{.col}}) & !grepl('W|BY',{{.col}})
                             ~ paste0('FCWin',str_sub({{.col}},start=-12),paste0('_',{.fn})))
                            }"),
            across(matches("F.*T3.*Adj.*BY.*_s.*a05") & !matches("TF|WT|pval"),
                ~ tfun(.,get(str_sub(cur_column(),start=2,end=7))),
                .names="{paste0(str_sub({{.col}},start=2,end=5),'comb3_BY')}"),
            across(matches("F.*T3.*Adj.*holm.*_s.*a05") & !matches("TF|WT|pval"),
                ~ tfun(.,get(str_sub(cur_column(),start=2,end=7))),
                .names="{paste0(str_sub({{.col}},start=2,end=5),'comb3_holm')}")
            ) %>%
            rename(
            CoreT3_a05 = CoreT3,
            CWinT3_a05 = CWinT3,
            FCoreT3_a05 = FCoreT3,
            FCWinT3_a05 = FCWinT3,
            CoreWT3_a05 = CoreWT3,
            CWinWT3_a05 = CWinWT3,
            FCoreWT3_a05 = FCoreWT3,
            FCWinWT3_a05 = FCWinWT3) %>% select(-c(missing,remove_s,maj,pres))

# Save data
saveRDS(data,file="Finaldata.Rds")

##################################################################################
