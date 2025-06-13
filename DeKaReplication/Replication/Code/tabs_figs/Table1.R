##################### TABLE 1: Party level test results #######################

data <- data %>%
            mutate(
                across(starts_with("FCoreT3") & !contains("_a_") & contains(c("a01","a05","a10")),
                list(c1 = ~ fully(.,get(paste0("CoreT3",str_sub(cur_column(),start = -4)))),
                     c2 = ~ possibly(.,get(paste0("CoreT3",str_sub(cur_column(),start = -4)))),
                     c3 = ~ notc(.,get(paste0("CoreT3",str_sub(cur_column(),start = -4))))
                    ),
                .names = "{case_when(
                            !grepl('Adj', {{.col}})
                             ~ paste0('Core', sub('.*T3','',{{.col}}), '_', {.fn}),
                            grepl('Adj', {{.col}})
                             ~ paste0('Core', sub('.*Adj','',{{.col}}), '_', {.fn})
                            )}"),
                across(starts_with("FCWinT3") & !contains("_a_") & contains(c("a01","a05","a10")),
                list(c1 = ~ fully(.,get(paste0("CWinT3",str_sub(cur_column(),start = -4)))),
                     c2 = ~ possibly(.,get(paste0("CWinT3",str_sub(cur_column(),start = -4)))),
                     c3 = ~ notc(.,get(paste0("CWinT3",str_sub(cur_column(),start = -4))))
                    ),
                .names = "{case_when(
                            !grepl('Adj', {{.col}})
                             ~ paste0('CWin', sub('.*T3','',{{.col}}), '_', {.fn}),
                            grepl('Adj', {{.col}})
                             ~ paste0('CWin', sub('.*Adj','',{{.col}}), '_', {.fn})
                            )}")
                ,
                across(starts_with("Core") & !contains(c("holm","BY")) & matches("c1|c2|c3"),
                list(
                    c1 = ~ .x * get(paste0("CWin_", str_match(cur_column(),
                            "Core_\\s*(.*?)\\s*_c")[,2],"_c1")),
                    c2 = ~ .x * get(paste0("CWin_", str_match(cur_column(),
                            "Core_\\s*(.*?)\\s*_c")[,2],"_c2")),
                    c3 = ~ .x * get(paste0("CWin_", str_match(cur_column(),
                            "Core_\\s*(.*?)\\s*_c")[,2],"_c3"))
                    ),
                .names = "{paste0(sub('.*Core_','',{{.col}}),'_',{.fn})}"
                    ),
                across(matches("Core_BY") & matches("c1|c2|c3"),
                list(
                    c1 = ~ .x * get(paste0("CWin_BY_s_", str_match(cur_column(),
                            "Core_BY_s_\\s*(.*?)\\s*_c")[,2],"_c1")),
                    c2 = ~ .x * get(paste0("CWin_BY_s_", str_match(cur_column(),
                            "Core_BY_s_\\s*(.*?)\\s*_c")[,2],"_c2")),
                    c3 = ~ .x * get(paste0("CWin_BY_s_", str_match(cur_column(),
                            "Core_BY_s_\\s*(.*?)\\s*_c")[,2],"_c3"))
                    ),
                .names = "{paste0(sub('.*Core_','',{{.col}}),'_',{.fn})}"
                    ),
                across(matches("Core_holm") & matches("c1|c2|c3"),
                list(
                    c1 = ~ .x * get(paste0("CWin_holm_s_", str_match(cur_column(),
                            "Core_holm_s_\\s*(.*?)\\s*_c")[,2],"_c1")),
                    c2 = ~ .x * get(paste0("CWin_holm_s_", str_match(cur_column(),
                            "Core_holm_s_\\s*(.*?)\\s*_c")[,2],"_c2")),
                    c3 = ~ .x * get(paste0("CWin_holm_s_", str_match(cur_column(),
                            "Core_holm_s_\\s*(.*?)\\s*_c")[,2],"_c3"))
                    ),
                .names = "{paste0(sub('.*Core_','',{{.col}}),'_',{.fn})}")
                )

summarydata <- data %>%
                    summarise(across(!contains(c("Core","CWin")) &
                            matches("c1|c2|c3") & contains("05"),
                            list(
                                sum = ~ sum(.x, na.rm = TRUE),
                                percent = ~ sum(.x, na.rm = TRUE)/1176))) %>%
                        select(order(names(.)))

unadjtable <- summarydata %>% select(!matches("BY|holm"))
count <- matrix(unadjtable[seq(length(unadjtable)) %%2 == 0])
percent <- matrix(unadjtable[seq(length(unadjtable)) %%2 == 1])
percent <- label_percent(accuracy = 0.1)(as.numeric(percent))

unadjtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

BYtable <- summarydata %>% select(matches("BY"))
count <- matrix(BYtable[seq(length(BYtable)) %%2 == 0])
percent <- matrix(BYtable[seq(length(BYtable)) %%2 == 1])
percent <- label_percent(accuracy = 0.1)(as.numeric(percent))
BYtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

holmtable <- summarydata %>% select(matches("holm"))
count <- matrix(holmtable[seq(length(holmtable)) %%2 == 0])
percent <- matrix(holmtable[seq(length(holmtable)) %%2 == 1])
percent <- label_percent(accuracy = 0.1)(as.numeric(percent))
holmtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

fulltable <- data.frame(cbind(unadjtable,BYtable,holmtable))

fulltable <- rbind(c("CW","Possible CW","Not CW","CW","Possible CW",
                        "Not CW","CW","Possible CW","Not CW"),fulltable)

colnames(fulltable) <- c("","Unadjusted","","","FDR control","","","FWER control","")

rownames(fulltable) <- c("","Core","    ","Possible core"," ","Not core","  ")
