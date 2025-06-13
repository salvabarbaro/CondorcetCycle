################ Table C.4: Party level weighted test results ###################

weightedsummary <- data %>% mutate(
                    across(starts_with("FCoreWT3") & !contains("_a_") & contains(c("a01","a05","a10")),
                    list(c1 = ~ fully(.,get(paste0("CoreWT3",str_sub(cur_column(),start = -4)))),
                         c2 = ~ possibly(.,get(paste0("CoreWT3",str_sub(cur_column(),start = -4)))),
                         c3 = ~ notc(.,get(paste0("CoreWT3",str_sub(cur_column(),start = -4))))
                        ),
                    .names = "{case_when(
                                !grepl('Adj', {{.col}})
                                 ~ paste0('CoreW', sub('.*T3','',{{.col}}), '_', {.fn}),
                                grepl('Adj', {{.col}})
                                 ~ paste0('CoreW', sub('.*Adj','',{{.col}}), '_', {.fn})
                                )}"),
                    across(starts_with("FCWinWT3") & !contains("_a_") & contains(c("a01","a05","a10")),
                    list(c1 = ~ fully(.,get(paste0("CWinWT3",str_sub(cur_column(),start = -4)))),
                         c2 = ~ possibly(.,get(paste0("CWinWT3",str_sub(cur_column(),start = -4)))),
                         c3 = ~ notc(.,get(paste0("CWinWT3",str_sub(cur_column(),start = -4))))
                        ),
                    .names = "{case_when(
                                !grepl('Adj', {{.col}})
                                 ~ paste0('CWinW', sub('.*T3','',{{.col}}), '_', {.fn}),
                                grepl('Adj', {{.col}})
                                 ~ paste0('CWinW', sub('.*Adj','',{{.col}}), '_', {.fn})
                                )}"),
                    across(starts_with("CoreW") & !contains(c("holm","BY")) & matches("c1|c2|c3"),
                    list(
                        c1 = ~ .x * get(paste0("CWinW_", str_match(cur_column(), "CoreW_\\s*(.*?)\\s*_c")[,2],"_c1")),
                        c2 = ~ .x * get(paste0("CWinW_", str_match(cur_column(), "CoreW_\\s*(.*?)\\s*_c")[,2],"_c2")),
                        c3 = ~ .x * get(paste0("CWinW_", str_match(cur_column(), "CoreW_\\s*(.*?)\\s*_c")[,2],"_c3"))
                        ),
                    .names = "{paste0(sub('.*Core','',{{.col}}),'_',{.fn})}"
                        ),
                        across(matches("CoreW_BY") & matches("c1|c2|c3"),
                    list(
                        c1 = ~ .x * get(paste0("CWinW_BY_s_", str_match(cur_column(), "CoreW_BY_s_\\s*(.*?)\\s*_c")[,2],"_c1")),
                        c2 = ~ .x * get(paste0("CWinW_BY_s_", str_match(cur_column(), "CoreW_BY_s_\\s*(.*?)\\s*_c")[,2],"_c2")),
                        c3 = ~ .x * get(paste0("CWinW_BY_s_", str_match(cur_column(), "CoreW_BY_s_\\s*(.*?)\\s*_c")[,2],"_c3"))
                        ),
                    .names = "{paste0(sub('.*Core','',{{.col}}),'_',{.fn})}"
                        ),
                        across(matches("CoreW_holm") & matches("c1|c2|c3"),
                    list(
                        c1 = ~ .x * get(paste0("CWinW_holm_s_", str_match(cur_column(), "CoreW_holm_s_\\s*(.*?)\\s*_c")[,2],"_c1")),
                        c2 = ~ .x * get(paste0("CWinW_holm_s_", str_match(cur_column(), "CoreW_holm_s_\\s*(.*?)\\s*_c")[,2],"_c2")),
                        c3 = ~ .x * get(paste0("CWinW_holm_s_", str_match(cur_column(), "CoreW_holm_s_\\s*(.*?)\\s*_c")[,2],"_c3"))
                        ),
                    .names = "{paste0(sub('.*Core','',{{.col}}),'_',{.fn})}"))

summarydata <- weightedsummary %>% summarise(across(!contains(c("Core","CWin")) & matches("c1|c2|c3") & contains("W"),
                            list(
                                sum = ~ sum(.x, na.rm = TRUE),
                                percent = ~ label_percent(accuracy=0.1)(sum(.x, na.rm = TRUE)/1176)))) %>%
                        select(order(names(.)))

unadjtable <- summarydata %>% select(!matches("BY|holm"))
count <- matrix(unadjtable[seq(length(unadjtable)) %%2 == 0])
percent <- matrix(unadjtable[seq(length(unadjtable)) %%2 == 1])
unadjtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

BYtable <- summarydata %>% select(matches("BY"))
count <- matrix(BYtable[seq(length(BYtable)) %%2 == 0])
percent <- matrix(BYtable[seq(length(BYtable)) %%2 == 1])
BYtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

holmtable <- summarydata %>% select(matches("holm"))
count <- matrix(holmtable[seq(length(holmtable)) %%2 == 0])
percent <- matrix(holmtable[seq(length(holmtable)) %%2 == 1])
holmtable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

fulltable <- data.frame(cbind(unadjtable,BYtable,holmtable))

fulltable <- cbind(c("Core","","Possible core","","Not core","","Core ","",
                        "Possible core","","Not core","","Core  ","",
                        "Possible core","","Not core",""),fulltable)

fulltable <- rbind(c("",rep(c("CW","Possible CW","Not CW"),times = 3)),fulltable)

colnames(fulltable) <- c("","","Unadjusted","","","FDR adjusted","","","FWER adjusted","")
rownames(fulltable) <- c(""," ","  ","$\\alpha$","=0.01","   ","    ","     ","      ","$\\alpha$ ","=0.05","       ","        ","         ","          ","$\\alpha$  ","=0.1","           ","            ")
