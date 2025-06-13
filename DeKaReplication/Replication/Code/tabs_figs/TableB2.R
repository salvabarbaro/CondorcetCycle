################## Table B.2: Party level test results ##########################

summarydata <- data %>% summarise(across(!contains(c("Core","CWin")) & matches("c1|c2|c3"),
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
