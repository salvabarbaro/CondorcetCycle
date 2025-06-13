##################### Table 9: Core party base preferences #######################

order <- c(9,2,1,11,6,5,10,4,3,12,8,7)

corebasesub <- corebase %>% filter(regob == 1) %>%
                summarise(
                    across(contains(c("maj")) & !contains("survey"),
                        list(~mean(.,na.rm = TRUE),~sum(.,na.rm = TRUE))),
                    across(where(is.numeric) & !contains(c("year","regob","majsurvey","missing","maj")),
                        ~mean(.,na.rm = TRUE)),
                    n=n()) %>%
                mutate(across(contains(c("_1","Prop")), ~label_percent(accuracy=0.1)(.x)))

ncorebase <- corebasesub$n

corebasesub <- corebasesub[,order]

corebasesub <- data.frame(rbindlist(matsplitter(corebasesub,6),use.names=FALSE))

colnames(corebasesub) <- c("Party base","weakly","prefers","Party base","strictly","prefers")
corebasesub <- rbind(c("Average Base share","Majority support (#)","Majority support (%)",
                         "Average Base share","Majority support (#)","Majority support (%)"),corebasesub)
rownames(corebasesub) <- c("","C over NC","NC over C")
