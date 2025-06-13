############# Table 8: Government coalition party base preferences ##############

order <- c(9,2,1,11,6,5,10,4,3,12,8,7)

govbasesub <- govbase %>% filter(regob == 1) %>%
                summarise(
                    across(contains(c("maj")) & !contains("survey"),
                        list(~mean(.,na.rm = TRUE),~sum(.,na.rm = TRUE))),
                    across(where(is.numeric) &
                        !contains(c("year","regob","majsurvey","missing","maj")),
                        ~mean(.,na.rm = TRUE)),
                    n=n()) %>%
                mutate(across(contains(c("_1","Prop")), ~label_percent(accuracy=0.1)(.x)))

ngovbase <- govbasesub$n

govbasesub <- govbasesub[,order]
govbasesub <- data.frame(rbindlist(matsplitter(govbasesub,6),use.names=FALSE))

colnames(govbasesub) <- c("Party base","weakly","prefers","Party base","strictly","prefers")
govbasesub <- rbind(c("Average Base share","Majority support (#)","Majority support (%)",
                        "Average Base share","Majority support (#)","Majority support (%)"), govbasesub)
rownames(govbasesub) <- c("","G over NG","NG over G")
