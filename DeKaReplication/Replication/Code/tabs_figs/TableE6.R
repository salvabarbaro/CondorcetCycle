################ Table E.6: Fraction of seats earned by parties by survey ###########################

coveragenum <- regdatas %>% group_by(id) %>%
                    mutate(
                        regseatshare = sum(lhseat, na.rm=TRUE),
                        g99 = ifelse(regseatshare>=99,1,0),
                        l99g95 = ifelse(regseatshare>=95 & regseatshare < 99,1,0),
                        l95g90 = ifelse(regseatshare>=90 & regseatshare <95,1,0),
                        l90 = ifelse(regseatshare<90,1,0)) %>% select(g99,l99g95,l95g90,l90,id) %>%
                    summarise(across(everything(),~mean(.x,na.rm=TRUE))) %>% ungroup() %>%
                    summarise(across(where(is.numeric),list(~sum(.x,na.rm=TRUE))))

coveragenum <- as.matrix(coveragenum)
colnames(coveragenum) <- c(">= 99%","(99%,95%]","(95%,90%]","<90%")
rownames(coveragenum) <- c("Surveys in Tables 5,6, and 7")
