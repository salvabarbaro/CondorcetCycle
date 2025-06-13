######## Table 10: Legislative preferences for core v. non-core parties ##########

orderleg <- c(1,5,6,2,7,8,3,9,10,4,11,12)

basesubsummary <- base %>% filter(regob == 1) %>%
                    mutate(
                        WmajseatC = ifelse(corer_seat > 50, 1, 0),
                        PmajseatC = ifelse(corep_seat > 50, 1, 0),
                        WmajseatNC = ifelse(ncorer_seat > 50, 1, 0),
                        PmajseatNC = ifelse(ncorep_seat > 50, 1, 0)) %>%
                    select(corer_seat,WmajseatC,corep_seat,PmajseatC,ncorer_seat,WmajseatNC,ncorep_seat,PmajseatNC) %>%
                    summarise(
                        across(contains("core"), ~mean(.x,na.rm=TRUE)),
                        across(contains("maj"), list(~sum(.x,na.rm=TRUE),~mean(.x,na.rm=TRUE))),
                        n=n())%>%
                mutate(across(contains(c("_2","Prop")), ~label_percent(accuracy=0.1)(.x)))

nbaseleg <- basesubsummary$n

basesubsummary <- basesubsummary[,orderleg]

basesubsummary <- data.frame(rbindlist(matsplitter(basesubsummary,6),use.names = FALSE)) %>% mutate(across(where(is.numeric), ~round(.x,digits=1)))


basesubsummary <- rbind(c("Average seat share sum (%)","Seat share majority (#)",
                              "Seat share majority (%)","Average seat share sum (%)",
                              "Seat share majority (#)","Seat share majority (%)"),basesubsummary)

colnames(basesubsummary) <- c("Party base","weakly","prefers","Party base","strictly","prefers")
rownames(basesubsummary) <- c("","parties that prefer C over NC","parties that prefer NC over C")
