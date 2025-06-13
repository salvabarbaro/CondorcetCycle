############# TABLE 2: Survey-level tests of existence of core/CW #############

surveys_nonadjust <- data %>% select(id,CorepvalT3,CwinpvalT3) %>% group_by(id) %>%
                        summarise(
                            CorepvalT3 = max(CorepvalT3, na.rm=TRUE),
                            CwinpvalT3 = max(CwinpvalT3, na.rm=TRUE)) %>% select(-id)

surveys_adjust <- data %>%
                    select(id,ntparty,starts_with(c("FCorepvalT3","FCwinpvalT3"))
                            & !contains(c("Adj"))) %>%
                    group_by(id) %>%
                    summarise(
                        FCorepvalT3 = min(min(FCorepvalT3, na.rm=TRUE) *
                            max(ntparty, na.rm=TRUE),1),
                        FCwinpvalT3 = min(min(FCwinpvalT3, na.rm=TRUE) *
                            max(ntparty, na.rm=TRUE),1)) %>%
                    ungroup() %>%
                    mutate( FCorepvalT3_BY = p.adjust(FCorepvalT3, method = "BY"),
                            FCwinpvalT3_BY = p.adjust(FCwinpvalT3, method = "BY"),
                            FCorepvalT3_holm = p.adjust(FCorepvalT3, method = "holm"),
                            FCwinpvalT3_holm = p.adjust(FCwinpvalT3, method = "holm")) %>%
                    select(!contains("id"))

surveys <- cbind(surveys_nonadjust,surveys_adjust) %>%
                mutate(
                    across(starts_with("Corepval"),
                    list(
                        a01 = ~ifelse(.x > .01, 1, 0),
                        a05 = ~ifelse(.x > .05, 1, 0),
                        a10 = ~ifelse(.x > .1, 1, 0)),
                    .names = "Core_{.fn}"),
                    across(starts_with("Cwinpval"),
                    list(
                        a01 = ~ifelse(.x > .01, 1, 0),
                        a05 = ~ifelse(.x > .05, 1, 0),
                        a10 = ~ifelse(.x > .1, 1, 0)),
                    .names = "Cwin_{.fn}"),
                    across(starts_with("FCorepval"),
                    list(
                        a01 = ~ifelse(.x <= .01, 1, 0),
                        a05 = ~ifelse(.x <= .05, 1, 0),
                        a10 = ~ifelse(.x <= .1, 1, 0)),
                    .names = "{paste0('FCore','_',{.fn},sub('.*T3','',{{.col}}))}"),
                    across(starts_with("FCwinpval"),
                    list(
                        a01 = ~ifelse(.x <= .01, 1, 0),
                        a05 = ~ifelse(.x <= .05, 1, 0),
                        a10 = ~ifelse(.x <= .1, 1, 0)),
                    .names = "{paste0('FCwin','_',{.fn},sub('.*T3','',{{.col}}))}")) %>%
                select(!contains("pval"))

surveys_05 <- surveys %>% select(contains("05")) %>%
                mutate(
                    across(starts_with("Core"),
                        list(a1 = ~ fully(.,get(paste0("F",cur_column()))),
                            a2 = ~ possibly(.,get(paste0("F",cur_column()))),
                            a3 = ~ notc(.,get(paste0("F",cur_column()))),
                            BY_a1 = ~ fully(.,get(paste0("F",cur_column(),"_BY"))),
                            BY_a2 = ~ possibly(.,get(paste0("F",cur_column(),"_BY"))),
                            BY_a3 = ~ notc(.,get(paste0("F",cur_column(),"_BY"))),
                            holm_a1 = ~ fully(.,get(paste0("F",cur_column(),"_holm"))),
                            holm_a2 = ~ possibly(.,get(paste0("F",cur_column(),"_holm"))),
                            holm_a3 = ~ notc(.,get(paste0("F",cur_column(),"_holm")))
                        ),
                        .names = "Core_{.fn}"),
                    across(starts_with("Cwin"),
                        list(a1 = ~ fully(.,get(paste0("F",cur_column()))),
                            a2 = ~ possibly(.,get(paste0("F",cur_column()))),
                            a3 = ~ notc(.,get(paste0("F",cur_column()))),
                            BY_a1 = ~ fully(.,get(paste0("F",cur_column(),"_BY"))),
                            BY_a2 = ~ possibly(.,get(paste0("F",cur_column(),"_BY"))),
                            BY_a3 = ~ notc(.,get(paste0("F",cur_column(),"_BY"))),
                            holm_a1 = ~ fully(.,get(paste0("F",cur_column(),"_holm"))),
                            holm_a2 = ~ possibly(.,get(paste0("F",cur_column(),"_holm"))),
                            holm_a3 = ~ notc(.,get(paste0("F",cur_column(),"_holm")))
                        ),
                        .names = "Cwin_{.fn}")
                    ) %>%
                select(!contains("05")) %>%
                summarise(
                    across(everything(),
                    list(
                        sum = ~sum(.x,na.rm=TRUE),
                        percent = ~label_percent(accuracy=0.1)(sum(.x,na.rm=TRUE)/196))))

count <- matrix(surveys_05[seq(length(surveys_05)) %%2 == 1])
percent <- matrix(surveys_05[seq(length(surveys_05)) %%2 == 0])
surveytable <- do.call(rbind,matsplitter(t(cbind(count,percent)),3))

surveytable <- data.frame(cbind(surveytable[1:6,],surveytable[7:12,]))

rownames(surveytable) <- c("Unadjusted","","FDR control"," ","FWER control","  ")
colnames(surveytable) <- c("Core is not empty","Core may be empty","Empty core","CW exists","CW may exist","CW does not exist")

