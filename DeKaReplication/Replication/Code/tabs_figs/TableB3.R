############# Table B.3: Survey level tests of existence of core/CW #############

surveys_all <- surveys %>%
                mutate(
                    across(starts_with("Core"),
                        list(
                            aa =        ~ fully(.,get(paste0("F",cur_column()))),
                            ab =        ~ possibly(.,get(paste0("F",cur_column()))),
                            ac =        ~ notc(.,get(paste0("F",cur_column()))),
                            aa_BY =     ~ fully(.,get(paste0("F",cur_column(),"_BY"))),
                            ab_BY =     ~ possibly(.,get(paste0("F",cur_column(),"_BY"))),
                            ac_BY =     ~ notc(.,get(paste0("F",cur_column(),"_BY"))),
                            aa_holm =   ~ fully(.,get(paste0("F",cur_column(),"_holm"))),
                            ab_holm =   ~ possibly(.,get(paste0("F",cur_column(),"_holm"))),
                            ac_holm =   ~ notc(.,get(paste0("F",cur_column(),"_holm")))
                        ),
                        .names = "{paste0('Core','_',{.fn},'_',sub('.*_','',{{.col}}))}"),
                    across(starts_with("Cwin"),
                        list(
                            aa =        ~ fully(.,get(paste0("F",cur_column()))),
                            ab =        ~ possibly(.,get(paste0("F",cur_column()))),
                            ac =        ~ notc(.,get(paste0("F",cur_column()))),
                            aa_BY =     ~ fully(.,get(paste0("F",cur_column(),"_BY"))),
                            ab_BY =     ~ possibly(.,get(paste0("F",cur_column(),"_BY"))),
                            ac_BY =     ~ notc(.,get(paste0("F",cur_column(),"_BY"))),
                            aa_holm =   ~ fully(.,get(paste0("F",cur_column(),"_holm"))),
                            ab_holm =   ~ possibly(.,get(paste0("F",cur_column(),"_holm"))),
                            ac_holm =   ~ notc(.,get(paste0("F",cur_column(),"_holm")))
                        ),
                        .names = "{paste0('Cwin','_',{.fn},'_',sub('.*_','',{{.col}}))}")
                    ) %>%
                select(contains(c("aa","ab","ac"))) %>%
                summarise(
                    across(everything(),
                    list(sum = ~sum(.x,na.rm=TRUE), percent = ~label_percent(accuracy=0.1)(sum(.x,na.rm=TRUE)/196)))) %>%
                select(str_sort(names(.)))

count <- matrix(surveys_all[seq(length(surveys_all)) %%2 == 0])
percent <- matrix(surveys_all[seq(length(surveys_all)) %%2 == 1])
surveytable <- data.frame(do.call(rbind,matsplitter(t(cbind(count,percent)),9)))

colnames(surveytable) <-

surveytable <- rbind(rep(c("0.01","0.05","0.1"),times = 3),surveytable)

colnames(surveytable) <- c("","Unadjusted","","","FDR control","","","FWER control","")
rownames(surveytable) <- c("$\\alpha$","Non-empty core","","Core may be empty"," ","Empty core","  ","CW exists","   ","CW may exist","     ","CW does not exist","      ")
