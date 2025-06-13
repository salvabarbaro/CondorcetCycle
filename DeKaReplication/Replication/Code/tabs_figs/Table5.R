######################## TABLE 5: Summary statistics ###########################

regdatav <- data %>% select(
                    id,partycountry,
                    post_gov,pmalt,
                    lhseat,plu,
                    wmedlrcvr,difmedlrcvr,
                    pre_gov,prepmalt,
                    starts_with(c("a05","a01")) &
                    !contains(c("Core","CWin","BY","holm")),
                    starts_with(c("Core_a05","CWin_a05")),
                    starts_with(c("Core_a01 ","CWin_a01 ")),
                    regob) %>%
                    filter(regob == 1) %>%
                    select(-c(regob))

partyobs <- regdatav %>% select(!starts_with("a01")) %>%
                summarise(
                    across(!starts_with(c("id","partycountry","a05","Core","CWin")) &
                        !ends_with(c("pvalT3")),
                    list(
                         condorcet = ~sum(.x * get("a05_c1_c1"),na.rm=TRUE)/
                                    sum(get("a05_c1_c1"), na.rm=TRUE),
                         pcondorcet = ~sum(.x * get("a05_c1_c2"),na.rm=TRUE)/
                                    sum(get("a05_c1_c2"), na.rm=TRUE),
                         corencon = ~sum(.x * get("a05_c1_c3"),na.rm=TRUE)/
                                    sum(get("a05_c1_c3"), na.rm=TRUE),
                         pcore = ~sum(.x * get("a05_c2_c3"),na.rm=TRUE)/
                                    sum(get("a05_c2_c3"), na.rm=TRUE),
                         ncore = ~sum(.x * get("a05_c3_c3"),na.rm=TRUE)/
                                    sum(get("a05_c3_c3"), na.rm=TRUE),
                         all = ~mean(.x,na.rm=TRUE)
                         ),
                    .names = "{.col}_{.fn}"),
                    across(starts_with("a05") & !contains(c("c2_c1","c2_c2","c3_c1","c3_c2")),
                        list(sum,mean)),
                    allparties = n(),
                    allparties_share = 1
                    ) %>%
                mutate(
                    across(contains(c("post_gov","pmalt","plu","wmedlrcvr","pre_gov",
                        "prepmalt","_2","_share")), ~ label_percent(accuracy = 0.1)(.x))) %>%
                mutate(across(where(is.numeric), ~ round(.x,2)))

splitparty <- matsplitter(partyobs,6)
partyobsnew <- data.frame(rbindlist(splitparty,use.names=FALSE))
rows <- nrow(partyobsnew)

baselinegov <- mean(regdatav$post_gov[regdatav$Core_a05_c3 == 1])
catgov <- regdatav %>% select(starts_with("a05") | contains("post_gov")) %>%
            summarise(across(post_gov,list(
                         condorcet = ~sum(.x * get("a05_c1_c1"),na.rm=TRUE)/
                                    sum(get("a05_c1_c1"), na.rm=TRUE),
                         pcondorcet = ~sum(.x * get("a05_c1_c2"),na.rm=TRUE)/
                                    sum(get("a05_c1_c2"), na.rm=TRUE),
                         corencon = ~sum(.x * get("a05_c1_c3"),na.rm=TRUE)/
                                    sum(get("a05_c1_c3"), na.rm=TRUE),
                         pcore = ~sum(.x * get("a05_c2_c3"),na.rm=TRUE)/
                                    sum(get("a05_c2_c3"), na.rm=TRUE),
                         ncore = ~sum(.x * get("a05_c3_c3"),na.rm=TRUE)/
                                    sum(get("a05_c3_c3"), na.rm=TRUE),
                         all = ~mean(.x,na.rm=TRUE)
                         )))
catgov[length(catgov)] <- NA
reloddsgov <- as.matrix(round(catgov/baselinegov,1))
colnames(reloddsgov) <- NULL

baselinepm <- mean(regdatav$pmalt[regdatav$Core_a05_c3 == 1])
catpm <- regdatav %>% select(starts_with("a05") | contains("pmalt")) %>%
            summarise(across(pmalt,list(
                         condorcet = ~sum(.x * get("a05_c1_c1"),na.rm=TRUE)/
                                    sum(get("a05_c1_c1"), na.rm=TRUE),
                         pcondorcet = ~sum(.x * get("a05_c1_c2"),na.rm=TRUE)/
                                    sum(get("a05_c1_c2"), na.rm=TRUE),
                         corencon = ~sum(.x * get("a05_c1_c3"),na.rm=TRUE)/
                                    sum(get("a05_c1_c3"), na.rm=TRUE),
                         pcore = ~sum(.x * get("a05_c2_c3"),na.rm=TRUE)/
                                    sum(get("a05_c2_c3"), na.rm=TRUE),
                         ncore = ~sum(.x * get("a05_c3_c3"),na.rm=TRUE)/
                                    sum(get("a05_c3_c3"), na.rm=TRUE),
                         all = ~mean(.x,na.rm=TRUE)
                         )))
catpm[length(catpm)] <- NA
reloddspm <- as.matrix(round(catpm/baselinepm,1))
colnames(reloddspm) <- NULL

tochange <- matrix(t(partyobsnew[(rows-1):rows,]))
percent <- t(matrix(tochange[seq(length(tochange)) %%2 == 0]))
count <- as.numeric(t(matrix(tochange[seq(length(tochange)) %%2 == 1])))

partyobsnew[(rows-1):rows,] <- rbind(count,percent)
partyobsnew[(rows+1):(rows+2),] <- rbind(reloddsgov,reloddspm)

order <- c(rows-1,rows,1,(rows+1),2,(rows+2),3:(rows-2))
partyobsnew <- partyobsnew[order,]

rownames(partyobsnew) <- c("#","%","% In Gov","Relative odds (gov)","% PM","Relative odds (PM)","LH seat share (%)",
                        "% Seat Plurality Party","% Legislative Median","Distance to Median",
                        "% Incumbent","% Incumbent PM")
colnames(partyobsnew) <- c("CW","Possibly CW","Core & not CW","Possible Core",
                            "Not Core","All Parties")
