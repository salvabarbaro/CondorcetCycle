###################### TABLE 3: Reported Indifferences  #######################

indiffsum <- indiff %>% summarise(
                across(contains(c("totalpairs","indiffcount")) & !contains(c("pcore","ncore")),
                list(
                    sum = ~sum(.x,na.rm=TRUE),
                    percent = ~label_percent(accuracy=0.1)(sum(.x,na.rm = TRUE) /
                                                            sum(totalpairs,na.rm=TRUE)),
                    percenti = ~label_percent(accuracy=0.1)(sum(.x,na.rm = TRUE) /
                                                            sum(indiffcount,na.rm=TRUE))
                    )))

total <- matrix(indiffsum[seq(length(indiffsum)) %%3 == 1])
totalshare <- matrix(indiffsum[seq(length(indiffsum)) %%3 == 2])
indiffshare <- matrix(indiffsum[seq(length(indiffsum)) %%3 == 0])

indiffsumtable <- data.frame(t(cbind(total,totalshare,indiffshare)))
indiffsumtable[3,1] <- NA


colnames(indiffsumtable) <- c("Total pairwise comparisons","any pair","core parties",
                              "core party and complement core party","compelement of core parties")

