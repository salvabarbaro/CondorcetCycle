########### TABLE 4: Reported Indifferences and profile of responses ###########

indiffcat <- indiff %>%
                    summarise(across(c(indiffall,indiffallbutone,indiffsome,indiffnone),
                    list(
                        shareres = ~ label_percent(accuracy=0.1)(sum(.x, na.rm = TRUE)/
                                                                sum(!is.na(totalpairs)*1)),
                        sharecomp = ~ label_percent(accuracy=0.1)(sum(.x * totalpairs, na.rm = TRUE) /
                                                                sum(totalpairs, na.rm = TRUE)),
                        shareindiff = ~ label_percent(accuracy=0.1)(sum(.x * indiffcount, na.rm = TRUE) /
                                                                sum(indiffcount, na.rm = TRUE)))))
indiffcat <- matsplitter(indiffcat,3)
names(indiffcat[[1]]) <- names(indiffcat[[2]]) <- names(indiffcat[[3]]) <- names(indiffcat[[4]]) <- c(1,2,3)
indiffcattable <- data.frame(t(rbindlist(indiffcat)))

colnames(indiffcattable) <- c("All","All but top","Some","None")
rownames(indiffcattable) <- c("Share of respondents","Share of comparisons",
                            "Share of indifferent comparisons")
