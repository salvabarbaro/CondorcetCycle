####################### Table D.5: Country Aggregates ###########################

T3ncountry <- data %>%
    select(id, country, a05_c1_c1, a05_c1_c2, a05_c1_c3, a05_c2_c3, ntparty, nelec) %>%
    group_by(id, country) %>%
    summarise(
        across(contains("a05"),
            ~sum(.x,na.rm=TRUE)),
        across(!contains("a05"),
            ~mean(.x,na.rm=TRUE))) %>%
    group_by(country) %>%
    summarise(
        across(!contains(c("id","nelec")),
            ~mean_sd(.x, denote_sd = "paren", na_rm = TRUE,
                        show_n = "never")),
        nelec = mean(nelec,na.rm=TRUE),.groups = 'drop')

colnames(T3ncountry) <-
  c(
    "Country",
    "Avg. # of CW",
    "Avg. # of possible CW",
    "Avg. # of Core & not CW",
    "Avg. # of possible Core",
    "Avg. # of parties",
    "# of elections"
  )

even2 <- seq_len(nrow(T3ncountry)) %% 2   # index
T3country1 <- data.frame(T3ncountry[!!even2,])
T3country2 <- data.frame(T3ncountry[!even2,])

colnames(T3country1) <- colnames(T3country2) <- colnames(T3ncountry)
