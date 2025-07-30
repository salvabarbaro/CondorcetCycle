### Netherlands 2017
nl2017.parties <- partyname.df %>% filter(., case_ID == "Netherlands_2017")
### Parties: D (CW), A, E are the parties with the highest number of pairwise wins.
 ## D: D66, A: VVD (market-orientated, conservative), E: GreenLeft (merger of communists, radicals...)
## 1. Restrict d on Netherlands_2017
nl2017 <- d %>% filter(., case_ID == "Netherlands_2017") %>%
  select(c("party_rating_A", "party_rating_D", "party_rating_E"))
ranked.nl17 <- as.data.frame(t(apply(nl2017, 1, 
                                     function(x) frank(-x, 
                                                       ties.method = "max", 
                                                       na.last = TRUE)))) %>%
  setNames(LETTERS[1:3])
## Note: B = D (D66), C = E (GroenLeft)

## restrict to asymmetric relations only:
nl17.asymm <- ranked.nl17 %>% 
  mutate(sums = rowSums(.)) %>%
  filter(., sums == 6)  

# Define a function to categorize each row into an ordering
classify_ordering <- function(row) {
  if (row['A'] < row['B'] & row['B'] < row['C']) {
    return("A > B > C")
  } else if (row['A'] < row['C'] & row['C'] < row['B']) {
    return("A > C > B")
  } else if (row['B'] < row['A'] & row['A'] < row['C']) {
    return("B > A > C")
  } else if (row['B'] < row['C'] & row['C'] < row['A']) {
    return("B > C > A")
  } else if (row['C'] < row['A'] & row['A'] < row['B']) {
    return("C > A > B")
  } else if (row['C'] < row['B'] & row['B'] < row['A']) {
    return("C > B > A")
  } else if (row['A'] == row['B'] & row['A'] < row['C']) {
    return("A ~ B > C")
  } else if (row['A'] == row['C'] & row['A'] < row['B']) {
    return("A ~ C > B")
  } else if (row['B'] == row['C'] & row['B'] < row['A']) {
    return("B ~ C > A")
  } else if (row['A'] == row['B'] & row['B'] > row['C']) {
    return("C > A ~ B")
  } else if (row['A'] == row['C'] & row['C'] > row['B']) {
    return("B > A ~ C")
  } else if (row['B'] == row['C'] & row['C'] > row['A']) {
    return("A > B ~ C")
  } else if (row['A'] == row['B'] & row['B'] == row['C']) {
    return("A ~ B ~ C")
  } else {
    return(NA)
  }
}

nl17.asymm$order <- apply(nl17.asymm, 1, classify_ordering)
nl17.symmetric <- ranked.nl17
nl17.symmetric$order <- apply(nl17.symmetric, 1, classify_ordering)


order_counts <- table(nl17.asymm$order) %>% 
  as.data.frame(.) %>% 
  setNames(c("Ordering", "Count"))

order_counts <- table(nl17.symmetric$order) %>% 
  as.data.frame(.) %>% 
  setNames(c("Ordering", "Count"))

###################################################################
# Example dataset based on your `order_counts` result
# Define the strict orderings
strict.orderings <- c("A > B > C", "A > C > B", "B > A > C", "B > C > A", "C > B > A", "C > A > B")

order_counts_df <- as.data.frame(order_counts) %>%
  setNames(c("Ordering", "Frequency")) %>%
  right_join(data.frame(Ordering = strict.orderings), by = "Ordering") %>%
  replace_na(list(Frequency = 0))

# Parse the ordering into ranks
parse_ordering <- function(order_str) {
  ranks <- strsplit(gsub(" > ", ">", order_str), ">")[[1]]
  as.numeric(factor(c("A", "B", "C"), levels = ranks))
}

# Transform into long format for plotting
order_counts_long <- order_counts_df %>%
  rowwise() %>%
  mutate(Ranks = list(parse_ordering(Ordering))) %>%
  unnest_wider(Ranks, names_sep = "_") %>%
  rename(A = Ranks_1, B = Ranks_2, C = Ranks_3) %>%
  pivot_longer(cols = A:C, names_to = "Option", values_to = "Rank")


partynames.nl17 <- data.frame(Option = LETTERS[1:3], 
                              Party = factor(c("VVD", "D66", "GreenLeft"), 
                                             levels = c("GreenLeft", "D66", "VVD")))

nl17.plotdata <- order_counts_long %>% left_join(x = ., y = partynames.nl17, by = "Option")

# Plotting with ggplot
ggplot(nl17.plotdata, 
       aes(x = Party, y = Rank, group = Ordering)) +
  geom_line(aes(linewidth = Frequency, color = Ordering)) +
  geom_point(aes(size = Frequency, color = Ordering)) +
  scale_y_reverse(breaks = 1:3) +  # Reverse y-axis for higher ranks at the top
  labs(x = "Parties", y = "Rank", title = "Netherlands 2017") +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom") + theme(legend.position = "none")
#  guides(size = guide_legend(title = "Frequency"))

