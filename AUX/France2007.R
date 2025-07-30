### France 2007
### Parties: A (CW), B, C are the parties with the highest number of pairwise wins.
## 1. Restrict d on France_2007
fr07 <- d %>% filter(., case_ID == "France_2007") %>%
  select(c("party_rating_A", "party_rating_B", "party_rating_C"))
ranked.fr07 <- as.data.frame(t(apply(fr07, 1, 
                                     function(x) frank(-x, 
                                                       ties.method = "max", 
                                                       na.last = TRUE)))) %>%
  setNames(LETTERS[1:3])

## restrict to asymmetric relations only:
fr07.asymm <- ranked.fr07 %>% 
  mutate(sums = rowSums(.)) %>%
  filter(., sums == 6)  # we lose little observations only. 

table(fr07.asymm$A)
table(fr07.asymm$B)
table(fr07.asymm$C)



#################################################################################
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

# Apply the function to each row in the dataframe and count occurrences
fr07.asymm$order <- apply(fr07.asymm, 1, classify_ordering)
order_counts <- table(fr07.asymm$order) %>% 
  as.data.frame(.) %>% 
  setNames(c("Ordering", "Count"))

###################################################################
# Example dataset based on your `order_counts` result
# Define the strict orderings
strict.orderings <- c("A > B > C", "A > C > B", "B > A > C", "B > C > A", "C > B > A", "C > A > B")

# Convert order_counts to a data frame and left_join with strict.orderings to fill in missing frequencies with 0
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

partynames.fr07 <- data.frame(Option = LETTERS[1:3], 
                                  Party = factor(c("UMP", "PS", "MoDem"), 
                                                 levels = c("PS", "MoDem", "UMP")))
# UMP: Union for a Popular Movement (UMP)
# PS: Socialist Party
# MoDem:  Democratic Movement (MoDem)
fr07.plotdata <- order_counts_long %>% left_join(x = ., y = partynames.fr07, by = "Option")


# Plotting with ggplot
ggplot(fr07.plotdata, 
       aes(x = Party, y = Rank, group = Ordering)) +
  geom_line(aes(size = Frequency, color = Ordering)) +
  geom_point(aes(size = Frequency, color = Ordering)) +
  scale_y_reverse(breaks = 1:3) +  # Reverse y-axis for higher ranks at the top
  labs(x = "Parties", y = "Rank", title = "France 2007") +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom") + theme(legend.position = "none")
#  guides(size = guide_legend(title = "Frequency"))

save.image("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/RScripts/France2007.RData")
load("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/RScripts/France2007.RData")
