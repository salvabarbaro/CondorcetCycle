library(vote)
library(dplyr)

g1 <- 600
g2 <- 400
g3 <- 199

df1 <- data.frame(l = rep(1, g1), c = rep(2, g1), r = rep(3, g1))
df2 <- data.frame(l = rep(3, g2), c = rep(1, g2), r = rep(2, g2))
df3 <- data.frame(l = rep(2, g3), c = rep(3, g3), r = rep(1, g3))
df <- rbind(df1, df2, df3)
rm(df1, df2, df3)

condorcet(df)


# Function to check if Condorcet winner exists
condorcet_exists <- function(data) {
  result <- condorcet(data, quiet = T)
  # If exactly one winner exists and no cycle: Condorcet winner
  return(length(result$elected) == 1)
}

# Run bootstrap
set.seed(55234)  # for reproducibility
n_boot <- 1000
n <- nrow(df)

boot_results <- replicate(n_boot, {
  sample_df <- df[sample(1:n, size = n, replace = TRUE), ]
  condorcet_exists(sample_df)
})

# Summary
summary_df <- data.frame(
  CondorcetWinnerExists = c(TRUE, FALSE),
  Count = c(sum(boot_results), sum(!boot_results))
)

print(summary_df)



##############################
### Alternative number: g1 = g2

g1 <- 620
g2 <- 600
g3 <- 100

df1 <- data.frame(l = rep(1, g1), c = rep(2, g1), r = rep(3, g1))
df2 <- data.frame(l = rep(3, g2), c = rep(1, g2), r = rep(2, g2))
df3 <- data.frame(l = rep(2, g3), c = rep(3, g3), r = rep(1, g3))
df <- rbind(df1, df2, df3)
rm(df1, df2, df3)

condorcet(df)


# Function to check if Condorcet winner exists
condorcet_exists <- function(data) {
  result <- condorcet(data, quiet = T)
  # If exactly one winner exists and no cycle: Condorcet winner
  return(length(result$elected) == 1)
}

# Run bootstrap
set.seed(55234)  # for reproducibility
n_boot <- 1000
n <- nrow(df)

boot_results <- replicate(n_boot, {
  sample_df <- df[sample(1:n, size = n, replace = TRUE), ]
  condorcet_exists(sample_df)
})

# Summary
summary_df <- data.frame(
  CondorcetWinnerExists = c(TRUE, FALSE),
  Count = c(sum(boot_results), sum(!boot_results))
)

print(summary_df)
