library(vote)
library(dplyr)

g1 <- 600
g2 <- 200
g3 <- 199

df1 <- data.frame(l = rep(1, g1), c = rep(2, g1), r = rep(3, g1))
df2 <- data.frame(l = rep(3, g2), c = rep(1, g2), r = rep(2, g2))
df3 <- data.frame(l = rep(2, g3), c = rep(3, g3), r = rep(1, g3))
df <- rbind(df1, df2, df3)
rm(df1, df2, df3)

c1 <- condorcet(df)


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




########################################################################
########################################################################

########################################################################
bootstrap_res_party <- function(condorcet_obj, R = 100, ncpus = 8) {
  profil <- condorcet_obj[['data']]
  boot.fun <- function(data, indices) {
    boot_sample <- data[indices, ]  
    result <- tryCatch(
      condorcet(boot_sample, quiet = TRUE), 
      error = function(e) return(NULL)  # Return NULL if no Condorcet winner exists 
    )
    # if $elected is NULL, then this loop:
    if (is.null(result) || is.null(result$elected)) {
      if (!is.null(condorcet_obj[["totals"]])) {
        totals_df <- condorcet_obj[["totals"]] %>%
          as.data.frame() %>%
          mutate(wins = rowSums(.))
        
        win_counts <- table(totals_df$wins)
        # depending on the loop, the following will be returned:
        if (any(win_counts == 2)) {
          return("Tie")  # There is a win-value twice
        } else if (any(win_counts == 3)) {
          return("Cycle")  # There is a win-value in a triple of times
        } else {
          return("Tie")  # Else: Tie (like two ties)
        }
      } else {
        return("nototals")  # this case considers is.null(totals) == TRUE
      }
    } else {  ## end of loop
      return(result$elected[1])  # Return the first elected candidate
    }
  }
  # run bootstrap 
  boot(data = profil, 
       statistic = boot.fun, 
       R = R, 
       parallel = "multicore",  # For Linux/Unix
       ncpus = ncpus)
}

## run...
set.seed(55234) 


boot_result <- bootstrap_res_party(c2, R = 1000, ncpus = 8)
table(boot_result$t)
#########################################################################

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

c2 <- condorcet(df)


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
