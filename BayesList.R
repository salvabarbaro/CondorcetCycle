#!/usr/bin/env Rscript
##
library(dplyr)
library(vote)
library(gtools)
library(parallel)
num_cores <- 38
#
load("rankings.RData")


# Function to process one dataset and compute the posterior probability
process_ranking <- function(data, repl.nb = 10000) {
  process_preferences <- function(data) {
    preferences <- apply(data, 1, function(row) paste(row, collapse = "-"))
    table(preferences)
  }
  
  preference_counts <- process_preferences(data)
  prior_alpha <- rep(1, length(preference_counts))  # Flat prior
  posterior_alpha <- prior_alpha + as.numeric(preference_counts)
  
  set.seed(55234)
  posterior_samples <- rdirichlet(repl.nb, posterior_alpha)
  
  simulate_condorcet <- function(sample, n_voters, candidates) {
    simulated_counts <- rmultinom(1, n_voters, prob = sample)
    simulated_data <- matrix(0, nrow = n_voters, ncol = length(candidates))
    row_idx <- 1
    for (i in seq_along(simulated_counts)) {
      count <- simulated_counts[i]
      preference_order <- strsplit(names(preference_counts)[i], "-")[[1]]
      preference_order <- as.numeric(preference_order)
      for (j in 1:count) {
        if (row_idx > n_voters) break  # Prevent out-of-bounds indexing
        simulated_data[row_idx, ] <- preference_order
        row_idx <- row_idx + 1
      }
    }
    
    if (row_idx <= n_voters) {
      simulated_data <- simulated_data[1:(row_idx - 1), , drop = FALSE]
    }
    
    condorcet_result <- condorcet(as.matrix(simulated_data), quiet = TRUE)
    return(is.null(condorcet_result$elected))
  }
  
  n_voters <- nrow(data)
  candidates <- colnames(data)
  
  paradox_count <- sum(sapply(1:repl.nb, function(i) !simulate_condorcet(posterior_samples[i, ], n_voters, candidates)))
  posterior_probability <- 1 - (paradox_count / repl.nb)  # Probability of no paradox
  return(posterior_probability)
}

ranking_results <- mclapply(
  names(ranking.list),
  function(ranking_name) {
    cat("Processing:", ranking_name, "\n")
    data <- ranking.list[[ranking_name]]
    posterior_probability <- process_ranking(data)
    return(data.frame(ranking_name = ranking_name, posterior_probability = posterior_probability))
  },
  mc.cores = num_cores
)

final_results <- do.call(rbind, ranking_results)
write.csv(final_results, "posterior_probabilities.csv", row.names = FALSE)
##
##
ranking_results2 <- mclapply(
  names(ranking.list2),
  function(ranking_name) {
    cat("Processing:", ranking_name, "\n")
    data <- ranking.list2[[ranking_name]]
    posterior_probability <- process_ranking(data)
    return(data.frame(ranking_name = ranking_name, posterior_probability = posterior_probability))
  },
  mc.cores = num_cores
)

final_results2 <- do.call(rbind, ranking_results2)
write.csv(final_results2, "posterior_probabilities2.csv", row.names = FALSE)
