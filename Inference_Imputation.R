#!/usr/bin/env Rscript
#
#
library(boot)
library(dplyr)
library(tidyr)
library(vote)
########################################################################
load("resWI.RData")  # consists of res.party

bootstrap_res_party <- function(condorcet_obj, R = 10000, ncpus = 38) {  
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

set.seed(55234) 
boot_results <- lapply(res.partyWI, 
                       function(obj) bootstrap_res_party(obj, 
                                                         R = 10000,    
                                                         ncpus = 38))  

# summarise results
elected_list <- boot_results %>%
  purrr::map(~ as.data.frame(.x$t)) %>%  
  bind_cols() %>% 
  setNames(names(res.partyWI))

cwinner_party.df <- elected_list
rm(elected_list)
summary.df <- cwinner_party.df %>%
   pivot_longer(cols = everything(), names_to = "case", values_to = "result") %>%
#  gather(key = "case", value = "result") %>%
  group_by(result) %>%
  summarise(count = n())
print(summary.df)
write.csv(cwinner_party.df, "bootstrapWI.csv",
          row.names = F)
