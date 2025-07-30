### identify party names and LETTERS
partyname.df <- read.csv("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/Data/partyname.csv")
partyinfo.fun <- function(data) {
  case_id <- readline(prompt = "Enter the case_ID: ")
  party_id <- as.integer(readline(prompt = "Enter the partyID: "))
  result <- data %>%
    filter(case_ID == case_id & partyID == party_id) %>%
    select(Letter, partyNAME)
  if (nrow(result) == 0) {
    return("No matching entry found. Please check your inputs.")
  }
  return(result)
}

partyinfo.fun(partyname.df)

