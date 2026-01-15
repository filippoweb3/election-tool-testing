library(jsonlite)
library(httr)
library(dplyr)
library(stringr)

accounts <- c(
  "15j4dg5GzsL1bw2U2AWgeyAk6QTxq43V7ZPbXdAmbVLjvDCK",
  "14gAowz3LaAqYkRjqUZkjZUxKFUzLtN2oZJSfr3ziHBRhwgc",
  "13SkL2uACPqBzpKBh3d2n5msYNFB2QapA5vEDeKeLjG2LS3Y"
)

big_data <- list()

for (j in 1:length(accounts)){

  # Fetch data from Subscan ----

  scanner_data <- fetch_scanner_data(accounts[j])

  # Compile simulations ----

  sim_data <- fromJSON("simulate_11215541_20iter.json")

  summary_data <- summarize_sim_data(sim_data = sim_data, account = accounts[j])

  # Tests ----

  merged_data <- list()

  for (i in 1:length(scanner_data[,1])) {
    if(sum(summary_data$stash == scanner_data$stash[i]) == 1){
      merged_data[[i]] <- summary_data[summary_data$stash == scanner_data$stash[i], c(3:7)]
    } else {
      merged_data[[i]] <- data.frame(self_stake = NA, total_stake = NA, nominations_count = NA, nominator_stake = NA, active = NA)
    }
  }

  merged_data <- do.call(rbind, merged_data)
  colnames(merged_data) <- c("sim_self_stake", "sim_total_stake", "sim_nominations_count", "sim_nominator_stake", "sim_active")

  merged_data <- cbind(scanner_data, merged_data)

  merged_data$d_self_stake <- round((merged_data$sim_self_stake - merged_data$self_stake)/merged_data$self_stake*100, 2)
  merged_data$d_total_stake <- round((merged_data$sim_total_stake - merged_data$total_stake)/merged_data$total_stake*100, 2)
  merged_data$d_nom_count <- round((merged_data$sim_nominations_count - merged_data$nominations_count)/merged_data$nominations_count*100, 2)
  merged_data$d_nom_stake <- round((merged_data$sim_nominator_stake - merged_data$nominator_stake)/merged_data$nominator_stake*100, 2)

  big_data[[j]] <- merged_data

}

do.call(rbind, big_data)

