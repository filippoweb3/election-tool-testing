library(jsonlite)
library(httr)
library(dplyr)
setwd("/Users/filippo/Desktop")

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













# Functions ----

fetch_scanner_data <- function(account) {
  api_url <- "https://assethub-polkadot.api.subscan.io/api/scan/staking/voted"
  api_key <- "9186f194f5ee47fcb462a97135c35626"
  
  body <- list(
    address = account,
    page = 0,
    row = 50
  )
  
  res <- POST(
    url = api_url,
    add_headers(
      `Content-Type` = "application/json",
      `x-api-key` = api_key
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  scanner_content <- content(res, as = "text", encoding = "UTF-8")
  scanner_data <- fromJSON(scanner_content)
  
  scanner_data <- scanner_data$data$list
  scanner_data <- data.frame(
    nominator_account = account,
    stash = scanner_data$stash_account_display$address,
    self_stake = as.numeric(scanner_data$bonded_owner)/10^10,
    total_stake = as.numeric(scanner_data$bonded_nominators)/10^10 + as.numeric(scanner_data$bonded_owner)/10^10,
    nominations_count = scanner_data$count_nominators,
    nominator_stake = as.numeric(scanner_data$bonded)/10^10,
    active = scanner_data$active
  )
  
  return(scanner_data)
  
}

summarize_sim_data <- function(sim_data, account) {
  
  nominations <- sim_data$active_validators$nominations
  
  summary_data <- list()
  
  for (i in 1:length(nominations)) {
    if(account %in% nominations[[i]]$nominator) {
      summary_data[[i]] <- data.frame(
        nominator_account = nominations[[i]][account == nominations[[i]]$nominator,1],
        stash = sim_data$active_validators$stash[[i]],
        self_stake = as.numeric(str_remove(sim_data$active_validators$self_stake[[i]], " DOT")),
        total_stake = as.numeric(str_remove(sim_data$active_validators$total_stake[[i]], " DOT")),
        #commission = sim_data$active_validators$commission[[i]],
        nominations_count = sim_data$active_validators$nominations_count[[i]],
        nominator_stake = as.numeric(str_remove(nominations[[i]][account == nominations[[i]]$nominator,2], " DOT")),
        active = TRUE
      )
    }
  }
  
  summary_data <- do.call(rbind, summary_data[lengths(summary_data) > 0])
  
  return(summary_data)
  
}


