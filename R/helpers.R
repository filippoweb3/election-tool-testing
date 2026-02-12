#' @export
fetch_scanner_data <- function(account, chain = "Polkadot") {
  api_key <- "9186f194f5ee47fcb462a97135c35626"

  if(chain == "Polkadot"){
    api_url <- "https://assethub-polkadot.api.subscan.io/api/scan/staking/voted"
    scale <- 10^10
  } else if(chain == "Kusama"){
    api_url <- "https://assethub-kusama.api.subscan.io/api/scan/staking/voted"
    scale <- 10^12
  }

  body <- list(
    address = account,
    page = 0,
    row = 50
  )

  res <- httr::POST(
    url = api_url,
    httr::add_headers(
      `Content-Type` = "application/json",
      `x-api-key` = api_key
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  scanner_content <- httr::content(res, as = "text", encoding = "UTF-8")
  scanner_data <- jsonlite::fromJSON(scanner_content)

  scanner_data <- scanner_data$data$list
  scanner_data <- data.frame(
    nominator_account = account,
    stash = scanner_data$stash_account_display$address,
    self_stake = as.numeric(scanner_data$bonded_owner)/scale,
    total_stake = as.numeric(scanner_data$bonded_nominators)/scale + as.numeric(scanner_data$bonded_owner)/scale,
    nominations_count = scanner_data$count_nominators,
    nominator_stake = as.numeric(scanner_data$bonded)/scale,
    active = scanner_data$active
  )

  return(scanner_data)

}

#' @export
summarize_sim_data_bilinear <- function(sim_data, account, chain = "Polkadot", params = NULL, block_hash = NULL) {

  if(is.null(sim_data)){
    sim_data <- get_simulation(block_hash = block_hash, params = params)
    sim_data <- sim_data$result
  }

  if(chain == "Polkadot"){
    unit <- " DOT"
  } else if(chain == "Kusama"){
    unit <- " KSM"
  }

  nominations <- sim_data$active_validators$nominations

  summary_data <- list()

  for (i in 1:length(nominations)) {
    if(account %in% nominations[[i]]$nominator) {
      summary_data[[i]] <- data.frame(
        nominator_account = nominations[[i]][account == nominations[[i]]$nominator,1],
        stash = sim_data$active_validators$stash[[i]],
        self_stake = as.numeric(stringr::str_remove(sim_data$active_validators$self_stake[[i]], unit)),
        total_stake = as.numeric(stringr::str_remove(sim_data$active_validators$total_stake[[i]], unit)),
        #commission = sim_data$active_validators$commission[[i]],
        nominations_count = sim_data$active_validators$nominations_count[[i]],
        nominator_stake = as.numeric(stringr::str_remove(nominations[[i]][account == nominations[[i]]$nominator,2], unit)),
        active = TRUE
      )
    }
  }

  summary_data <- do.call(rbind, summary_data[lengths(summary_data) > 0])

  return(summary_data)

}

#' @export
summarize_sim_data_antiers <- function(sim_data, account, chain = "Polkadot") {

  if(chain == "Polkadot"){
    unit <- " DOT"
  } else if(chain == "Kusama"){
    unit <- " KSM"
  }

  nominations <- sim_data$results$nominators

  summary_data <- list()

  for (i in 1:length(nominations)) {
    if(account %in% nominations[[i]]$address) {
      summary_data[[i]] <- data.frame(
        nominator_account = nominations[[i]][account == nominations[[i]]$address,1],
        stash = sim_data$results$account[i],
        self_stake = as.numeric(stringr::str_remove(sim_data$results$self_stake[i], unit)),
        total_stake = as.numeric(stringr::str_remove(sim_data$results$total_stake[i], unit)),
        #commission = sim_data$active_validators$commission[[i]],
        nominations_count = sim_data$results$nominator_count[i],
        nominator_stake = as.numeric(stringr::str_remove(nominations[[i]][account == nominations[[i]]$address,2], unit)),
        active = TRUE
      )
    }
  }

  summary_data <- do.call(rbind, summary_data[lengths(summary_data) > 0])

  return(summary_data)

}

#' @export
get_snapshot <- function(block_hash){
  snapshot_api_url <- "http://127.0.0.1:8080/snapshot"

  res <- httr::GET(
    url = snapshot_api_url,
    query = list(block = block_hash)
  )

  content <- httr::content(res, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, simplifyVector = TRUE)

  return(data)

}

#' @export
get_simulation <- function(block_hash,
                           params = list(
                             desired_validators = 600,
                             algorithm = "SeqPhragmen",
                             iterations = 20,
                             reduce = TRUE,
                             max_nominations = 16
                           )
){

  simulate_api_url <- "http://127.0.0.1:8080/simulate"

  res <- httr::POST(
    url = simulate_api_url,
    httr::add_headers(`Content-Type` = "application/json"),
    body = jsonlite::toJSON(params, auto_unbox = TRUE),
    query = list(block = block_hash)
  )

  content <- httr::content(res, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, simplifyVector = TRUE)

  return(data)

}


#' @export
get_simulation_antiers <- function(block_number,
                           params = list(
                             desired_validators = 600,
                             algorithm = "SeqPhragmen",
                             balancing_iterations = 20,
                             do_reduce = TRUE
                           )
){

  simulate_api_url <- "http://127.0.0.1:8080/simulate"

  res <- httr::POST(
    url = simulate_api_url,
    httr::add_headers(`Content-Type` = "application/json"),
    body = jsonlite::toJSON(params, auto_unbox = TRUE),
    query = list(block = block_number)
  )

  content <- httr::content(res, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, simplifyVector = TRUE)

  return(data)

}
