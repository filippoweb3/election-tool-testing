#' @export
fetch_scanner_data <- function(account) {
  api_url <- "https://assethub-polkadot.api.subscan.io/api/scan/staking/voted"
  api_key <- "9186f194f5ee47fcb462a97135c35626"

  body <- list(
    address = account,
    page = 0,
    row = 50
  )

  res <- httr::POST(
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

#' @export
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
