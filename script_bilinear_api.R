snapshot_api_url <- "http://127.0.0.1:8080/snapshot"
block_hash <- "0x40fff61f23141f72a0b6b72c5284c6f6a077f157840d0bc67815f412f63571e8"

snapshot_res <- httr::GET(
  url = simulate_api_url,
  query = list(block = block_hash)
)

content <- httr::content(res, as = "text", encoding = "UTF-8")
data <- jsonlite::fromJSON(content, simplifyVector = TRUE)

snapshot <- data$result



simulate_api_url <- "http://127.0.0.1:8080/simulate"

body <- list(
  desired_validators = 297,
  algorithm = "SeqPhragmen",
  iterations = 10,
  reduce = TRUE,
  max_nominations = 16
)

res <- httr::POST(
  url = simulate_api_url,
  httr::add_headers(`Content-Type` = "application/json"),
  body = jsonlite::toJSON(body, auto_unbox = TRUE),
  query = list(block = block_hash)
)

content <- httr::content(res, as = "text", encoding = "UTF-8")
data <- jsonlite::fromJSON(content, simplifyVector = TRUE)


