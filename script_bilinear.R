#library(jsonlite)
#library(httr)
#library(dplyr)
#library(stringr)

accounts <- c(
  "EX9uchmfeSqKTM7cMMg8DkH49XV8i4R7a7rqCn8btpZBHDP",
  "G1rrUNQSk7CjjEmLSGcpNu72tVtyzbWdUvgmSer9eBitXWf",
  "HgTtJusFEn2gmMmB5wmJDnMRXKD6dzqCpNR7a99kkQ7BNvX"
)





accounts <- c(
  "15j4dg5GzsL1bw2U2AWgeyAk6QTxq43V7ZPbXdAmbVLjvDCK",
  "14gAowz3LaAqYkRjqUZkjZUxKFUzLtN2oZJSfr3ziHBRhwgc",
  "13SkL2uACPqBzpKBh3d2n5msYNFB2QapA5vEDeKeLjG2LS3Y",
  "156U1ffF2ZSR5sBpYNUxvXu3rprh4x4of3WdYuBcReD87qcc",
  "12K2F1PeLUiVmt7GfBgiQvtGP4HbtUJ55cLvwbybE8W3nau5",
  "13aUbVbnthMvYuSLUbfK6eTQWaDWLriRrP89ExD17Ep19BkK",
  "12ZMM3vPtEwqmNgzjLqELYYeCvoP1cpf3yn2qwD9J8K1Q4pn"
)

big_data <- list()

for (j in 1:length(accounts)){

  # Fetch data from Subscan ----

  scanner_data <- fetch_scanner_data(accounts[j], chain = "Kusama")

  # Compile simulations ----

  sim_data <- jsonlite::fromJSON("simulate-9.json")

  summary_data <- summarize_sim_data_bilinear(sim_data = sim_data, account = accounts[j], chain = "Kusama")

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

  if(sum(merged_data$self_stake == 0) > 0) {
    for (i in 1:sum(merged_data$self_stake == 0)){
      if(merged_data$self_stake[i] == 0) {
        merged_data$d_self_stake[i] <- 0
      } else {
        merged_data$d_self_stake[i] <- round((merged_data$sim_self_stake[i] - merged_data$self_stake[i])/merged_data$self_stake[i]*100, 2)
      }
    }
  } else {
    merged_data$d_self_stake <- round((merged_data$sim_self_stake - merged_data$self_stake)/merged_data$self_stake*100, 2)
  }

  merged_data$d_total_stake <- round((merged_data$sim_total_stake - merged_data$total_stake)/merged_data$total_stake*100, 2)
  merged_data$d_nom_count <- round((merged_data$sim_nominations_count - merged_data$nominations_count)/merged_data$nominations_count*100, 2)
  merged_data$d_nom_stake <- round((merged_data$sim_nominator_stake - merged_data$nominator_stake)/merged_data$nominator_stake*100, 2)

  big_data[[j]] <- merged_data

}

do.call(rbind, big_data)

# Snapshots ----

fetched <- jsonlite::fromJSON("snapshot_11312861.json")
built <- jsonlite::fromJSON("snapshot_11312826.json")

fetched_nom <- fetched$nominators$stash
built_nom <- built$nominators$stash

fetched_val <- fetched$validators$stash
built_val <- built$validators$stash

sum(fetched_nom %in% built_nom)/length(fetched_nom)
sum(fetched_val %in% built_val)/length(fetched_val)

# Manual override ----

candidates <- c(
  "14CRo92REj3aXfUeonVSti1VEHgxhWbtKY9hwxvD5T3BBXkK",
  "12JZr1HgK8w6zsbBj6oAEVRkvisn8j3MrkXugqtvc4E8uwLo",
  "13HtFCrxyz55KgkPWcnhHPwE8f8GmZrfXR3uC6jNrihGzmqz",
  "16ARoGkkDSTmeu9tDvfBDksu4qURGz6s1HSvXzrwGnsjFtKg",
  "14jgoaaLe7L12zhRnyACPQXhNtRNCXXygvHc3FgZFxmQkXBJ",
  "168X6YCFoEDh8ZjxCcwzdjgZZhRjNHG6R45S7A3euxJcNXfN",
  "133TTqBqmhHo9X5YWxYKB3JNJCuLdnLQZLbaZDHqprAKVxgD",
  "141Qnt9LPnAfnurdY8RXcB8th5S5SNZbgk4ULpDJVdp5DSr2",
  "144fdQQrXGwWWwATr9b3CL7RWQyxNRGMm2XJanVKGrNSoepc",
  "13wVpSQPLxkFLSUsiAaChXKiTqHXfRGuevqGEo9ARobGqRZb",
  "1AXSTNj1JhUpg83Rwq9ayBfHVnQmoXmpvYKDffLNdmNY9gm",
  "161FEAs63HJqQnG2v4xgSpdfgPkDTqd3T8gzLzPaSnjPDaMZ",
  "1o6KaLJCHyz2CoTkxBmqabvrPHyhcpnjn4jCbt16mPgEx8v",
  "1bAKJKQg3bWCeNVzGuc1TzF2y2h4DJfWR92CWAoLVNn7nN8",
  "1631N24odmeJguz7CvHMA6Bmqze4pG9RZwXV8DSAxci3Fwum",
  "14TZSozkvCNsTsBYgZnZEwCLc7T6BzRw6pxTUjyL3FWqC7th",
  "16kN6q7zAtiRGWFfa8sdC5vhnmE4nxdAeN8zX4RpR9E13tiV",
  "14pRyAssutxsoZUmeyRqLCSUogihy8U9q4962N6mLZ8mLofx",
  "16Xr1KqpLNcWidW7MoPtD8LSagE4QUnkLbi5aV5H72tSukvZ",
  "13m2ttpCh1ho7rShHxsm8MtoJskzC6D4phaQGynHn3d8Q2MG",
  "13Vuqok4VAnKghMtzxr6c2YwyG58HPZU9Xih2PhbDj8V9AoJ",
  "1qQvg7wr5tHHog1eLrQ9PCQ4KAZN5DSWkVQvx9yzbizVeRJ",
  "12bbPhujKmy6Ryd41mU1wwTjf3kZcwU3GfdCngY4VJiKBLrS",
  "162uNfr8GeQiqmWnGnsp5vf1FkcBqF5rLiovEGCnn3f9CQFt",
  "1puaLjmorA2b1CEn9eeqcT8z47X4Zd9eJi4TG6v6pYmbKYq",
  "14xM5U9EhZ9FTTDmUWKPPvmRnQ3oF9NJANjnq8E5FsQDjnpn",
  "165EH5A6sUMJDexTQrvsyw9yQtv1FxLe1F8NjL4wm2xQY6GD",
  "14W4PGjfXFCPtx3oN2VRYn7e22n5mkzjAQzygmkNSkDvCSpV",
  "14mM9FRDDtwSYicjNxSvMfQkap8o4m9zHq7hNW4JpbSL4PPU",
  "12eXzTdsTTCYTEF8zJgv8bNbXA4M1k4ovSanu9t7dj24oAov"
)

sum(candidates %in% sim_data$active_validators$stash) / length(candidates)
