#============================================================================
# Task: Download Ukrainian Day-Ahead Market (DAM) data
#============================================================================

task_dam_ua <- function(end_date = lubridate::today() - lubridate::days(1)) {
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(httr)
  library(jsonlite)
  library(glue)
  
  source("src/helpers/csv_utils.R")
  source("src/helpers/download_ua.R")
  
  filepath <- "data/data_raw/DAM_UA.csv"
  start_date <- get_start_date(filepath, datetime_col = "hour")
  
  if (start_date > end_date) {
    message("  [SKIP] DAM UA is up to date")
    return(list(task = "dam_ua", status = "skipped", message = "Up to date"))
  }
  
  message("  Downloading DAM UA: ", start_date, " to ", end_date)
  
  # Download DAM data
  new_dam_raw <- download_dam_ua(start_date, end_date)
  
  if (nrow(new_dam_raw) == 0) {
    message("  [WARN] OREE API returned no data (may be geo-restricted)")
    return(list(task = "dam_ua", status = "warning", 
                message = "API returned no data"))
  }
  
  # Download FX rates and join
  fx_data <- get_nbu_fx(start_date, end_date, valcode = "EUR")
  
  new_dam <- new_dam_raw |>
    mutate(date = as.Date(hour)) |>
    left_join(fx_data, by = "date") |>
    mutate(price_eur_mwh = price_uah / rate)
  
  na_rate_count <- sum(is.na(new_dam$rate))
  if (na_rate_count > 0) {
    warning("  ", na_rate_count, " rows have missing FX rates")
  }
  
  # Update file
  ok <- update_csv(new_dam, filepath, start_date, end_date, datetime_col = "hour")
  
  status <- if (ok) "success" else "error"
  return(list(task = "dam_ua", status = status,
              message = paste0(nrow(new_dam), " rows, ", start_date, " to ", end_date)))
}

# Run if called directly
if (sys.nframe() == 0) {
  result <- task_dam_ua()
  message(result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
