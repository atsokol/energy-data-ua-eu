#============================================================================
# Task: Download Ukrainian solar yield data
#============================================================================

task_solar_ua <- function(end_date = lubridate::floor_date(lubridate::today(), "month") - lubridate::days(1)) {
  
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
  
  filepath <- "data/data_raw/yield_solar_UA.csv"
  start_date <- get_start_date(filepath, datetime_col = "date")
  
  if (start_date > end_date) {
    message("  [SKIP] Solar UA is up to date")
    return(list(task = "solar_ua", status = "skipped", message = "Up to date"))
  }
  
  message("  Downloading Solar UA: ", start_date, " to ", end_date)
  
  dates <- seq(start_date, end_date, by = "day")
  new_solar <- download_yield_list(dates, gen = "1") |>
    bind_rows() |>
    mutate(date = as.Date(date))
  
  if (nrow(new_solar) == 0) {
    return(list(task = "solar_ua", status = "warning", 
                message = "GPEE returned no data"))
  }
  
  ok <- update_csv(new_solar, filepath, start_date, end_date, datetime_col = "date")
  
  status <- if (ok) "success" else "error"
  return(list(task = "solar_ua", status = status,
              message = paste0(nrow(new_solar), " rows, ", start_date, " to ", end_date)))
}

if (sys.nframe() == 0) {
  result <- task_solar_ua()
  message(result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
