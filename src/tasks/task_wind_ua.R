#============================================================================
# Task: Download Ukrainian wind yield data
#============================================================================

task_wind_ua <- function(end_date = lubridate::today() - lubridate::days(1)) {
  
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
  
  filepath <- "data/data_raw/yield_wind_UA.csv"
  start_date <- get_start_date(filepath, datetime_col = "date")
  
  if (start_date > end_date) {
    message("  [SKIP] Wind UA is up to date")
    return(list(task = "wind_ua", status = "skipped", message = "Up to date"))
  }
  
  message("  Downloading Wind UA: ", start_date, " to ", end_date)
  
  dates <- seq(start_date, end_date, by = "day")
  new_wind <- download_yield_list(dates, gen = "2") |>
    bind_rows() |>
    mutate(date = as.Date(date))
  
  if (nrow(new_wind) == 0) {
    return(list(task = "wind_ua", status = "warning", 
                message = "GPEE returned no data"))
  }
  
  ok <- update_csv(new_wind, filepath, start_date, end_date, datetime_col = "date")
  
  status <- if (ok) "success" else "error"
  return(list(task = "wind_ua", status = status,
              message = paste0(nrow(new_wind), " rows, ", start_date, " to ", end_date)))
}

if (sys.nframe() == 0) {
  result <- task_wind_ua()
  message(result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
