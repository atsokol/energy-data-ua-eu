#============================================================================
# Task: Download UEEX natural gas standardized product quotations
#   Source: https://www.ueex.com.ua/exchange-quotations/natural-gas/standardized-products/
#   Output: data/data_raw/ueex_gas_UA.csv
#
# Columns: date, product, supply_conditions, volume_tcm,
#          price_uah_excl_vat, price_uah_incl_vat
# Units:   volume in thousand cubic metres; price in UAH per thousand cubic metres
#============================================================================

task_ueex_gas <- function(end_date = lubridate::today() - lubridate::days(1)) {

  library(dplyr)
  library(readr)
  library(lubridate)
  library(httr)
  library(rvest)
  library(xml2)
  library(purrr)
  library(glue)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_ueex.R")

  filepath <- "data/data_raw/ueex_gas_UA.csv"

  # Re-fetch from the start of the last available month so partial months
  # (current month still in progress) get fully refreshed
  start_date <- get_start_date_monthly(
    filepath,
    date_col      = "date",
    default_start = as.Date("2020-10-01")   # data available from Oct 2020
  )

  if (start_date > end_date) {
    message("  [SKIP] UEEX gas price is up to date")
    return(list(task = "ueex_gas", status = "skipped", message = "Up to date"))
  }

  message("  Downloading UEEX gas: ", start_date, " to ", end_date)

  tryCatch({

    new_data <- download_ueex_gas(start_date, end_date)

    if (nrow(new_data) == 0) {
      message("  [WARN] UEEX gas: no data returned")
      return(list(task = "ueex_gas", status = "warning", message = "No data returned"))
    }

    ok <- update_csv(new_data, filepath, start_date, end_date, datetime_col = "date")
    status <- if (ok) "success" else "error"

    list(
      task    = "ueex_gas",
      status  = status,
      message = paste0(nrow(new_data), " rows (", min(new_data$date),
                       " – ", max(new_data$date), ")")
    )

  }, error = function(e) {
    message("  [ERROR] UEEX gas: ", e$message)
    list(task = "ueex_gas", status = "error", message = e$message)
  })
}

if (sys.nframe() == 0) {
  result <- task_ueex_gas()
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
