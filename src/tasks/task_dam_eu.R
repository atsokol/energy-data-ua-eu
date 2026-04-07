#============================================================================
# Task: Download EU DAM prices from ENTSO-E (hourly + 15-min)
#============================================================================

task_dam_eu <- function(end_date = lubridate::today() - lubridate::days(1)) {

  library(dplyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(httr)
  library(glue)
  library(entsoeapi)

  Sys.setenv(ENTSOE_PAT = Sys.getenv("ENTSOE_PAT"))

  source("src/config.R")
  source("src/helpers/csv_utils.R")
  source("src/helpers/download_entsoe.R")

  zones <- ENTSO_ZONES

  results <- list()

  to_dt    <- function(d) ymd_hms(paste(d, "00:00:00"), tz = "UTC")
  end_dt   <- to_dt(end_date + days(1))

  run_subtask <- function(name, filepath, datetime_col = "hour",
                          default_start = "2022-01-01", download_fn) {
    start_date <- get_start_date(filepath, datetime_col = datetime_col,
                                 default_start = as.Date(default_start))

    if (start_date > end_date) {
      message("  [SKIP] ", name, " is up to date")
      return(list(task = name, status = "skipped", message = "Up to date"))
    }

    message("  Downloading ", name, ": ", start_date, " to ", end_date)

    tryCatch({
      new_data <- download_fn(to_dt(start_date), end_dt)

      if (is.null(new_data) || nrow(new_data) == 0) {
        message("  [WARN] ", name, ": no data returned")
        return(list(task = name, status = "warning", message = "No data returned"))
      }

      ok <- update_csv(new_data, filepath, start_date, end_date,
                       datetime_col = datetime_col)

      status <- if (ok) "success" else "error"
      return(list(task = name, status = status,
                  message = paste0(nrow(new_data), " rows")))
    }, error = function(e) {
      message("  [ERROR] ", name, ": ", e$message)
      return(list(task = name, status = "error", message = e$message))
    })
  }

  results$dam <- run_subtask(
    "DAM prices EU (hourly)", "data/data_raw/DAM_EU.csv",
    download_fn = function(s, e) download_price_eu(zones, s, e, time_aggregate = TRUE)
  )

  results$dam_15m <- run_subtask(
    "DAM prices EU (15min)", "data/data_raw/DAM_EU_15m.csv",
    download_fn = function(s, e) download_price_eu(zones, s, e, time_aggregate = FALSE)
  )

  # ── Summary ────────────────────────────────────────────────────────────────
  statuses <- sapply(results, \(r) r$status)
  n_ok   <- sum(statuses == "success")
  n_skip <- sum(statuses == "skipped")
  n_warn <- sum(statuses == "warning")
  n_err  <- sum(statuses == "error")

  overall <- if (n_err > 0) "error" else if (n_warn > 0) "warning" else "success"

  return(list(
    task    = "dam_eu",
    status  = overall,
    message = paste0(n_ok, " ok, ", n_skip, " skipped, ",
                     n_warn, " warnings, ", n_err, " errors"),
    details = results
  ))
}

if (sys.nframe() == 0) {
  result <- task_dam_eu()
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
