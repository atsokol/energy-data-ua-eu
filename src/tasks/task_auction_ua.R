#============================================================================
# Task: Download Ukrainian daily cross-border capacity auction results
#       from ua.energy
#
# No VPN required. ua.energy auction files are accessible via plain httr.
#
# Output: data/data_raw/auction_dam_UA.csv
# Columns: date, hour, border, currency, price, cap_offered_mw,
#          cap_allocated_mw
#
# Flow:
#   1. Fetch full date→URL map from ua.energy (page scraped via httr)
#   2. Determine which dates are not yet in the CSV
#   3. Download and parse only the new files
#   4. Append to existing data and write atomically
#============================================================================

task_auction_ua <- function(
  end_date = lubridate::today() - lubridate::days(1)
) {

  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(readxl)
  library(stringr)
  library(httr)
  library(rvest)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_auction_ua.R")
  source("src/helpers/download_ua.R")

  out_filepath <- "data/data_raw/auction_dam_UA.csv"

  # ── 1. Fetch full URL map from the page ───────────────────────────────────
  url_map <- tryCatch(
    get_auction_urls(),
    error = function(e) {
      message("  Failed to fetch auction URL list: ", e$message)
      NULL
    }
  )

  if (is.null(url_map) || length(url_map) == 0) {
    return(list(
      task    = "auction_ua",
      status  = "error",
      message = "Could not retrieve auction URL list from ua.energy"
    ))
  }

  # ── 2. Determine which dates still need downloading ───────────────────────
  default_start <- as.Date("2024-01-01")
  start_date    <- get_start_date(out_filepath, datetime_col = "date",
                                  default_start = default_start)

  if (start_date > end_date) {
    message("  [SKIP] Auction UA is up to date")
    return(list(
      task    = "auction_ua",
      status  = "skipped",
      message = "Up to date"
    ))
  }

  # Filter URL map to the date window we need
  map_dates <- as.Date(names(url_map))
  to_fetch  <- url_map[map_dates >= start_date & map_dates <= end_date]

  if (length(to_fetch) == 0) {
    message("  [SKIP] No new auction files available for ",
            start_date, " to ", end_date)
    return(list(
      task    = "auction_ua",
      status  = "skipped",
      message = "No new files on page"
    ))
  }

  message("  Downloading ", length(to_fetch), " auction file(s): ",
          min(names(to_fetch)), " to ", max(names(to_fetch)))

  # ── 3. Download and parse each file ───────────────────────────────────────
  new_data <- purrr::imap_dfr(to_fetch, function(url, date_str) {
    download_auction_day(as.Date(date_str), url)
  })

  if (nrow(new_data) == 0) {
    return(list(
      task    = "auction_ua",
      status  = "error",
      message = paste0(
        "Downloaded files but parsed 0 rows for ",
        start_date, " to ", end_date
      )
    ))
  }

  # ── Rename long-form border labels to standard country-code format ────────
  border_rename <- c(
    "KhNPP (UA) -RZE (PL)" = "UA-PL",
    "RZE (PL) -KhNPP (UA)" = "PL-UA"
  )
  new_data <- new_data |>
    dplyr::mutate(border = dplyr::recode(.data$border, !!!border_rename)) |>
    dplyr::filter(.data$border != "UA-SK-35") |>
    dplyr::arrange(.data$date, .data$border, .data$hour)

  # ── Convert UAH prices to EUR using NBU daily rates ───────────────────────
  uah_rows <- new_data |> dplyr::filter(.data$currency == "UAH")
  if (nrow(uah_rows) > 0) {
    message("  Converting ", nrow(uah_rows), " UAH rows to EUR via NBU rates")
    fx <- get_nbu_fx(
      min(as.Date(uah_rows$date)),
      max(as.Date(uah_rows$date))
    )
    new_data <- new_data |>
      dplyr::left_join(fx, by = "date") |>
      dplyr::mutate(
        price    = dplyr::if_else(
          .data$currency == "UAH",
          round(.data$price / .data$rate, 4),
          .data$price
        ),
        currency = dplyr::if_else(
          .data$currency == "UAH", "EUR", .data$currency
        )
      ) |>
      dplyr::select(-.data$rate)
  }

  # ── 4. Merge with existing data and write ─────────────────────────────────
  if (file.exists(out_filepath)) {
    existing <- read_csv(out_filepath, show_col_types = FALSE) |>
      dplyr::filter(as.Date(.data$date) < start_date)
    combined <- dplyr::bind_rows(existing, new_data) |>
      dplyr::distinct(.data$date, .data$hour, .data$border, .keep_all = TRUE) |>
      dplyr::arrange(.data$date, .data$border, .data$hour)
  } else {
    combined <- new_data
  }

  tmp_path <- paste0(out_filepath, ".tmp")
  write_csv(combined, tmp_path)
  file.rename(tmp_path, out_filepath)

  message("  \u2705 Auction UA updated: ", nrow(combined), " total rows")

  list(
    task    = "auction_ua",
    status  = "success",
    message = paste0(nrow(new_data), " new rows, ", nrow(combined), " total")
  )
}

# Run if called directly
if (sys.nframe() == 0) {
  result <- task_auction_ua()
  message("\n", result$task, ": ", result$status, " \u2014 ", result$message)
  if (result$status == "error") quit(status = 1)
}
