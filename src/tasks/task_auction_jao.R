#============================================================================
# Task: Download cross-border capacity auction data from JAO API
#       for Ukraine-related corridors:
#         UA-SK, SK-UA, UA-HU, HU-UA, UA-PL, PL-UA, UA-RO, RO-UA
#
# No VPN required. JAO API is a public REST API (token-authenticated).
#
# Output: data/data_raw/auction_dam_UA.csv
#   JAO data takes priority for JAO corridors (replaces Ukrenergo rows
#   for dates >= start_date). Ukrenergo-only corridors (MD-UA, UA-MD,
#   KhNPP variants, UA-SK-35) are preserved unchanged.
#
# Requires: JAO_TOKEN environment variable (set in .Renviron locally,
#           JAO_TOKEN secret in GitHub Actions)
#============================================================================

task_auction_jao <- function(
    end_date = lubridate::today("Europe/Paris") - lubridate::days(1)
) {

  library(dplyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(httr)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_jao.R")

  out_filepath  <- "data/data_raw/auction_dam_UA.csv"
  default_start <- as.Date("2024-03-01")   # JAO UA data starts ~2024-03-02

  # ── Determine start date from existing JAO-corridor rows ───────────────────
  # Only EUR rows in JAO corridors count as "already downloaded from JAO".
  # UAH rows in the same corridors are from Ukrenergo and will be replaced
  # once JAO data covers those dates.
  start_date <- if (file.exists(out_filepath)) {
    existing  <- read_csv(out_filepath, show_col_types = FALSE)
    jao_rows  <- existing |>
      filter(.data$border %in% JAO_UA_CORRIDORS,
             toupper(.data$currency) == "EUR")
    if (nrow(jao_rows) == 0) {
      default_start   # first JAO run → full backfill from 2024-03-01
    } else {
      # 2-day lookback so late auction revisions are picked up
      max(as.Date(jao_rows$date), na.rm = TRUE) - days(2)
    }
  } else {
    default_start
  }

  start_date <- max(start_date, default_start)

  if (start_date > end_date) {
    message("  [SKIP] JAO auction data is up to date")
    return(list(task = "auction_jao", status = "skipped", message = "Up to date"))
  }

  message("  Downloading JAO auction data: ", start_date, " to ", end_date)

  # ── Download ────────────────────────────────────────────────────────────────
  new_data <- tryCatch(
    get_jao_ua_auctions(start_date, end_date),
    error = function(e) {
      message("  JAO download failed: ", e$message)
      NULL
    }
  )

  if (is.null(new_data) || nrow(new_data) == 0) {
    return(list(
      task    = "auction_jao",
      status  = "error",
      message = "No data returned from JAO API"
    ))
  }

  new_data <- new_data |>
    filter(!is.na(.data$date), !is.na(.data$hour)) |>
    arrange(.data$date, .data$border, .data$hour)

  message("  Downloaded ", nrow(new_data), " JAO rows")

  # ── Merge: JAO corridors updated, non-JAO corridors preserved ─────────────
  if (file.exists(out_filepath)) {
    existing <- read_csv(out_filepath, show_col_types = FALSE)

    # Only replace existing rows for corridors JAO actually returned data for.
    # This prevents silently dropping Ukrenergo data when JAO has no coverage
    # for a corridor listed in JAO_UA_CORRIDORS (e.g. UA-RO, RO-UA).
    active_jao_corridors <- unique(new_data$border)

    # Keep existing rows that are either:
    #   (a) a corridor JAO returned no data for (all dates), or
    #   (b) a JAO-active corridor but before the new download window
    preserved <- existing |>
      filter(
        !.data$border %in% active_jao_corridors |
          as.Date(.data$date) < start_date
      )

    combined <- bind_rows(preserved, new_data) |>
      distinct(.data$date, .data$hour, .data$border, .keep_all = TRUE) |>
      arrange(.data$date, .data$border, .data$hour)
  } else {
    combined <- new_data
  }

  # ── Atomic write ────────────────────────────────────────────────────────────
  tmp_path <- paste0(out_filepath, ".tmp")
  write_csv(combined, tmp_path)
  file.rename(tmp_path, out_filepath)

  message("  \u2705 JAO auction data updated: ", nrow(combined), " total rows")

  return(list(
    task    = "auction_jao",
    status  = "success",
    message = paste0(nrow(new_data), " new rows, ", nrow(combined), " total")
  ))
}

# Run if called directly
if (sys.nframe() == 0) {
  result <- task_auction_jao()
  message("\n", result$task, ": ", result$status, " \u2014 ", result$message)
  if (result$status == "error") quit(status = 1)
}
