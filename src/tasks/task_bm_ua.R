#============================================================================
# Task: Download Ukrainian Balancing Market data from ua.energy
#
# Combines the two monthly datasets published on the BM results page —
#   Результати балансуючого ринку → volume_bm, price_bm_uah  (by direction)
#   Маржинальні ціни              → price_marg_uah           (by direction)
# — and joins DAM prices + the UAH/EUR rate from DAM_UA.csv.
#
# Output: data/data_raw/BM_UA.csv, long by direction (up / down).
#
# The imbalance datasets from the same page live in Imbalance_UA.csv —
# see src/tasks/task_imbalance_ua.R.
#
# No VPN required, but ua.energy is behind Cloudflare — chromote (headless
# Chrome) is used for the page and for the download session cookies.
#============================================================================

BM_UA_COLS <- c(
  "country", "hour", "date", "direction",
  "volume_bm", "price_bm_eur", "price_bm_uah",
  "price_marg_eur", "price_marg_uah",
  "price_dam_eur", "price_dam_uah", "volume_dam", "rate"
)

#' @param end_date Last day to include (default: end of previous month)
#' @param start_date Explicit start; default resumes from the existing CSV
#' @param rebuild If TRUE, rebuild the whole history from BM_UA_DEFAULT_START
task_bm_ua <- function(end_date = lubridate::floor_date(lubridate::today(), "month") - lubridate::days(1),
                       start_date = NULL,
                       rebuild = FALSE) {

  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(httr)
  library(rvest)
  library(readxl)
  library(stringr)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_bm_ua.R")

  bm_filepath  <- "data/data_raw/BM_UA.csv"
  dam_filepath <- "data/data_raw/DAM_UA.csv"
  default_start <- as.Date("2022-03-01")  # first month present in BM_UA.csv

  # ── Determine date range ────────────────────────────────────────────────
  if (is.null(start_date)) {
    start_date <- if (rebuild) {
      default_start
    } else {
      get_start_date_monthly(bm_filepath, date_col = "date",
                             default_start = default_start)
    }
  }
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (start_date > end_date) {
    message("  [SKIP] BM UA is up to date")
    return(list(task = "bm_ua", status = "skipped", message = "Up to date"))
  }

  message("  Downloading BM UA: ", start_date, " to ", end_date,
          if (rebuild) "  (full rebuild)" else "")

  # ── Download the two monthly datasets ───────────────────────────────────
  results_raw  <- download_bm_section("results",  start_date, end_date)
  marginal_raw <- download_bm_section("marginal", start_date, end_date)

  if (nrow(results_raw) == 0) {
    return(list(task = "bm_ua", status = "error",
                message = "No BM results data downloaded"))
  }
  if (nrow(marginal_raw) == 0)
    message("  [WARN] No marginal price data — those columns will be NA")

  # ── Load DAM data for join ──────────────────────────────────────────────
  if (!file.exists(dam_filepath)) {
    return(list(task = "bm_ua", status = "error",
                message = "DAM_UA.csv not found — run task_dam_ua first"))
  }

  dam_data <- read_csv(dam_filepath, show_col_types = FALSE) |>
    mutate(
      date = as_date(hour),
      hour = hour(as.POSIXct(hour, tz = "UTC"))
    ) |>
    select(country, date, hour,
           price_dam_eur = price_eur_mwh, price_dam_uah = price_uah,
           volume_dam = volume, rate) |>
    distinct(country, date, hour, .keep_all = TRUE)

  # ── Reshape results + marginal prices to long by direction ──────────────
  results_long <- results_raw |>
    pivot_longer(
      cols      = c(volume_up, price_up, volume_down, price_down),
      names_to  = c(".value", "direction"),
      names_sep = "_"
    ) |>
    rename(volume_bm = volume, price_bm_uah = price)

  bm <- results_long

  if (nrow(marginal_raw) > 0) {
    marginal_long <- marginal_raw |>
      pivot_longer(
        cols      = c(price_marg_up, price_marg_down),
        names_to  = "direction",
        names_prefix = "price_marg_",
        values_to = "price_marg_uah"
      )
    bm <- left_join(bm, marginal_long,
                    by = join_by(country, date, hour, direction),
                    relationship = "one-to-one")
  } else {
    bm <- mutate(bm, price_marg_uah = NA_real_)
  }

  # ── Join DAM prices and convert UAH → EUR ───────────────────────────────
  new_bm <- bm |>
    left_join(dam_data,
              by = join_by(country, date, hour),
              relationship = "many-to-one") |>
    mutate(
      price_bm_eur   = price_bm_uah / rate,
      price_marg_eur = price_marg_uah / rate
    ) |>
    # Drop hours with no balancing activation at all (zero volume, zero price)
    filter(!(coalesce(volume_bm, -1) == 0 & coalesce(price_bm_uah, -1) == 0)) |>
    select(all_of(BM_UA_COLS)) |>
    arrange(date, hour, direction)

  n_no_dam <- sum(is.na(new_bm$rate))
  if (n_no_dam > 0)
    message("  [WARN] ", n_no_dam, " row(s) have no matching DAM data — ",
            "run task_dam_ua() first to fill price_*_eur")

  # ── Merge with existing data ────────────────────────────────────────────
  if (file.exists(bm_filepath) && !rebuild) {
    existing <- read_csv(bm_filepath, show_col_types = FALSE)

    # Older files predate the marginal/imbalance columns — add them as NA
    for (col in setdiff(BM_UA_COLS, names(existing)))
      existing[[col]] <- NA_real_

    bm_all <- bind_rows(
      existing |> filter(as.Date(date) < start_date) |> select(all_of(BM_UA_COLS)),
      new_bm
    ) |>
      distinct(country, date, hour, direction, .keep_all = TRUE) |>
      arrange(date, hour, direction)
  } else {
    bm_all <- new_bm
  }

  # ── Sanity check before overwriting ─────────────────────────────────────
  if (file.exists(bm_filepath)) {
    old_rows <- nrow(read_csv(bm_filepath, show_col_types = FALSE, lazy = TRUE))
    if (nrow(bm_all) < old_rows * 0.95) {
      return(list(task = "bm_ua", status = "error",
                  message = paste0("Refusing to write: row count would drop ",
                                   old_rows, " -> ", nrow(bm_all))))
    }
  }

  # ── Write atomically ───────────────────────────────────────────────────
  tmp <- paste0(bm_filepath, ".tmp")
  write_csv(bm_all, tmp)
  file.rename(tmp, bm_filepath)
  message("  ✅ BM UA updated: ", nrow(bm_all), " total rows")

  return(list(task = "bm_ua", status = "success",
              message = paste0(nrow(new_bm), " new rows, ",
                               nrow(bm_all), " total")))
}

# Run if called directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  result <- task_bm_ua(rebuild = "--rebuild" %in% args)
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
