#============================================================================
# Task: Download Ukrainian imbalance data from ua.energy
#
# Combines the two imbalance datasets published on the BM results page —
#   Сумарний небаланс електроенергії → imbalance volume (MWh)
#   Фактичні ціни небалансів         → payment price + actual price (UAH/MWh)
#
# Output: data/data_raw/Imbalance_UA.csv, long by direction — two rows per
# hour, `positive` and `negative`:
#
#   imbalance     volume of imbalance in that direction (MWh)
#   price         payment price for that direction (UAH/MWh)
#   price_actual  actual imbalance price (IMSP) — a single hourly value, so
#                 it repeats on both direction rows
#
# The two datasets publish on different schedules: volumes appear as whole
# months only, prices also as partial-month files within the current month.
# The join is a full join, so the most recent days carry prices with NA
# volumes until the monthly volume file is published.
#
# No VPN required, but ua.energy is behind Cloudflare — chromote (headless
# Chrome) is used for the page and for the download session cookies.
#============================================================================

IMBALANCE_UA_COLS <- c(
  "country", "date", "hour", "direction",
  "imbalance", "price", "price_actual"
)

IMBALANCE_UA_KEY <- c("country", "date", "hour", "direction")

#' Pivot the combined wide imbalance data to long by direction
#'
#' @param wide Tibble with imbalance_pos/imbalance_neg and
#'   price_positive/price_negative columns
#' @return Tibble with a `direction` column ("positive" / "negative")
pivot_imbalance_long <- function(wide) {
  wide |>
    dplyr::rename(imbalance_positive = "imbalance_pos",
                  imbalance_negative = "imbalance_neg") |>
    tidyr::pivot_longer(
      cols = c("imbalance_positive", "imbalance_negative",
               "price_positive", "price_negative"),
      names_to  = c(".value", "direction"),
      names_sep = "_"
    )
}

#' @param end_date Last day to include (default: today)
#' @param start_date Explicit start; default resumes from the existing CSV
#' @param rebuild If TRUE, rebuild the whole history from the default start
task_imbalance_ua <- function(end_date = lubridate::today(),
                              start_date = NULL,
                              rebuild = FALSE) {

  library(dplyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(httr)
  library(rvest)
  library(readxl)
  library(stringr)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_bm_ua.R")

  filepath <- "data/data_raw/Imbalance_UA.csv"
  # Both datasets use the current filename conventions from 2021 onwards;
  # 2022-01 matches the project-wide default start.
  default_start <- as.Date("2022-01-01")

  # ── Determine date range ────────────────────────────────────────────────
  if (is.null(start_date)) {
    start_date <- if (rebuild) {
      default_start
    } else {
      get_start_date_monthly(filepath, date_col = "date",
                             default_start = default_start)
    }
  }
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (start_date > end_date) {
    message("  [SKIP] Imbalance UA is up to date")
    return(list(task = "imbalance_ua", status = "skipped",
                message = "Up to date"))
  }

  message("  Downloading Imbalance UA: ", start_date, " to ", end_date,
          if (rebuild) "  (full rebuild)" else "")

  # ── Download both datasets ──────────────────────────────────────────────
  volumes <- download_bm_section("imbalance", start_date, end_date)
  prices  <- download_bm_section("imbprice",  start_date, end_date)

  if (nrow(volumes) == 0 && nrow(prices) == 0) {
    return(list(task = "imbalance_ua", status = "error",
                message = "No imbalance data downloaded"))
  }
  if (nrow(volumes) == 0)
    message("  [WARN] No imbalance volumes — those columns will be NA")
  if (nrow(prices) == 0)
    message("  [WARN] No imbalance prices — those columns will be NA")

  # ── Combine ─────────────────────────────────────────────────────────────
  if (nrow(volumes) == 0) {
    new_data <- prices |>
      mutate(imbalance_pos = NA_real_, imbalance_neg = NA_real_)
  } else if (nrow(prices) == 0) {
    new_data <- volumes |>
      mutate(price_actual = NA_real_, price_positive = NA_real_,
             price_negative = NA_real_)
  } else {
    # Full join: prices run further into the current month than volumes
    new_data <- full_join(volumes, prices,
                          by = join_by(country, date, hour),
                          relationship = "one-to-one")
  }

  n_no_vol   <- sum(is.na(new_data$imbalance_pos))
  n_no_price <- sum(is.na(new_data$price_actual))
  if (n_no_vol > 0)
    message("  ", n_no_vol, " hour(s) have prices but no volumes yet ",
            "(monthly volume file not published)")
  if (n_no_price > 0)
    message("  ", n_no_price, " hour(s) have volumes but no prices")

  # ── Pivot to long by direction ──────────────────────────────────────────
  new_data <- new_data |>
    pivot_imbalance_long() |>
    filter(date >= start_date, date <= end_date) |>
    select(all_of(IMBALANCE_UA_COLS)) |>
    arrange(date, hour, direction)

  # ── Merge with existing data ────────────────────────────────────────────
  if (file.exists(filepath) && !rebuild) {
    existing <- read_csv(filepath, show_col_types = FALSE)
    for (col in setdiff(IMBALANCE_UA_COLS, names(existing)))
      existing[[col]] <- NA_real_

    combined <- bind_rows(
      existing |> filter(as.Date(date) < start_date) |>
        select(all_of(IMBALANCE_UA_COLS)),
      new_data
    ) |>
      distinct(across(all_of(IMBALANCE_UA_KEY)), .keep_all = TRUE) |>
      arrange(date, hour, direction)
  } else {
    combined <- new_data
  }

  # ── Sanity check before overwriting ─────────────────────────────────────
  if (file.exists(filepath)) {
    old_rows <- nrow(read_csv(filepath, show_col_types = FALSE, lazy = TRUE))
    if (nrow(combined) < old_rows * 0.95) {
      return(list(task = "imbalance_ua", status = "error",
                  message = paste0("Refusing to write: row count would drop ",
                                   old_rows, " -> ", nrow(combined))))
    }
  }

  # ── Write atomically ───────────────────────────────────────────────────
  tmp <- paste0(filepath, ".tmp")
  write_csv(combined, tmp)
  file.rename(tmp, filepath)
  message("  ✅ Imbalance UA updated: ", nrow(combined), " total rows")

  return(list(task = "imbalance_ua", status = "success",
              message = paste0(nrow(new_data), " new rows, ",
                               nrow(combined), " total")))
}

# Run if called directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  result <- task_imbalance_ua(rebuild = "--rebuild" %in% args)
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
