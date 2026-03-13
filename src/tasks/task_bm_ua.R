#============================================================================
# Task: Download Ukrainian Balancing Market data from ua.energy
#
# REQUIRES VPN (Ukrainian IP) — ua.energy is geo-blocked by Cloudflare.
# This task is NOT included in the GitHub Actions workflow.
# Run locally via:  Rscript src/run_local_vpn.R
#============================================================================

task_bm_ua <- function(end_date = lubridate::floor_date(lubridate::today(), "month") - lubridate::days(1)) {

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
  source("src/helpers/download_ua.R")
  source("src/helpers/download_bm_ua.R")

  bm_filepath  <- "data/data_raw/BM_UA.csv"
  dam_filepath <- "data/data_raw/DAM_UA.csv"

  # ── Check connectivity ──────────────────────────────────────────────────
  if (!check_ua_energy_access()) {
    return(list(
      task = "bm_ua", status = "error",
      message = "ua.energy not reachable — activate Ukrainian VPN first"
    ))
  }

  # ── Determine date range ────────────────────────────────────────────────
  start_date <- get_start_date_monthly(bm_filepath, date_col = "date",
                                       default_start = as.Date("2022-03-01"))

  if (start_date > end_date) {
    message("  [SKIP] BM UA is up to date")
    return(list(task = "bm_ua", status = "skipped", message = "Up to date"))
  }

  message("  Downloading BM UA: ", start_date, " to ", end_date)

  # ── Scrape xlsx URLs ────────────────────────────────────────────────────
  all_urls <- get_bm_urls()

  if (length(all_urls) == 0) {
    return(list(task = "bm_ua", status = "error",
                message = "No xlsx links found on BM results page"))
  }

  # Filter URLs for months >= start_date
  new_urls <- all_urls[vapply(all_urls, function(url) {
    file_date <- extract_bm_date(basename(url))
    !is.na(file_date) && file_date >= start_date
  }, logical(1))]

  message("  Found ", length(new_urls), " file(s) to download (from ",
          length(all_urls), " total)")

  if (length(new_urls) == 0) {
    return(list(task = "bm_ua", status = "skipped",
                message = "No new files to download"))
  }

  # ── Download and parse xlsx files ───────────────────────────────────────
  new_bm_raw <- purrr::map_dfr(new_urls, download_bm_file)

  if (nrow(new_bm_raw) == 0) {
    return(list(task = "bm_ua", status = "error",
                message = "All xlsx downloads/parses failed"))
  }

  # ── Load DAM data for join ──────────────────────────────────────────────
  if (!file.exists(dam_filepath)) {
    return(list(task = "bm_ua", status = "error",
                message = "DAM_UA.csv not found — run task_dam_ua first"))
  }

  dam_data <- read_csv(dam_filepath, show_col_types = FALSE) |>
    mutate(
      date     = as_date(hour),
      hour_num = hour(as.POSIXct(hour, tz = "UTC"))
    ) |>
    select(country, date, hour_num,
           price_dam_eur = price_eur_mwh, price_dam_uah = price_uah,
           volume_dam = volume, rate)

  # ── Process BM data ────────────────────────────────────────────────────
  new_bm <- new_bm_raw |>
    mutate(
      date     = as_date(hour),
      hour_num = hour(hour)
    ) |>
    pivot_longer(
      cols = c(volume_up, price_up, volume_down, price_down),
      names_to = c(".value", "direction"),
      names_sep = "_"
    ) |>
    rename(price_bm_uah = price, volume_bm = volume) |>
    left_join(dam_data,
              by = join_by(country, date, hour_num),
              relationship = "many-to-one") |>
    mutate(
      price_bm_eur = price_bm_uah / rate,
      .before = price_bm_uah
    ) |>
    filter(!(volume_bm == 0 & price_bm_eur == 0)) |>
    select(-hour) |>
    rename(hour = hour_num) |>
    relocate(country, date, hour, direction)

  # ── Merge with existing data ────────────────────────────────────────────
  if (file.exists(bm_filepath)) {
    existing <- read_csv(bm_filepath, show_col_types = FALSE)
    # Keep existing rows before start_date, add new
    bm_all <- bind_rows(
      existing |> filter(date < start_date),
      new_bm
    ) |>
      distinct(country, date, hour, direction, .keep_all = TRUE) |>
      arrange(date, hour, direction)
  } else {
    bm_all <- new_bm |> arrange(date, hour, direction)
  }

  # ── Write atomically ───────────────────────────────────────────────────
  tmp <- paste0(bm_filepath, ".tmp")
  write_csv(bm_all, tmp)
  file.rename(tmp, bm_filepath)
  message("  \u2705 BM UA updated: ", nrow(bm_all), " total rows")

  return(list(task = "bm_ua", status = "success",
              message = paste0(nrow(new_bm), " new rows, ",
                               nrow(bm_all), " total")))
}

# Run if called directly
if (sys.nframe() == 0) {
  result <- task_bm_ua()
  message("\n", result$task, ": ", result$status, " \u2014 ", result$message)
  if (result$status == "error") quit(status = 1)
}
