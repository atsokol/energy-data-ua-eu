#============================================================================
# Task: Transform raw data into output datasets
# Produces: capture_factors_daily.csv, capture_factors_monthly.csv,
#           DAM weighted price UA.csv,
#           BM_DAM_spread.parquet, spread_daily.csv
#============================================================================

task_transform <- function() {
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(glue)
  library(httr)

  source("src/helpers/csv_utils.R")
  source("src/helpers/transform_utils.R")
  
  message("  Loading raw data...")
  
  # ── Load EU data ───────────────────────────────────────────────────────────
  gen_eu   <- read_csv("data/data_raw/yield_RES_EU.csv", show_col_types = FALSE)
  price_eu <- read_csv("data/data_raw/DAM_EU.csv", show_col_types = FALSE)
  
  data_eu <- gen_eu |>
    left_join(price_eu, by = c("country", "hour"), relationship = "many-to-one")
  
  # ── Load UA data ───────────────────────────────────────────────────────────
  price_ua <- read_csv("data/data_raw/DAM_UA.csv", show_col_types = FALSE)
  solar_ua <- read_csv("data/data_raw/yield_solar_UA.csv", show_col_types = FALSE)
  wind_ua  <- read_csv("data/data_raw/yield_wind_UA.csv", show_col_types = FALSE)
  
  gen_ua <- rbind(
    solar_ua |> mutate(type = "Solar"),
    wind_ua  |> mutate(type = "Wind onshore")
  ) |>
    transmute(
      hour = ymd_h(paste(format(date, "%Y-%m-%d"), .data$hour - 1), tz = "UTC"),
      type = type,
      gen_mw = if_else(actual < 0, 0, actual)
    )
  
  data_ua <- left_join(
    gen_ua, price_ua,
    by = c("hour" = "hour"),
    relationship = "many-to-one"
  ) |>
    rename(tech = type) |>
    filter(!is.na(gen_mw), !is.na(price_eur_mwh)) |>
    mutate(date = as_date(hour)) |>
    select(country, hour, tech, gen_mw, price_eur = price_eur_mwh, volume)
  
  data_all <- rbind(data_eu, select(data_ua, -volume))
  
  # ── Calculate capture factors ──────────────────────────────────────────────
  message("  Calculating capture factors...")
  
  factor_d <- data_all |>
    mutate(date = floor_date(hour, unit = "days")) |>
    group_by(country, tech, date) |>
    summarise(
      price_res  = sum(price_eur * gen_mw, na.rm = TRUE) / sum(gen_mw, na.rm = TRUE),
      price_base = mean(price_eur, na.rm = TRUE),
      cap_factor = price_res / price_base,
      .groups = "drop"
    )
  
  factor_m <- factor_d |>
    group_by(country, tech, date = floor_date(date, unit = "months")) |>
    summarise(cap_factor = median(cap_factor, na.rm = TRUE), .groups = "drop")
  
  write_csv(factor_d, "data/data_output/capture_factors_daily.csv")
  write_csv(factor_m, "data/data_output/capture_factors_monthly.csv")
  
  # ── Calculate weighted average DAM prices ──────────────────────────────────
  message("  Calculating weighted DAM prices...")
  
  price_wgt <- price_ua |>
    mutate(date = as_date(hour, tz = "UTC")) |>
    group_by(country, date) |>
    summarise(
      price_uah = sum(price_uah * volume, na.rm = TRUE) / sum(volume, na.rm = TRUE),
      price_eur = sum(price_eur_mwh * volume, na.rm = TRUE) / sum(volume, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(price_wgt, "data/data_output/DAM weighted price UA.csv")
  
  # ── Calculate EU BM-DAM spreads ────────────────────────────────────────────
  bm_spread_msg <- "skipped (missing BM EU files)"
  bm_eu_files   <- c("data/data_raw/BM_EU.csv",
                     "data/data_raw/BM_EU_vol.csv",
                     "data/data_raw/DAM_EU_15m.csv")

  if (all(file.exists(bm_eu_files))) {
    message("  Calculating EU BM-DAM spreads...")

    price_bm <- read_csv("data/data_raw/BM_EU.csv",     show_col_types = FALSE)
    vol_bm   <- read_csv("data/data_raw/BM_EU_vol.csv", show_col_types = FALSE)
    price_dam_15m <- read_csv("data/data_raw/DAM_EU_15m.csv", show_col_types = FALSE) |>
      standardize_dam_to_15min() |>
      rename(price_dam_eur = price_eur)

    price_bm <- price_bm |>
      convert_to_eur(
        date_col     = "datetime",
        currency_col = "currency",
        price_col    = "price",
        end_date     = as.character(today())
      ) |>
      rename(price_bm_eur = price_eur) |>
      mutate(
        country_code = case_when(
          str_detect(country, "Poland")          ~ "PL",
          str_detect(country, "Hungary")         ~ "HU",
          str_detect(country, "Romania")         ~ "RO",
          str_detect(country, "Slovak Republic") ~ "SK"
        )
      ) |>
      left_join(vol_bm |> select(-reserve_type),
                by = c("country", "datetime", "direction"),
                relationship = "many-to-many") |>
      left_join(price_dam_15m,
                by = c("datetime" = "hour", "country_code" = "country"),
                relationship = "many-to-many") |>
      mutate(energy_activated = replace_na(energy_activated, 0))

    spread_hourly <- price_bm |>
      filter(reserve_type == "Automatic frequency restoration reserve") |>
      mutate(
        spread = ifelse(direction == "UP",
                        price_bm_eur - price_dam_eur,
                        price_dam_eur - price_bm_eur),
        activation_share = if_else(
          capacity_offered == 0 | is.na(capacity_offered),
          NA_real_,
          energy_activated / capacity_offered
        )
      )

    spread_daily_bm <- price_bm |>
      filter(reserve_type == "Automatic frequency restoration reserve") |>
      mutate(spread = ifelse(direction == "UP",
                             price_bm_eur - price_dam_eur,
                             price_dam_eur - price_bm_eur)) |>
      group_by(date = floor_date(datetime, "day"), country, direction) |>
      summarise(
        spread_wave      = sum(spread * energy_activated, na.rm = TRUE) / sum(energy_activated),
        spread_ave       = mean(spread, na.rm = TRUE),
        activation_share = sum(energy_activated, na.rm = TRUE) / sum(capacity_offered, na.rm = TRUE),
        revenue_per_mw   = sum(spread * energy_activated, na.rm = TRUE) / sum(capacity_offered, na.rm = TRUE),
        .groups = "drop"
      )

    arrow::write_parquet(spread_hourly,  "data/data_output/BM_DAM_spread.parquet")
    write_csv(spread_daily_bm, "data/data_output/spread_daily.csv")

    bm_spread_msg <- paste0(nrow(spread_hourly), " hourly, ",
                            nrow(spread_daily_bm), " daily rows")
    message("  BM-DAM spread: ", bm_spread_msg)
  } else {
    missing <- bm_eu_files[!file.exists(bm_eu_files)]
    message("  [SKIP] BM-DAM spread — missing: ", paste(basename(missing), collapse = ", "))
  }

  # ── Validate outputs ──────────────────────────────────────────────────────
  v1 <- validate_csv("data/data_output/capture_factors_daily.csv",
                     expected_cols = c("country", "tech", "date", "cap_factor"))
  v2 <- validate_csv("data/data_output/capture_factors_monthly.csv",
                     expected_cols = c("country", "tech", "date", "cap_factor"))
  v3 <- validate_csv("data/data_output/DAM weighted price UA.csv",
                     expected_cols = c("country", "date", "price_uah", "price_eur"))

  all_ok <- all(v1$ok, v2$ok, v3$ok)

  for (v in list(v1, v2, v3)) {
    message("  [", if (v$ok) "OK" else "FAIL", "] ", v$message)
  }

  status <- if (all_ok) "success" else "error"
  return(list(task = "transform", status = status,
              message = paste0("daily: ", v1$rows, ", monthly: ", v2$rows,
                               ", wgt_price: ", v3$rows,
                               ", bm_spread: ", bm_spread_msg)))
}

if (sys.nframe() == 0) {
  result <- task_transform()
  message(result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
