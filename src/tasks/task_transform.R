#============================================================================
# Task: Transform raw data into output datasets
# Produces: capture_factors_daily.csv, capture_factors_monthly.csv,
#           DAM weighted price UA.csv
#============================================================================

task_transform <- function() {
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  
  source("src/helpers/csv_utils.R")
  
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
                               ", wgt_price: ", v3$rows, " rows")))
}

if (sys.nframe() == 0) {
  result <- task_transform()
  message(result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
