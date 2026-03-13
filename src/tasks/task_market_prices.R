#============================================================================
# Task: Download TTF gas and EUA carbon prices from Yahoo Finance
#   - TTF=F  → data/data_raw/ttf_daily_yahoo.csv
#   - CO2.L + GBPEUR=X → data/data_raw/eua_daily_yahoo.csv
#============================================================================

task_market_prices <- function(end_date = lubridate::today() - lubridate::days(1)) {

  library(dplyr)
  library(readr)
  library(lubridate)
  library(quantmod)
  library(tibble)

  source("src/config.R")
  source("src/helpers/csv_utils.R")
  source("src/helpers/download_ua.R")

  results <- list()

  # ── TTF Natural Gas Futures (TTF=F) ────────────────────────────────────────

  ttf_path  <- "data/data_raw/ttf_daily_yahoo.csv"
  ttf_start <- get_start_date(ttf_path, datetime_col = "date",
                               default_start = as.Date("2022-01-01"))

  if (ttf_start > end_date) {
    message("  [SKIP] TTF gas price is up to date")
    results$ttf <- list(task = "ttf_gas", status = "skipped", message = "Up to date")
  } else {
    message("  Downloading TTF=F: ", ttf_start, " to ", end_date)
    results$ttf <- tryCatch({

      ttf_raw <- getSymbols("TTF=F", src = "yahoo", auto.assign = FALSE,
                            from = as.character(ttf_start))

      ttf_new <- data.frame(
        date  = index(ttf_raw),
        price = as.numeric(Cl(ttf_raw))
      ) |>
        as_tibble() |>
        filter(!is.na(price), as.Date(date) <= end_date) |>
        arrange(date)

      if (nrow(ttf_new) == 0) {
        message("  [WARN] TTF=F: no new data returned")
        list(task = "ttf_gas", status = "warning", message = "No data returned")
      } else {
        # Enrich with NBU FX rates for UAH price calculation
        fx_eur <- get_nbu_fx(min(ttf_new$date), max(ttf_new$date), valcode = "EUR")
        fx_usd <- get_nbu_fx(min(ttf_new$date), max(ttf_new$date), valcode = "USD")

        ttf_enriched <- ttf_new |>
          left_join(fx_eur |> rename(rate_eur = rate), by = "date") |>
          left_join(fx_usd |> rename(rate_usd = rate), by = "date") |>
          mutate(
            price_no_vat  = (price + TTF_TRANSPORT_EUR_MWH) * TTF_MWH_TO_MCM * rate_eur +
              TTF_ENTRY_TARIFF_USD * TTF_TARIFF_MULTIPLIER * rate_usd,
            price_with_vat = price_no_vat * TTF_VAT_RATE
          ) |>
          select(date, price_eur_mwh = price, rate_eur, rate_usd,
                 price_no_vat, price_with_vat)

        ok <- update_csv(ttf_enriched, ttf_path, ttf_start, end_date,
                         datetime_col = "date")
        status <- if (ok) "success" else "error"
        list(task = "ttf_gas", status = status,
             message = paste0(nrow(ttf_enriched), " rows"))
      }
    }, error = function(e) {
      message("  [ERROR] TTF=F: ", e$message)
      list(task = "ttf_gas", status = "error", message = e$message)
    })
  }

  # ── EUA Carbon price (CO2.L + GBPEUR=X) ───────────────────────────────────
  # CO2.L (Invesco EUA ETC, LSE) is priced in GBp; 1 unit = 0.01 EUA
  # => price_eur = CO2.L (GBp) * GBPEUR rate

  eua_path  <- "data/data_raw/eua_daily_yahoo.csv"
  eua_start <- get_start_date(eua_path, datetime_col = "date",
                               default_start = as.Date("2022-01-01"))

  if (eua_start > end_date) {
    message("  [SKIP] EUA carbon price is up to date")
    results$eua <- list(task = "eua_carbon", status = "skipped", message = "Up to date")
  } else {
    message("  Downloading CO2.L + GBPEUR=X: ", eua_start, " to ", end_date)
    results$eua <- tryCatch({

      eua_raw   <- getSymbols("CO2.L",    src = "yahoo", auto.assign = FALSE,
                              from = as.character(eua_start))
      fx_gbpeur <- getSymbols("GBPEUR=X", src = "yahoo", auto.assign = FALSE,
                              from = as.character(eua_start))

      eua_new <- merge(Cl(eua_raw), Cl(fx_gbpeur)) |>
        as.data.frame() |>
        rownames_to_column("date") |>
        transmute(
          date      = as.Date(date),
          price_eur = `CO2.L.Close` * `GBPEUR.X.Close`
        ) |>
        as_tibble() |>
        filter(!is.na(price_eur), date <= end_date) |>
        arrange(date)

      if (nrow(eua_new) == 0) {
        message("  [WARN] EUA: no new data returned")
        list(task = "eua_carbon", status = "warning", message = "No data returned")
      } else {
        ok <- update_csv(eua_new, eua_path, eua_start, end_date,
                         datetime_col = "date")
        status <- if (ok) "success" else "error"
        list(task = "eua_carbon", status = status,
             message = paste0(nrow(eua_new), " rows"))
      }
    }, error = function(e) {
      message("  [ERROR] EUA: ", e$message)
      list(task = "eua_carbon", status = "error", message = e$message)
    })
  }

  # ── Summary ────────────────────────────────────────────────────────────────
  statuses <- sapply(results, \(r) r$status)
  n_err  <- sum(statuses == "error")
  n_warn <- sum(statuses == "warning")
  overall <- if (n_err > 0) "error" else if (n_warn > 0) "warning" else "success"

  list(
    task    = "market_prices",
    status  = overall,
    message = paste0(
      sum(statuses == "success"), " ok, ",
      sum(statuses == "skipped"), " skipped, ",
      n_warn, " warnings, ", n_err, " errors"
    ),
    details = results
  )
}

if (sys.nframe() == 0) {
  result <- task_market_prices()
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
