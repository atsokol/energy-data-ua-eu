#============================================================================
# Utility functions for data transformation
# Ported from RES analysis/src/helper_func_EU.R
#============================================================================

# Download daily exchange rates from ECB Data Portal
# Returns tibble with columns: date, rate (units: 1 EUR = rate * currency)
get_ecb_exchange_rate <- function(currency_code, start_date, end_date) {
  url <- glue::glue(
    "https://data-api.ecb.europa.eu/service/data/EXR/D.{currency_code}.EUR.SP00.A",
    "?startPeriod={start_date}&endPeriod={end_date}&format=csvdata"
  )

  tryCatch({
    response <- httr::GET(url)
    if (httr::http_error(response)) {
      stop("ECB API request failed for ", currency_code, ": ", httr::status_code(response))
    }
    csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
    fx_data  <- read.csv(text = csv_text, stringsAsFactors = FALSE)
    fx_data |>
      dplyr::transmute(
        date = as.Date(TIME_PERIOD),
        rate = as.numeric(OBS_VALUE)
      ) |>
      dplyr::filter(!is.na(rate), !is.na(date)) |>
      dplyr::arrange(date)
  }, error = function(e) {
    message("Error downloading ECB rates for ", currency_code, ": ", e$message)
    tibble::tibble(date = as.Date(character()), rate = numeric())
  })
}

# Convert a price column in df to EUR using ECB exchange rates.
# Rows already in EUR are left unchanged.
convert_to_eur <- function(df,
                           date_col     = "hour",
                           currency_col = "currency",
                           price_col    = "price",
                           start_date   = "2022-01-01",
                           end_date     = as.character(lubridate::today())) {
  currencies_to_convert <- unique(df[[currency_col]])
  currencies_to_convert <- currencies_to_convert[currencies_to_convert != "EUR"]

  fx_start_date <- as.character(as.Date(start_date) - lubridate::days(5))

  exchange_rates <- purrr::map_df(currencies_to_convert, function(curr) {
    message("    Downloading ECB rates for ", curr)
    get_ecb_exchange_rate(curr, fx_start_date, end_date) |>
      dplyr::mutate(currency = curr)
  }) |>
    dplyr::group_by(currency) |>
    tidyr::complete(date = seq(min(date), max(date), by = "1 day")) |>
    tidyr::fill(rate, .direction = "down") |>
    dplyr::ungroup()

  df |>
    dplyr::mutate(date = as.Date(.data[[date_col]])) |>
    dplyr::left_join(exchange_rates, by = c(currency_col, "date")) |>
    dplyr::mutate(
      price_eur = dplyr::if_else(
        .data[[currency_col]] == "EUR",
        .data[[price_col]],
        .data[[price_col]] / rate
      )
    ) |>
    dplyr::select(-date, -rate)
}

# Expand hourly DAM prices to 15-minute resolution by repeating each hourly
# value as four identical 15-minute slots.  Rows already at sub-hourly
# resolution are passed through unchanged.
standardize_dam_to_15min <- function(df,
                                     time_col   = "hour",
                                     value_cols = "price_eur") {
  group_cols <- setdiff(names(df), time_col)

  df_parsed <- df |>
    dplyr::mutate(
      hour_parsed = lubridate::ymd_hms(.data[[time_col]], tz = "UTC", quiet = TRUE)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::arrange(hour_parsed, .by_group = TRUE) |>
    dplyr::mutate(
      time_diff = as.numeric(difftime(hour_parsed, dplyr::lag(hour_parsed), units = "mins"))
    ) |>
    dplyr::ungroup()

  hourly_obs    <- df_parsed |> dplyr::filter(is.na(time_diff) | time_diff >= 60)
  subhourly_obs <- df_parsed |> dplyr::filter(!is.na(time_diff) & time_diff < 60)

  if (nrow(hourly_obs) > 0) {
    hourly_expanded <- hourly_obs |>
      dplyr::select(-time_diff, -!!rlang::sym(time_col)) |>
      dplyr::mutate(
        hour_list = purrr::map(hour_parsed, ~ .x + lubridate::minutes(c(0, 15, 30, 45)))
      ) |>
      tidyr::unnest(hour_list) |>
      dplyr::select(-hour_parsed) |>
      dplyr::rename(!!rlang::sym(time_col) := hour_list)
  } else {
    hourly_expanded <- tibble::tibble()
  }

  dplyr::bind_rows(
    hourly_expanded,
    subhourly_obs |> dplyr::select(-time_diff, -hour_parsed)
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(setdiff(names(df), c(time_col, value_cols)), time_col)
    )))
}
