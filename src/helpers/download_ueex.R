#============================================================================
# Download functions for UEEX natural gas exchange quotations
# Source: https://www.ueex.com.ua/exchange-quotations/natural-gas/standardized-products/
# Pure functions — no side effects, no file I/O
#============================================================================

#' Fetch one month of UEEX natural gas standardized product quotations
#'
#' @param year  Integer year (e.g. 2025)
#' @param month Integer month 1–12
#' @param product_tab Character tab identifier: "wnd" (Within-Day, default),
#'   "da" (Day-Ahead), "w" (Week), "mo" (Month). Only "wnd" has data.
#' @return Tibble with date, product, supply_conditions, volume_tcm,
#'   price_uah_excl_vat, price_uah_incl_vat; empty tibble on failure.
fetch_ueex_gas_month <- function(year, month, product_tab = NULL) {

  month_str <- sprintf("%02d.%d", month, year)

  url <- if (is.null(product_tab)) {
    glue::glue(
      "https://www.ueex.com.ua/exchange-quotations/natural-gas/",
      "standardized-products/?m={month_str}"
    )
  } else {
    glue::glue(
      "https://www.ueex.com.ua/exchange-quotations/natural-gas/",
      "standardized-products/?t={product_tab}&m={month_str}"
    )
  }

  resp <- tryCatch(
    httr::GET(
      url,
      httr::timeout(30),
      httr::user_agent("Mozilla/5.0 (compatible; R data pipeline)")
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::http_error(resp)) {
    message("  [WARN] UEEX: HTTP error for ", month_str)
    return(dplyr::tibble())
  }

  html <- httr::content(resp, as = "text", encoding = "UTF-8") |>
    xml2::read_html()

  # Find all tables; take the first one that has 6 columns
  tables <- rvest::html_elements(html, "table")

  if (length(tables) == 0) {
    message("  [WARN] UEEX: no tables found for ", month_str)
    return(dplyr::tibble())
  }

  parsed <- NULL
  for (tbl in tables) {
    rows <- rvest::html_elements(tbl, "tr")
    if (length(rows) < 2) next  # need header + at least one data row

    # Count columns from header row
    header_cells <- rvest::html_elements(rows[[1]], "th, td")
    if (length(header_cells) < 5) next  # expect at least 5 columns

    df <- tryCatch(rvest::html_table(tbl, fill = TRUE), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      parsed <- df
      break
    }
  }

  if (is.null(parsed) || nrow(parsed) == 0) {
    # No data for this month (normal for future/empty periods)
    return(dplyr::tibble())
  }

  # Standardise column names regardless of Ukrainian header text
  # Expected order: date | product | supply_conditions | volume | price_excl | price_incl
  if (ncol(parsed) < 6) {
    message("  [WARN] UEEX: unexpected column count (", ncol(parsed), ") for ", month_str)
    return(dplyr::tibble())
  }

  names(parsed)[1:6] <- c(
    "date_raw", "product", "supply_conditions",
    "volume_tcm", "price_uah_excl_vat", "price_uah_incl_vat"
  )

  parsed |>
    dplyr::as_tibble() |>
    dplyr::select(date_raw, product, supply_conditions,
                  volume_tcm, price_uah_excl_vat, price_uah_incl_vat) |>
    # Drop header-repeat rows (cells equal to column name or empty)
    dplyr::filter(
      !is.na(date_raw),
      nchar(trimws(date_raw)) > 0,
      grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", trimws(date_raw))
    ) |>
    dplyr::mutate(
      date               = lubridate::dmy(trimws(date_raw)),
      product            = trimws(product),
      supply_conditions  = trimws(supply_conditions),
      # Site uses Ukrainian/European format: space as thousands, comma as decimal
      # e.g. "22 748,97" = 22748.97;  "14,00" = 14.00
      # Strip any spaces (incl. non-breaking U+00A0), then swap comma → period
      volume_tcm         = as.numeric(
        gsub(",", ".", gsub("[\u00a0 ]", "", as.character(volume_tcm)))),
      price_uah_excl_vat = as.numeric(
        gsub(",", ".", gsub("[\u00a0 ]", "", as.character(price_uah_excl_vat)))),
      price_uah_incl_vat = as.numeric(
        gsub(",", ".", gsub("[\u00a0 ]", "", as.character(price_uah_incl_vat))))
    ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::select(date, product, supply_conditions,
                  volume_tcm, price_uah_excl_vat, price_uah_incl_vat) |>
    dplyr::arrange(date)
}


#' Download UEEX natural gas quotations for a date range
#'
#' Iterates month-by-month from start_date to end_date.
#'
#' @param start_date Start date (uses the month it falls in)
#' @param end_date   End date   (uses the month it falls in)
#' @return Combined tibble for all months; empty tibble if nothing downloaded
download_ueex_gas <- function(start_date, end_date) {

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  # Build sequence of year-month pairs to fetch
  months_seq <- seq(
    lubridate::floor_date(start_date, "month"),
    lubridate::floor_date(end_date,   "month"),
    by = "month"
  )

  results <- purrr::map(months_seq, function(m) {
    yr <- lubridate::year(m)
    mo <- lubridate::month(m)
    message("  Fetching UEEX gas: ", sprintf("%02d/%d", mo, yr))
    Sys.sleep(0.5)  # polite crawling
    fetch_ueex_gas_month(yr, mo)
  })

  combined <- dplyr::bind_rows(results)

  if (nrow(combined) == 0) return(dplyr::tibble())

  combined |>
    dplyr::filter(date >= start_date, date <= end_date) |>
    dplyr::arrange(date)
}
