#============================================================================
# Download functions for Ukrainian data sources (OREE, NBU, GPEE)
# Pure functions — no side effects, no file I/O
#============================================================================

#' Download UAH/EUR exchange rates from NBU
#' 
#' Fetches data from 7 days before start_date to handle weekends/holidays,
#' then fills forward missing days.
#'
#' @param start_date Start date
#' @param end_date End date
#' @param valcode Currency code (default "EUR")
#' @return Tibble with date, rate columns (complete daily series)
get_nbu_fx <- function(start_date, end_date, valcode = "EUR") {
  # Fetch 7 days early to ensure we can fill weekends at the start

  fetch_start <- start_date - lubridate::days(7)
  
  url <- glue::glue(
    "https://bank.gov.ua/NBU_Exchange/exchange_site",
    "?start={format(fetch_start, '%Y%m%d')}",
    "&end={format(end_date, '%Y%m%d')}",
    "&valcode={valcode}",
    "&sort=exchangedate&order=desc&json"
  )
  
  resp <- httr::GET(url)
  
  if (httr::http_error(resp)) {
    stop("NBU FX request failed: ", httr::status_code(resp))
  }
  
  raw <- httr::content(resp, "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      date = lubridate::dmy(exchangedate),
      rate = as.numeric(rate)
    ) |>
    dplyr::arrange(date)
  
  if (nrow(raw) == 0) {
    warning("NBU returned no FX data for ", valcode, " (", start_date, " to ", end_date, ")")
    return(dplyr::tibble(date = as.Date(character()), rate = numeric()))
  }
  
  # Fill forward for weekends/holidays and trim to requested range
  raw |>
    tidyr::complete(date = seq(min(raw$date), end_date, by = "day")) |>
    tidyr::fill(rate, .direction = "down") |>
    dplyr::filter(date >= start_date, date <= end_date)
}

#' Fetch a single day of OREE day-ahead market data
#' 
#' @param date Date to fetch
#' @param market Market type (default "DAM")
#' @param zone Zone number (default 2)
#' @return Tibble with date, hour_num, price, volume columns, or empty tibble
fetch_oree_day <- purrr::possibly(function(date, market = "DAM", zone = 2) {
  url <- glue::glue(
    "https://www.oree.com.ua/index.php/PXS/get_pxs_hdata/",
    "{format(date, '%d.%m.%Y')}/{market}/{zone}"
  )
  
  resp <- httr::GET(url)
  
  if (httr::http_error(resp)) {
    message("  No data for ", date, " (HTTP error)")
    return(dplyr::tibble())
  }
  
  txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt)
  
  dplyr::tibble(
    date     = date,
    hour_num = as.integer(json$labels),
    price    = as.numeric(json$pricesData),
    volume   = as.numeric(json$amountsData)
  )
}, otherwise = dplyr::tibble())

#' Download Ukrainian DAM data for a date range
#' 
#' @param start_date Start date
#' @param end_date End date
#' @return Tibble with country, hour, date, price_uah, volume columns
download_dam_ua <- function(start_date, end_date) {
  dates <- seq(start_date, end_date, by = "day")
  
  raw <- purrr::map_dfr(dates, \(d) fetch_oree_day(d, market = "DAM", zone = 2))
  
  if (nrow(raw) == 0) {
    warning("No DAM data returned for ", start_date, " to ", end_date)
    return(dplyr::tibble(
      country = character(), hour = as.POSIXct(character()),
      date = as.Date(character()), price_uah = numeric(), volume = numeric()
    ))
  }
  
  raw |>
    dplyr::transmute(
      country = "UA",
      hour = lubridate::ymd_h(
        paste(format(date, "%Y-%m-%d"), hour_num - 1L), tz = "UTC"
      ),
      date = date,
      price_uah = price,
      volume = volume
    )
}

#' Download solar or wind yield data for a single day from GPEE
#'
#' @param date Date to fetch
#' @param gen Generation type: "1" = solar, "2" = wind
#' @return Data frame with date, hour, actual, projected columns, or NULL
download_yield_day <- function(date, gen = "1") {
  params <- list(date = as.character(date), zona = "1", gen = as.character(gen))
  
  response <- httr::POST(
    "https://www.gpee.com.ua/main/loadCharts",
    body = params, encode = "form"
  )
  
  if (httr::status_code(response) != 200) {
    warning("GPEE returned ", httr::status_code(response), " for ", date)
    return(NULL)
  }
  
  data_raw <- httr::content(response, as = "text", encoding = "UTF-8")
  vec <- jsonlite::fromJSON(data_raw, flatten = TRUE)
  
  data.frame(
    date = date,
    hour = seq(1, 24),
    actual = as.numeric(vec[1:24]),
    projected = as.numeric(vec[26:49])
  )
}

#' Download yield data for a list of dates (with error handling per day)
#'
#' @param dates Vector of dates
#' @param gen Generation type: "1" = solar, "2" = wind
#' @return List of data frames (NULLs for failed days)
download_yield_list <- function(dates, gen) {
  lapply(dates, function(day) {
    tryCatch(
      download_yield_day(date = day, gen = gen),
      error = function(e) {
        message("  Error downloading yield for ", day, ": ", conditionMessage(e))
        return(NULL)
      }
    )
  })
}
