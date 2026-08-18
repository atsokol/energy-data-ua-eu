#============================================================================
# PSE S.A. (Poland) API download functions
# Base URL : https://api.raporty.pse.pl/api/
# Auth     : none — requires Accept: application/json header
# Coverage : 2024-09-27 onwards (new portal launch date)
# Resolution: 15-minute intervals (96 records per day)
#
# OData filter syntax:
#   $filter=business_date ge '2024-09-27' and business_date le '2024-09-30'
# Pagination: follow `nextLink` field in each response
#============================================================================

PSE_API_BASE <- "https://api.raporty.pse.pl/api"

#' Fetch a single page from the PSE API
#' @param url Full URL including filter/pagination parameters
#' @param timeout_sec Request timeout in seconds (default 120)
#' @return Parsed JSON list with `value` (data frame) and `nextLink` (string or NULL)
fetch_pse_page <- function(url, timeout_sec = 120) {
  resp <- httr::GET(
    url,
    httr::add_headers(Accept = "application/json"),
    httr::timeout(timeout_sec)
  )
  httr::stop_for_status(resp)
  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(text, flatten = TRUE)
}

#' Download all records for one endpoint within a date range
#'
#' Follows `nextLink` pagination automatically.
#'
#' @param endpoint Endpoint name, e.g. "poze-redoze"
#' @param date_from Start date (Date object, inclusive)
#' @param date_to   End date   (Date object, inclusive)
#' @return Tibble with all records; empty tibble if no data
download_pse_endpoint <- function(endpoint, date_from, date_to) {
  filter_str <- paste0(
    "business_date ge '", format(date_from), "'",
    " and business_date le '", format(date_to), "'"
  )
  url <- paste0(
    PSE_API_BASE, "/", endpoint,
    "?$filter=", utils::URLencode(filter_str, repeated = FALSE)
  )

  pages <- list()
  repeat {
    result <- fetch_pse_page(url)
    if (!is.null(result$value) && is.data.frame(result$value) && nrow(result$value) > 0)
      pages[[length(pages) + 1]] <- result$value
    next_url <- result$nextLink
    if (is.null(next_url) || !nzchar(next_url)) break
    url <- next_url
    Sys.sleep(0.05)
  }

  if (length(pages) == 0) return(dplyr::tibble())
  dplyr::bind_rows(pages)
}

#' Download PSE endpoint data in monthly chunks
#'
#' Breaks large date ranges into chunks to keep each request small and
#' to allow partial progress logging.
#'
#' @param endpoint   Endpoint name
#' @param date_from  Start date (Date)
#' @param date_to    End date   (Date)
#' @param chunk_days Days per chunk (default 30 ≈ monthly)
#' @param verbose    Print progress messages (default TRUE)
#' @return Tibble with all records across all chunks
download_pse_chunked <- function(endpoint, date_from, date_to,
                                  chunk_days = 30, verbose = TRUE,
                                  max_retries = 3) {
  chunk_starts <- seq.Date(date_from, date_to, by = paste(chunk_days, "days"))

  purrr::map_df(chunk_starts, function(chunk_start) {
    chunk_end <- min(chunk_start + chunk_days - 1L, date_to)
    if (verbose)
      message("    [PSE/", endpoint, "] ", chunk_start, " → ", chunk_end)

    attempt <- 1L
    repeat {
      result <- tryCatch(
        download_pse_endpoint(endpoint, chunk_start, chunk_end),
        error = function(e) e
      )
      if (!inherits(result, "error")) return(result)
      if (attempt >= max_retries) stop(conditionMessage(result))
      wait <- 2 ^ attempt
      message("    [RETRY ", attempt, "/", max_retries - 1L,
              "] after ", wait, "s — ", conditionMessage(result))
      Sys.sleep(wait)
      attempt <- attempt + 1L
    }
  })
}
