#============================================================================
# Download functions for JAO (Joint Allocation Office) cross-border
# capacity auction data via the JAO Auction Data API
#
# Documentation: https://www.jao.eu/page-api/market-data
# Auth: AUTH_API_KEY header required (request token from JAO website)
#
# UA corridors available: UA-SK, SK-UA, UA-HU, HU-UA, UA-PL, PL-UA,
#                         UA-RO, RO-UA
# Data available from: ~2024-03-02 (CET delivery date)
# Max query window: 31 days per request (handled by chunking below)
#
# Date convention: JAO uses CET/CEST (Europe/Paris) for delivery dates.
#   marketPeriodStart is stored in UTC; delivery date = that timestamp
#   converted to Europe/Paris and truncated to date.
#   productHour "HH:00-(HH+1):00" is 1-indexed as hour = start_hour + 1.
#============================================================================

JAO_UA_CORRIDORS <- c("UA-SK", "SK-UA", "UA-HU", "HU-UA",
                       "UA-PL", "PL-UA", "UA-RO", "RO-UA")

.JAO_BASE_URL <- "https://api.jao.eu/OWSMP"

# ── Internal helpers ─────────────────────────────────────────────────────────

#' Parse a JAO getauctions response data frame into tidy hourly rows
#'
#' @param auctions_df Data frame from jsonlite::fromJSON on a getauctions call
#' @return Tibble: date, hour, border, currency, price,
#'         cap_offered_mw, cap_allocated_mw
.parse_jao_results <- function(auctions_df) {
  purrr::map_dfr(seq_len(nrow(auctions_df)), function(i) {
    r <- auctions_df$results[[i]]
    if (is.null(r) || !is.data.frame(r) || nrow(r) == 0) return(tibble::tibble())

    # Delivery date: marketPeriodStart (UTC) → CET/CEST date
    mps <- lubridate::ymd_hms(auctions_df$marketPeriodStart[i], tz = "UTC")
    delivery_date <- lubridate::as_date(
      lubridate::with_tz(mps + lubridate::minutes(1), "Europe/Paris")
    )

    r |>
      dplyr::mutate(
        date = delivery_date,
        # "00:00-01:00" → start_hour 0 → hour 1; "23:00-24:00" → hour 24
        hour             = as.integer(
          stringr::str_extract(.data$productHour, "^\\d+")
        ) + 1L,
        border           = .data$corridorCode,
        currency         = "EUR",
        price            = as.numeric(.data$auctionPrice),
        cap_offered_mw   = as.integer(.data$offeredCapacity),
        cap_allocated_mw = as.integer(.data$allocatedCapacity)
      ) |>
      dplyr::select(date, hour, border, currency, price,
                    cap_offered_mw, cap_allocated_mw)
  })
}

# ── Public API ────────────────────────────────────────────────────────────────

#' Download JAO daily auction data for one corridor over a date range
#'
#' Splits the request into 30-delivery-day chunks to stay within the
#' 31-day API window limit.
#'
#' @param corridor Corridor code (e.g. "UA-SK")
#' @param start_date First delivery date to fetch (Date, CET)
#' @param end_date Last delivery date to fetch (Date, CET)
#' @param token JAO API authentication token
#' @return Tibble with date, hour, border, currency, price,
#'         cap_offered_mw, cap_allocated_mw — empty tibble on failure
get_jao_corridor <- function(corridor, start_date, end_date, token) {
  chunk_starts <- seq(start_date, end_date, by = "30 days")

  purrr::map_dfr(chunk_starts, function(cs) {
    ce <- min(cs + lubridate::days(29), end_date)

    # fromdate = cs - 1: captures the auction whose marketPeriodStart is
    # (cs-1)T23:00 UTC, which delivers on cs in CET.
    resp <- tryCatch(
      httr::GET(
        paste0(.JAO_BASE_URL, "/getauctions"),
        httr::add_headers("AUTH_API_KEY" = token),
        query = list(
          corridor = corridor,
          horizon  = "Daily",
          fromdate = format(cs - lubridate::days(1), "%Y-%m-%d"),
          todate   = format(ce, "%Y-%m-%d")
        ),
        httr::timeout(60)
      ),
      error = function(e) {
        message("    [JAO] Request error for ", corridor, " ", cs,
                ": ", e$message)
        NULL
      }
    )

    if (is.null(resp)) return(tibble::tibble())

    status <- httr::status_code(resp)
    if (status == 400L) {
      message("    [JAO] No data: ", corridor, " ", cs, " to ", ce)
      return(tibble::tibble())
    }
    if (status != 200L) {
      message("    [JAO] HTTP ", status, " for ", corridor, " ", cs)
      return(tibble::tibble())
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8")),
      error = function(e) {
        message("    [JAO] JSON parse error: ", e$message)
        NULL
      }
    )

    if (is.null(parsed) || !is.data.frame(parsed) || nrow(parsed) == 0)
      return(tibble::tibble())

    rows <- .parse_jao_results(parsed)
    message("    [JAO] ", corridor, " ", cs, " to ", ce,
            ": ", nrow(rows), " rows")
    rows
  })
}

#' Download JAO daily auction data for all UA corridors
#'
#' @param start_date First delivery date to fetch (Date, CET)
#' @param end_date Last delivery date to fetch (Date, CET)
#' @param token JAO API token; defaults to JAO_TOKEN env var
#' @param corridors Corridors to download; defaults to JAO_UA_CORRIDORS
#' @return Tibble combining all corridors
get_jao_ua_auctions <- function(start_date, end_date,
                                 token     = Sys.getenv("JAO_TOKEN"),
                                 corridors = JAO_UA_CORRIDORS) {
  if (nchar(token) == 0)
    stop("JAO_TOKEN not set. Add it to .Renviron or pass explicitly.")

  message("  [JAO] ", length(corridors), " corridor(s): ",
          start_date, " to ", end_date)

  purrr::map_dfr(corridors, function(corridor) {
    get_jao_corridor(corridor, start_date, end_date, token)
  })
}
