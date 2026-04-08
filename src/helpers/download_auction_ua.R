#============================================================================
# Download functions for Ukrainian cross-border capacity auction data
# (daily auctions, ua.energy)
#
# No VPN required. ua.energy xlsx files and the auction page are accessible
# via plain httr with a browser User-Agent. The specific VPN exit IPs used
# earlier were blacklisted by ua.energy ("No Access 2").
#
# Filenames on ua.energy are completely inconsistent (multiple naming
# variants, typos, ISO vs DD.MM.YYYY dates, -1 duplicate suffixes).
# The only reliable key is the *link text*: always
#   "Результати добового аукціону на DD.MM.YYYY"
# So get_auction_urls() builds a date→URL map from link text, not filenames.
#============================================================================

.auction_url_cache <- NULL

.ua_agent <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

# ── URL discovery ─────────────────────────────────────────────────────────────

#' Fetch all day-ahead auction xlsx URLs from the ua.energy results page
#'
#' Uses plain httr GET with a browser User-Agent. The page is PHP-rendered
#' (all CQ Tabs links are in the server response HTML).
#'
#' @param force Re-fetch even if cache is populated (default FALSE)
#' @return Named character vector: names = "YYYY-MM-DD" dates, values = URLs
get_auction_urls <- function(force = FALSE) {
  if (!force && !is.null(.auction_url_cache)) return(.auction_url_cache)

  page_url <- paste0(
    "https://ua.energy/uchasnikam_rinku/auktsiony/",
    "rezultaty-auktsioniv-z-dostupu-do-mizhderzhavnyh-peretyniv/"
  )

  message("  Fetching auction URL list from ua.energy...")

  resp <- tryCatch(
    httr::GET(page_url, httr::user_agent(.ua_agent), httr::timeout(30)),
    error = function(e) {
      stop("httr error fetching auction page: ", e$message)
    }
  )

  if (httr::status_code(resp) != 200L)
    stop("Auction page returned HTTP ", httr::status_code(resp))

  html <- httr::content(resp, "text", encoding = "UTF-8")

  if (is.null(html) || nchar(html) < 500) {
    message("  Response (first 500 chars): ", substr(html %||% "", 1, 500))
    stop("Empty or invalid response from auction page (",
         nchar(html %||% ""), " chars)")
  }

  message("  Page HTML received (", nchar(html), " chars), extracting links...")

  page  <- rvest::read_html(html)
  nodes <- page |> rvest::html_nodes("a")
  hrefs <- rvest::html_attr(nodes, "href")
  texts <- rvest::html_text(nodes, trim = TRUE)

  # Filter: href contains "Rezultaty-dobovogo-auktsionu" and ends with .xlsx
  is_da <- !is.na(hrefs) &
    stringr::str_detect(hrefs, "Rezultaty-dobovogo-auktsionu") &
    stringr::str_ends(hrefs, "\\.xlsx")
  hrefs <- hrefs[is_da]
  texts <- texts[is_da]

  # Extract date from link text — always ends with "DD.MM.YYYY"
  date_str <- stringr::str_extract(texts, "\\d{2}\\.\\d{2}\\.\\d{4}")
  dates    <- suppressWarnings(lubridate::dmy(date_str))

  valid   <- !is.na(dates)
  url_map <- stats::setNames(hrefs[valid], format(dates[valid], "%Y-%m-%d"))

  message("  Found ", length(url_map), " day-ahead auction file(s)")
  .auction_url_cache <<- url_map
  url_map
}

# ── File download ─────────────────────────────────────────────────────────────

#' Download a single daily auction xlsx from a known URL
#'
#' @param url Full URL to the xlsx file
#' @param date A Date object (used for temp file naming and as parse fallback)
#' @return Path to the downloaded temp file, or NULL on failure
download_auction_xlsx <- function(url, date) {
  tmp <- file.path(
    tempdir(), paste0("auction_dam_", format(date, "%Y%m%d"), ".xlsx")
  )

  resp <- tryCatch(
    httr::GET(url, httr::user_agent(.ua_agent),
              httr::write_disk(tmp, overwrite = TRUE),
              httr::timeout(60)),
    error = function(e) NULL
  )

  if (!is.null(resp) && httr::status_code(resp) == 200L) {
    f     <- file(tmp, "rb")
    magic <- readBin(f, raw(), 2L)
    close(f)
    if (identical(magic, as.raw(c(0x50, 0x4b)))) return(tmp)
  }

  if (file.exists(tmp)) file.remove(tmp)
  NULL
}

# ── Parsing ───────────────────────────────────────────────────────────────────

#' Parse one sheet of a daily auction xlsx into a tidy tibble
#'
#' Layout (consistent across all border sheets):
#'   Row 1: border label  e.g. "Україна → Румунія"
#'   Row 2: column headers (price, OC, RPS, ...)
#'   Row 3: empty
#'   Rows 4–27: 24 hourly rows; date only filled in row 4
#'
#' @param file_path Path to the xlsx file
#' @param sheet Sheet name
#' @param date The auction date (fallback if Excel date cell is unreadable)
#' @return Tibble with columns: date, hour, border, currency, price,
#'         cap_offered_mw, cap_allocated_mw — or empty tibble on failure
parse_auction_sheet <- function(file_path, sheet, date) {
  tryCatch({
    raw <- suppressMessages(
      readxl::read_excel(file_path, sheet = sheet, col_names = FALSE)
    )

    # Currency: UA-PL uses UAH (₴), others use EUR (€)
    price_header <- as.character(raw[[3]][2])
    currency <- dplyr::if_else(
      stringr::str_detect(price_header, "\u20b4"),  # ₴
      "UAH", "EUR"
    )

    # Data rows: skip first 3 header rows
    data_rows <- raw[4:nrow(raw), 1:5]
    names(data_rows) <- c(
      "date_raw", "hour_raw", "price", "cap_offered_mw", "cap_allocated_mw"
    )

    data_rows |>
      dplyr::mutate(
        date = dplyr::coalesce(
          suppressWarnings(lubridate::as_date(.data$date_raw)),
          suppressWarnings(lubridate::dmy(as.character(.data$date_raw)))
        )
      ) |>
      tidyr::fill(date, .direction = "down") |>
      dplyr::mutate(
        date = dplyr::if_else(is.na(.data$date), .env$date, .data$date),
        hour = as.integer(.data$hour_raw),
        border = sheet,
        currency = currency,
        price = suppressWarnings(as.numeric(.data$price)),
        cap_offered_mw  = suppressWarnings(as.numeric(.data$cap_offered_mw)),
        cap_allocated_mw = suppressWarnings(as.numeric(.data$cap_allocated_mw))
      ) |>
      dplyr::filter(!is.na(.data$hour)) |>
      dplyr::select(
        date, hour, border, currency, price, cap_offered_mw, cap_allocated_mw
      )

  }, error = function(e) {
    message("    Error parsing sheet '", sheet, "': ", e$message)
    tibble::tibble()
  })
}

#' Download and parse a single day of auction data from a known URL
#'
#' @param date A Date object
#' @param url Full URL to the xlsx file
#' @return Tibble with all border sheets merged, or empty tibble on failure
download_auction_day <- function(date, url) {
  message("  Downloading auction: ", format(date, "%Y-%m-%d"))

  tmp <- download_auction_xlsx(url, date)

  if (is.null(tmp)) {
    message("    Download failed for ", format(date, "%Y-%m-%d"))
    return(tibble::tibble())
  }

  sheets <- tryCatch(readxl::excel_sheets(tmp), error = function(e) character())

  if (length(sheets) == 0) {
    if (file.exists(tmp)) file.remove(tmp)
    return(tibble::tibble())
  }

  result <- purrr::map_dfr(sheets, \(sh) parse_auction_sheet(tmp, sh, date))
  if (file.exists(tmp)) file.remove(tmp)

  if (nrow(result) > 0) {
    message("    Parsed ", nrow(result), " rows across ",
            length(sheets), " border(s)")
  }
  result
}
