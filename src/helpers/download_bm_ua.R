#============================================================================
# Download functions for Ukrainian Balancing Market data (ua.energy)
# No VPN required.
#
# The BM results PAGE is Cloudflare-protected (plain httr → 403), so
# get_bm_urls() uses chromote (headless Chrome) to fetch the HTML.
# Individual xlsx downloads are plain httr (no Cloudflare restriction).
#============================================================================

.ua_bm_agent <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

# ── chromote helpers ─────────────────────────────────────────────────────────

.chromote_session <- NULL

.get_chromote <- function() {
  if (!requireNamespace("chromote", quietly = TRUE))
    stop("Package 'chromote' is required: install.packages('chromote')")

  session_ok <- !is.null(.chromote_session) && tryCatch({
    .chromote_session$Runtime$evaluate("1+1", wait_ = TRUE)
    TRUE
  }, error = function(e) FALSE)

  if (!session_ok) {
    message("  [chromote] launching Chrome...")
    b <- chromote::ChromoteSession$new()
    b$Page$addScriptToEvaluateOnNewDocument(
      source = 'Object.defineProperty(navigator,"webdriver",{get:()=>undefined})'
    )
    message("  [chromote] warming up Cloudflare session on ua.energy...")
    b$Page$navigate("https://ua.energy/", wait_ = FALSE)
    Sys.sleep(6)
    .chromote_session <<- b
    message("  [chromote] session ready")
  }
  .chromote_session
}

#' Fetch a URL via headless Chrome, return body HTML
#'
#' @param url URL to fetch
#' @param timeout Seconds to wait after navigation (default 10)
#' @return Named list: status_code (integer), body (character)
ci_fetch <- function(url, timeout = 10L) {
  message("  [chromote] fetching ", url)
  b <- .get_chromote()
  b$Page$navigate(url, wait_ = FALSE)
  Sys.sleep(min(timeout, 10))
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML", wait_ = TRUE
  )$result$value
  blocked <- grepl("Just a moment\\.\\.\\.|cf-error-details|Attention Required|No Access", html)
  list(status_code = if (blocked) 403L else 200L, body = html)
}

# ── BM URL discovery ─────────────────────────────────────────────────────────

#' Get all BM xlsx URLs from ua.energy results page
#'
#' Uses chromote (headless Chrome) because the page is Cloudflare-protected.
#'
#' @return Character vector of xlsx URLs
get_bm_urls <- function() {
  bm_page_url <- "https://ua.energy/uchasnikam_rinku/rezultaty-balansuyuchogo-rynku-2/"

  resp <- ci_fetch(bm_page_url, timeout = 10L)

  if (resp$status_code != 200L)
    stop("Failed to load BM results page: HTTP ", resp$status_code)

  page <- rvest::read_html(resp$body)

  # Try the specific div first
  bm_div <- rvest::html_node(
    page, xpath = '//*[@id="1590479495940-174989ce-bac9"]'
  )
  if (is.na(bm_div)) {
    message("Specific div not found, searching entire page for xlsx links...")
    bm_div <- page
  }

  links <- bm_div |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.xlsx$")

  message("Found ", length(links), " xlsx link(s) on BM results page")
  links
}

# ── BM date extraction ───────────────────────────────────────────────────────

#' Extract date from BM filename (Ukrainian transliterated month names)
#'
#' @param filename Basename of the xlsx file
#' @return Date (first day of the month) or NA
extract_bm_date <- function(filename) {
  month_match <- stringr::str_extract(
    tolower(filename),
    paste0("(sichen|lyutyj|berezen|kviten|traven|cherven",
           "|lypen|serpen|veresen|zhovten|lystopad|gruden)")
  )

  month_map <- c(
    "sichen" = 1L, "lyutyj" = 2L, "berezen" = 3L, "kviten" = 4L,
    "traven" = 5L, "cherven" = 6L, "lypen" = 7L, "serpen" = 8L,
    "veresen" = 9L, "zhovten" = 10L, "lystopad" = 11L, "gruden" = 12L
  )

  year_match <- stringr::str_extract(filename, "20\\d{2}")

  if (!is.na(month_match) && !is.na(year_match)) {
    month_num <- month_map[month_match]
    return(as.Date(paste(year_match, month_num, "01", sep = "-")))
  }

  NA_Date_
}

# ── BM file download ─────────────────────────────────────────────────────────

#' Download and parse a single BM xlsx file
#'
#' Uses plain httr — BM xlsx files are not Cloudflare-protected.
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with: country, hour (POSIXct), volume_up, price_up,
#'         volume_down, price_down
download_bm_file <- function(url) {
  filename  <- basename(url)
  temp_file <- file.path(tempdir(), filename)

  message("  Downloading: ", filename)

  tryCatch({
    resp <- httr::GET(url, httr::user_agent(.ua_bm_agent),
                      httr::write_disk(temp_file, overwrite = TRUE),
                      httr::timeout(60))

    if (httr::status_code(resp) != 200L) {
      message("    Download failed: HTTP ", httr::status_code(resp))
      return(tibble::tibble())
    }

    bm_data <- readxl::read_excel(temp_file, skip = 2, col_names = c(
      "date", "time", "volume_up", "price_up", "volume_down", "price_down"
    ))

    result <- bm_data |>
      tidyr::fill(date, .direction = "down") |>
      dplyr::mutate(
        country  = "UA",
        hour_num = as.numeric(stringr::str_extract(.data$time, "^\\d+")),
        hour     = lubridate::ymd_h(
          paste(format(.data$date, "%Y-%m-%d"), .data$hour_num),
          tz = "UTC"
        ),
        volume_up   = as.numeric(.data$volume_up),
        volume_down = as.numeric(.data$volume_down),
        price_up    = as.numeric(.data$price_up),
        price_down  = as.numeric(.data$price_down)
      ) |>
      dplyr::filter(!is.na(.data$date), !is.na(.data$hour)) |>
      dplyr::select(
        country, hour, volume_up, price_up, volume_down, price_down
      )

    if (file.exists(temp_file)) file.remove(temp_file)
    message("    Processed ", nrow(result), " rows")
    result

  }, error = function(e) {
    message("    Error processing ", filename, ": ", e$message)
    if (file.exists(temp_file)) file.remove(temp_file)
    tibble::tibble()
  })
}
