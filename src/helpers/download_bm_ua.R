#============================================================================
# Download functions for Ukrainian Balancing Market data (ua.energy)
# No VPN required.
#
# The BM results page publishes one xlsx per month for each of three
# datasets, all keyed by date + hour:
#
#   "results"   Результати балансуючого ринку
#               → volume/price of activated balancing energy, up + down
#   "marginal"  Маржинальні ціни активованої балансуючої енергії
#               → marginal price, завантаження (up) + розвантаження (down)
#   "imbalance" Сумарний небаланс електроенергії
#               → total system imbalance volume, positive + negative
#   "imbprice"  Фактичні ціни небалансів
#               → actual imbalance price (IMSP) + positive/negative payment
#                 prices. Published further into the current month than the
#                 others (partial-month files, e.g. "01-10.08.2026").
#
# "results" + "marginal" feed BM_UA.csv; "imbalance" + "imbprice" feed
# Imbalance_UA.csv.
#
# Both the PAGE and the xlsx files are Cloudflare-protected (plain httr →
# 403), so chromote (headless Chrome) fetches the HTML and supplies the
# Cloudflare session cookies used for the xlsx downloads.
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
    .chromote_session$Runtime$evaluate("1+1", wait_ = TRUE, timeout_ = 5)
    TRUE
  }, error = function(e) FALSE)

  if (!session_ok) {
    message("  [chromote] launching Chrome...")
    chrome <- chromote::Chrome$new(args = c(
      "--disable-blink-features=AutomationControlled",
      "--no-sandbox",
      "--disable-setuid-sandbox",
      paste0("--user-agent=", .ua_bm_agent)
    ))
    b <- chromote::ChromoteSession$new(parent = chromote::Chromote$new(chrome))
    b$Network$setUserAgentOverride(userAgent = .ua_bm_agent)
    b$Page$addScriptToEvaluateOnNewDocument(source = paste0(
      'Object.defineProperty(navigator,"webdriver",{get:()=>undefined});',
      'window.chrome={runtime:{}};',
      'Object.defineProperty(navigator,"plugins",{get:()=>[1,2,3,4,5]});',
      'Object.defineProperty(navigator,"languages",',
      '{get:()=>["uk-UA","uk","en-US","en"]});'
    ))
    message("  [chromote] warming up Cloudflare session on ua.energy...")
    tryCatch(
      b$Page$navigate("https://ua.energy/", wait_ = TRUE, timeout_ = 30),
      error = function(e) message("  [chromote] warmup warning: ", e$message)
    )
    Sys.sleep(12)
    .chromote_session <<- b
    message("  [chromote] session ready")
  }
  .chromote_session
}

#' Fetch a URL via headless Chrome, return body HTML
#'
#' @param url URL to fetch
#' @param timeout Seconds to wait after navigation (default 15)
#' @return Named list: status_code (integer), body (character)
ci_fetch <- function(url, timeout = 20L) {
  message("  [chromote] fetching ", url)
  b <- .get_chromote()
  tryCatch(
    b$Page$navigate(url, wait_ = FALSE, timeout_ = 30),
    error = function(e) message("  [chromote] navigate warning: ", e$message)
  )
  Sys.sleep(min(timeout, 20))
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML", wait_ = TRUE, timeout_ = 15
  )$result$value
  blocked <- grepl(
    "Just a moment\\.\\.\\.|cf-error-details|Attention Required|No Access",
    html
  )
  list(status_code = if (blocked) 403L else 200L, body = html)
}

# ── BM URL discovery ─────────────────────────────────────────────────────────

BM_PAGE_URL <- "https://ua.energy/uchasnikam_rinku/rezultaty-balansuyuchogo-rynku-2/"

# Each dataset is identified by a stable pattern in the xlsx FILENAME. This is
# far more robust than locating the page section by div id / heading position,
# both of which change when the CMS page is re-edited.
# ("cumarnyj" covers a 2020 typo in one imbalance filename; "faktichni" a
# 2020 typo in one imbalance-price filename.)
BM_SECTION_PATTERNS <- c(
  results   = "rezultaty-balansuyuchogo-rynku",
  imbalance = "[sc]umarnyj-nebalans",
  marginal  = "marzhynalni-tsiny",
  imbprice  = "fakt[iy]chni-tsiny-nebalans"
)

BM_SECTIONS <- names(BM_SECTION_PATTERNS)

.bm_page_cache <- NULL

#' Fetch the BM results page HTML (cached per session)
#'
#' Tries plain httr first; falls back to chromote if Cloudflare blocks it.
#' VPN IPs are blacklisted by ua.energy — run without VPN.
#'
#' @param refresh Force a re-fetch instead of using the cached HTML
#' @return HTML source as a character string
get_bm_page_html <- function(refresh = FALSE) {
  if (!refresh && !is.null(.bm_page_cache)) return(.bm_page_cache)

  blocked_pat <- "No Access|Just a moment|cf-error-details|Attention Required"

  # ── Try plain httr first ─────────────────────────────────────────────────
  message("  Fetching BM page from ua.energy (plain httr)...")
  resp_httr <- tryCatch(
    httr::GET(BM_PAGE_URL, httr::user_agent(.ua_bm_agent), httr::timeout(30)),
    error = function(e) NULL
  )

  html_body <- NULL
  if (!is.null(resp_httr) && httr::status_code(resp_httr) == 200L) {
    body <- httr::content(resp_httr, "text", encoding = "UTF-8")
    if (!is.null(body) && nchar(body) > 1000 && !grepl(blocked_pat, body)) {
      html_body <- body
      message("  Plain httr succeeded (", nchar(html_body), " chars)")
    }
  }

  # ── Fall back to chromote if needed ─────────────────────────────────────
  if (is.null(html_body)) {
    message("  Plain httr blocked, trying chromote...")
    resp_cf <- ci_fetch(BM_PAGE_URL, timeout = 20L)
    if (resp_cf$status_code != 200L)
      stop("Failed to load BM results page: HTTP ", resp_cf$status_code)
    html_body <- resp_cf$body
  }

  .bm_page_cache <<- html_body
  html_body
}

#' Get monthly xlsx URLs for one BM dataset
#'
#' @param section One of "results", "imbalance", "marginal", "imbprice"
#' @param refresh Force a re-fetch of the page HTML
#' @return Character vector of xlsx URLs
get_bm_urls <- function(section = BM_SECTIONS, refresh = FALSE) {
  section <- match.arg(section)

  links <- rvest::read_html(get_bm_page_html(refresh)) |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.xlsx$") |>
    unique()

  hits <- links[stringr::str_detect(
    tolower(basename(links)), BM_SECTION_PATTERNS[[section]]
  )]

  message("  [", section, "] ", length(hits), " monthly file(s) on page")
  hits
}

# ── BM date extraction ───────────────────────────────────────────────────────

#' Extract date from BM filename (Ukrainian transliterated month names)
#'
#' Handles both the nominative form used in most filenames ("za-zhovten")
#' and the genitive form that occasionally appears ("za-zhovtnya").
#'
#' @param filename Basename of the xlsx file
#' @return Date (first day of the month) or NA
extract_bm_date <- function(filename) {
  month_map <- c(
    "sichen" = 1L, "lyutyj" = 2L, "berezen" = 3L, "kviten" = 4L,
    "traven" = 5L, "cherven" = 6L, "lypen" = 7L, "serpen" = 8L,
    "veresen" = 9L, "zhovten" = 10L, "lystopad" = 11L, "gruden" = 12L,
    # genitive forms
    "sichnya" = 1L, "lyutogo" = 2L, "bereznya" = 3L, "kvitnya" = 4L,
    "travnya" = 5L, "chervnya" = 6L, "lypnya" = 7L, "serpnya" = 8L,
    "veresnya" = 9L, "zhovtnya" = 10L, "lystopada" = 11L, "grudnya" = 12L
  )

  month_match <- stringr::str_extract(
    tolower(filename),
    paste0("(", paste(names(month_map), collapse = "|"), ")")
  )

  year_match <- stringr::str_extract(filename, "20\\d{2}")

  if (!is.na(month_match) && !is.na(year_match)) {
    month_num <- month_map[month_match]
    return(as.Date(paste(year_match, month_num, "01", sep = "-")))
  }

  NA_Date_
}

#' Extract the month of a file from its name, per dataset
#'
#' The imbalance-price files are named by day range instead of month name
#' ("Faktychni-tsiny-nebalansiv-01-31.03.2022-ukr_eng.xlsx", and partial
#' months such as "01-10.08.2026"), so they need their own pattern. Older
#' 2019/2020 files of that dataset use month names and fall back to
#' extract_bm_date().
#'
#' @param filename Basename of the xlsx file
#' @param section One of "results", "imbalance", "marginal", "imbprice"
#' @return Date (first day of the month) or NA
extract_section_date <- function(filename, section) {
  if (section == "imbprice") {
    m <- stringr::str_match(filename, "\\d{1,2}\\s*-\\s*\\d{1,2}\\.(\\d{2})\\.(\\d{4})")
    if (!is.na(m[1, 1]))
      return(as.Date(paste(m[1, 3], m[1, 2], "01", sep = "-")))
  }
  extract_bm_date(filename)
}

# ── BM file download ─────────────────────────────────────────────────────────

#' Get Cloudflare cookies from active chromote session as httr cookie header
.get_cf_cookies <- function(domain = "ua.energy") {
  if (is.null(.chromote_session)) return(NULL)
  tryCatch({
    cookies <- .chromote_session$Network$getCookies(
      urls = list(paste0("https://", domain)), wait_ = TRUE
    )$cookies
    if (length(cookies) == 0) return(NULL)
    cookie_str <- paste(
      vapply(cookies, function(c) paste0(c$name, "=", c$value), character(1)),
      collapse = "; "
    )
    httr::add_headers(Cookie = cookie_str)
  }, error = function(e) NULL)
}

#' Download one xlsx to a session-local cache, retrying through Cloudflare
#'
#' Cloudflare cookies expire mid-run on long backfills, so a 403 triggers a
#' chromote (re-)warm-up and a fresh cookie jar before retrying.
#'
#' @param url Full URL to the xlsx file
#' @param retries Number of retries after the first attempt
#' @return Path to the downloaded file, or NULL on failure
.bm_download_xlsx <- function(url, retries = 2L) {
  cache_dir <- file.path(tempdir(), "bm_ua_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(cache_dir, basename(url))

  # Re-use an already-downloaded file (backfills re-run often)
  if (file.exists(dest) && file.size(dest) > 10000) return(dest)

  for (attempt in seq_len(retries + 1L)) {
    if (attempt > 1L) {
      message("    Retry ", attempt - 1L, " (refreshing Cloudflare session)")
      # .get_chromote() only relaunches a *dead* session; an live session with
      # an expired clearance cookie needs a fresh navigation to renew it
      tryCatch({
        b <- .get_chromote()
        b$Page$navigate(BM_PAGE_URL, wait_ = FALSE, timeout_ = 30)
      }, error = function(e) message("    Refresh warning: ", e$message))
      Sys.sleep(8)
    }

    resp <- tryCatch(
      httr::GET(url, httr::user_agent(.ua_bm_agent), .get_cf_cookies(),
                httr::write_disk(dest, overwrite = TRUE), httr::timeout(90)),
      error = function(e) {
        message("    Download error: ", e$message)
        NULL
      }
    )

    if (!is.null(resp) && httr::status_code(resp) == 200L &&
        file.exists(dest) && file.size(dest) > 10000) {
      return(dest)
    }

    if (!is.null(resp))
      message("    Download failed: HTTP ", httr::status_code(resp))
  }

  if (file.exists(dest)) file.remove(dest)
  NULL
}

#' Coerce a ua.energy date column (POSIXct, Date, text or Excel serial) to Date
.bm_as_date <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "Date")) return(as.Date(x))

  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))

  # Excel serial numbers (origin 1899-12-30)
  is_serial <- !is.na(x) & grepl("^\\d{5}(\\.\\d+)?$", x)
  out[is_serial] <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")

  txt <- !is.na(x) & !is_serial
  if (any(txt)) {
    parsed <- suppressWarnings(lubridate::ymd(x[txt], quiet = TRUE))
    na_txt <- is.na(parsed)
    if (any(na_txt))
      parsed[na_txt] <- suppressWarnings(lubridate::dmy(x[txt][na_txt], quiet = TRUE))
    out[txt] <- parsed
  }

  out
}

#' Coerce a ua.energy numeric column (character, possibly comma-decimal) to numeric
.bm_as_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x <- gsub("[[:space:]  ]", "", as.character(x), perl = TRUE)
  # "1234,56" → decimal comma; "1,234.56" → thousands separator
  x <- ifelse(grepl(",", x) & !grepl("\\.", x),
              sub(",", ".", x, fixed = TRUE),
              gsub(",", "", x))
  suppressWarnings(as.numeric(x))
}

#' Column indices belonging to the "ОЕС України" block
#'
#' Files published before the March 2022 ENTSO-E synchronisation carry a
#' second block of columns for the Burshtyn TPP island. Everything from the
#' "ОЕС України" header up to (but excluding) the "Бурштин" header is the
#' mainland system. Date/time columns (1-2) are always included.
#'
#' @param hdr Character vector of flattened header text, one entry per column
#' @return Integer vector of column indices
.bm_ua_block_cols <- function(hdr) {
  ua <- grep("ОЕС України", hdr)
  if (length(ua) == 0) return(seq_along(hdr))

  start <- min(ua)
  burshtyn <- grep("Бурштин", hdr)
  burshtyn <- burshtyn[burshtyn > start]
  end <- if (length(burshtyn)) min(burshtyn) - 1L else length(hdr)

  unique(c(1L, 2L, seq(start, end)))
}

#' Parse a ua.energy monthly BM workbook
#'
#' All four datasets share the same shape: column 1 holds the date (only on
#' the first row of each day), column 2 an "HH:MM - HH:MM" interval, and the
#' remaining columns the values. The number of banner/header rows above the
#' data differs per dataset and has changed over the years, so data rows are
#' identified by the time pattern in column 2 rather than by a fixed skip.
#'
#' Value columns are located either positionally or, when `value_spec` is
#' named, by matching a regex against the column header. Header matching is
#' required where the layout is not stable: February 2022 inserts a
#' "Гранична ціна РДН" column into the imbalance-price files, shifting the
#' payment-price columns one to the right.
#'
#' @param path Path to the xlsx file
#' @param value_spec Either an unnamed character vector of output names (the
#'   value columns are then taken left to right), or a named character vector
#'   mapping output name → header regex
#' @return Tibble with country, date, hour and the named value columns
.bm_parse_xlsx <- function(path, value_spec) {
  raw <- readxl::read_excel(path, sheet = 1, col_names = FALSE,
                            .name_repair = "unique_quiet")
  if (ncol(raw) < 3L) stop("expected >= 3 columns, found ", ncol(raw))

  is_data <- grepl("^\\s*\\d{1,2}:\\d{2}", as.character(raw[[2]]))
  if (!any(is_data))
    stop("no data rows found (no 'HH:MM' values in column 2)")

  # Flatten the banner rows above the data into one header string per column
  hdr_rows <- seq_len(min(which(is_data)) - 1L)
  hdr <- vapply(raw, function(col) {
    paste(stats::na.omit(as.character(col[hdr_rows])), collapse = " | ")
  }, character(1))

  ua_cols  <- .bm_ua_block_cols(hdr)
  val_cols <- setdiff(ua_cols, 1:2)

  if (is.null(names(value_spec))) {
    if (length(val_cols) < length(value_spec))
      stop("expected >= ", length(value_spec), " value columns, found ",
           length(val_cols))
    idx <- val_cols[seq_along(value_spec)]
    value_names <- value_spec
  } else {
    idx <- vapply(value_spec, function(pat) {
      hit <- val_cols[grepl(pat, hdr[val_cols], ignore.case = TRUE)]
      if (length(hit) == 0) NA_integer_ else hit[1]
    }, integer(1))
    if (anyNA(idx))
      stop("could not locate column(s) by header: ",
           paste(names(idx)[is.na(idx)], collapse = ", "))
    value_names <- names(value_spec)
  }

  raw <- raw[, c(1L, 2L, idx)]
  names(raw) <- c("date", "time", value_names)
  raw$.is_data <- is_data

  raw |>
    dplyr::mutate(date = .bm_as_date(.data$date)) |>
    tidyr::fill(date, .direction = "down") |>
    dplyr::filter(.data$.is_data) |>
    dplyr::mutate(
      country = "UA",
      hour    = as.integer(stringr::str_extract(as.character(.data$time), "\\d{1,2}")),
      dplyr::across(dplyr::all_of(value_names), .bm_as_num)
    ) |>
    dplyr::filter(!is.na(.data$date), !is.na(.data$hour)) |>
    # A 25-hour DST day repeats hour 2; keep the first occurrence so the key
    # (country, date, hour, direction) stays unique across all datasets
    dplyr::distinct(country, date, hour, .keep_all = TRUE) |>
    dplyr::select(country, date, hour, dplyr::all_of(value_names))
}

#' Download and parse one monthly file of a BM dataset
#'
#' @param url Full URL to the xlsx file
#' @param section One of "results", "imbalance", "marginal", "imbprice"
#' @return Tibble with country, date, hour + dataset-specific value columns,
#'         or an empty tibble if the download or parse failed
download_bm_section_file <- function(url, section = BM_SECTIONS) {
  section <- match.arg(section)

  # Unnamed = positional; named = located by header regex (see .bm_parse_xlsx).
  # The imbalance-price workbook also carries DAM price columns; they are
  # skipped here because DAM prices live in DAM_UA.csv.
  value_spec <- switch(section,
    results   = c("volume_up", "price_up", "volume_down", "price_down"),
    marginal  = c("price_marg_up", "price_marg_down"),
    imbalance = c(imbalance_pos = "Позитивний",
                  imbalance_neg = "Негативний"),
    # June 2023 labels the IMSP column "Оперативна ціна небалансу /
    # Estimated imbalance price" instead of "Фактична / Actual", so match on
    # the part both wordings share.
    imbprice  = c(price_actual   = "ціна небалансу|IMSP",
                  price_positive = "позитивний небаланс",
                  price_negative = "негативний небаланс")
  )

  filename <- basename(url)
  message("  [", section, "] ", filename)

  path <- .bm_download_xlsx(url)
  if (is.null(path)) return(tibble::tibble())

  tryCatch({
    result <- .bm_parse_xlsx(path, value_spec)
    message("    Processed ", nrow(result), " rows")
    result
  }, error = function(e) {
    message("    Error processing ", filename, ": ", e$message)
    tibble::tibble()
  })
}

#' Download and parse a single BM results (Результати БР) xlsx file
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with country, date, hour, volume_up, price_up,
#'         volume_down, price_down
download_bm_file <- function(url) {
  download_bm_section_file(url, "results")
}

#' Download and parse a single total-imbalance (Сумарний небаланс) xlsx file
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with country, date, hour, imbalance_pos, imbalance_neg
download_imbalance_file <- function(url) {
  download_bm_section_file(url, "imbalance")
}

#' Download and parse a single marginal-price (Маржинальні ціни) xlsx file
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with country, date, hour, price_marg_up, price_marg_down
download_marginal_file <- function(url) {
  download_bm_section_file(url, "marginal")
}

#' Download and parse a single imbalance-price (Фактичні ціни небалансів) file
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with country, date, hour, price_actual, price_positive,
#'         price_negative
download_imbprice_file <- function(url) {
  download_bm_section_file(url, "imbprice")
}

#' Download every monthly file of one dataset within a date range
#'
#' @param section One of "results", "imbalance", "marginal", "imbprice"
#' @param start_date First month to include (any day within it)
#' @param end_date Last month to include (any day within it)
#' @return Tibble of all parsed rows (empty if nothing downloaded)
download_bm_section <- function(section, start_date, end_date) {
  section <- match.arg(section, BM_SECTIONS)

  urls <- get_bm_urls(section)
  if (length(urls) == 0) return(tibble::tibble())

  first_month <- lubridate::floor_date(as.Date(start_date), "month")
  last_month  <- lubridate::floor_date(as.Date(end_date), "month")

  keep <- vapply(urls, function(u) {
    d <- extract_section_date(basename(u), section)
    !is.na(d) && d >= first_month && d <= last_month
  }, logical(1))

  urls <- urls[keep]
  message("  [", section, "] ", length(urls), " file(s) in range ",
          format(first_month, "%Y-%m"), " .. ", format(last_month, "%Y-%m"))

  if (length(urls) == 0) return(tibble::tibble())

  out <- purrr::map_dfr(urls, function(u) {
    res <- download_bm_section_file(u, section)
    Sys.sleep(0.5)  # be polite to ua.energy
    res
  })

  if (nrow(out) == 0) return(out)

  # A file that fails to download or parse returns an empty tibble, which
  # would otherwise leave a silent hole in the middle of the history
  expected <- sort(unique(vapply(basename(urls), extract_section_date,
                                 numeric(1), section = section)))
  expected <- as.Date(expected, origin = "1970-01-01")
  got <- unique(lubridate::floor_date(out$date, "month"))
  missing <- setdiff(expected, got)
  if (length(missing) > 0)
    warning("  [", section, "] no data for ",
            paste(format(as.Date(missing, origin = "1970-01-01"), "%Y-%m"),
                  collapse = ", "), call. = FALSE, immediate. = TRUE)

  out |>
    # A month can be covered by several partial files (e.g. "01-10.08" later
    # superseded by "01-20.08"); overlapping days carry identical values
    dplyr::distinct(country, date, hour, .keep_all = TRUE) |>
    dplyr::arrange(date, hour)
}
