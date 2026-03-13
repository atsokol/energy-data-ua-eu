#============================================================================
# Download functions for Ukrainian Balancing Market data (ua.energy)
# Requires Ukrainian IP (VPN) — ua.energy is geo-blocked by Cloudflare
#============================================================================

#' Check if ua.energy is reachable (i.e. VPN is active)
#'
#' @return TRUE if ua.energy responds with HTTP 200, FALSE otherwise
check_ua_energy_access <- function() {
  tryCatch({
    resp <- httr::GET(
      "https://ua.energy/",
      httr::timeout(15),
      httr::user_agent(paste0(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/131.0.0.0 Safari/537.36"
      ))
    )
    status <- httr::status_code(resp)
    if (status == 200) {
      message("\u2705 ua.energy is reachable (HTTP 200)")
      return(TRUE)
    } else {
      message("\u274c ua.energy returned HTTP ", status, " \u2014 VPN likely not active")
      return(FALSE)
    }
  }, error = function(e) {
    message("\u274c ua.energy unreachable: ", e$message)
    return(FALSE)
  })
}

#' Get all BM xlsx URLs from ua.energy results page
#'
#' Uses a simple GET + HTML parse (no chromote needed when VPN is active).
#'
#' @return Character vector of xlsx URLs
get_bm_urls <- function() {
  bm_page_url <- "https://ua.energy/uchasnikam_rinku/rezultaty-balansuyuchogo-rynku-2/"

  resp <- httr::GET(
    bm_page_url,
    httr::timeout(30),
    httr::user_agent(paste0(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
      "AppleWebKit/537.36 (KHTML, like Gecko) ",
      "Chrome/131.0.0.0 Safari/537.36"
    ))
  )

  if (httr::http_error(resp)) {
    stop("Failed to load BM results page: HTTP ", httr::status_code(resp))
  }

  page <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

  # Try the specific div first (XPath from old code)
  bm_div <- rvest::html_node(page, xpath = '//*[@id="1590479495940-174989ce-bac9"]')
  if (is.na(bm_div)) {
    message("Specific div not found, searching entire page for xlsx links...")
    bm_div <- page
  }

  links <- bm_div |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.xlsx$")

  message("Found ", length(links), " xlsx link(s) on BM results page")
  return(links)
}

#' Extract date from BM filename (Ukrainian transliterated month names)
#'
#' @param filename Basename of the xlsx file
#' @return Date (first day of the month) or NA
extract_bm_date <- function(filename) {
  month_match <- stringr::str_extract(
    tolower(filename),
    "(sichen|lyutyj|berezen|kviten|traven|cherven|lypen|serpen|veresen|zhovten|lystopad|gruden)"
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

  return(NA_Date_)
}

#' Download and parse a single BM xlsx file
#'
#' @param url Full URL to the xlsx file
#' @return Tibble with: country, hour (POSIXct), volume_up, price_up,
#'         volume_down, price_down
download_bm_file <- function(url) {
  filename <- basename(url)
  temp_file <- file.path(tempdir(), filename)

  message("  Downloading: ", filename)

  tryCatch({
    resp <- httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(60),
      httr::user_agent(paste0(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/131.0.0.0 Safari/537.36"
      ))
    )

    if (httr::http_error(resp)) {
      message("    Download failed: HTTP ", httr::status_code(resp))
      return(tibble::tibble())
    }

    # Read Excel: skip first 2 header rows
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
      dplyr::select(country, hour, volume_up, price_up, volume_down, price_down)

    file.remove(temp_file)
    message("    Processed ", nrow(result), " rows")
    return(result)

  }, error = function(e) {
    message("    Error processing ", filename, ": ", e$message)
    if (file.exists(temp_file)) file.remove(temp_file)
    return(tibble::tibble())
  })
}
