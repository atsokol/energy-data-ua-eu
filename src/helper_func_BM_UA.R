#============================================================================
# Functions to download balancing market data
#============================================================================

library(chromote)
library(rvest)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)

#============================================================================
# Ukraine Balancing Market Functions
#============================================================================

# Function to get all BM file URLs from the website
get_all_bm_urls <- function() {
  # Use chromote to bypass Cloudflare protection
  b <- ChromoteSession$new()
  
  # Navigate to the page
  b$Page$navigate("https://ua.energy/uchasnikam_rinku/rezultaty-balansuyuchogo-rynku-2/")
  Sys.sleep(5)  # Wait for page to load
  
  # Get HTML content
  html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  b$close()
  
  # Parse HTML
  page <- read_html(html_content)
  
  # Use XPath to find the specific div
  bm_div <- page |>
    html_node(xpath = '//*[@id="1590479495940-174989ce-bac9"]')
  
  if (is.na(bm_div)) {
    message("Specific div not found, searching entire page...")
    bm_div <- page
  }
  
  # Extract all xlsx links
  all_links <- bm_div |>
    html_nodes("a") |>
    html_attr("href") |>
    str_subset("\\.xlsx$")
  
  return(all_links)
}

# Function to extract date from BM filename
extract_bm_date <- function(filename) {
  # Extract month name from filename
  month_match <- str_extract(filename, "(sichen|lyutyj|berezen|kviten|traven|cherven|lypen|serpen|veresen|zhovten|lystopad|gruden)")
  
  # Month name mapping (Ukrainian to number)
  month_map <- c(
    "sichen" = 1, "lyutyj" = 2, "berezen" = 3, "kviten" = 4,
    "traven" = 5, "cherven" = 6, "lypen" = 7, "serpen" = 8,
    "veresen" = 9, "zhovten" = 10, "lystopad" = 11, "gruden" = 12
  )
  
  # Extract year
  year_match <- str_extract(filename, "20\\d{2}")
  
  if (!is.na(month_match) && !is.na(year_match)) {
    month_num <- month_map[month_match]
    return(as.Date(paste(year_match, month_num, "01", sep = "-")))
  }
  
  return(NA)
}

# Function to download and process a single BM file
download_bm_file <- function(url) {
  filename <- basename(url)
  temp_file <- file.path(tempdir(), filename)
  
  message("Downloading: ", filename)
  
  tryCatch({
    # Use chromote to download
    b <- ChromoteSession$new()
    
    b$Browser$setDownloadBehavior(
      behavior = "allow",
      downloadPath = tempdir()
    )
    
    b$Page$navigate(url)
    Sys.sleep(10)
    b$close()
    
    # Find downloaded file
    files <- list.files(tempdir(), pattern = basename(url), full.names = TRUE)
    
    if (length(files) == 0) {
      message("  Download failed")
      return(tibble())
    }
    
    # Read Excel file, skip first 2 rows
    bm_data <- read_excel(files[1], skip = 2, col_names = c(
      "date", "time", "volume_up", "price_up", "volume_down", "price_down"
    ))
    
    # Process the data
    result <- bm_data |>
      fill(date, .direction = "down") |>  # Fill down the date column
      mutate(
        country = "UA",
        # Extract hour from time range (e.g., "00:00 - 01:00" -> 0)
        hour_num = as.numeric(str_extract(time, "^\\d+")),
        # Combine date and hour
        hour = ymd_h(paste(format(date, "%Y-%m-%d"), hour_num), tz = "UTC"),
        # Convert volumes and prices to numeric
        volume_up = as.numeric(volume_up),
        volume_down = as.numeric(volume_down),
        price_up = as.numeric(price_up),
        price_down = as.numeric(price_down)
      ) |>
      filter(!is.na(date), !is.na(hour)) |>
      select(country, hour, volume_up, price_up, volume_down, price_down)
    
    file.remove(files[1])
    message("  Processed ", nrow(result), " rows")
    return(result)
    
  }, error = function(e) {
    message("  Error: ", e$message)
    return(tibble())
  })
}

#============================================================================
# Ukraine Auxiliary Services Market (ASM) Functions
#============================================================================

# Direction label mapping (Ukrainian -> English)
# "Симетричний" = symmetric, "Вгору" = up, "Вниз" = down
asm_direction_map <- c(
  "\u0421\u0438\u043c\u0435\u0442\u0440\u0438\u0447\u043d\u0438\u0439" = "symmetric",  # Симетричний
  "\u0421\u0438\u043c\u0435\u0442\u0440\u0438\u0447\u043d\u043e"       = "symmetric",  # Симетрично (alt)
  "\u0412\u0433\u043e\u0440\u0443"                                     = "up",         # Вгору
  "\u0412\u043d\u0438\u0437"                                           = "down"        # Вниз
)

# Extract all ASM URLs from the saved HTML links file
extract_asm_urls <- function(filepath = "data/data_raw/ASM links.txt") {
  html_text <- paste(readLines(filepath, encoding = "UTF-8"), collapse = "\n")
  page <- rvest::read_html(html_text)
  links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.xlsx$")
  unique(links)
}

# Download and parse a single ASM xlsx — returns a tidy data frame
# combining FCR, aFRR and mFRR sheets
download_asm_file <- function(url) {
  filename <- basename(url)
  temp_path <- file.path(tempdir(), filename)

  message("Downloading ASM: ", filename)

  tryCatch({
    download.file(url, destfile = temp_path, mode = "wb", quiet = TRUE,
                  headers = c("User-Agent" = paste0(
                    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                    "AppleWebKit/537.36 (KHTML, like Gecko) ",
                    "Chrome/120.0.0.0 Safari/537.36"
                  )))

    sheets <- readxl::excel_sheets(temp_path)
    # Keep only known ASM sheets
    asm_sheets <- intersect(sheets, c("FCR", "aFRR", "mFRR"))

    if (length(asm_sheets) == 0) {
      message("  No FCR/aFRR/mFRR sheets found in ", filename)
      return(tibble::tibble())
    }

    result <- purrr::map_dfr(asm_sheets, function(sheet) {
      # Read with header row (skip=0), then select the 7 data columns by position
      # (sheets contain 5 trailing empty columns that vary by file version)
      raw <- readxl::read_excel(temp_path, sheet = sheet, col_names = FALSE)

      # Row 1 is the Ukrainian header — data starts from row 2
      # Keep only the first 7 columns: zone, type, date, interval, direction, volume, price
      raw <- raw[2:nrow(raw), 1:7]
      colnames(raw) <- c("trading_zone", "reserve_type", "date_raw",
                         "hour_interval", "direction_ua", "volume_mw", "price_uah")

      raw |>
        dplyr::filter(!is.na(date_raw), !is.na(hour_interval)) |>
        dplyr::mutate(
          # date_raw is character when cols are selected by position after
          # col_names=FALSE read; convert Excel serial number to Date
          date = as.Date(as.numeric(date_raw), origin = "1899-12-30"),
          # Extract start hour from "HH:MM-HH:MM"
          hour      = as.integer(stringr::str_extract(as.character(hour_interval), "^\\d+")),
          volume_mw = suppressWarnings(as.numeric(volume_mw)),
          price_uah = suppressWarnings(as.numeric(price_uah)),
          direction = dplyr::recode(as.character(direction_ua),
                                    !!!asm_direction_map, .default = direction_ua),
          reserve_type = sheet
        ) |>
        dplyr::filter(!is.na(date), !is.na(hour)) |>
        dplyr::select(date, hour, reserve_type, direction, volume_mw, price_uah)
    })

    file.remove(temp_path)
    message("  Processed ", nrow(result), " rows (FCR/aFRR/mFRR combined)")
    result

  }, error = function(e) {
    message("  Error: ", e$message)
    tibble::tibble()
  })
}
