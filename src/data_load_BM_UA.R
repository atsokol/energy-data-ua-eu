# Load required packages and functions
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)

# NOTE: This script requires a VPN connection to Ukraine due to geo-restrictions
# on ua.energy website

source("src/helper_func_BM_UA.R")

# Function to get the last date from existing file
get_last_bm_date <- function(filepath) {
  if (file.exists(filepath)) {
    existing_data <- read_csv(filepath, show_col_types = FALSE)
    last_date <- max(as.Date(existing_data$date), na.rm = TRUE)
    
    # Return the first day of the last month to re-download partial months
    return(floor_date(last_date, "month"))
  } else {
    return(as.Date("2022-01-01"))
  }
}

# Get the last date we have data for
last_date <- get_last_bm_date("data/data_raw/BM_UA.csv")
message("Last complete month in data: ", format(last_date - days(1), "%Y-%m"))

# Get all BM URLs from website
all_urls <- get_all_bm_urls()

if (length(all_urls) == 0) {
  message("No BM URLs found on website")
} else {
  # Filter for URLs with dates >= last_date
  new_urls <- all_urls[sapply(all_urls, function(url) {
    file_date <- extract_bm_date(basename(url))
    !is.na(file_date) && file_date >= last_date
  })]
  
  message("Found ", length(new_urls), " file(s) to download")
  
  if (length(new_urls) == 0) {
    message("BM data is up to date")
  } else {
    message("Downloading ", length(new_urls), " new file(s)...")
    
    # Download and process each new file
    new_bm_data <- map_dfr(new_urls, download_bm_file)
    
    if (nrow(new_bm_data) > 0) {
      # Process new data: pivot and join with DAM data
      new_bm_processed <- new_bm_data |>
        mutate(
          date = as_date(hour),
          hour_num = hour(hour)
        ) |>
        pivot_longer(
          cols = c(volume_up, price_up, volume_down, price_down),
          names_to = c(".value", "direction"),
          names_sep = "_"
        ) |> 
        rename(
          price_bm_uah = price,
          volume_bm = volume
        ) |> 
        left_join(
          read_csv("data/data_raw/DAM_UA.csv", show_col_types = FALSE) |>
            mutate(
              date = as_date(as.POSIXct(hour, tz = "UTC")),
              hour_num = hour(as.POSIXct(hour, tz = "UTC"))
            ) |>
            select(country, date, hour_num, price_dam_eur = price_eur_mwh, price_dam_uah = price_uah, volume_dam = volume, rate),
          by = join_by(country, date, hour_num),
          relationship = "many-to-one"
        ) |> 
        mutate(
          price_bm_eur = price_bm_uah / rate,
          .before = price_bm_uah
        ) |>
        filter(!(volume_bm == 0 & price_bm_eur == 0)) |>
        select(-hour) |>
        rename(hour = hour_num) |>
        relocate(country, date, hour, direction)
      
      # Read existing data if it exists and combine
      if (file.exists("data/data_raw/BM_UA.csv")) {
        existing_bm <- read_csv("data/data_raw/BM_UA.csv", show_col_types = FALSE)
        
        # Combine and remove duplicates
        bm_ua <- bind_rows(existing_bm, new_bm_processed) |>
          distinct(country, date, hour, direction, .keep_all = TRUE) |>
          arrange(date, hour, direction)
      } else {
        bm_ua <- new_bm_processed |>
          arrange(date, hour, direction)
      }

      # Save
      write_csv(bm_ua, "data/data_raw/BM_UA.csv")
      message("BM data updated. Total rows: ", nrow(bm_ua))
      
    } else {
      
      message("No data retrieved from files")
    }
  }
}
