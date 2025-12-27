# Load required packages and functions
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(entsoeapi)

# Read ENTSOE_PAT from environment variable
Sys.setenv(ENTSOE_PAT = Sys.getenv("ENTSOE_PAT"))

source("src/helper_func_EU.R")

# Define ENTSO-E zones and generation types
zones <- c(
  PL = "10YPL-AREA-----S",
  RO = "10YRO-TEL------P",
  HU = "10YHU-MAVIR----U",
  SK = "10YSK-SEPS-----K"
)

gen_types <- c("B16", "B19") # Solar and Wind onshore

# Define end date - convert to datetime
end_date <- floor_date(today(), "month")  # First day of current month
end_datetime <- ymd_hms(paste(end_date, "00:00:00"), tz = "UTC")

# Function to determine start date based on existing file
get_start_datetime <- function(filepath, default_start = ymd_hms("2022-01-01 00:00:00", tz = "UTC")) {
  if (file.exists(filepath)) {
    existing_data <- read_csv(filepath, show_col_types = FALSE)
    
    # Parse datetime format with Z suffix
    last_datetime <- max(ymd_hms(existing_data$hour, tz = "UTC"), na.rm = TRUE)
    
    # Check if parsing was successful
    if (is.na(last_datetime) || !is.finite(last_datetime)) {
      warning("Could not parse dates from ", filepath, ". Using default start date.")
      return(default_start)
    }
    
    return(last_datetime + hours(1))
  } else {
    return(default_start)
  }
}

# Determine start dates for each dataset
gen_start <- get_start_datetime("data/data_raw/yield_RES_EU.csv")
price_start <- get_start_datetime("data/data_raw/DAM_EU.csv")
load_start <- get_start_datetime("data/data_raw/load_EU.csv")

# Download and update RES generation data
if (gen_start <= end_datetime) {
  new_gen <- download_gen_eu(zones, gen_types, gen_start, end_datetime)
  
  if (file.exists("data/data_raw/yield_RES_EU.csv")) {
    existing_gen <- read_csv("data/data_raw/yield_RES_EU.csv", show_col_types = FALSE)
    gen_data <- bind_rows(existing_gen, new_gen)
  } else {
    gen_data <- new_gen
  }
  
  write_csv(gen_data, "data/data_raw/yield_RES_EU.csv")
  message("RES generation data updated from ", gen_start, " to ", end_datetime)
} else {
  message("RES generation data is up to date")
}

# Download and update DAM price data
if (price_start <= end_datetime) {
  new_price <- download_price_eu(zones, price_start, end_datetime)
  
  if (file.exists("data/data_raw/DAM_EU.csv")) {
    existing_price <- read_csv("data/data_raw/DAM_EU.csv", show_col_types = FALSE)
    price_data <- bind_rows(existing_price, new_price)
  } else {
    price_data <- new_price
  }
  
  write_csv(price_data, "data/data_raw/DAM_EU.csv")
  message("DAM price data updated from ", price_start, " to ", end_datetime)
} else {
  message("DAM price data is up to date")
}

# Download and update load data
if (load_start <= end_datetime) {
  new_load <- download_load_eu(zones, load_start, end_datetime)
  
  if (file.exists("data/data_raw/load_EU.csv")) {
    existing_load <- read_csv("data/data_raw/load_EU.csv", show_col_types = FALSE)
    load_data <- bind_rows(existing_load, new_load)
  } else {
    load_data <- new_load
  }
  
  write_csv(load_data, "data/data_raw/load_EU.csv")
  message("Load data updated from ", load_start, " to ", end_datetime)
} else {
  message("Load data is up to date")
}
