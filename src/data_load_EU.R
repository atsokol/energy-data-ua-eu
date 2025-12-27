# Load required packages and functions
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(entsoeapi)
library(jsonlite)
library(httr)

source("src/helper_func_EU.R")

# Define end date
end_date <- floor_date(today(), "month") - days(1)  # Last day of previous month

# Function to determine start date based on existing file
get_start_date <- function(filepath, default_start = as.Date("2022-01-01")) {
  if (file.exists(filepath)) {
    existing_data <- read_csv(filepath, show_col_types = FALSE)
    last_date <- max(as.Date(existing_data$hour))
    return(last_date + days(1))
  } else {
    return(default_start)
  }
}

# Determine start dates for each dataset
gen_start <- get_start_date("data/data_raw/yield_RES_EU.csv")
price_start <- get_start_date("data/data_raw/DAM_EU.csv")
load_start <- get_start_date("data/data_raw/load_EU.csv")

# Download and update RES generation data
if (gen_start <= end_date) {
  new_gen <- download_gen_eu(gen_start, end_date)
  
  if (file.exists("data/data_raw/yield_RES_EU.csv")) {
    existing_gen <- read_csv("data/data_raw/yield_RES_EU.csv", show_col_types = FALSE)
    gen_data <- bind_rows(existing_gen, new_gen)
  } else {
    gen_data <- new_gen
  }
  
  write_csv(gen_data, "data/data_raw/yield_RES_EU.csv")
  message("RES generation data updated from ", gen_start, " to ", end_date)
} else {
  message("RES generation data is up to date")
}

# Download and update DAM price data
if (price_start <= end_date) {
  new_price <- download_price_eu(price_start, end_date)
  
  if (file.exists("data/data_raw/DAM_EU.csv")) {
    existing_price <- read_csv("data/data_raw/DAM_EU.csv", show_col_types = FALSE)
    price_data <- bind_rows(existing_price, new_price)
  } else {
    price_data <- new_price
  }
  
  write_csv(price_data, "data/data_raw/DAM_EU.csv")
  message("DAM price data updated from ", price_start, " to ", end_date)
} else {
  message("DAM price data is up to date")
}

# Download and update load data
if (load_start <= end_date) {
  new_load <- download_load_eu(load_start, end_date)
  
  if (file.exists("data/data_raw/load_EU.csv")) {
    existing_load <- read_csv("data/data_raw/load_EU.csv", show_col_types = FALSE)
    load_data <- bind_rows(existing_load, new_load)
  } else {
    load_data <- new_load
  }
  
  write_csv(load_data, "data/data_raw/load_EU.csv")
  message("Load data updated from ", load_start, " to ", end_date)
} else {
  message("Load data is up to date")
}