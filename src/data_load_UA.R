# Load required packages and functions
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(httr)
library(jsonlite)
library(lubridate)
library(glue)

source("src/helper_func_UA.R")

# Define end date
end_date <- floor_date(today(), "month") - days(1)  # Last day of previous month

# Function to determine start date based on existing file
get_start_date <- function(filepath, default_start = as.Date("2022-01-01")) {
  if (file.exists(filepath)) {
    existing_data <- read_csv(filepath, show_col_types = FALSE)
    last_date <- max(as.Date(existing_data$date))
    return(last_date + days(1))
  } else {
    return(default_start)
  }
}

# Determine start dates for each dataset
dam_start <- get_start_date("data/data_raw/DAM_UA.csv")
solar_start <- get_start_date("data/data_raw/yield_solar_UA.csv")
wind_start <- get_start_date("data/data_raw/yield_wind_UA.csv")

# Download and update DAM data
if (dam_start <= end_date) {
  fx_data <- get_nbu_fx(dam_start, end_date, valcode = "EUR")
  
  new_dam <- download_dam_ua(dam_start, end_date) |>
    mutate(date = as.Date(hour)) |>
    left_join(fx_data, by = "date") |> 
    mutate(price_eur_mwh = price_uah / rate) 
  
  if (file.exists("data/data_raw/DAM_UA.csv")) {
    existing_dam <- read_csv("data/data_raw/DAM_UA.csv", show_col_types = FALSE)
    dam_ua <- bind_rows(existing_dam, new_dam)
  } else {
    dam_ua <- new_dam
  }
  
  write_csv(dam_ua, "data/data_raw/DAM_UA.csv")
  message("DAM data updated from ", dam_start, " to ", end_date)
} else {
  message("DAM data is up to date")
}

# Download and update solar yield data
if (solar_start <= end_date) {
  dates <- seq(solar_start, end_date, by = "day")
  new_solar <- download_yield_list(dates, gen = "1") |>
    bind_rows() |>
    mutate(date = as.Date(date))
  
  if (file.exists("data/data_raw/yield_solar_UA.csv")) {
    existing_solar <- read_csv("data/data_raw/yield_solar_UA.csv", show_col_types = FALSE)
    solar_data <- bind_rows(existing_solar, new_solar)
  } else {
    solar_data <- new_solar
  }
  
  write_csv(solar_data, "data/data_raw/yield_solar_UA.csv")
  message("Solar yield data updated from ", solar_start, " to ", end_date)
} else {
  message("Solar yield data is up to date")
}

# Download and update wind yield data
if (wind_start <= end_date) {
  dates <- seq(wind_start, end_date, by = "day")
  new_wind <- download_yield_list(dates, gen = "2") |>
    bind_rows() |>
    mutate(date = as.Date(date))
  
  if (file.exists("data/data_raw/yield_wind_UA.csv")) {
    existing_wind <- read_csv("data/data_raw/yield_wind_UA.csv", show_col_types = FALSE)
    wind_data <- bind_rows(existing_wind, new_wind)
  } else {
    wind_data <- new_wind
  }
  
  write_csv(wind_data, "data/data_raw/yield_wind_UA.csv")
  message("Wind yield data updated from ", wind_start, " to ", end_date)
} else {
  message("Wind yield data is up to date")
}