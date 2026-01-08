#============================================================================
# ENTSOE Balancing Market Functions
#============================================================================

library(entsoeapi)
library(dplyr)
library(tidyr)
library(lubridate)

#============================================================================
# Balancing Prices Functions
#============================================================================

# Download balancing prices from ENTSOE API

balancing_prices <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  
  # Check inputs
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }
  if (security_token == "") stop("Valid security token should be provided.")
  
  # Convert timestamps
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  # Build query string
  query_string <- paste0(
    "documentType=A84",
    "&processType=A16",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
  )
  
  # Make API request
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string,
    security_token = security_token
  )
  
  # Extract and return response
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

# Process balancing prices data from entsoeapi

process_balancing_prices <- function(data) {
  
  if (nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  # Unnest the ts_point column to get individual price observations
  data_unnested <- data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name,
      ts_flow_direction_def,
      ts_business_type_def,
      ts_resolution,
      ts_time_interval_start,
      ts_time_interval_end,
      ts_point_ts_point_position,
      ts_point_ts_point_activation_price_amount,
      ts_currency_unit_name,
      ts_price_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name,
      direction = ts_flow_direction_def,
      reserve_type = ts_business_type_def,
      resolution = ts_resolution,
      interval_start = ts_time_interval_start,
      interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      price = ts_point_ts_point_activation_price_amount,
      currency = ts_currency_unit_name,
      unit = ts_price_measure_unit_name
    )
  
  # Calculate actual timestamp for each point based on position and resolution
  data_unnested <- data_unnested |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30,
        TRUE ~ NA_real_
      ),
      datetime = interval_start + lubridate::minutes((position - 1) * resolution_minutes)
    ) |>
    dplyr::select(
      country,
      datetime,
      direction,
      reserve_type,
      price,
      currency,
      unit
    )
  
  return(data_unnested)
}


# Download balancing prices from ENTSOE

get_balancing_prices <- function(
  eic,
  period_start,
  period_end,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  
  # Call the balancing_prices function with tidy_output = FALSE
  raw_data <- balancing_prices(
    eic = eic,
    period_start = period_start,
    period_end = period_end,
    reserve_type = NULL,
    tidy_output = FALSE,
    security_token = security_token
  )
  
  # Process the nested structure
  processed_data <- process_balancing_prices(raw_data)
  
  return(processed_data)
}

# Download balancing prices for multiple zones

download_balancing_prices_eu <- function(zones, start_datetime, end_datetime, chunk_days = 365) {
  
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  map_df(names(zones), function(country) {
    map_df(date_chunks, function(chunk) {
      tryCatch({
        bm_raw <- get_balancing_prices(
          eic = zones[country],
          period_start = chunk$start,
          period_end = chunk$end
        )
        
        if (nrow(bm_raw) > 0) {
          return(bm_raw)
        } else {
          return(tibble())
        }
      }, error = function(e) {
        message("  Error for ", country, " (", 
                format(chunk$start, "%Y-%m-%d"), " to ", 
                format(chunk$end, "%Y-%m-%d"), "): ", e$message)
        return(tibble())
      })
    })
  }) |>
    filter(
      datetime >= start_datetime,
      datetime < end_datetime
    )
}

#============================================================================
# Balancing Energy Volumes Functions  
#============================================================================

# Download balancing energy volumes from ENTSOE API
# Note: A24 = Aggregated Balancing Energy Bids, processType A51

balancing_volumes <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  
  # Check inputs
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }
  if (security_token == "") stop("Valid security token should be provided.")
  
  # Convert timestamps
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  # Build query string for aggregated balancing energy bids (A24)
  query_string <- paste0(
    "documentType=A24",
    "&processType=A51",
    "&area_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
  )
  
  # Make API request
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string,
    security_token = security_token
  )
  
  # Extract and return response
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

# Process balancing volumes data from entsoeapi

process_balancing_volumes <- function(data) {
  
  if (nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  # Unnest the ts_point column to get individual volume observations
  data_unnested <- data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name,
      ts_flow_direction_def,
      ts_business_type_def,
      ts_resolution,
      ts_time_interval_start,
      ts_time_interval_end,
      ts_point_ts_point_position,
      ts_point_ts_point_quantity,
      ts_point_ts_point_secondary_quantity,
      ts_quantity_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name,
      direction = ts_flow_direction_def,
      reserve_type = ts_business_type_def,
      resolution = ts_resolution,
      interval_start = ts_time_interval_start,
      interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      capacity_offered = ts_point_ts_point_quantity,
      energy_activated = ts_point_ts_point_secondary_quantity,
      unit = ts_quantity_measure_unit_name
    )
  
  # Calculate actual timestamp for each point based on position and resolution
  data_unnested <- data_unnested |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30,
        TRUE ~ NA_real_
      ),
      datetime = interval_start + lubridate::minutes((position - 1) * resolution_minutes)
    ) |>
    dplyr::select(
      country,
      datetime,
      direction,
      reserve_type,
      capacity_offered,
      energy_activated,
      unit
    )
  
  return(data_unnested)
}

# High-level wrapper to get balancing volumes

get_balancing_volumes <- function(
  eic,
  period_start,
  period_end,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  
  # Call the balancing_volumes function with tidy_output = FALSE
  raw_data <- balancing_volumes(
    eic = eic,
    period_start = period_start,
    period_end = period_end,
    reserve_type = NULL,
    tidy_output = FALSE,
    security_token = security_token
  )
  
  # Process the nested structure
  processed_data <- process_balancing_volumes(raw_data)
  
  return(processed_data)
}

# Download balancing volumes for multiple zones

download_balancing_volumes_eu <- function(zones, start_datetime, end_datetime, chunk_days = 365) {
  
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  map_df(names(zones), function(country) {
    map_df(date_chunks, function(chunk) {
      tryCatch({
        bm_raw <- get_balancing_volumes(
          eic = zones[country],
          period_start = chunk$start,
          period_end = chunk$end
        )
        
        if (nrow(bm_raw) > 0) {
          return(bm_raw)
        } else {
          message("  No volume data for ", country, " (", 
                  format(chunk$start, "%Y-%m-%d"), " to ", 
                  format(chunk$end, "%Y-%m-%d"), ")")
          return(tibble())
        }
      }, error = function(e) {
        message("  Error for ", country, " (", 
                format(chunk$start, "%Y-%m-%d"), " to ", 
                format(chunk$end, "%Y-%m-%d"), "): ", e$message)
        return(tibble())
      })
    })
  }) |>
    filter(
      datetime >= start_datetime,
      datetime < end_datetime
    )
}
