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

#============================================================================
# Contracted aFRR Reserves Functions
#============================================================================

# Download contracted reserve capacity from ENTSOE API
# Document Type A81: Contracted reserves
# Business Type B95: Procured capacity
# processType: A51 = Automatic frequency restoration reserve (aFRR)
#              A52 = Frequency containment reserve (FCR)
#              A47 = Manual frequency restoration reserve (mFRR)
#              A46 = Replacement reserve (RR)

contracted_reserves <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  market_agreement_type = "A13",  # Default to hourly
  process_type = "A51",  # Default to aFRR
  psr_type = NULL,  # Optional: A03 = Mixed; A04 = Generation; A05 = Load
  offset = 0,  # For pagination (0-4800)
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
  if (offset < 0 || offset > 4800) stop("Offset must be between 0 and 4800")
  
  # Convert timestamps
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  # Build query string for contracted reserves (A81)
  query_string <- paste0(
    "documentType=A81",
    "&businessType=B95",
    "&Type_MarketAgreement.Type=", market_agreement_type,
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    "&processType=", process_type,
    if (is.null(psr_type)) "" else paste0("&psrType=", psr_type),
    if (offset > 0) paste0("&offset=", offset) else ""
  )
  
  # Make API request
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string,
    security_token = security_token
  )
  
  # Extract and return response
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

# Process contracted reserves data from entsoeapi

process_contracted_reserves <- function(data) {
  
  if (nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  # Unnest the ts_point column to get individual reserve observations
  data_unnested <- data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name,
      ts_flow_direction_def,
      ts_business_type_def,
      ts_process_process_type_def,
      ts_contract_market_agreement_type_def,
      ts_resolution,
      ts_time_interval_start,
      ts_time_interval_end,
      ts_point_ts_point_position,
      ts_point_ts_point_quantity,
      ts_quantity_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name,
      direction = ts_flow_direction_def,
      business_type = ts_business_type_def,
      process_type = ts_process_process_type_def,
      contract_type = ts_contract_market_agreement_type_def,
      resolution = ts_resolution,
      interval_start = ts_time_interval_start,
      interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      contracted_capacity = ts_point_ts_point_quantity,
      unit = ts_quantity_measure_unit_name
    )
  
  # Calculate actual timestamp for each point based on position and resolution
  data_unnested <- data_unnested |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30,
        resolution == "P1D" ~ 1440,   # Daily
        resolution == "P1M" ~ NA_real_, # Monthly - handled separately
        TRUE ~ NA_real_
      ),
      datetime = dplyr::case_when(
        resolution == "P1M" ~ interval_start,  # For monthly, use interval start
        TRUE ~ interval_start + lubridate::minutes((position - 1) * resolution_minutes)
      )
    ) |>
    dplyr::select(
      country,
      datetime,
      direction,
      business_type,
      process_type,
      contract_type,
      contracted_capacity,
      unit
    )
  
  return(data_unnested)
}

# High-level wrapper to get contracted reserves for a single market agreement type
# Handles pagination automatically when data exceeds 100 documents

get_contracted_reserves <- function(
  eic,
  period_start,
  period_end,
  market_agreement_type = "A13",
  process_type = "A51",  # A51 = aFRR, A52 = FCR, A47 = mFRR, A46 = RR
  psr_type = NULL,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  
  all_data <- list()
  offset <- 0
  max_offset <- 4800
  
  repeat {
    # Call the contracted_reserves function with current offset
    raw_data <- tryCatch({
      contracted_reserves(
        eic = eic,
        period_start = period_start,
        period_end = period_end,
        market_agreement_type = market_agreement_type,
        process_type = process_type,
        psr_type = psr_type,
        offset = offset,
        tidy_output = FALSE,
        security_token = security_token
      )
    }, error = function(e) {
      # Check if error is about exceeding 100 documents
      if (grepl("exceeds the allowed maximum", e$message)) {
        message("    Data exceeds 100 documents at offset ", offset, 
                ". Consider using smaller time chunks.")
      }
      return(NULL)
    })
    
    # If no data returned or error, break
    if (is.null(raw_data) || nrow(raw_data) == 0) {
      break
    }
    
    # Process and store the data
    processed_data <- process_contracted_reserves(raw_data)
    
    if (nrow(processed_data) > 0) {
      all_data[[length(all_data) + 1]] <- processed_data
    }
    
    # If we got less than 100 rows, we've reached the end
    if (nrow(raw_data) < 100) {
      break
    }
    
    # Move to next page
    offset <- offset + 100
    
    # Safety check to avoid infinite loops
    if (offset > max_offset) {
      warning("Reached maximum offset (4800). Some data may be missing.")
      break
    }
    
    # Small delay to be nice to the API
    Sys.sleep(0.5)
  }
  
  # Combine all pages
  if (length(all_data) > 0) {
    return(dplyr::bind_rows(all_data))
  } else {
    return(tibble::tibble())
  }
}

# Download contracted reserves for multiple zones and all market agreement types

download_contracted_reserves_eu <- function(zones, start_datetime, end_datetime, 
                                           process_type = "A51", chunk_days = 365) {
  
  # Define market agreement types with appropriate chunk sizes
  # Hourly data is extremely granular (15-min resolution × 2 directions)
  # Each hour = 4 slots × 2 directions = 8 documents
  # 12 hours = 96 documents (just under 100 limit)
  market_configs <- list(
    A13 = list(type = "A13", name = "Hourly", chunk_hours = 12),   # 12 hours = ~96 documents
    A01 = list(type = "A01", name = "Daily", chunk_days = 7)       # 7 days for daily
  )
  
  result <- map_df(names(zones), function(country) {
    map_df(market_configs, function(mkt_config) {
      
      # Create appropriate chunks based on config
      if (!is.null(mkt_config$chunk_hours)) {
        # For hourly data, chunk by hours
        chunk_duration <- lubridate::hours(mkt_config$chunk_hours)
        date_chunks <- list()
        current_start <- start_datetime
        
        while (current_start < end_datetime) {
          chunk_end <- min(current_start + chunk_duration, end_datetime)
          date_chunks[[length(date_chunks) + 1]] <- list(
            start = current_start,
            end = chunk_end
          )
          current_start <- chunk_end
        }
      } else {
        # For other data, use standard day-based chunks
        date_chunks <- create_date_chunks(start_datetime, end_datetime, 
                                         chunk_days = mkt_config$chunk_days)
      }
      
      map_df(date_chunks, function(chunk) {
        tryCatch({
          reserves_raw <- get_contracted_reserves(
            eic = zones[country],
            period_start = chunk$start,
            period_end = chunk$end,
            market_agreement_type = mkt_config$type,
            process_type = process_type
          )
          
          if (nrow(reserves_raw) > 0) {
            message("  ✓ ", mkt_config$name, " ", country, " ", 
                    format(chunk$start, "%Y-%m-%d %H:%M"), ": ", 
                    nrow(reserves_raw), " rows")
            return(reserves_raw)
          } else {
            return(tibble())
          }
        }, error = function(e) {
          message("  ✗ ", country, " ", mkt_config$name, " ", 
                  format(chunk$start, "%Y-%m-%d %H:%M"), ": ", e$message)
          return(tibble())
        })
      })
    })
  })
  
  # Only filter if there's data with a datetime column
  if (nrow(result) > 0 && "datetime" %in% names(result)) {
    result <- result |> filter(datetime >= start_datetime, datetime < end_datetime)
  }
  
  return(result)
}
