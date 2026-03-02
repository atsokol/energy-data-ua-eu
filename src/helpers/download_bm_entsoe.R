#============================================================================
# ENTSOE Balancing Market Functions
# Balancing prices, volumes, and contracted reserves
#============================================================================

#============================================================================
# Balancing Prices
#============================================================================

balancing_prices <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }
  if (security_token == "") stop("Valid security token should be provided.")
  
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  query_string <- paste0(
    "documentType=A84",
    "&processType=A16",
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start,
    "&periodEnd=", period_end,
    if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
  )
  
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string,
    security_token = security_token
  )
  
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

process_balancing_prices <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())
  
  data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name, ts_flow_direction_def, ts_business_type_def,
      ts_resolution, ts_time_interval_start, ts_time_interval_end,
      ts_point_ts_point_position, ts_point_ts_point_activation_price_amount,
      ts_currency_unit_name, ts_price_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name, direction = ts_flow_direction_def,
      reserve_type = ts_business_type_def, resolution = ts_resolution,
      interval_start = ts_time_interval_start, interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      price = ts_point_ts_point_activation_price_amount,
      currency = ts_currency_unit_name, unit = ts_price_measure_unit_name
    ) |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15, resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30, TRUE ~ NA_real_
      ),
      datetime = interval_start + lubridate::minutes((position - 1) * resolution_minutes)
    ) |>
    dplyr::select(country, datetime, direction, reserve_type, price, currency, unit)
}

get_balancing_prices <- function(eic, period_start, period_end,
                                security_token = Sys.getenv("ENTSOE_PAT")) {
  raw_data <- balancing_prices(
    eic = eic, period_start = period_start, period_end = period_end,
    reserve_type = NULL, tidy_output = FALSE, security_token = security_token
  )
  process_balancing_prices(raw_data)
}

download_balancing_prices_eu <- function(zones, start_datetime, end_datetime,
                                        chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  purrr::map_df(names(zones), function(country) {
    purrr::map_df(date_chunks, function(chunk) {
      tryCatch({
        bm_raw <- get_balancing_prices(
          eic = zones[country], period_start = chunk$start, period_end = chunk$end
        )
        if (nrow(bm_raw) > 0) bm_raw else dplyr::tibble()
      }, error = function(e) {
        message("  Error for ", country, " (", format(chunk$start, "%Y-%m-%d"),
                " to ", format(chunk$end, "%Y-%m-%d"), "): ", e$message)
        dplyr::tibble()
      })
    })
  }) |>
    dplyr::filter(datetime >= start_datetime, datetime < end_datetime)
}

#============================================================================
# Balancing Volumes
#============================================================================

balancing_volumes <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  reserve_type = NULL,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }
  if (security_token == "") stop("Valid security token should be provided.")
  
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  query_string <- paste0(
    "documentType=A24", "&processType=A51",
    "&area_Domain=", eic,
    "&periodStart=", period_start, "&periodEnd=", period_end,
    if (is.null(reserve_type)) "" else paste0("&businessType=", reserve_type)
  )
  
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string, security_token = security_token
  )
  
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

process_balancing_volumes <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())
  
  data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name, ts_flow_direction_def, ts_business_type_def,
      ts_resolution, ts_time_interval_start, ts_time_interval_end,
      ts_point_ts_point_position, ts_point_ts_point_quantity,
      ts_point_ts_point_secondary_quantity, ts_quantity_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name, direction = ts_flow_direction_def,
      reserve_type = ts_business_type_def, resolution = ts_resolution,
      interval_start = ts_time_interval_start, interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      capacity_offered = ts_point_ts_point_quantity,
      energy_activated = ts_point_ts_point_secondary_quantity,
      unit = ts_quantity_measure_unit_name
    ) |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15, resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30, TRUE ~ NA_real_
      ),
      datetime = interval_start + lubridate::minutes((position - 1) * resolution_minutes)
    ) |>
    dplyr::select(country, datetime, direction, reserve_type,
                  capacity_offered, energy_activated, unit)
}

get_balancing_volumes <- function(eic, period_start, period_end,
                                 security_token = Sys.getenv("ENTSOE_PAT")) {
  raw_data <- balancing_volumes(
    eic = eic, period_start = period_start, period_end = period_end,
    reserve_type = NULL, tidy_output = FALSE, security_token = security_token
  )
  process_balancing_volumes(raw_data)
}

download_balancing_volumes_eu <- function(zones, start_datetime, end_datetime,
                                         chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  purrr::map_df(names(zones), function(country) {
    purrr::map_df(date_chunks, function(chunk) {
      tryCatch({
        bm_raw <- get_balancing_volumes(
          eic = zones[country], period_start = chunk$start, period_end = chunk$end
        )
        if (nrow(bm_raw) > 0) bm_raw else dplyr::tibble()
      }, error = function(e) {
        message("  Error for ", country, " (", format(chunk$start, "%Y-%m-%d"),
                " to ", format(chunk$end, "%Y-%m-%d"), "): ", e$message)
        dplyr::tibble()
      })
    })
  }) |>
    dplyr::filter(datetime >= start_datetime, datetime < end_datetime)
}

#============================================================================
# Contracted Reserves
#============================================================================

contracted_reserves <- function(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(7), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  market_agreement_type = "A13",
  process_type = "A51",
  psr_type = NULL,
  offset = 0,
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
) {
  if (is.null(eic)) stop("One control area EIC should be provided.")
  if (length(eic) > 1L) stop("This wrapper only supports one control area EIC per request.")
  if (difftime(period_end, period_start, units = "day") > 365) {
    stop("One year range limit should be applied!")
  }
  if (security_token == "") stop("Valid security token should be provided.")
  if (offset < 0 || offset > 4800) stop("Offset must be between 0 and 4800")
  
  period_start <- entsoeapi:::url_posixct_format(period_start)
  period_end <- entsoeapi:::url_posixct_format(period_end)
  
  query_string <- paste0(
    "documentType=A81", "&businessType=B95",
    "&Type_MarketAgreement.Type=", market_agreement_type,
    "&controlArea_Domain=", eic,
    "&periodStart=", period_start, "&periodEnd=", period_end,
    "&processType=", process_type,
    if (is.null(psr_type)) "" else paste0("&psrType=", psr_type),
    if (offset > 0) paste0("&offset=", offset) else ""
  )
  
  en_cont_list <- entsoeapi:::api_req_safe(
    query_string = query_string, security_token = security_token
  )
  
  return(entsoeapi:::extract_response(content = en_cont_list, tidy_output = tidy_output))
}

process_contracted_reserves <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())
  
  data |>
    tidyr::unnest(ts_point, names_sep = "_") |>
    dplyr::select(
      area_domain_name, ts_flow_direction_def, ts_business_type_def,
      ts_process_process_type_def, ts_contract_market_agreement_type_def,
      ts_resolution, ts_time_interval_start, ts_time_interval_end,
      ts_point_ts_point_position, ts_point_ts_point_quantity,
      ts_quantity_measure_unit_name
    ) |>
    dplyr::rename(
      country = area_domain_name, direction = ts_flow_direction_def,
      business_type = ts_business_type_def,
      process_type = ts_process_process_type_def,
      contract_type = ts_contract_market_agreement_type_def,
      resolution = ts_resolution,
      interval_start = ts_time_interval_start, interval_end = ts_time_interval_end,
      position = ts_point_ts_point_position,
      contracted_capacity = ts_point_ts_point_quantity,
      unit = ts_quantity_measure_unit_name
    ) |>
    dplyr::mutate(
      resolution_minutes = dplyr::case_when(
        resolution == "PT15M" ~ 15, resolution == "PT60M" ~ 60,
        resolution == "PT30M" ~ 30, resolution == "P1D" ~ 1440,
        resolution == "P1M" ~ NA_real_, TRUE ~ NA_real_
      ),
      datetime = dplyr::case_when(
        resolution == "P1M" ~ interval_start,
        TRUE ~ interval_start + lubridate::minutes((position - 1) * resolution_minutes)
      )
    ) |>
    dplyr::select(country, datetime, direction, business_type, process_type,
                  contract_type, contracted_capacity, unit)
}

get_contracted_reserves <- function(
  eic, period_start, period_end,
  market_agreement_type = "A13", process_type = "A51",
  psr_type = NULL, security_token = Sys.getenv("ENTSOE_PAT")
) {
  all_data <- list()
  offset <- 0
  
  repeat {
    raw_data <- tryCatch({
      contracted_reserves(
        eic = eic, period_start = period_start, period_end = period_end,
        market_agreement_type = market_agreement_type,
        process_type = process_type, psr_type = psr_type,
        offset = offset, tidy_output = FALSE, security_token = security_token
      )
    }, error = function(e) {
      if (grepl("exceeds the allowed maximum", e$message)) {
        message("    Data exceeds 100 documents at offset ", offset)
      }
      return(NULL)
    })
    
    if (is.null(raw_data) || nrow(raw_data) == 0) break
    
    processed_data <- process_contracted_reserves(raw_data)
    if (nrow(processed_data) > 0) {
      all_data[[length(all_data) + 1]] <- processed_data
    }
    
    if (nrow(raw_data) < 100) break
    offset <- offset + 100
    if (offset > 4800) {
      warning("Reached maximum offset (4800). Some data may be missing.")
      break
    }
    Sys.sleep(0.5)
  }
  
  if (length(all_data) > 0) dplyr::bind_rows(all_data)
  else tibble::tibble()
}

download_contracted_reserves_eu <- function(zones, start_datetime, end_datetime,
                                           process_type = "A51", chunk_days = 365) {
  market_configs <- list(
    A13 = list(type = "A13", name = "Hourly", chunk_hours = 12),
    A01 = list(type = "A01", name = "Daily", chunk_days = 7)
  )
  
  result <- purrr::map_df(names(zones), function(country) {
    purrr::map_df(market_configs, function(mkt_config) {
      if (!is.null(mkt_config$chunk_hours)) {
        chunk_duration <- lubridate::hours(mkt_config$chunk_hours)
        date_chunks <- list()
        current_start <- start_datetime
        while (current_start < end_datetime) {
          chunk_end <- min(current_start + chunk_duration, end_datetime)
          date_chunks[[length(date_chunks) + 1]] <- list(
            start = current_start, end = chunk_end
          )
          current_start <- chunk_end
        }
      } else {
        date_chunks <- create_date_chunks(start_datetime, end_datetime,
                                         chunk_days = mkt_config$chunk_days)
      }
      
      purrr::map_df(date_chunks, function(chunk) {
        tryCatch({
          reserves_raw <- get_contracted_reserves(
            eic = zones[country], period_start = chunk$start,
            period_end = chunk$end,
            market_agreement_type = mkt_config$type,
            process_type = process_type
          )
          if (nrow(reserves_raw) > 0) {
            message("  ✓ ", mkt_config$name, " ", country, " ",
                    format(chunk$start, "%Y-%m-%d %H:%M"), ": ",
                    nrow(reserves_raw), " rows")
            reserves_raw
          } else {
            dplyr::tibble()
          }
        }, error = function(e) {
          message("  ✗ ", country, " ", mkt_config$name, " ",
                  format(chunk$start, "%Y-%m-%d %H:%M"), ": ", e$message)
          dplyr::tibble()
        })
      })
    })
  })
  
  if (nrow(result) > 0 && "datetime" %in% names(result)) {
    result <- result |> dplyr::filter(datetime >= start_datetime,
                                      datetime < end_datetime)
  }
  
  return(result)
}
