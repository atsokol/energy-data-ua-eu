#============================================================================
# ENTSO-E download functions for generation, prices, load, transmission
# Pure functions — no side effects, no file I/O
# Wraps entsoeapi calls with chunking and error handling
#============================================================================

#' Create date chunks for API requests (max ~365 days per call)
create_date_chunks <- function(start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- list()
  current_start <- start_datetime
  
  while (current_start < end_datetime) {
    chunk_end <- min(current_start + lubridate::days(chunk_days), end_datetime)
    date_chunks[[length(date_chunks) + 1]] <- list(
      start = current_start, end = chunk_end
    )
    current_start <- chunk_end
  }
  
  return(date_chunks)
}

#' Download RES generation data from ENTSO-E
download_gen_eu <- function(zones, gen_types, start_datetime, end_datetime, 
                            chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  gen_all <- purrr::map_df(names(zones), function(z) {
    purrr::map_df(date_chunks, function(chunk) {
      purrr::map_df(gen_types, purrr::possibly(function(gen_type) {
        gen_raw <- entsoeapi::gen_per_prod_type(
          eic = zones[z],
          period_start = chunk$start,
          period_end   = chunk$end,
          gen_type     = gen_type,
          tidy_output  = TRUE
        )
        
        gen_raw |>
          dplyr::mutate(
            country = z,
            tech = dplyr::recode(ts_mkt_psr_type,
                                 "B16" = "Solar", "B19" = "Wind onshore"),
            hour = lubridate::floor_date(ts_point_dt_start, unit = "hour")
          ) |>
          dplyr::group_by(country, hour, tech) |>
          dplyr::summarise(gen_mw = mean(ts_point_quantity, na.rm = TRUE),
                           .groups = "drop")
      }, otherwise = dplyr::tibble()))
    })
  })
  
  gen_all |>
    tidyr::complete(country, hour, tech, fill = list(gen_mw = 0))
}

#' Download DAM price data from ENTSO-E
download_price_eu <- function(zones, start_datetime, end_datetime,
                              chunk_days = 365, time_aggregate = TRUE) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  result <- purrr::map_df(names(zones), function(country) {
    purrr::map_df(date_chunks, function(chunk) {
      px_raw <- entsoeapi::transm_day_ahead_prices(
        eic = zones[country],
        period_start = chunk$start,
        period_end   = chunk$end,
        tidy_output  = TRUE
      )
      
      if (time_aggregate) {
        px_raw |>
          dplyr::mutate(country = country,
                        hour = lubridate::floor_date(ts_point_dt_start, unit = "hour")) |>
          dplyr::group_by(country, hour) |>
          dplyr::summarise(price_eur = mean(ts_point_price_amount, na.rm = TRUE),
                           .groups = "drop")
      } else {
        px_raw |>
          dplyr::mutate(country = country, hour = ts_point_dt_start) |>
          dplyr::select(country, hour, price_eur = ts_point_price_amount)
      }
    })
  }) |>
    dplyr::filter(hour >= start_datetime, hour < end_datetime)
  
  return(result)
}

#' Download total load data from ENTSO-E
download_load_eu <- function(zones, start_datetime, end_datetime,
                             chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  purrr::map_df(names(zones), function(country) {
    purrr::map_df(date_chunks, function(chunk) {
      load_raw <- entsoeapi::load_actual_total(
        eic = zones[country],
        period_start = chunk$start,
        period_end   = chunk$end,
        tidy_output  = TRUE
      )
      
      load_raw |>
        dplyr::mutate(country = country,
                      hour = lubridate::floor_date(ts_point_dt_start, unit = "hour")) |>
        dplyr::group_by(country, hour) |>
        dplyr::summarise(volume = mean(ts_point_quantity, na.rm = TRUE),
                         .groups = "drop")
    })
  }) |>
    dplyr::filter(hour >= start_datetime, hour < end_datetime)
}

#' Download scheduled commercial exchange data from ENTSO-E
download_transm_sched_eu <- function(zone_pairs, start_datetime, end_datetime,
                                     chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  purrr::map_df(1:nrow(zone_pairs), function(i) {
    from_country <- zone_pairs$from_country[i]
    to_country   <- zone_pairs$to_country[i]
    from_eic     <- zone_pairs$from_eic[i]
    to_eic       <- zone_pairs$to_eic[i]
    
    purrr::map_df(date_chunks, purrr::possibly(function(chunk) {
      sched_raw <- entsoeapi::transm_total_comm_sched(
        eic_in = to_eic, eic_out = from_eic,
        period_start = chunk$start, period_end = chunk$end,
        tidy_output = TRUE
      )
      
      if (nrow(sched_raw) == 0 || !"ts_point_dt_start" %in% names(sched_raw)) {
        return(tibble::tibble())
      }
      
      sched_raw |>
        dplyr::mutate(from_country = from_country, to_country = to_country,
                      hour = lubridate::floor_date(ts_point_dt_start, unit = "hour")) |>
        dplyr::group_by(from_country, to_country, hour) |>
        dplyr::summarise(scheduled_mw = mean(ts_point_quantity, na.rm = TRUE),
                         .groups = "drop")
    }, otherwise = tibble::tibble()))
  }) |>
    dplyr::filter(hour >= start_datetime, hour < end_datetime)
}

#' Download cross-border physical flow data from ENTSO-E
download_transm_phys_eu <- function(zone_pairs, start_datetime, end_datetime,
                                    chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days)
  
  purrr::map_df(1:nrow(zone_pairs), function(i) {
    from_country <- zone_pairs$from_country[i]
    to_country   <- zone_pairs$to_country[i]
    from_eic     <- zone_pairs$from_eic[i]
    to_eic       <- zone_pairs$to_eic[i]
    
    purrr::map_df(date_chunks, purrr::possibly(function(chunk) {
      phys_raw <- entsoeapi::transm_x_border_phys_flow(
        eic_in = to_eic, eic_out = from_eic,
        period_start = chunk$start, period_end = chunk$end,
        tidy_output = TRUE
      )
      
      if (nrow(phys_raw) == 0 || !"ts_point_dt_start" %in% names(phys_raw)) {
        return(tibble::tibble())
      }
      
      phys_raw |>
        dplyr::mutate(from_country = from_country, to_country = to_country,
                      hour = lubridate::floor_date(ts_point_dt_start, unit = "hour")) |>
        dplyr::group_by(from_country, to_country, hour) |>
        dplyr::summarise(physical_flow_mw = mean(ts_point_quantity, na.rm = TRUE),
                         .groups = "drop")
    }, otherwise = tibble::tibble()))
  }) |>
    dplyr::filter(hour >= start_datetime, hour < end_datetime)
}
