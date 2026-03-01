#============================================================================
# Functions to download data for EU countries from ENTSO-E
#============================================================================

# Function to determine start date based on existing file (returns Date)

get_start_date <- function(filepath, date_col = "hour", default_start = as.Date("2022-01-01")) {
  if (file.exists(filepath)) {
    existing_data <- read_csv(filepath, show_col_types = FALSE)
    
    # Parse ISO 8601 datetime format and convert to Date
    parsed_dates <- as.Date(ymd_hms(existing_data[[date_col]], tz = "UTC"))
    last_date <- max(parsed_dates, na.rm = TRUE)
    
    # Check if parsing was successful
    if (is.na(last_date) || !is.finite(last_date)) {
      warning("Could not parse dates from ", filepath, ". Using default start date.")
      return(default_start)
    }
    
    return(last_date + days(1))
  } else {
    return(default_start)
  }
}

# Download RES generation data
download_gen_eu <- function(zones, gen_types, start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  gen_all <- map_df(names(zones), function(z) {
    map_df(date_chunks, function(chunk) {
      
      map_df(gen_types, possibly(function(gen_type) {
        gen_raw <- gen_per_prod_type(
          eic = zones[z],
          period_start = chunk$start,
          period_end   = chunk$end,
          gen_type     = gen_type,
          tidy_output  = TRUE
        )
        
        gen_raw |>
          mutate(
            country = z,
            tech = recode(ts_mkt_psr_type,
                          "B16" = "Solar", 
                          "B19" = "Wind onshore"),
            hour = floor_date(ts_point_dt_start, unit = "hour")
          ) |>
          group_by(country, hour, tech) |>
          summarise(
            gen_mw = mean(ts_point_quantity, na.rm = TRUE),
            .groups = "drop"
          )
      }, otherwise = tibble()))
    })
  })
  
  # Complete missing combinations AFTER collecting all data
  gen_all |>
    complete(
      country, 
      hour, 
      tech, 
      fill = list(gen_mw = 0)
    )
}

# Download DAM price data 
download_price_eu <- function(zones, start_datetime, end_datetime, chunk_days = 365, time_aggregate = TRUE) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  result <- map_df(names(zones), function(country) {
    map_df(date_chunks, function(chunk) {
      px_raw <- transm_day_ahead_prices(
        eic = zones[country],
        period_start = chunk$start,
        period_end   = chunk$end,
        tidy_output  = TRUE
      )
      
      if (time_aggregate) {
        # Aggregate to hourly
        px_raw |>
          mutate(
            country = country,
            hour = floor_date(ts_point_dt_start, unit = "hour")
          ) |>
          group_by(country, hour) |>
          summarise(
            price_eur = mean(ts_point_price_amount, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # Keep original resolution
        px_raw |>
          mutate(
            country = country,
            hour = ts_point_dt_start
          ) |>
          select(country, hour, price_eur = ts_point_price_amount)
      }
    })
  }) |>
    filter(
      hour >= start_datetime,
      hour < end_datetime
    )
  
  return(result)
}

# Download total load data
download_load_eu <- function(zones, start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  map_df(names(zones), function(country) {
    map_df(date_chunks, function(chunk) {
      load_raw <- load_actual_total( 
        eic = zones[country],
        period_start = chunk$start,
        period_end   = chunk$end,
        tidy_output  = TRUE
      )
      
      load_raw |>
        mutate(
          country = country,
          hour = floor_date(ts_point_dt_start, unit = "hour")
        ) |>
        group_by(country, hour) |>
        summarise(
          volume = mean(ts_point_quantity, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }) |>
    filter(
      hour >= start_datetime,
      hour < end_datetime
    )
}


# Create date chunks for API requests
create_date_chunks <- function(start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- list()
  current_start <- start_datetime
  
  while (current_start < end_datetime) {
    chunk_end <- min(current_start + days(chunk_days), end_datetime)
    date_chunks[[length(date_chunks) + 1]] <- list(
      start = current_start,
      end = chunk_end
    )
    current_start <- chunk_end
  }
  
  return(date_chunks)
}

# Update CSV file with new data
update_csv_file <- function(new_data, filepath, start_date, end_date, datetime_col = "hour") {
  if (nrow(new_data) > 0) {
    if (file.exists(filepath)) {
      existing_data <- read_csv(filepath, show_col_types = FALSE)
      # Remove existing rows from start_date onward to avoid duplicates
      existing_data <- existing_data |>
        filter(as.Date(.data[[datetime_col]]) < start_date)
      combined_data <- bind_rows(existing_data, new_data)
    } else {
      combined_data <- new_data
    }
    
    write_csv(combined_data, filepath)
    message(basename(filepath), " updated from ", start_date, " to ", end_date)
  } else {
    message("No new data retrieved for ", basename(filepath))
  }
}

# Download scheduled commercial exchange data
download_transm_sched_eu <- function(zone_pairs, start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  map_df(1:nrow(zone_pairs), function(i) {
    from_country <- zone_pairs$from_country[i]
    to_country <- zone_pairs$to_country[i]
    from_eic <- zone_pairs$from_eic[i]
    to_eic <- zone_pairs$to_eic[i]
    
    map_df(date_chunks, possibly(function(chunk) {
      sched_raw <- transm_total_comm_sched(
        eic_in = to_eic,
        eic_out = from_eic,
        period_start = chunk$start,
        period_end = chunk$end,
        tidy_output = TRUE
      )
      
      if (nrow(sched_raw) == 0 || !"ts_point_dt_start" %in% names(sched_raw)) {
        return(tibble::tibble())
      }
      
      sched_raw |>
        mutate(
          from_country = from_country,
          to_country = to_country,
          hour = floor_date(ts_point_dt_start, unit = "hour")
        ) |>
        group_by(from_country, to_country, hour) |>
        summarise(
          scheduled_mw = mean(ts_point_quantity, na.rm = TRUE),
          .groups = "drop"
        )
    }, otherwise = tibble::tibble()))
  }) |>
    filter(
      hour >= start_datetime,
      hour < end_datetime
    )
}

# Download cross-border physical flow data
download_transm_phys_eu <- function(zone_pairs, start_datetime, end_datetime, chunk_days = 365) {
  date_chunks <- create_date_chunks(start_datetime, end_datetime, chunk_days = chunk_days)
  
  map_df(1:nrow(zone_pairs), function(i) {
    from_country <- zone_pairs$from_country[i]
    to_country <- zone_pairs$to_country[i]
    from_eic <- zone_pairs$from_eic[i]
    to_eic <- zone_pairs$to_eic[i]
    
    map_df(date_chunks, possibly(function(chunk) {
      phys_raw <- transm_x_border_phys_flow(
        eic_in = to_eic,
        eic_out = from_eic,
        period_start = chunk$start,
        period_end = chunk$end,
        tidy_output = TRUE
      )
      
      if (nrow(phys_raw) == 0 || !"ts_point_dt_start" %in% names(phys_raw)) {
        return(tibble::tibble())
      }
      
      phys_raw |>
        mutate(
          from_country = from_country,
          to_country = to_country,
          hour = floor_date(ts_point_dt_start, unit = "hour")
        ) |>
        group_by(from_country, to_country, hour) |>
        summarise(
          physical_flow_mw = mean(ts_point_quantity, na.rm = TRUE),
          .groups = "drop"
        )
    }, otherwise = tibble::tibble()))
  }) |>
    filter(
      hour >= start_datetime,
      hour < end_datetime
    )
}

# Function to download ECB exchange rates
get_ecb_exchange_rate <- function(currency_code, start_date, end_date) {
  # ECB Data Portal API endpoint for exchange rates
  # Format: https://data-api.ecb.europa.eu/service/data/EXR/D.{CURRENCY}.EUR.SP00.A
  
  url <- glue::glue(
    "https://data-api.ecb.europa.eu/service/data/EXR/D.{currency_code}.EUR.SP00.A",
    "?startPeriod={start_date}&endPeriod={end_date}&format=csvdata"
  )
  
  tryCatch({
    # Download CSV data
    response <- httr::GET(url)
    
    if (httr::http_error(response)) {
      stop("ECB API request failed for ", currency_code, ": ", httr::status_code(response))
    }
    
    # Parse CSV response
    csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
    fx_data <- read.csv(text = csv_text, stringsAsFactors = FALSE)
    
    # Extract date and rate columns
    # ECB returns: TIME_PERIOD, OBS_VALUE, etc.
    result <- fx_data |>
      dplyr::transmute(
        date = as.Date(TIME_PERIOD),
        rate = as.numeric(OBS_VALUE)
      ) |>
      dplyr::filter(!is.na(rate), !is.na(date)) |>
      dplyr::arrange(date)
    
    if (nrow(result) == 0) {
      warning("No exchange rate data returned for ", currency_code)
    }
    
    return(result)
    
  }, error = function(e) {
    message("Error downloading ECB rates for ", currency_code, ": ", e$message)
    return(tibble::tibble(date = as.Date(character()), rate = numeric()))
  })
}

# Function to get ECB exchange rates and convert prices to EUR
convert_to_eur <- function(df, date_col = "hour", currency_col = "currency", 
                          price_col = "price", start_date = "2022-01-01", 
                          end_date = "2025-12-31") {
  
  currencies_to_convert <- unique(df[[currency_col]])
  currencies_to_convert <- currencies_to_convert[currencies_to_convert != "EUR"]
  
  # Download rates starting 5 days earlier to handle holidays at year start
  fx_start_date <- as.character(as.Date(start_date) - days(5))
  
  exchange_rates <- map_df(currencies_to_convert, function(curr) {
    message("Downloading exchange rates for ", curr)
    get_ecb_exchange_rate(curr, fx_start_date, end_date) |>
      mutate(currency = curr)
  }) |>
    # Fill forward rates for weekends/holidays
    group_by(currency) |>
    complete(date = seq(min(date), max(date), by = "1 day")) |>
    fill(rate, .direction = "down") |>
    ungroup()
  
  df |>
    mutate(date = as.Date(.data[[date_col]])) |>
    left_join(exchange_rates, by = c("currency", "date")) |>
    mutate(
      price_eur = if_else(
        .data[[currency_col]] == "EUR",
        .data[[price_col]],
        .data[[price_col]] / rate  # Divide because ECB rate is "1 EUR = X currency"
      )
    ) |>
    select(-date, -rate)
}

# Function to standardize DAM prices to 15-minute resolution
# Expands hourly observations to four 15-minute slots with the same price
standardize_dam_to_15min <- function(df, time_col = "hour", value_cols = "price_eur") {
  
  # Parse datetime and group by all columns except time
  group_cols <- setdiff(names(df), time_col)
  
  df_parsed <- df |>
    mutate(hour_parsed = ymd_hms(.data[[time_col]], tz = "UTC", quiet = TRUE)) |>
    group_by(across(all_of(group_cols))) |>
    arrange(hour_parsed, .by_group = TRUE) |>
    mutate(
      time_diff = as.numeric(difftime(hour_parsed, lag(hour_parsed), units = "mins"))
    ) |>
    ungroup()
  
  # Identify hourly observations (first obs per group or 60+ minute gaps)
  hourly_obs <- df_parsed |>
    filter(is.na(time_diff) | time_diff >= 60)
  
  # Identify sub-hourly observations (15-minute or 30-minute intervals)
  subhourly_obs <- df_parsed |>
    filter(!is.na(time_diff) & time_diff < 60)
  
  # Expand hourly observations to 15-minute resolution
  if (nrow(hourly_obs) > 0) {
    hourly_expanded <- hourly_obs |>
      select(-time_diff, -!!sym(time_col)) |>
      mutate(
        # Create 4 time slots: original hour + 0, 15, 30, 45 minutes
        hour_list = map(hour_parsed, ~ .x + minutes(c(0, 15, 30, 45)))
      ) |>
      unnest(hour_list) |>
      select(-hour_parsed) |>
      rename(!!sym(time_col) := hour_list)
  } else {
    hourly_expanded <- tibble()
  }
  
  # Combine expanded hourly data with original sub-hourly data
  result <- bind_rows(
    hourly_expanded,
    subhourly_obs |> select(-time_diff, -hour_parsed)
  ) |>
    # Remove any duplicate timestamps (can occur during DST transitions)
    distinct() |>
    arrange(across(all_of(c(setdiff(names(df), c(time_col, value_cols)), time_col))))
  
  return(result)
}
