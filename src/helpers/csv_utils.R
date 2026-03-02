#============================================================================
# Standardized CSV read/merge/write/validate utilities
#============================================================================

#' Get start date from an existing CSV file
#' 
#' Reads only the datetime column to determine where to resume downloading.
#' Returns the day after the last available date, or default_start if file missing.
#'
#' @param filepath Path to the CSV file
#' @param datetime_col Name of the datetime column (default "hour")
#' @param default_start Default start date if file doesn't exist
#' @return A Date object
get_start_date <- function(filepath, datetime_col = "hour", 
                           default_start = as.Date("2022-01-01")) {
  if (!file.exists(filepath)) {
    return(as.Date(default_start))
  }
  
  tryCatch({
    existing_data <- readr::read_csv(filepath, show_col_types = FALSE)
    
    if (nrow(existing_data) == 0 || !datetime_col %in% names(existing_data)) {
      warning("File ", filepath, " is empty or missing column '", datetime_col, "'")
      return(as.Date(default_start))
    }
    
    # Try parsing as datetime first, fall back to Date
    raw_col <- existing_data[[datetime_col]]
    parsed <- suppressWarnings(as.Date(lubridate::ymd_hms(raw_col, tz = "UTC")))
    if (all(is.na(parsed))) {
      parsed <- suppressWarnings(as.Date(raw_col))
    }
    
    last_date <- max(parsed, na.rm = TRUE)
    
    if (is.na(last_date) || !is.finite(last_date)) {
      warning("Could not parse dates from ", filepath)
      return(as.Date(default_start))
    }
    
    return(last_date + lubridate::days(1))
  }, error = function(e) {
    warning("Error reading ", filepath, ": ", e$message)
    return(as.Date(default_start))
  })
}

#' Get start date for monthly files (BM_UA, ASM_UA)
#' 
#' Returns the first day of the last available month, so partial months
#' get re-downloaded.
#'
#' @param filepath Path to the CSV file
#' @param date_col Name of the date column (default "date")
#' @param default_start Default start date if file doesn't exist
#' @return A Date object (first day of a month)
get_start_date_monthly <- function(filepath, date_col = "date",
                                   default_start = as.Date("2022-01-01")) {
  if (!file.exists(filepath)) {
    return(as.Date(default_start))
  }
  
  tryCatch({
    existing_data <- readr::read_csv(filepath, show_col_types = FALSE)
    
    if (nrow(existing_data) == 0 || !date_col %in% names(existing_data)) {
      return(as.Date(default_start))
    }
    
    last_date <- max(as.Date(existing_data[[date_col]]), na.rm = TRUE)
    
    if (is.na(last_date)) return(as.Date(default_start))
    
    # Return first day of last month so partial months get re-downloaded
    return(lubridate::floor_date(last_date, "month"))
  }, error = function(e) {
    warning("Error reading ", filepath, ": ", e$message)
    return(as.Date(default_start))
  })
}

#' Update a CSV file with new data (safe merge with dedup + validation)
#'
#' Removes overlapping rows from start_date onward in the existing file,
#' appends new data, validates schema consistency, and writes atomically
#' (to a temp file first, then renames).
#'
#' @param new_data Tibble of new data
#' @param filepath Path to the target CSV file
#' @param start_date The start date of the new data (used to cut old data)
#' @param end_date The end date (for logging only)
#' @param datetime_col Column name used for date filtering
#' @return TRUE on success, FALSE on failure (never throws)
update_csv <- function(new_data, filepath, start_date, end_date, 
                       datetime_col = "hour") {
  
  if (is.null(new_data) || nrow(new_data) == 0) {
    message("  [SKIP] No new data for ", basename(filepath))
    return(FALSE)
  }
  
  tryCatch({
    if (file.exists(filepath)) {
      existing_data <- readr::read_csv(filepath, show_col_types = FALSE)
      old_rows <- nrow(existing_data)
      
      # Validate schema: new data columns must be a subset of existing
      missing_cols <- setdiff(names(existing_data), names(new_data))
      extra_cols <- setdiff(names(new_data), names(existing_data))
      if (length(missing_cols) > 0) {
        warning("  New data is missing columns: ", paste(missing_cols, collapse = ", "))
      }
      if (length(extra_cols) > 0) {
        warning("  New data has extra columns: ", paste(extra_cols, collapse = ", "))
      }
      
      # Remove existing rows from start_date onward
      existing_data <- existing_data |>
        dplyr::filter(as.Date(.data[[datetime_col]]) < start_date)
      combined_data <- dplyr::bind_rows(existing_data, new_data)
    } else {
      old_rows <- 0
      combined_data <- new_data
    }
    
    new_rows <- nrow(combined_data)
    
    # Sanity check: warn if file would shrink by > 20%
    if (old_rows > 0 && new_rows < old_rows * 0.8) {
      warning("  [WARN] Row count dropped from ", old_rows, " to ", new_rows,
              " (", round((1 - new_rows / old_rows) * 100), "% decrease)")
    }
    
    # Atomic write: write to temp file, then rename
    tmp_path <- paste0(filepath, ".tmp")
    readr::write_csv(combined_data, tmp_path)
    file.rename(tmp_path, filepath)
    
    message("  [OK] ", basename(filepath), ": ", nrow(new_data), " new rows",
            " (total: ", new_rows, ")")
    return(TRUE)
    
  }, error = function(e) {
    message("  [ERROR] Failed to update ", basename(filepath), ": ", e$message)
    # Clean up temp file if it exists
    tmp_path <- paste0(filepath, ".tmp")
    if (file.exists(tmp_path)) file.remove(tmp_path)
    return(FALSE)
  })
}

#' Validate a CSV file exists, is non-empty, and has expected columns
#'
#' @param filepath Path to the CSV file
#' @param expected_cols Character vector of expected column names (optional)
#' @param min_rows Minimum expected number of rows (default 1)
#' @return A list with ok (logical), rows (integer), message (character)
validate_csv <- function(filepath, expected_cols = NULL, min_rows = 1) {
  if (!file.exists(filepath)) {
    return(list(ok = FALSE, rows = 0, 
                message = paste0(basename(filepath), ": file not found")))
  }
  
  tryCatch({
    data <- readr::read_csv(filepath, show_col_types = FALSE, n_max = 5)
    total_rows <- nrow(readr::read_csv(filepath, show_col_types = FALSE, 
                                        lazy = TRUE))
    
    issues <- character()
    
    if (total_rows < min_rows) {
      issues <- c(issues, paste0("only ", total_rows, " rows (expected >= ", min_rows, ")"))
    }
    
    if (!is.null(expected_cols)) {
      missing <- setdiff(expected_cols, names(data))
      if (length(missing) > 0) {
        issues <- c(issues, paste0("missing columns: ", paste(missing, collapse = ", ")))
      }
    }
    
    if (length(issues) > 0) {
      return(list(ok = FALSE, rows = total_rows,
                  message = paste0(basename(filepath), ": ", 
                                   paste(issues, collapse = "; "))))
    }
    
    return(list(ok = TRUE, rows = total_rows,
                message = paste0(basename(filepath), ": OK (", total_rows, " rows)")))
    
  }, error = function(e) {
    return(list(ok = FALSE, rows = 0,
                message = paste0(basename(filepath), ": read error - ", e$message)))
  })
}
