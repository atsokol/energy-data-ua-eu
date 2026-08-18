#============================================================================
# Task: Download Polish PV curtailment and system data from PSE S.A.
#
# Outputs (all under data/data_raw/pl/):
#   pse_redispatch_res.csv  — 15-min PV & wind non-market redispatch [MW]
#   pse_kse_load.csv        — 15-min KSE system load actual + forecast [MW]
#
# Static reports (downloaded once to data/data_raw/pl/reports/):
#   gus_renewable_energy_2023.pdf     — GUS energy statistics 2023
#   gus_renewable_energy_2024.pdf     — GUS energy statistics 2024
#   ure_balancing_market_reform.pdf   — URE balancing market reform assessment
#
# API data coverage: 2024-09-27 onwards (PSE new portal launch date)
# Endpoints: https://api.raporty.pse.pl/api/
#============================================================================

task_pse_pl <- function(end_date = lubridate::today() - lubridate::days(1)) {

  library(dplyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(glue)

  source("src/helpers/csv_utils.R")
  source("src/helpers/download_pse_pl.R")

  PSE_START <- as.Date("2024-09-27")   # first day data exists on new portal
  dir.create("data/data_raw/pl/reports", recursive = TRUE, showWarnings = FALSE)

  results <- list()

  # ── Helper ──────────────────────────────────────────────────────────────────
  # Saves each chunk immediately after download so progress survives interrupts.
  run_subtask <- function(name, filepath, endpoint, chunk_days = 30) {
    start_date <- get_start_date(
      filepath,
      datetime_col  = "dtime",
      default_start = PSE_START
    )

    if (start_date > end_date) {
      message("  [SKIP] ", name, " is up to date")
      return(list(task = name, status = "skipped", message = "Up to date"))
    }

    message("  Downloading ", name, ": ", start_date, " to ", end_date)

    chunk_starts <- seq.Date(start_date, end_date,
                             by = paste(chunk_days, "days"))
    total_rows <- 0L
    n_errors   <- 0L

    for (i in seq_along(chunk_starts)) {
      chunk_start <- chunk_starts[[i]]           # [[]] preserves Date class
      chunk_end   <- min(chunk_start + chunk_days - 1L, end_date)
      message("    [PSE/", endpoint, "] ", chunk_start, " → ", chunk_end)

      chunk_data <- tryCatch({
        attempt <- 1L
        repeat {
          result <- tryCatch(
            download_pse_endpoint(endpoint, chunk_start, chunk_end),
            error = function(e) e
          )
          if (!inherits(result, "error")) break
          if (attempt >= 3L) stop(conditionMessage(result))
          Sys.sleep(2 ^ attempt)
          attempt <- attempt + 1L
        }
        result
      }, error = function(e) {
        message("    [ERROR] chunk failed: ", e$message)
        n_errors <<- n_errors + 1L
        NULL
      })

      if (!is.null(chunk_data) && nrow(chunk_data) > 0) {
        update_csv(chunk_data, filepath, chunk_start, chunk_end,
                   datetime_col = "dtime")
        total_rows <- total_rows + nrow(chunk_data)
      }
    }

    status <- if (n_errors > 0) "warning" else "success"
    list(task = name, status = status,
         message = paste0(total_rows, " rows",
                          if (n_errors > 0) paste0(", ", n_errors, " chunk errors")))
  }

  # ── API sub-tasks ────────────────────────────────────────────────────────────

  message("  --- poze-redoze (PV & wind non-market redispatch) ---")
  results$redispatch <- run_subtask(
    name       = "PSE redispatch RES",
    filepath   = "data/data_raw/pl/pse_redispatch_res.csv",
    endpoint   = "poze-redoze",
    chunk_days = 30
  )

  message("  --- kse-load (KSE system load) ---")
  results$kse_load <- run_subtask(
    name       = "PSE KSE load",
    filepath   = "data/data_raw/pl/pse_kse_load.csv",
    endpoint   = "kse-load",
    chunk_days = 7   # API hangs on pagination beyond ~7 days per request
  )

  # ── Static PDF downloads (download once, skip if file exists) ───────────────

  reports_dir <- "data/data_raw/pl/reports"
  static_files <- list(
    list(
      filename = "gus_renewable_energy_2023.pdf",
      url      = "https://new.stat.gov.pl/file/117083/download?token=4wkH85ec",
      label    = "GUS Renewable Energy 2023"
    ),
    list(
      filename = "gus_renewable_energy_2024.pdf",
      url      = "https://rzeszow.stat.gov.pl/download/gfx/rzeszow/en/defaultaktualnosci/916/5/3/1/energy_from_renewable_sources_2024.pdf",
      label    = "GUS Renewable Energy 2024"
    ),
    list(
      filename = "ure_balancing_market_reform.pdf",
      url      = "https://www.ure.gov.pl/download/9/15866/Ocenawplywureformyrynkubilansujacego.pdf",
      label    = "URE Balancing Market Reform Assessment"
    )
  )

  pdf_statuses <- map(static_files, function(f) {
    dest <- file.path(reports_dir, f$filename)

    if (file.exists(dest)) {
      message("  [SKIP] ", f$label, " already archived")
      return(list(task = f$label, status = "skipped", message = "File exists"))
    }

    message("  Downloading ", f$label, " ...")
    tryCatch({
      resp <- httr::GET(
        f$url,
        httr::user_agent("Mozilla/5.0"),
        httr::write_disk(dest, overwrite = FALSE),
        httr::timeout(120)
      )
      if (httr::http_error(resp)) {
        file.remove(dest)
        stop("HTTP ", httr::status_code(resp))
      }
      size_kb <- round(file.size(dest) / 1024)
      message("  [OK] ", f$label, " (", size_kb, " KB)")
      list(task = f$label, status = "success", message = paste0(size_kb, " KB"))
    }, error = function(e) {
      if (file.exists(dest)) file.remove(dest)
      message("  [ERROR] ", f$label, ": ", e$message)
      list(task = f$label, status = "error", message = e$message)
    })
  })

  results <- c(results, setNames(pdf_statuses, map_chr(static_files, "filename")))

  # ── Summary ────────────────────────────────────────────────────────────────
  statuses <- sapply(results, \(r) r$status)
  n_ok   <- sum(statuses == "success")
  n_skip <- sum(statuses == "skipped")
  n_warn <- sum(statuses == "warning")
  n_err  <- sum(statuses == "error")

  overall <- if (n_err > 0) "error" else if (n_warn > 0) "warning" else "success"

  list(
    task    = "pse_pl",
    status  = overall,
    message = paste0(n_ok, " ok, ", n_skip, " skipped, ",
                     n_warn, " warnings, ", n_err, " errors"),
    details = results
  )
}

if (sys.nframe() == 0) {
  result <- task_pse_pl()
  message("\n", result$task, ": ", result$status, " — ", result$message)
  if (result$status == "error") quit(status = 1)
}
