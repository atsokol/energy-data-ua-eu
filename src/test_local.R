#============================================================================
# Local test script
#
# Simulates what GitHub Actions does: runs each task in a FRESH R session
# via callr::r() to catch function masking and environment issues.
#
# Usage:
#   Rscript src/test_local.R          # test all tasks
#   Rscript src/test_local.R dam_ua   # test one task
#============================================================================

if (!requireNamespace("callr", quietly = TRUE)) {
  install.packages("callr")
}

args <- commandArgs(trailingOnly = TRUE)

# ── Test definitions ─────────────────────────────────────────────────────────

tests <- list(
  
  csv_utils = list(
    name = "csv_utils (unit tests)",
    fn = function() {
      source("src/helpers/csv_utils.R")
      
      # Test get_start_date with missing file
      d <- get_start_date("nonexistent_file.csv")
      stopifnot(d == as.Date("2022-01-01"))
      
      # Test update_csv with temp files
      tmp <- tempfile(fileext = ".csv")
      df1 <- data.frame(
        hour = as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "UTC"),
        value = c(10, 20)
      )
      readr::write_csv(df1, tmp)
      
      df2 <- data.frame(
        hour = as.POSIXct(c("2025-01-02", "2025-01-03"), tz = "UTC"),
        value = c(25, 30)
      )
      ok <- update_csv(df2, tmp, as.Date("2025-01-02"), as.Date("2025-01-03"))
      stopifnot(ok)
      
      result <- readr::read_csv(tmp, show_col_types = FALSE)
      stopifnot(nrow(result) == 3)  # Jan 1 kept + Jan 2-3 new
      stopifnot(result$value[result$hour == as.POSIXct("2025-01-02", tz = "UTC")] == 25)
      
      # Test validate_csv
      v <- validate_csv(tmp, expected_cols = c("hour", "value"))
      stopifnot(v$ok)
      
      v2 <- validate_csv(tmp, expected_cols = c("hour", "missing_col"))
      stopifnot(!v2$ok)
      
      file.remove(tmp)
      
      # Test update_csv with empty data
      ok2 <- update_csv(data.frame(), tmp, as.Date("2025-01-01"), as.Date("2025-01-01"))
      stopifnot(!ok2)
      
      # Test get_start_date_monthly
      tmp2 <- tempfile(fileext = ".csv")
      df3 <- data.frame(date = as.Date(c("2025-03-15", "2025-03-20")), x = 1:2)
      readr::write_csv(df3, tmp2)
      d2 <- get_start_date_monthly(tmp2, date_col = "date")
      stopifnot(d2 == as.Date("2025-03-01"))  # first of last month
      file.remove(tmp2)
      
      list(task = "csv_utils", status = "success", message = "All unit tests passed")
    }
  ),
  
  dam_ua = list(
    name = "DAM UA (fresh session)",
    fn = function() {
      source("src/tasks/task_dam_ua.R")
      task_dam_ua()
    }
  ),
  
  solar_ua = list(
    name = "Solar UA (fresh session)",
    fn = function() {
      source("src/tasks/task_solar_ua.R")
      task_solar_ua()
    }
  ),
  
  wind_ua = list(
    name = "Wind UA (fresh session)",
    fn = function() {
      source("src/tasks/task_wind_ua.R")
      task_wind_ua()
    }
  ),
  
  eu = list(
    name = "EU data / ENTSO-E (fresh session)",
    fn = function() {
      source("src/tasks/task_eu.R")
      task_eu()
    }
  ),

  market_prices = list(
    name = "Market prices / Yahoo Finance (fresh session)",
    fn = function() {
      source("src/tasks/task_market_prices.R")
      task_market_prices()
    }
  ),

  ueex_gas = list(
    name = "UEEX gas UA (fresh session)",
    fn = function() {
      source("src/tasks/task_ueex_gas.R")
      task_ueex_gas()
    }
  ),

  auction_ua = list(
    name = "Auction UA / Ukrenergo (fresh session)",
    fn = function() {
      source("src/tasks/task_auction_ua.R")
      task_auction_ua()
    }
  ),

  auction_jao = list(
    name = "Auction JAO / UA corridors (fresh session)",
    fn = function() {
      source("src/tasks/task_auction_jao.R")
      task_auction_jao()
    }
  ),

  transform = list(
    name = "Transform (fresh session)",
    fn = function() {
      source("src/tasks/task_transform.R")
      task_transform()
    }
  )
)

# ── Filter tests if argument provided ────────────────────────────────────────
if (length(args) > 0) {
  tests <- tests[args]
  if (length(tests) == 0) {
    message("Unknown test(s): ", paste(args, collapse = ", "))
    message("Available: ", paste(names(tests), collapse = ", "))
    quit(status = 1)
  }
}

# ── Run each test in a fresh R session ───────────────────────────────────────
message("=== Local Test Suite ===")
message("Running ", length(tests), " test(s) in fresh R sessions...\n")

wd <- getwd()
results <- list()

for (test_name in names(tests)) {
  test <- tests[[test_name]]
  message("── ", test$name, " ──")
  
  result <- tryCatch({
    # Run in a completely fresh R session
    callr::r(
      test$fn,
      wd = wd,
      show = TRUE,      # Show output in real time
      env = c(
        ENTSOE_PAT = Sys.getenv("ENTSOE_PAT"),
        GITHUB_PAT = Sys.getenv("GITHUB_PAT"),
        JAO_TOKEN  = Sys.getenv("JAO_TOKEN")
      )
    )
  }, error = function(e) {
    list(task = test_name, status = "error", message = e$message)
  })
  
  results[[test_name]] <- result
  
  icon <- switch(result$status,
    success = "✓", skipped = "–", warning = "⚠", error = "✗", "?"
  )
  message("  ", icon, " ", result$status, ": ", result$message, "\n")
}

# ── Summary ──────────────────────────────────────────────────────────────────
message("=== Test Summary ===")
for (r in results) {
  icon <- switch(r$status,
    success = "✓", skipped = "–", warning = "⚠", error = "✗", "?"
  )
  message("  ", icon, " ", r$task, ": ", r$message)
}

statuses <- sapply(results, \(r) r$status)
n_err <- sum(statuses == "error")
n_ok  <- sum(statuses %in% c("success", "skipped"))

message("\n", n_ok, "/", length(results), " passed, ", n_err, " failed")

if (n_err > 0) {
  quit(status = 1)
} else {
  message("All tests passed — safe to push!")
}
