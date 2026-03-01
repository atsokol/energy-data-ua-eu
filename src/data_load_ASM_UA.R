# Download and update Ukrainian Auxiliary Services Market (ASM) data
# Covers FCR, aFRR and mFRR from monthly xlsx files published on ua.energy
#
# NOTE: Requires VPN connection to Ukraine (ua.energy is geo-restricted)
#
# Output: data/data_raw/ASM_UA.csv
#   Columns: date, hour, reserve_type (FCR/aFRR/mFRR), direction (up/down/symmetric),
#            volume_mw, price_uah

library(dplyr)
library(readr)
library(lubridate)
library(purrr)

source("src/helper_func_BM_UA.R")

output_path <- "data/data_raw/ASM_UA.csv"

# ── Determine which months already exist ─────────────────────────────────────
get_last_asm_date <- function(filepath) {
  if (file.exists(filepath)) {
    existing <- read_csv(filepath, show_col_types = FALSE)
    last_date <- max(as.Date(existing$date), na.rm = TRUE)
    # Return first day of the last month to re-download partial months
    return(floor_date(last_date, "month"))
  }
  as.Date("2023-01-01")  # earliest available month on ua.energy
}

last_date <- get_last_asm_date(output_path)
message("Updating ASM data from: ", format(last_date, "%Y-%m"))

# ── Extract all URLs from saved links file ────────────────────────────────────
all_urls <- extract_asm_urls("data/data_raw/ASM links.txt")
message("Total ASM URLs found: ", length(all_urls))

# ── Filter to months not yet downloaded ──────────────────────────────────────
new_urls <- all_urls[sapply(all_urls, function(url) {
  file_date <- extract_bm_date(basename(url))  # reuse BM date extractor
  !is.na(file_date) && file_date >= last_date
})]

message("New months to download: ", length(new_urls))

if (length(new_urls) == 0) {
  message("ASM data is up to date.")
} else {
  # ── Download and parse all new files ───────────────────────────────────────
  new_data <- map_dfr(new_urls, download_asm_file)

  if (nrow(new_data) == 0) {
    message("No data retrieved — check VPN connection.")
  } else {
    # ── Combine with existing data ─────────────────────────────────────────
    if (file.exists(output_path)) {
      existing <- read_csv(output_path, show_col_types = FALSE)
      asm_ua <- bind_rows(existing, new_data) |>
        distinct(date, hour, reserve_type, direction, .keep_all = TRUE) |>
        arrange(date, hour, reserve_type, direction)
    } else {
      asm_ua <- new_data |>
        arrange(date, hour, reserve_type, direction)
    }

    write_csv(asm_ua, output_path)
    message("ASM data saved to ", output_path,
            " — total rows: ", nrow(asm_ua),
            " (", n_distinct(floor_date(as.Date(asm_ua$date), "month")),
            " months)")
  }
}
