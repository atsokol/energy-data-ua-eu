library(httr)
library(jsonlite)

# Source your download functions
source("download_functions.R")

# Define paths to solar and wind CSV files
csv_file_solar <- "data/data_output/data solar.csv"
csv_file_wind <- "data/data_output/data wind.csv"

# Helper function to get last date from a CSV file
get_last_date <- function(file) {
    if (file.exists(file) && file.info(file)$size > 0) {
        df <- read.csv(file)
        if ("date" %in% names(df)) {
            df$date <- as.Date(df$date)
            return(max(df$date, na.rm = TRUE))
        }
    }
}

# Get last dates from solar and wind files
last_date_solar <- get_last_date(csv_file_solar)
last_date_wind <- get_last_date(csv_file_wind)

# Set the new date range (from the day after last_date to yesterday)
end_date <- Sys.Date() - 1

# Download and append new solar data
start_date_solar <- last_date_solar + 1
if (start_date_solar <= end_date) {
    dates_new_solar <- seq(start_date_solar, end_date, by = "day")
    data_solar_new <- download_data_all_zones(dates_new_solar, gen = "1", zonas = c("1", "2", "3", "4"))
    if (!is.null(data_solar_new) && nrow(data_solar_new) > 0) {
        write.table(data_solar_new, csv_file_solar, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        message("Appended new solar data for dates: ", start_date_solar, " to ", end_date)
    } else {
        message("No new solar data to append")
    }
} else {
    message("Solar data is already up to date.")
}

# Download and append new wind data
start_date_wind <- last_date_wind + 1
if (start_date_wind <= end_date) {
    dates_new_wind <- seq(start_date_wind, end_date, by = "day")
    data_wind_new <- download_data_all_zones(dates_new_wind, gen = "2", zonas = c("1", "2", "3", "4"))
    if (!is.null(data_wind_new) && nrow(data_wind_new) > 0) {
        write.table(data_wind_new, csv_file_wind, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        message("Appended new wind data for dates: ", start_date_wind, " to ", end_date)
    } else {
        message("No new wind data to append.")
    }
} else {
    message("Wind data is already up to date.")
}
