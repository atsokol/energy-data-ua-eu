library(httr)
library(jsonlite)

source("download_functions.R")

# Generate a sequence of dates for the desired year
dates <- seq(as.Date("2025-01-01"), Sys.Date(), by = "day")

# Loop through the dates and download the data for all zones
# gen = "1" is solar and gen = "2" is wind
# Download data for all zones (1-4) and combine into single dataframe
data_solar <- download_data_all_zones(dates, gen = "1", zonas = c("1", "2", "3", "4"))

data_wind <- download_data_all_zones(dates, gen = "2", zonas = c("1", "2", "3", "4"))


