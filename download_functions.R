# Define the target URL
url <- "https://www.gpee.com.ua/main/loadCharts" 

# Function to download data for a specific date and zona
download_day_data <- function(date, gen = "1", zona = "1") {
  # Create the payload. Adjust keys and values as required.
  payload <- list(
    date = as.character(date),
    zona = as.character(zona),
    gen  = as.character(gen)
  )
  
  # Send the POST request
  response <- POST(url,
                   body = payload,
                   encode = "form")
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    warning("Failed to retrieve data for ", date, " zona ", zona)
    return(NULL)
  }
  
  # Parse the response content assuming it's JSON.
  data_raw <- content(response, as = "text", encoding = "UTF-8")
  vec <- fromJSON(data_raw, flatten = TRUE)
  output <- data.frame(
    date = date,
    zona = zona,
    hour = seq(1,24),
    actual = vec[1:24],
    projected = vec[26:49]
  )
  
  return(output)
}

# Function to download data for a set of dates into a list
download_data_list <- function(dates, gen, zona = "1") { 
  lapply(dates, function(day) {
    tryCatch({
      download_day_data(day, gen, zona)
    }, error = function(e) {
      message("Error downloading data for ", day, " zona ", zona, ": ", e)
      return(NULL)
    })
  })
}

# Function to download data for all zones and combine into a single dataframe
download_data_all_zones <- function(dates, gen, zonas = c("1", "2", "3", "4")) {
  all_data <- lapply(zonas, function(z) {
    message("Downloading data for zona ", z)
    zone_list <- download_data_list(dates, gen, zona = z)
    zone_data <- do.call(rbind, zone_list)
    return(zone_data)
  })
  
  # Combine all zones into a single dataframe
  combined_data <- do.call(rbind, all_data)
  return(combined_data)
}