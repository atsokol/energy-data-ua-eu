
#============================================================================
# Functions to download data for EU countries from ENTSO-E
#============================================================================


# Function to download UAH/EUR exchange rates
get_nbu_fx <- function(start_date, end_date, valcode = "EUR") {
  
  url <- glue::glue(
    "https://bank.gov.ua/NBU_Exchange/exchange_site?start={format(start_date, '%Y%m%d')}&end={format(end_date, '%Y%m%d')}&valcode={valcode}&sort=exchangedate&order=desc&json"
  )
  
  resp <- GET(url)
  
  if (http_error(resp)) {
    stop("Request failed: ", status_code(resp))
  }
  
  content(resp, "text", encoding = "UTF-8") |>
    fromJSON() |>
    as_tibble() |>
    transmute(
      date = dmy(exchangedate),
      rate = rate
    ) |>
    arrange(date)
}

# Function to fetch OREE day-ahead market data
fetch_oree_day <- possibly(function(date, market = "DAM", zone = 2) {
  url <- glue::glue(
    "https://www.oree.com.ua/index.php/PXS/get_pxs_hdata/{format(date, '%d.%m.%Y')}/{market}/{zone}"
  )
  
  resp <- GET(url)
  
  if (http_error(resp)) {
    message("No data for ", date, " (HTTP error).")
    return(tibble())
  }
  
  txt  <- content(resp, as = "text", encoding = "UTF-8")
  json <- fromJSON(txt)
  
  tibble(
    date   = date,
    hour   = json$labels,
    price  = json$pricesData,
    volume = json$amountsData
  )
}, otherwise = tibble())

# Function to download Ukrainian DAM data
download_dam_ua <- function(start_date, end_date) {
  dates <- seq(start_date, end_date, by = "day")
  
  map_dfr(
    dates,
    ~fetch_oree_day(.x, market = "DAM", zone = 2)
  ) |> 
    transmute(
      country = "UA",
      hour = ymd_h(paste(format(date, "%Y-%m-%d"), hour - 1), tz = "UTC"),
      price_uah = price,
      volume = volume
    ) 
}

# Functions to download solar and wind yield data
download_yield_day <- function(
  url = "https://www.gpee.com.ua/main/loadCharts", # target URL
  date, 
  gen = "1"
) {
  # Define parameters of the request
  params <- list(
    date = as.character(date),
    zona = "1",
    gen  = as.character(gen)
  )
  
  # Send the POST request
  response <- POST(url,
                   body = params,
                   encode = "form")
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    warning("Failed to retrieve data for ", date)
    return(NULL)
  }
  
  # Parse the response content assuming it's JSON.
  data_raw <- content(response, as = "text", encoding = "UTF-8")
  vec <- fromJSON(data_raw, flatten = TRUE)
  output <- data.frame(
    date = date,
    hour = seq(1,24),
    actual = vec[1:24],
    projected = vec[26:49]
  )
  
  return(output)
}

download_yield_list <- function(dates, gen) { 
  lapply(dates, function(day) {
    tryCatch({
      download_yield_day(day = day, gen = gen)
    }, error = function(e) {
      message("Error downloading data for ", day, ": ", e)
      return(NULL)
    })
  })
}
