library(tidyverse)

caps <- read_csv("data/data_raw/UA price caps.csv")

caps_clean <- caps |>
  group_by(date) |>
  filter(!hour > 24) |> 
  mutate(
    hour = if (first(hour) == 1 && last(hour) == 24) {
      as.integer(hour - 1)
    } else {
      as.integer(hour)
    }
  ) |>
  ungroup() %>%
  bind_rows(
    filter(., hour == 23) |> mutate(hour = 24L)
  ) |>
  arrange(date, energy_system, hour)

# Get the last date and extend to 31.12.2025
last_date <- max(caps_clean$date)
new_dates <- seq(last_date + days(1), as.Date("2025-12-31"), by = "day")

# Get the reference values from the last date
reference_values <- caps_clean |>
  filter(date == last_date) |>
  select(-date)

# Create extended data
extended_caps <- map_dfr(new_dates, function(d) {
  reference_values |> mutate(date = d)
})

# Combine original and extended data
caps_extended <- bind_rows(caps_clean, extended_caps) |>
  arrange(date, hour)

write_csv(caps_extended, "data/data_raw/UA price caps.csv")
