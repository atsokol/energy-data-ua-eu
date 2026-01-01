library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Combine EU generation and price data 
if (!exists("gen_eu")) gen_eu <- read_csv("data/data_raw/yield_RES_EU.csv")
if (!exists("price_eu")) price_eu <- read_csv("data/data_raw/DAM_EU.csv")


data_eu <- gen_eu |>
  left_join(price_eu, by = c("country", "hour"), relationship = "many-to-one") 

# Combine UA generation and price data
if (!exists("price_ua")) price_ua <- read_csv("data/data_raw/DAM_UA.csv")
if (!exists("solar_ua")) solar_ua <- read_csv("data/data_raw/yield_solar_UA.csv") 
if (!exists("wind_ua")) wind_ua <- read_csv("data/data_raw/yield_wind_UA.csv") 

gen_ua <- rbind(
  solar_ua |> mutate(type = "Solar"), 
  wind_ua |> mutate(type = "Wind onshore")
) |> 
    transmute(
        hour = ymd_h(paste(format(date, "%Y-%m-%d"), hour - 1), tz = "UTC"),
        type = type,
        gen_mw = if_else(actual < 0, 0, actual)
        )

data_ua <- left_join(
    gen_ua,
    price_ua,
    by = c("hour" = "hour"),
    relationship = "many-to-one"
) |> 
    rename(
        tech = type
    ) |> 
    filter(
        !is.na(gen_mw)
    ) |> 
    mutate(date = as_date(hour)) |> 
    select(
        country,
        hour,
        tech,
        gen_mw,
        price_eur = price_eur_mwh,
        volume
    )

data_all <- rbind(data_eu, select(data_ua, -volume))

# ================================= 
# Calculate capacity factors for each generation type
#=================================

factor_d <- data_all |>
  filter(country != NA)
  mutate(date = floor_date(hour, unit = "days")) |>
  group_by(country, tech, date) |>
  summarise(
    price_res = sum(price_eur * gen_mw, na.rm = TRUE) / sum(gen_mw, na.rm = TRUE),
    price_base = mean(price_eur, na.rm = TRUE),
    cap_factor = price_res / price_base,
    .groups = "drop") 

# Monthly median of daily values
factor_m <- factor_d |> 
  group_by(country, tech, date = floor_date(date, unit = "months")) |> 
  summarise(
    cap_factor = median(cap_factor, na.rm = TRUE),
    .groups = "drop"
  ) 

# Write results to files
write_csv(factor_d, "data/data_output/capture_factors_daily.csv")
write_csv(factor_m, "data/data_output/capture_factors_monthly.csv")

# ================================= 
# Calculate weighted average DAM prices
#=================================

price_wgt <- price_ua |>
  mutate(date = floor_date(hour, unit = "days") |> as_date()) |>
  group_by(country, date) |>
  summarise(
    price_uah = sum(price_uah * volume, na.rm = TRUE) / sum(volume, na.rm = TRUE),
    price_eur = sum(price_eur_mwh * volume, na.rm = TRUE) / sum(volume, na.rm = TRUE),
    .groups = "drop")

# Write results to file
write_csv(price_wgt, "data/data_output/DAM weighted price UA.csv")
