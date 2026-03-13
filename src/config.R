#============================================================================
# Project-wide constants
# Source this file in any task that needs these values:
#   source("src/config.R")
#============================================================================

# ── Default date range ───────────────────────────────────────────────────────
# Used as fallback start date when a CSV does not yet exist
DEFAULT_START_DATE <- as.Date("2022-01-01")

# ── ENTSO-E zone EIC codes ───────────────────────────────────────────────────
ENTSO_ZONES <- c(
  PL = "10YPL-AREA-----S",
  RO = "10YRO-TEL------P",
  HU = "10YHU-MAVIR----U",
  SK = "10YSK-SEPS-----K"
)

UA_EIC <- "10Y1001C--00003F"

# Bidding zone pairs for cross-border flow downloads (UA ↔ neighbours)
ENTSO_ZONE_PAIRS <- tibble::tribble(
  ~from_country, ~to_country, ~from_eic,          ~to_eic,
  "UA",          "PL",        UA_EIC,              "10YPL-AREA-----S",
  "UA",          "HU",        UA_EIC,              "10YHU-MAVIR----U",
  "UA",          "RO",        UA_EIC,              "10YRO-TEL------P",
  "UA",          "SK",        UA_EIC,              "10YSK-SEPS-----K",
  "PL",          "UA",        "10YPL-AREA-----S",  UA_EIC,
  "HU",          "UA",        "10YHU-MAVIR----U",  UA_EIC,
  "RO",          "UA",        "10YRO-TEL------P",  UA_EIC,
  "SK",          "UA",        "10YSK-SEPS-----K",  UA_EIC,
)

# Generation types to download (Solar = B16, Wind Onshore = B19)
ENTSO_GEN_TYPES <- c("B16", "B19")

# ── TTF gas price adjustments (used in task_market_prices.R) ─────────────────
# These convert TTF hub price (€/MWh) into Ukrainian border price (UAH)
TTF_TRANSPORT_EUR_MWH <- 5       # €/MWh hub-to-UA-border transport cost
TTF_MWH_TO_MCM        <- 10.675 # MWh → 1000 m³ (thousand cubic metres) conversion
TTF_ENTRY_TARIFF_USD  <- 4.45   # USD/1000 m³ GTS entry tariff
TTF_TARIFF_MULTIPLIER <- 1.45   # tariff adjustment multiplier
TTF_VAT_RATE          <- 1.20   # 20% Ukrainian VAT
