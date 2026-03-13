# Ukraine and EU Energy Data Download

Automated data collection system for renewable energy generation, electricity prices, and related market data from Ukraine and EU countries.

## Data Sources

### Ukraine
- **Solar and Wind Generation**: [GPEE (Guaranteed Buyer)](https://www.gpee.com.ua/) — hourly generation yields
- **Day-Ahead Market Prices**: [OREE (Market Operator)](https://www.oree.com.ua/) — hourly electricity prices with UAH→EUR conversion
- **Exchange Rates**: [National Bank of Ukraine](https://bank.gov.ua/) — daily UAH/EUR and UAH/USD rates
- **Balancing Market**: [ua.energy](https://ua.energy/) — hourly BM prices and volumes (**requires Ukrainian VPN — geo-blocked**)
- **Gas Exchange**: [UEEX](https://www.ueex.com.ua/) — daily natural gas standardised product quotations

### European Union
- **Generation, Prices, Load, Flows**: [ENTSO-E Transparency Platform](https://transparency.entsoe.eu/) — hourly data for Poland (PL), Romania (RO), Hungary (HU), Slovakia (SK)
  - Solar (B16) and Wind Onshore (B19) generation
  - Day-ahead market prices (hourly + 15-minute resolution)
  - Total electricity load
  - Balancing market prices and volumes
  - Scheduled and physical cross-border flows (UA ↔ neighbours)

### Global Market Data
- **TTF Natural Gas Futures**: Yahoo Finance (`TTF=F`) — daily closing price with UAH price calculation
- **EUA Carbon Price**: Yahoo Finance (`CO2.L` + `GBPEUR=X`) — daily EUA price in EUR

## Setup

**R version:** 4.4+

**Required packages:** listed in `renv.lock`. Install with:
```r
renv::restore()
```

**Environment variables** — create `.Renviron` in the project root:
```
ENTSOE_PAT=your_entso_e_api_token
```
Get an ENTSO-E API token at https://transparency.entsoe.eu/ (requires registration).

**GitHub Actions secret:** set `ENTSOE_PAT` in repository Settings → Secrets.

## Running

### Automatic (CI)
GitHub Actions runs on the 1st of each month at 20:00 UTC and auto-commits updated data.
Covers all tasks except BM_UA (geo-blocked).

### Local — all public tasks
```bash
Rscript src/run_all.R
```

### Local — with VPN (includes Balancing Market UA)
```bash
# Connect to Ukrainian VPN first, then:
Rscript src/run_all.R --vpn
```

### Individual task
```bash
Rscript src/tasks/task_solar_ua.R
Rscript src/tasks/task_wind_ua.R
Rscript src/tasks/task_dam_ua.R
Rscript src/tasks/task_eu.R
Rscript src/tasks/task_market_prices.R
Rscript src/tasks/task_ueex_gas.R
Rscript src/tasks/task_bm_ua.R   # VPN required
Rscript src/tasks/task_transform.R
```

### Tests
```bash
Rscript src/test_local.R           # run all tasks in fresh sessions
Rscript src/test_local.R dam_ua    # run a single task test
```

## Project Structure

```
src/
  config.R              # shared constants (EIC codes, TTF parameters, etc.)
  run_all.R             # orchestrator (runs all tasks, handles VPN flag)
  test_local.R          # local test suite using callr::r()
  tasks/                # one script per data source
  helpers/              # download + CSV utility functions
data/
  data_raw/             # raw CSV files (gitignored, updated by CI)
  data_output/          # processed outputs (capture factors, weighted prices)
.github/workflows/
  data_update.yaml      # monthly GitHub Actions pipeline
```

## Data Processing

1. **Incremental updates** — each task reads the latest date from the existing CSV and downloads only new data
2. **Atomic writes** — data is written to a temp file and renamed, preventing corruption on failure
3. **Transform** — `task_transform.R` computes daily and monthly capture factors and volume-weighted DAM prices
