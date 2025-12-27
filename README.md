# Ukraine and EU Energy Data Download

Automated data collection system for renewable energy generation, electricity prices, and load data from Ukraine and EU countries.

## Data Sources

### Ukraine
- **Solar and Wind Generation**: [GPEE (Guaranteed Buyer)](https://www.gpee.com.ua/) - hourly generation data
- **Day-Ahead Market Prices**: [OREE (Market Operator)](https://www.oree.com.ua/) - hourly electricity prices
- **Exchange Rates**: [National Bank of Ukraine](https://bank.gov.ua/) - daily UAH/EUR rates

### European Union
- **Generation, Prices, and Load**: [ENTSO-E Transparency Platform](https://transparency.entsoe.eu/) - hourly data for Poland (PL), Romania (RO), Hungary (HU), and Slovakia (SK)
  - Solar (B16) and Wind Onshore (B19) generation
  - Day-ahead market prices
  - Total electricity load

## Data Processing

The repository contains R scripts that:

1. **Incremental Updates**: Check existing data files and download only new data since the last update
2. **API Integration**: 
   - Fetch data from Ukrainian energy market APIs (GPEE, OREE, NBU)
   - Query ENTSO-E API for EU country data using the `entsoeapi` package
3. **Data Transformation**:
   - Parse and standardize datetime formats (UTC timezone)
   - Aggregate data to hourly resolution
   - Complete missing time periods with zero values for generation data
   - Convert prices to EUR using NBU exchange rates for Ukrainian data
4. **Automated Workflow**: GitHub Actions runs monthly on the 1st at 20:00 UTC to update all datasets
5. **Output**: Clean CSV files with standardized columns (country/zone, hour, technology type, values)
