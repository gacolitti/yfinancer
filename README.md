# yfinancer <a href="github.com/gacolitti/yfinancer"><img src="man/figures/logo.png" align="right" height="138" /></a>

`yfinancer` provides access to Yahoo Finance's API for retrieving market data. The package includes functions for downloading historical price data, accessing company information, retrieving financial statements, and searching for tickers. It handles API interactions, data parsing, and returns results in tidy tibble format.

<!-- badges: start -->
[![experimental](https://img.shields.io/badge/experimental-orange.svg)](https://github.com/gaclitti/yfinancer)
[![Codecov test coverage](https://codecov.io/gh/gacolitti/yfinancer/graph/badge.svg)](https://app.codecov.io/gh/gacolitti/yfinancer)
[![R-CMD-check](https://github.com/gacolitti/yfinancer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gacolitti/yfinancer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Usage Examples

### Working with a Single Ticker

```r
# Get a ticker object for Apple Inc.
apple <- get_tickers("AAPL")

# Get historical market data (default: 1 month of daily data)
history <- get_history(apple)

# Get 1 year of daily data
history_1y <- get_history(apple, period = "1y")

# Get 1 month of hourly data
history_hourly <- get_history(apple, period = "1mo", interval = "1h")

# Get data for a custom date range
history_custom <- get_history(
  apple, 
  start_date = "2022-01-01", 
  end_date = "2022-12-31"
)

# Get company information
info <- get_info(apple)

# Get financial statements
# Annual statements (default)
income_stmt <- get_income_statement(apple)
balance_sheet <- get_balance_sheet(apple)
cash_flow <- get_cash_flow(apple)

# Get all financial statements at once
financials <- get_financial_statements(apple)

# Quarterly statements
quarterly_income <- get_income_statement(apple, freq = "quarterly")
```

### Working with Multiple Tickers

```r
# Get multiple tickers
tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))

# Get historical data for all tickers
tech_history <- get_tickers_history(tech_tickers, period = "1y")

# Get company information for all tickers
tech_info <- get_tickers_info(tech_tickers)
```

### Search for Tickers

```r
# Search for companies with "tech" in their name
tech_search <- search_tickers("tech", limit = 20)

# Search with news results
results <- search_tickers("Apple", quotes_only = FALSE)
quotes <- results$quotes
news <- results$news
```

## Core Features and Components

Based on the Python yfinance package, the R port includes the following main components:

1. **Ticker Functions**: Access single ticker data
   - ✅ Historical market data (OHLCV)
   - ✅ Basic company information
   - ✅ Financial statements (income statement, balance sheet, cash flow)
   - ✅ Dividends and stock splits (included in get_history())
   - ⬜ Options data
   - ⬜ Analyst recommendations and price targets
   - ⬜ News

2. **Market Information**:
   - ✅ Comprehensive company data via get_info() function with available modules including:
     - Company profile and asset information
     - Key statistics (PE, EPS, EBITDA, enterprise value, etc.)
     - Financial KPIs (revenue, margins, cash flow metrics)
     - Future earnings dates and earnings history
     - ESG scores (environmental, social, governance metrics)
     - Ownership data (insider, institutional, fund)
     - Insider transactions
     - Analyst recommendations and upgrade/downgrade history
     - SEC filings
     - And more (over 30 different data modules available)

3. **Search Functionality**:
   - ✅ Search for quotes
   - ✅ Get news from search

4. **Screener Tools**:
   - ⬜ Build equity and fund queries
   - ⬜ Screen market based on criteria

## Legal Disclaimer

Yahoo!, Y!Finance, and Yahoo! Finance are registered trademarks of Yahoo, Inc.

yfinancer is not affiliated, endorsed, or vetted by Yahoo, Inc. It's an open-source tool that uses Yahoo's publicly available APIs, and is intended for research and educational purposes.

Users should refer to Yahoo!'s terms of use for details on rights to use the actual data downloaded.
