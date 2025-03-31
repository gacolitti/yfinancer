# Test file for financial statement retrieval functions (income statement, balance sheet, cash flow)
# Validates the functionality of get_income_statement(), get_balance_sheet(), get_cashflow(),
# and get_financials() including parameter validation and error handling

library(testthat)

test_that("get_income_statement returns data for a valid ticker", {
  skip_on_cran()

  # Create ticker object
  ticker_obj <- get_tickers("AAPL")

  # Test default parameters
  income_stmt <- get_income_statement(ticker_obj)
  expect_true(tibble::is_tibble(income_stmt))
  expect_gt(nrow(income_stmt), 0)
  expect_true("date" %in% names(income_stmt))

  # Test quarterly data
  quarterly_income <- get_income_statement(ticker_obj, freq = "quarterly")
  expect_true(tibble::is_tibble(quarterly_income))
  expect_gt(nrow(quarterly_income), 0)

  # Test timestamp parameters
  start_ts <- as.integer(as.POSIXct("2020-01-01"))
  end_ts <- as.integer(as.POSIXct("2022-12-31"))
  dated_income <- get_income_statement(ticker_obj, start = start_ts, end = end_ts)
  expect_true(tibble::is_tibble(dated_income))
  expect_true(all(as.Date(dated_income$date) >= as.Date("2020-01-01")))
  expect_true(all(as.Date(dated_income$date) <= as.Date("2022-12-31")))

  # Test pretty parameter
  raw_income <- get_income_statement(ticker_obj, pretty = FALSE)
  expect_true(tibble::is_tibble(raw_income))
  expect_gt(nrow(raw_income), 0)

  # Test output parameter
  req <- get_income_statement(ticker_obj, output = "request")
  expect_true(inherits(req, "httr2_request"))

  resp <- get_income_statement(ticker_obj, output = "response")
  expect_true(inherits(resp, "httr2_response"))
})

test_that("get_balance_sheet returns data for a valid ticker", {
  skip_on_cran()

  # Create ticker object
  ticker_obj <- get_tickers("AAPL")

  # Test default parameters
  balance_sheet <- get_balance_sheet(ticker_obj)
  expect_true(tibble::is_tibble(balance_sheet))
  expect_gt(nrow(balance_sheet), 0)
  expect_true("date" %in% names(balance_sheet))

  # Test quarterly data
  quarterly_balance <- get_balance_sheet(ticker_obj, freq = "quarterly")
  expect_true(tibble::is_tibble(quarterly_balance))
  expect_gt(nrow(quarterly_balance), 0)

  # Test timestamp parameters
  start_ts <- as.integer(as.POSIXct("2020-01-01"))
  end_ts <- as.integer(as.POSIXct("2022-12-31"))
  dated_balance <- get_balance_sheet(ticker_obj, start = start_ts, end = end_ts)
  expect_true(tibble::is_tibble(dated_balance))
  expect_true(all(as.Date(dated_balance$date) >= as.Date("2020-01-01")))
  expect_true(all(as.Date(dated_balance$date) <= as.Date("2022-12-31")))

  # Test pretty parameter
  raw_balance <- get_balance_sheet(ticker_obj, pretty = FALSE)
  expect_true(tibble::is_tibble(raw_balance))
  expect_gt(nrow(raw_balance), 0)

  # Test output parameter
  req <- get_balance_sheet(ticker_obj, output = "request")
  expect_true(inherits(req, "httr2_request"))

  resp <- get_balance_sheet(ticker_obj, output = "response")
  expect_true(inherits(resp, "httr2_response"))
})

test_that("get_cashflow returns data for a valid ticker", {
  skip_on_cran()

  # Create ticker object
  ticker_obj <- get_tickers("AAPL")

  # Test default parameters
  cashflow <- get_cashflow(ticker_obj)
  expect_true(tibble::is_tibble(cashflow))
  expect_gt(nrow(cashflow), 0)
  expect_true("date" %in% names(cashflow))

  # Test quarterly data
  quarterly_cashflow <- get_cashflow(ticker_obj, freq = "quarterly")
  expect_true(tibble::is_tibble(quarterly_cashflow))
  expect_gt(nrow(quarterly_cashflow), 0)

  # Test timestamp parameters
  start_ts <- as.integer(as.POSIXct("2020-01-01"))
  end_ts <- as.integer(as.POSIXct("2022-12-31"))
  dated_cashflow <- get_cashflow(ticker_obj, start = start_ts, end = end_ts)
  expect_true(tibble::is_tibble(dated_cashflow))
  expect_true(all(as.Date(dated_cashflow$date) >= as.Date("2020-01-01")))
  expect_true(all(as.Date(dated_cashflow$date) <= as.Date("2022-12-31")))

  # Test pretty parameter
  raw_cashflow <- get_cashflow(ticker_obj, pretty = FALSE)
  expect_true(tibble::is_tibble(raw_cashflow))
  expect_gt(nrow(raw_cashflow), 0)

  # Test output parameter
  req <- get_cashflow(ticker_obj, output = "request")
  expect_true(inherits(req, "httr2_request"))

  resp <- get_cashflow(ticker_obj, output = "response")
  expect_true(inherits(resp, "httr2_response"))
})

test_that("get_financials returns all statements", {
  skip_on_cran()

  # Create ticker object
  ticker_obj <- get_tickers("AAPL")

  # Test default parameters
  financials <- get_financials(ticker_obj)
  expect_true(is.list(financials))
  expect_named(financials, c("income_statement", "balance_sheet", "cashflow"))
  expect_true(all(sapply(financials, tibble::is_tibble)))
  expect_true(all(sapply(financials, function(x) nrow(x) > 0)))

  # Test quarterly data
  quarterly_financials <- get_financials(ticker_obj, freq = "quarterly")
  expect_true(is.list(quarterly_financials))
  expect_named(quarterly_financials, c("income_statement", "balance_sheet", "cashflow"))
  expect_true(all(sapply(quarterly_financials, tibble::is_tibble)))

  # Test timestamp parameters
  start_ts <- as.integer(as.POSIXct("2020-01-01"))
  end_ts <- as.integer(as.POSIXct("2022-12-31"))
  dated_financials <- get_financials(ticker_obj, start = start_ts, end = end_ts)
  expect_true(is.list(dated_financials))
  expect_true(all(sapply(dated_financials, function(x) {
    all(as.Date(x$date) >= as.Date("2020-01-01")) &&
      all(as.Date(x$date) <= as.Date("2022-12-31"))
  })))

  # Test pretty parameter
  raw_financials <- get_financials(ticker_obj, pretty = FALSE)
  expect_true(is.list(raw_financials))
  expect_true(all(sapply(raw_financials, tibble::is_tibble)))

  # Test output parameter
  req_financials <- get_financials(ticker_obj, output = "request")
  expect_true(is.list(req_financials))
  expect_true(all(sapply(req_financials, function(x) inherits(x, "httr2_request"))))

  resp_financials <- get_financials(ticker_obj, output = "response")
  expect_true(is.list(resp_financials))
  expect_true(all(sapply(resp_financials, function(x) inherits(x, "httr2_response"))))
})

test_that("Financial statements methods handle invalid tickers", {
  skip_on_cran()

  # Create invalid ticker
  invalid_ticker <- "INVALID_TICKER_SYMBOL"
  expect_error(get_income_statement(invalid_ticker), "No valid ticker symbols provided")
  expect_error(get_balance_sheet(invalid_ticker), "No valid ticker symbols provided")
  expect_error(get_cashflow(invalid_ticker), "No valid ticker symbols provided")
  expect_error(get_financials(invalid_ticker), "No valid ticker symbols provided")
})

test_that("Financial statements validate input parameters", {
  # Create ticker object
  ticker_obj <- get_tickers("AAPL")

  # Test invalid frequency
  expected_error <- '`freq` must be one of "annual" or "quarterly", not "invalid"'
  expect_error(get_income_statement(ticker_obj, freq = "invalid"), expected_error)
  expect_error(get_balance_sheet(ticker_obj, freq = "invalid"), expected_error)
  expect_error(get_cashflow(ticker_obj, freq = "invalid"), expected_error)
  expect_error(get_financials(ticker_obj, freq = "invalid"), expected_error)

  # Test invalid output parameter
  expected_error <- '`output` must be one of "tibble", "response", or "request", not "invalid".'
  expect_error(get_income_statement(ticker_obj, output = "invalid"), expected_error)
  expect_error(get_balance_sheet(ticker_obj, output = "invalid"), expected_error)
  expect_error(get_cashflow(ticker_obj, output = "invalid"), expected_error)
  expect_error(get_financials(ticker_obj, output = "invalid"), expected_error)

  # Test invalid timestamp parameters
  expected_error <- "Invalid timestamp format. Please provide a unix timestamp, a Date object, or a YYYY-MM-DD HH:MM:SS string."
  expect_error(get_income_statement(ticker_obj, start = "invalid"), expected_error)
  expect_error(get_balance_sheet(ticker_obj, end = "invalid"), expected_error)

  # Test invalid pretty parameter
  expect_error(get_income_statement(ticker_obj, pretty = "yes"))
  expect_error(get_balance_sheet(ticker_obj, pretty = 1))

  # Test invalid wide parameter
  expect_error(get_income_statement(ticker_obj, wide = "yes"))
  expect_error(get_balance_sheet(ticker_obj, wide = 1))
})
