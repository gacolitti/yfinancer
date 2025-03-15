# File: test-tickers.R
# Description: Tests for the tickers functionality in the yfinancer package

library(testthat)

test_that("get_tickers creates a valid yf_tickers object", {
  # Create a tickers object
  tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))

  # Check structure
  expect_s3_class(tickers, "yf_tickers")
  expect_equal(tickers$symbols, c("AAPL", "MSFT", "GOOG"))
  expect_null(tickers$proxy)

  # Check that individual ticker objects were created
  expect_equal(length(tickers$tickers), 3)
  expect_s3_class(tickers$tickers[[1]], "yf_ticker")
  expect_s3_class(tickers$tickers[[2]], "yf_ticker")
  expect_s3_class(tickers$tickers[[3]], "yf_ticker")
})

test_that("get_tickers validates input", {
  # Check that empty vectors are rejected
  expect_error(get_tickers(character(0)))

  # Check that non-character input is rejected
  expect_error(get_tickers(123))
})

test_that("tickers handles different inputs", {
  # Check that lowercase is converted to uppercase
  tickers_lower <- get_tickers(c("aapl", "msft", "goog"))
  expect_equal(tickers_lower$symbols, c("AAPL", "MSFT", "GOOG"))

  # Check that whitespace is trimmed
  tickers_space <- get_tickers(c(" AAPL ", " MSFT ", " GOOG "))
  expect_equal(tickers_space$symbols, c("AAPL", "MSFT", "GOOG"))
})

# These tests require internet connection and might fail if Yahoo Finance API changes
test_that("get_tickers_history returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get history for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  history <- get_tickers_history(tickers, period = "5d")

  # Check that we got data
  expect_true(is.list(history))
  expect_equal(length(history), 2)
  expect_equal(names(history), c("AAPL", "MSFT"))

  # Check individual ticker data
  for (ticker_data in history) {
    expect_true(tibble::is_tibble(ticker_data))
    expect_gt(nrow(ticker_data), 0)

    # Check that the structure is correct
    expected_cols <- c("timestamp", "date", "open", "high", "low", "close", "volume")
    for (col in expected_cols) {
      expect_true(col %in% names(ticker_data))
    }
  }
})

test_that("get_tickers_info returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get info for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  info <- get_tickers_info(tickers)

  # Check that we got data
  expect_true(is.list(info))
  expect_equal(length(info), 2)
  expect_equal(names(info), c("AAPL", "MSFT"))

  # Check individual ticker info
  for (ticker_info in info) {
    expect_true(is.list(ticker_info))
    expect_gt(length(ticker_info), 0)
  }
})

# Tests for the financial statement methods for multiple tickers
test_that("get_tickers_income_statement returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get income statements for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  income_statements <- get_tickers_income_statement(tickers, freq = "annual")

  # Check that we got data
  expect_true(is.list(income_statements))
  expect_equal(length(income_statements), 2)
  expect_equal(names(income_statements), c("AAPL", "MSFT"))

  # Check individual ticker data
  for (income_stmt in income_statements) {
    expect_true(tibble::is_tibble(income_stmt) || length(income_stmt) == 0)
    if (tibble::is_tibble(income_stmt) && nrow(income_stmt) > 0) {
      expect_true("date" %in% names(income_stmt))
    }
  }
})

test_that("get_tickers_balance_sheet returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get balance sheets for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  balance_sheets <- get_tickers_balance_sheet(tickers, freq = "annual")

  # Check that we got data
  expect_true(is.list(balance_sheets))
  expect_equal(length(balance_sheets), 2)
  expect_equal(names(balance_sheets), c("AAPL", "MSFT"))

  # Check individual ticker data
  for (balance_sheet in balance_sheets) {
    expect_true(tibble::is_tibble(balance_sheet) || length(balance_sheet) == 0)
    if (tibble::is_tibble(balance_sheet) && nrow(balance_sheet) > 0) {
      expect_true("date" %in% names(balance_sheet))
    }
  }
})

test_that("get_tickers_cashflow returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get cash flow statements for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  cashflows <- get_tickers_cashflow(tickers, freq = "annual")

  # Check that we got data
  expect_true(is.list(cashflows))
  expect_equal(length(cashflows), 2)
  expect_equal(names(cashflows), c("AAPL", "MSFT"))

  # Check individual ticker data
  for (cashflow in cashflows) {
    expect_true(tibble::is_tibble(cashflow) || length(cashflow) == 0)
    if (tibble::is_tibble(cashflow) && nrow(cashflow) > 0) {
      expect_true("date" %in% names(cashflow))
    }
  }
})

test_that("get_tickers_financials returns data for valid tickers", {
  skip_if_offline()
  skip_on_cran()

  # Get all financial statements for tech companies
  tickers <- get_tickers(c("AAPL", "MSFT"))
  financials <- get_tickers_financials(tickers, freq = "annual")

  # Check that we got data
  expect_true(is.list(financials))
  expect_equal(length(financials), 2)
  expect_equal(names(financials), c("AAPL", "MSFT"))

  # Check individual ticker data
  for (financial in financials) {
    expect_true(is.list(financial))
    expect_equal(names(financial), c("income_statement", "balance_sheet", "cashflow"))

    # Check that each statement is either a tibble or empty
    for (statement in financial) {
      expect_true(tibble::is_tibble(statement) || length(statement) == 0)
    }
  }
})

test_that("tickers financial methods validate input", {
  # Create a non-tickers object
  not_tickers <- list(symbols = c("AAPL", "MSFT"))

  # Check that each method rejects invalid input
  expect_error(get_tickers_income_statement(not_tickers))
  expect_error(get_tickers_balance_sheet(not_tickers))
  expect_error(get_tickers_cashflow(not_tickers))
  expect_error(get_tickers_financials(not_tickers))
})
