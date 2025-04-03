# These tests require internet connection and might fail if Yahoo Finance API changes
test_that("get_history returns data for valid ticker", {
  skip_if_offline()
  skip_on_cran()

  # Get history for Apple
  ticker <- get_tickers("AAPL")
  history <- get_history(ticker, period = "1d")

  # Check that we got data
  expect_s3_class(history, "tbl_df")
  expect_gt(nrow(history), 0)

  # Check that the structure is correct
  expected_cols <- c("timestamp", "date", "open", "high", "low", "close", "volume", "adjusted_close")
  expect_named(history, expected_cols)

  # Check that dates are in ascending order
  expect_equal(history$date, sort(history$date))
})
