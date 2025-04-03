library(testthat)

test_that("search_tickers validates input", {
  # Check that empty strings are rejected
  expect_error(search_tickers(""))

  # Check that non-character input is rejected
  expect_error(search_tickers(123))

  # Check that vectors longer than 1 are rejected
  expect_error(search_tickers(c("Apple", "Microsoft")))
})

# These tests require internet connection and might fail if Yahoo Finance API changes
test_that("search_tickers returns results for valid query", {
  skip_if_offline()
  skip_on_cran()

  # Search for Apple
  results <- search_tickers("Apple")

  # Check that we got data
  expect_s3_class(results, "tbl_df")
  expect_gt(nrow(results), 0)

  # Check that the structure is correct
  expected_cols <- c("symbol", "name", "exchange", "quoteType")
  for (col in expected_cols) {
    expect_true(col %in% names(results))
  }

  # Check that we can find Apple in the results
  expect_true(any(grepl("AAPL", results$symbol)))
})

test_that("search_tickers returns news when requested", {
  skip_if_offline()
  skip_on_cran()

  # Search for Apple with news
  results <- search_tickers("Apple", quotes_only = FALSE)

  # Check that we got data
  expect_type(results, "list")
  expect_equal(names(results), c("quotes", "news"))

  # Check quotes
  expect_s3_class(results$quotes, "tbl_df")
  expect_gt(nrow(results$quotes), 0)

  # Check news (if any)
  expect_s3_class(results$news, "tbl_df")
})
