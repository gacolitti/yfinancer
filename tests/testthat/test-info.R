test_that("get_info returns data for valid ticker", {
  skip_if_offline()
  skip_on_cran()

  # Get summaryProfileinfo for Apple
  ticker <- get_tickers("AAPL")
  info <- get_info(ticker, modules = "summaryProfile")

  # Check that we got data
  expect_true(is.list(info))
  expect_gt(length(info), 0)

  # Check that some common fields exist
  expected_fields <- c(
    "address1", "city", "state", "zip", "country", "phone", "website",
    "industry", "industryKey", "industryDisp", "sector", "sectorKey",
    "sectorDisp", "longBusinessSummary", "fullTimeEmployees", "irWebsite",
    "maxAge"
  )
  expect_named(info, expected_fields)
})
