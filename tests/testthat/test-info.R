test_that("get_info returns data for valid ticker", {
  skip_if_offline()
  skip_on_cran()

  # Get summaryProfile info for Apple
  ticker <- get_tickers("AAPL")
  info <- get_info(ticker, modules = "summaryProfile")

  # Check that we got data
  expect_s3_class(info, "tbl")

  # Check that some common fields exist
  expected_fields <- c(
    "address1", "city", "state", "zip", "country", "phone", "website",
    "industry", "industryKey", "industryDisp", "sector", "sectorKey",
    "sectorDisp", "longBusinessSummary", "fullTimeEmployees", "companyOfficers",
    "irWebsite", "executiveTeam", "maxAge"
  )
  expect_named(info, expected_fields)
})

test_that("get_info returns data for multiple modules", {
  skip_if_offline()
  skip_on_cran()

  # Get assetProfile and earnings for Apple
  ticker <- get_tickers("AAPL")
  info <- get_info(ticker, modules = c("assetProfile", "earnings"))

  # Check that we got data
  expect_s3_class(info[[1]], "tbl")
  purrr::walk(info[[2]], ~ expect_s3_class(.x, "tbl"))

  # Check that fields exist
  # Asset profile
  expected_names <- c(
    "address1", "city", "state", "zip", "country", "phone", "website",
    "industry", "industryKey", "industryDisp", "sector", "sectorKey",
    "sectorDisp", "longBusinessSummary", "fullTimeEmployees", "companyOfficers",
    "auditRisk", "boardRisk", "compensationRisk", "shareHolderRightsRisk",
    "overallRisk", "governanceEpochDate", "compensationAsOfEpochDate",
    "irWebsite", "executiveTeam", "maxAge"
  )
  expect_named(info[[1]], expected_names)
  # Earnings
  expected_names <- c(
    "quarterly_earnings", "current_quarter_earnings", "yearly_financials",
    "quarterly_financials", "metadata"
  )
  expect_named(info[[2]], expected_names)
})
