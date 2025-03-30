test_that("req_add_auth uses environment variables when available", {
  # Set up environment variables
  withr::with_envvar(
    new = c(
      "YFINANCE_CRUMB" = "test_crumb",
      "YFINANCE_A1" = "test_a1"
    ),
    {
      req <- httr2::request("https://example.com")
      modified_req <- req_add_auth(req)
      
      # Check query parameters
      expect_equal(httr2::url_parse(modified_req$url)$query$crumb, "test_crumb")
      
      # Check headers
      expect_equal(modified_req$headers$Cookie, "A1=test_a1")
    }
  )
})

test_that("req_add_auth uses auth file when environment variables not available", {
  # Mock read_auth_file function
  mockery::stub(
    req_add_auth,
    "read_auth_file",
    list(crumb = "file_crumb", a1_cookie = "file_a1")
  )
  
  # Clear environment variables
  withr::with_envvar(
    new = c(
      "YFINANCE_CRUMB" = NA,
      "YFINANCE_A1" = NA
    ),
    {
      req <- httr2::request("https://example.com")
      modified_req <- req_add_auth(req)
      
      # Check query parameters
      expect_equal(httr2::url_parse(modified_req$url)$query$crumb, "file_crumb")
      
      # Check headers
      expect_equal(modified_req$headers$Cookie, "A1=file_a1")
    }
  )
})

test_that("req_add_auth handles refresh parameter", {
  # Mock read_auth_file function and verify refresh parameter
  mock_read_auth <- mockery::mock(
    list(crumb = "fresh_crumb", a1_cookie = "fresh_a1")
  )
  mockery::stub(req_add_auth, "read_auth_file", mock_read_auth)
  
  withr::with_envvar(
    new = c(
      "YFINANCE_CRUMB" = NA,
      "YFINANCE_A1" = NA
    ),
    {
      req <- httr2::request("https://example.com")
      modified_req <- req_add_auth(req, refresh = TRUE)
      
      # Verify read_auth_file was called with refresh = TRUE
      mockery::expect_args(mock_read_auth, 1, NULL, TRUE)
      
      # Check query parameters and headers
      expect_equal(httr2::url_parse(modified_req$url)$query$crumb, "fresh_crumb")
      expect_equal(modified_req$headers$Cookie, "A1=fresh_a1")
    }
  )
})
