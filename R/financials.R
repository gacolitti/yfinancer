# financials.R - Retrieve financial statements from Yahoo Finance
#
# This file contains functions for retrieving and processing financial statement data
# from Yahoo Finance, including income statements, balance sheets, and cash flow statements.
# The functions handle data formatting, time range selection, and provide options for
# both annual and quarterly financial data.
#
# The main functions are:
# - get_income_statement(): Retrieves income statement data
# - get_balance_sheet(): Retrieves balance sheet data
# - get_cashflow(): Retrieves cash flow statement data
# - get_financials(): Retrieves all three financial statements in one call

#' Get income statement for a ticker
#'
#' Retrieves income statement data from Yahoo Finance for a specified ticker symbol.
#' Income statements show a company's revenues, expenses, and profits over a specific period.
#'
#' @param ticker A ticker object created with `get_tickers()` or a ticker symbol string
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start Start timestamp as date, datetime, or string (default EOY 2016)
#' @param end End timestamp as date, datetime, or string (default current timestamp)
#' @param income_keys Vector of specific income statement keys to include (default all)
#'   See `valid_income_keys` for available options.
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format with dates as columns (default TRUE).
#'   If FALSE, returns data in long format with a date column.
#' @param proxy Optional proxy settings for the request
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return Either a tibble with income statement data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
#'
#' @section Available Income Keys:
#'
#' Examples:
#' - TotalRevenue
#' - GrossProfit
#' - OperatingIncome
#' - NetIncome
#'
#' See `valid_income_keys` for a full list of available income keys.
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#'
#' # Get annual income statement
#' income_stmt <- get_income_statement(apple)
#'
#' # Get quarterly income statement
#' quarterly_income <- get_income_statement(apple, freq = "quarterly")
#'
#' # Get specific income statement items
#' revenue_income <- get_income_statement(apple,
#'   income_keys = c("TotalRevenue", "NetIncome")
#' )
#'
#' # Get data for a specific time period
#' income_2020_2022 <- get_income_statement(apple,
#'   start = "2020-01-01",
#'   end = "2022-12-31"
#' )
#' }
#' @export
get_income_statement <- function(ticker, freq = c("annual", "quarterly"),
                                 start = NULL, end = NULL,
                                 income_keys = NULL, pretty = TRUE, wide = TRUE, proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)
  if (!is.null(income_keys) && !all(income_keys %in% valid_income_keys)) {
    rlang::abort(glue::glue("Invalid income statement keys: {income_keys}. See yfinance::income_keys for valid options."))
  }
  check_is_lgl(pretty)
  check_is_lgl(wide)

  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  # Get current timestamp for period2
  end <- as_timestamp(end, default = as.integer(Sys.time()))
  # Start from end of 2016
  start <- as_timestamp(start, default = as.integer(as.POSIXct("2016-12-31")))

  # Income statement keys
  if (is.null(income_keys)) {
    income_keys <- valid_income_keys
  }

  # Type parameter should be a comma-separated list of freq + key
  type_param <- paste0(paste0(freq, income_keys), collapse = ",")

  # Construct request object - using the fundamentals-timeseries endpoint
  req <- httr2::request(yf_query2_url) |>
    httr2::req_url_path("ws/fundamentals-timeseries/v1/finance/timeseries", ticker$symbol) |>
    httr2::req_url_query(
      symbol = ticker$symbol,
      type = type_param,
      period1 = start,
      period2 = end
    ) |>
    httr2::req_proxy(proxy) |>
    req_add_headers()

  if (output == "request") {
    return(req)
  }

  # Perform request
  resp <- httr2::req_perform(req)

  if (output == "response") {
    return(resp)
  }

  # Extract the data
  resp_json <- httr2::resp_body_json(resp)

  # Check if we have valid data
  if (is.null(resp_json$timeseries) || is.null(resp_json$timeseries$result) || length(resp_json$timeseries$result) == 0) {
    warning(sprintf("No income statement data available for %s", ticker$symbol))
    return(dplyr::tibble())
  }

  # Extract the result data
  result_data <- resp_json$timeseries$result

  # Process the timeseries data
  processed_data <- process_timeseries_data(result_data, pretty)

  return(processed_data)
}

#' Get balance sheet for a ticker
#'
#' Retrieves balance sheet data from Yahoo Finance for a specified ticker symbol.
#' Balance sheets show a company's assets, liabilities, and shareholders' equity
#' at a specific point in time.
#'
#' @inheritParams get_income_statement
#' @param balance_keys Vector of specific balance sheet keys to include (default all)
#'   See `valid_balance_keys` for available options.
#' @return Either a tibble with balance sheet data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
#'
#' @section Available Balance Keys:
#'
#' Examples:
#' - TotalAssets
#' - TotalCapitalization
#' - CurrentAssets
#'
#' See `valid_balance_keys` for a full list of available balance keys.
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#'
#' # Get annual balance sheet
#' balance_sheet <- get_balance_sheet(apple)
#'
#' # Get quarterly balance sheet
#' quarterly_balance <- get_balance_sheet(apple, freq = "quarterly")
#'
#' # Get specific balance sheet items
#' assets_liabilities <- get_balance_sheet(apple,
#'   balance_keys = c("TotalAssets", "TotalLiabilities")
#' )
#'
#' # Get data for a specific time period
#' balance_2020_2022 <- get_balance_sheet(apple,
#'   start = "2020-01-01",
#'   end = "2022-12-31"
#' )
#' }
#' @export
get_balance_sheet <- function(ticker, freq = c("annual", "quarterly"),
                              start = NULL, end = NULL, balance_keys = NULL,
                              pretty = TRUE, wide = TRUE, proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)
  if (!is.null(balance_keys) && !all(balance_keys %in% valid_balance_keys)) {
    rlang::abort(glue::glue("Invalid balance sheet keys: {balance_keys}. See yfinance::balance_keys for valid options."))
  }
  check_is_lgl(pretty)
  check_is_lgl(wide)


  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  # Get current timestamp for period2
  end <- as_timestamp(end, default = as.integer(Sys.time()))
  # Start from end of 2016 as in Python implementation
  start <- as_timestamp(start, default = as.integer(as.POSIXct("2016-12-31")))

  # Balance sheet keys
  if (is.null(balance_keys)) {
    balance_keys <- valid_balance_keys
  }

  # Type parameter should be a comma-separated list of freq + key
  type_param <- paste0(paste0(freq, balance_keys), collapse = ",")

  # Construct request object - using the fundamentals-timeseries endpoint
  req <- httr2::request(yf_query2_url) |>
    httr2::req_url_path("ws/fundamentals-timeseries/v1/finance/timeseries", ticker$symbol) |>
    httr2::req_url_query(
      symbol = ticker$symbol,
      type = type_param,
      period1 = start,
      period2 = end
    ) |>
    httr2::req_proxy(proxy) |>
    req_add_headers()

  if (output == "request") {
    return(req)
  }

  # Perform request
  resp <- httr2::req_perform(req)

  if (output == "response") {
    return(resp)
  }

  # Extract the data
  resp_json <- httr2::resp_body_json(resp)

  # Check if we have valid data
  if (is.null(resp_json$timeseries) || is.null(resp_json$timeseries$result) || length(resp_json$timeseries$result) == 0) {
    warning(sprintf("No balance sheet data available for %s", ticker$symbol))
    return(dplyr::tibble())
  }

  # Extract the result data
  result_data <- resp_json$timeseries$result

  # Process the timeseries data
  processed_data <- process_timeseries_data(result_data, pretty)

  return(processed_data)
}

#' Get cash flow statement for a ticker
#'
#' Retrieves cash flow statement data from Yahoo Finance for a specified ticker symbol.
#' Cash flow statements show how changes in balance sheet accounts and income affect
#' cash and cash equivalents, breaking the analysis down to operating, investing, and
#' financing activities.
#'
#' @inheritParams get_income_statement
#' @param cashflow_keys Vector of specific cash flow statement keys to include (default all)
#'   See `valid_cashflow_keys` for available options.
#' @return Either a tibble with cash flow statement data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
#'
#' @section Available Cashflow Keys:
#'
#' Examples:
#' - OperatingCashFlow
#' - FreeCashFlow
#'
#' See `valid_cashflow_keys` for a full list of available cashflow keys.
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#'
#' # Get annual cash flow statement
#' cash_flow <- get_cashflow(apple)
#'
#' # Get quarterly cash flow statement
#' quarterly_cash_flow <- get_cashflow(apple, freq = "quarterly")
#'
#' # Get specific cash flow items
#' operating_cash <- get_cashflow(apple,
#'   cashflow_keys = c("OperatingCashFlow", "FreeCashFlow")
#' )
#'
#' # Get data for a specific time period
#' cash_2020_2022 <- get_cashflow(apple,
#'   start = "2020-01-01",
#'   end = "2022-12-31"
#' )
#'
#' # Error handling example
#' tryCatch(
#'   {
#'     cash_flow <- get_cashflow("INVALID_TICKER")
#'   },
#'   error = function(e) {
#'     message("Error retrieving cash flow data: ", e$message)
#'     # Handle the error appropriately
#'   }
#' )
#' }
#' @export
get_cashflow <- function(ticker, freq = c("annual", "quarterly"),
                         start = NULL, end = NULL, cashflow_keys = NULL,
                         pretty = TRUE, wide = TRUE, proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)
  if (!is.null(cashflow_keys) && !all(cashflow_keys %in% valid_cashflow_keys)) {
    rlang::abort(glue::glue("Invalid cash flow keys: {cashflow_keys}. See yfinance::cashflow_keys for valid options."))
  }
  check_is_lgl(pretty)
  check_is_lgl(wide)

  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  freq <- validate_frequency(freq)

  # Get current timestamp for period2
  end <- as_timestamp(end, default = as.integer(Sys.time()))
  # Start from end of 2016 as in Python implementation
  start <- as_timestamp(start, default = as.integer(as.POSIXct("2016-12-31")))

  # Cash flow keys
  if (is.null(cashflow_keys)) {
    cash_flow_keys <- valid_cashflow_keys
  }

  # Type parameter should be a comma-separated list of freq + key
  type_param <- paste0(paste0(freq, cash_flow_keys), collapse = ",")

  # Construct request object - using the fundamentals-timeseries endpoint
  req <- httr2::request(yf_query2_url) |>
    httr2::req_url_path("ws/fundamentals-timeseries/v1/finance/timeseries", ticker$symbol) |>
    httr2::req_url_query(
      symbol = ticker$symbol,
      type = type_param,
      period1 = start,
      period2 = end
    ) |>
    httr2::req_proxy(proxy) |>
    req_add_headers()

  if (output == "request") {
    return(req)
  }

  # Perform request
  resp <- httr2::req_perform(req)

  if (output == "response") {
    return(resp)
  }

  # Extract the data
  resp_json <- httr2::resp_body_json(resp)

  # Check if we have valid data
  if (is.null(resp_json$timeseries) || is.null(resp_json$timeseries$result) || length(resp_json$timeseries$result) == 0) {
    warning(sprintf("No cash flow data available for %s", ticker$symbol))
    return(dplyr::tibble())
  }

  # Extract the result data
  result_data <- resp_json$timeseries$result

  # Process the timeseries data
  processed_data <- process_timeseries_data(result_data, pretty)

  processed_data
}

#' Process timeseries data from the fundamentals-timeseries endpoint
#'
#' @inheritParams get_income_statement
#' @param result_data List of timeseries results
#' @return A tidy tibble with processed timeseries data
#' @keywords internal
process_timeseries_data <- function(result_data, pretty = TRUE, wide = TRUE) {
  # If no data, return empty tibble
  if (length(result_data) == 0) {
    return(dplyr::tibble())
  }

  # Create a list to collect all observations
  all_observations <- list()

  # Process each result item (each metric)
  for (item in result_data) {
    # Extract the metric name from meta info
    if (!is.null(item$meta) && !is.null(item$meta$type) && length(item$meta$type) > 0) {
      metric_name <- item$meta$type[[1]]
    } else {
      # If metric name not found, skip this item
      next
    }

    # Find the corresponding data list (key will match metric_name)
    data_key <- NULL
    for (key in names(item)) {
      if (key != "meta" && key != "timestamp" && key == metric_name) {
        data_key <- key
        break
      }
    }

    if (is.null(data_key)) {
      next # Skip if data key not found
    }

    # Extract data points for this metric
    data_points <- item[[data_key]]

    # Process each data point
    for (point in data_points) {
      if (!is.null(point$asOfDate) && !is.null(point$reportedValue$raw)) {
        # Create new observation
        observation <- list(
          date = as.Date(point$asOfDate),
          metric = metric_name,
          value = point$reportedValue$raw,
          currency = point$currencyCode
        )

        # Add to the collection
        all_observations[[length(all_observations) + 1]] <- observation
      }
    }
  }

  # If no observations collected, return empty tibble
  if (length(all_observations) == 0) {
    return(dplyr::tibble())
  }

  # Convert to tibble
  result_df <- do.call(rbind, lapply(all_observations, as.data.frame))
  result_tbl <- dplyr::as_tibble(result_df)

  # Clean up metric names if pretty is TRUE
  if (pretty && nrow(result_tbl) > 0) {
    result_tbl$metric <- gsub("^(annual|quarterly)", "", result_tbl$metric)
    result_tbl$metric <- clean_names(result_tbl$metric)
  }

  # Sort by date (newest first) and metric
  result_tbl <- result_tbl[order(result_tbl$date, decreasing = TRUE, result_tbl$metric), ]

  # Return in wide format if wide is TRUE
  if (wide) {
    result_tbl <- tidyr::pivot_wider(result_tbl, names_from = "metric", values_from = "value")
  }

  result_tbl
}

#' Get all financial statements for a ticker
#'
#' Retrieves all three main financial statements (income statement, balance sheet, and
#' cash flow statement) from Yahoo Finance for a specified ticker symbol in a single call.
#' This is a convenience function that calls the individual statement functions and
#' returns the results as a list.
#'
#' Note that this function makes multiple API calls to Yahoo Finance. Be aware of potential
#' rate limiting issues when making frequent requests. If you encounter HTTP 429 (Too Many Requests)
#' errors, consider implementing a delay between requests or using a proxy.
#'
#' @inheritParams get_income_statement
#' @inheritParams get_balance_sheet
#' @inheritParams get_cashflow
#' @return A list containing three elements:
#'   - `income_statement`: Income statement data
#'   - `balance_sheet`: Balance sheet data
#'   - `cashflow`: Cash flow statement data
#'
#'   If output is "request" or "response", returns a list of httr2 request or response objects instead.
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#'
#' # Get all annual financial statements
#' financials <- get_financials(apple)
#'
#' # Access individual statements from the results
#' income <- financials$income_statement
#' balance <- financials$balance_sheet
#' cashflow <- financials$cashflow
#'
#' # Get all quarterly financial statements
#' quarterly_financials <- get_financials(apple, freq = "quarterly")
#'
#' # Get financial statements for a specific time period
#' financials_2020_2022 <- get_financials(apple,
#'   start = "2020-01-01",
#'   end = "2022-12-31"
#' )
#'
#' # Error handling for all financial statements
#' tryCatch(
#'   {
#'     financials <- get_financials("INVALID_TICKER")
#'   },
#'   error = function(e) {
#'     message("Error retrieving financial data: ", e$message)
#'     # Handle the error appropriately
#'   }
#' )
#'
#' # Using a proxy to avoid rate limiting
#' # Assuming you have a proxy service set up
#' proxy_url <- "http://your-proxy-server:port"
#' financials_with_proxy <- get_financials(apple, proxy = proxy_url)
#' }
#' @export
get_financials <- function(ticker, freq = "annual",
                           start = NULL, end = NULL,
                           cashflow_keys = NULL, balance_keys = NULL,
                           income_keys = NULL, pretty = TRUE, wide = TRUE,
                           proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)

  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  # Get all three financial statements
  income_stmt <- get_income_statement(ticker, freq, start, end, income_keys, pretty, wide, proxy, output)
  balance_sheet <- get_balance_sheet(ticker, freq, start, end, balance_keys, pretty, wide, proxy, output)
  cashflow <- get_cashflow(ticker, freq, start, end, cashflow_keys, pretty, wide, proxy, output)

  # Return as a list
  list(
    income_statement = income_stmt,
    balance_sheet = balance_sheet,
    cashflow = cashflow
  )
}
