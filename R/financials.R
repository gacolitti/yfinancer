#' Get income statement for a ticker
#'
#' @param ticker A ticker object created with get_tickers) or a ticker symbol
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start Start timestamp (default EOY 2016)
#' @param end End timestamp (default current timestamp)
#' @param income_keys Vector of income statement keys (default all)
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return Either a tibble with income statement data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
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
    return(tibble::tibble())
  }

  # Extract the result data
  result_data <- resp_json$timeseries$result

  # Process the timeseries data
  processed_data <- process_timeseries_data(result_data, pretty)

  return(processed_data)
}

#' Get balance sheet for a ticker
#'
#' @inheritParams get_income_statement
#' @param balance_keys Balance sheet keys to retrieve (default all)
#' @return Either a tibble with balance sheet data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
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
    return(tibble::tibble())
  }

  # Extract the result data
  result_data <- resp_json$timeseries$result

  # Process the timeseries data
  processed_data <- process_timeseries_data(result_data, pretty)

  return(processed_data)
}

#' Get cash flow statement for a ticker
#' @inheritParams get_income_statement
#' @param cashflow_keys Vector of cash flow statement keys (default all)
#' @return Either a tibble with cash flow statement data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
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
    return(tibble::tibble())
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
    return(tibble::tibble())
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
    return(tibble::tibble())
  }

  # Convert to tibble
  result_df <- do.call(rbind, lapply(all_observations, as.data.frame))
  result_tbl <- tibble::as_tibble(result_df)

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
#' @inheritParams get_income_statement
#' @inheritParams get_balance_sheet
#' @inheritParams get_cashflow
#' @return A list containing income statement, balance sheet, and cash flow statement,
#'   or if output is "request" or "response", a list of httr2 request or response objects
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#'
#' # Get all annual financial statements
#' financials <- get_financial_statements(apple)
#'
#' # Get all quarterly financial statements
#' quarterly_financials <- get_financial_statements(apple, freq = "quarterly")
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
