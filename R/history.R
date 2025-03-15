#' Build request parameters for history API
#' @keywords internal
build_history_params <- function(interval, prepost, period, start_date, end_date) {
  # Parameters for API request
  params <- list(
    interval = interval,
    includePrePost = if (prepost) 1 else 0,
    events = "div,splits"
  )

  # Use either period or start/end dates
  if (!is.null(start_date) && !is.null(end_date)) {
    params$period1 <- format_date(start_date)
    params$period2 <- format_date(end_date)
  } else {
    params$range <- period
  }

  return(params)
}

#' Extract and validate chart data from response
#' @keywords internal
extract_chart_data <- function(resp_json) {
  # Extract chart data
  chart_result <- resp_json$chart$result

  if (length(chart_result) == 0) {
    return(NULL)
  }

  # Extract the data
  timestamps <- unlist(chart_result[[1]]$timestamp)

  if (is.null(timestamps) || length(timestamps) == 0) {
    return(NULL)
  }

  return(chart_result)
}

#' Create historical data frame from chart result
#' @keywords internal
create_historical_df <- function(chart_result) {
  timestamps <- unlist(chart_result[[1]]$timestamp)
  quote <- chart_result[[1]]$indicators$quote[[1]]

  # Create data frame from historical data
  tibble::tibble(
    timestamp = lubridate::as_datetime(timestamps),
    date = unix_to_date(timestamps),
    open = quote$open,
    high = quote$high,
    low = quote$low,
    close = unlist(quote$close),
    volume = unlist(quote$volume)
  )
}

#' Apply price adjustments to data
#' @keywords internal
apply_adjustments <- function(data, chart_result, auto_adjust, back_adjust) {
  if (!auto_adjust || !("adjclose" %in% names(chart_result[[1]]$indicators))) {
    return(data)
  }

  # Get adjusted close prices
  adj_close <- unlist(chart_result[[1]]$indicators$adjclose[[1]]$adjclose)
  data$adjusted_close <- adj_close

  if (!back_adjust) {
    return(data)
  }

  # Calculate adjustment ratio
  close_values <- as.numeric(data$close)
  adj_close_values <- as.numeric(data$adjusted_close)

  # Only calculate ratio where we have valid numeric values
  valid_indices <- !is.na(close_values) & !is.na(adj_close_values) & close_values > 0
  ratio <- rep(1, nrow(data))
  ratio[valid_indices] <- adj_close_values[valid_indices] / close_values[valid_indices]

  # Adjust OHLC
  data$open <- as.numeric(data$open) * ratio
  data$high <- as.numeric(data$high) * ratio
  data$low <- as.numeric(data$low) * ratio
  data$close <- data$adjusted_close

  return(data)
}

#' Add dividends and splits to data frame
#' @keywords internal
add_events <- function(data, chart_result) {
  if (!("events" %in% names(chart_result[[1]]))) {
    return(data)
  }

  events <- chart_result[[1]]$events

  # Add dividends
  if ("dividends" %in% names(events)) {
    div_data <- events$dividends
    div_timestamps <- as.numeric(names(div_data))
    div_amounts <- purrr::map_dbl(div_data, "amount")

    div_df <- tibble::tibble(
      timestamp = div_timestamps,
      date = unix_to_date(div_timestamps),
      dividend = div_amounts
    )

    data <- dplyr::left_join(
      data,
      div_df,
      by = c("timestamp", "date")
    )
  }

  # Add splits
  if ("splits" %in% names(events)) {
    split_data <- events$splits
    split_timestamps <- as.numeric(names(split_data))
    split_ratios <- purrr::map_dbl(split_data, function(x) x$numerator / x$denominator)

    split_df <- tibble::tibble(
      timestamp = split_timestamps,
      date = unix_to_date(split_timestamps),
      split = split_ratios
    )

    data <- dplyr::left_join(
      data,
      split_df,
      by = c("timestamp", "date")
    )
  }

  return(data)
}

#' Repair missing data in time series
#' @keywords internal
repair_data <- function(data, repair = TRUE) {
  if (!repair) {
    return(data)
  }

  # Forward fill NA values
  cols_to_fill <- c("open", "high", "low", "close", "adjusted_close")
  cols_to_fill <- cols_to_fill[cols_to_fill %in% names(data)]
  if (length(cols_to_fill) > 0) {
    data <- tidyr::fill(data, dplyr::all_of(cols_to_fill), .direction = "down")
  }

  return(data)
}

#' Get historical market data for a ticker
#'
#' @param ticker A ticker name or ticker object created with get_tickers).
#' @param period The period to download data for (default "1mo")
#' @param interval The interval between data points (default "1d")
#' @param start_date Start date for custom date range (format: "YYYY-MM-DD")
#' @param end_date End date for custom date range (format: "YYYY-MM-DD")
#' @param prepost Include pre and post market data (default FALSE)
#' @param auto_adjust Adjust all OHLC automatically (default TRUE)
#' @param back_adjust Adjust data to reflect splits and dividends (default TRUE)
#' @param repair Repair missing data (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return Either a tibble with historical market data, an httr2 response object, or an httr2 request object
#'   depending on the value of the output argument.
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#' # Get 1 month of daily data
#' apple_history <- get_history(apple)
#' # Get 1 year of daily data
#' apple_history_1y <- get_history(apple, period = "1y")
#' # Get custom date range
#' apple_history_custom <- get_history(
#'   apple,
#'   start_date = "2022-01-01",
#'   end_date = "2022-12-31"
#' )
#' }
get_history <- function(ticker,
                        period = "1mo",
                        interval = "1d",
                        start_date = NULL,
                        end_date = NULL,
                        prepost = FALSE,
                        auto_adjust = TRUE,
                        back_adjust = TRUE,
                        repair = TRUE,
                        proxy = NULL,
                        output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  period <- validate_period(period)
  interval <- validate_interval(interval)

  # Build request parameters
  params <- build_history_params(interval, prepost, period, start_date, end_date)

  # Create request with query parameters
  req <- httr2::request(yf_base_url) |>
    httr2::req_url_path("v8/finance/chart", ticker$symbol) |>
    httr2::req_url_query(!!!params)

  if (output == "request") {
    return(req)
  }

  # Perform request
  resp <- httr2::req_perform(req)

  if (output == "response") {
    return(resp)
  }

  resp_json <- httr2::resp_body_json(resp)

  # Extract chart data
  chart_result <- extract_chart_data(resp_json)
  if (is.null(chart_result)) {
    return(tibble::tibble())
  }

  # Process data through a pipeline of operations
  data <- create_historical_df(chart_result) |>
    # Apply adjustments if needed
    apply_adjustments(chart_result, auto_adjust, back_adjust) |>
    # Add dividends and splits
    add_events(chart_result) |>
    # Repair missing data
    repair_data(repair) |>
    # Return data sorted by date
    dplyr::arrange(date)

  return(data)
}
