#' Get Ticker Objects
#'
#' Creates one or more ticker objects for accessing data for ticker symbols.
#' This function handles both single and multiple ticker symbols.
#'
#' @param ... One or more ticker symbols as separate arguments (e.g., "AAPL", "MSFT")
#' @param proxy Optional proxy settings
#' @return For a single symbol: A list containing ticker data and methods with class "yf_ticker"
#'         For multiple symbols: A list containing multiple ticker objects with class "yf_tickers"
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a single ticker
#' apple <- get_tickers("AAPL")
#'
#' # Get historical data for a single ticker
#' apple_history <- get_history(apple)
#'
#' # Get company information for a single ticker
#' apple_info <- get_info(apple)
#'
#' # Get multiple tickers
#' tech_tickers <- get_tickers("AAPL", "MSFT", "GOOG")
#'
#' # Get information for multiple tickers
#' tech_info <- get_tickers_info(tech_tickers)
#'
#' # Get historical data for multiple tickers
#' tech_history <- get_tickers_history(tech_tickers, period = "1y")
#' }
get_tickers <- function(..., proxy = NULL) {
  # Collect all symbols from ... 
  symbols <- unlist(list(...))
  
  # Validate all provided symbols
  validated <- validate_tickers(symbols)

  # Filter out invalid symbols
  valid_symbols <- validated$symbol[validated$isValid]

  if (length(valid_symbols) == 0) {
    rlang::abort("No valid ticker symbols provided")
  }

  # If any symbols were invalid, warn the user
  invalid_symbols <- validated$symbol[!validated$isValid]
  if (length(invalid_symbols) > 0) {
    warning(sprintf("Invalid ticker symbol(s): %s", paste(invalid_symbols, collapse = ", ")))
  }

  # Single ticker case
  if (length(valid_symbols) == 1 && length(symbols) == 1) {
    ticker_obj <- list(
      symbol = valid_symbols,
      proxy = proxy
    )

    class(ticker_obj) <- "yf_ticker"

    return(ticker_obj)
  }

  # Multiple tickers case
  # Create individual ticker objects
  ticker_objects <- purrr::map(valid_symbols, function(symbol) {
    ticker_obj <- list(
      symbol = symbol,
      proxy = proxy
    )

    class(ticker_obj) <- "yf_ticker"

    return(ticker_obj)
  })

  # Name the list with ticker symbols
  names(ticker_objects) <- valid_symbols

  tickers_obj <- list(
    symbols = valid_symbols,
    tickers = ticker_objects,
    proxy = proxy
  )

  class(tickers_obj) <- "yf_tickers"

  return(tickers_obj)
}

#' Format ticker object print output
#'
#' @param x The ticker object
#' @param ... Additional arguments passed to print
#' @return The ticker object (invisibly)
#' @export
#' @keywords internal
print.yf_ticker <- function(x, ...) {
  cat(sprintf("Yahoo Finance Ticker: %s\n", x$symbol))
  cat("Available functions for this ticker (used as: function_name(ticker, ...)):\n")
  cat("  - get_history(): Get historical market data\n")
  cat("  - get_info(): Get company information\n")

  # Financial statements
  cat("Financial Statement functions:\n")
  cat("  - get_financials(): Get all financial statements\n")
  cat("  - get_income_statement(): Get income statement\n")
  cat("  - get_balance_sheet(): Get balance sheet\n")
  cat("  - get_cash_flow(): Get cash flow statement\n")

  invisible(x)
}

#' Format tickers object print output
#'
#' @param x The tickers object
#' @param ... Additional arguments passed to print
#' @return The tickers object (invisibly)
#' @export
#' @keywords internal
print.yf_tickers <- function(x, ...) {
  cat(sprintf("Yahoo Finance Tickers: %s\n", paste(x$symbols, collapse = ", ")))
  cat("Available functions for these tickers (used as: function_name(tickers, ...)):\n")
  cat("  - get_tickers_history(): Get historical market data for all tickers\n")
  cat("  - get_tickers_info(): Get information for all tickers\n")

  # Financial Statements
  cat("Financial Statement functions:\n")
  cat("  - get_tickers_financials(): Get all financial statements for all tickers\n")
  cat("  - get_tickers_income_statement(): Get income statement for all tickers\n")
  cat("  - get_tickers_balance_sheet(): Get balance sheet for all tickers\n")
  cat("  - get_tickers_cashflow(): Get cash flow statement for all tickers\n")
  invisible(x)
}

#' Get information for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param modules Modules to retrieve information for (default "summaryProfile")
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @param proxy Optional proxy settings
#' @return A list of information for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_info <- get_tickers_info(tech_tickers)
#' }
get_tickers_info <- function(tickers_obj, modules = "summaryProfile", output = c("tibble", "response", "request"), proxy = NULL) {
  output <- rlang::arg_match(output)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get info for each ticker
  info_list <- purrr::map(
    tickers_obj$tickers,
    function(ticker) {
      get_info(ticker, modules = modules, output = output, proxy = proxy)
    }
  )

  # Name the list with ticker symbols
  names(info_list) <- tickers_obj$symbols

  return(info_list)
}

#' Get historical data for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param period The period to download data for (default "1mo")
#' @param interval The interval between data points (default "1d")
#' @param start_date Start date for custom date range (format: "YYYY-MM-DD")
#' @param end_date End date for custom date range (format: "YYYY-MM-DD")
#' @param prepost Include pre and post market data (default FALSE)
#' @param auto_adjust Adjust all OHLC automatically (default TRUE)
#' @param back_adjust Adjust data to reflect splits and dividends (default TRUE)
#' @param repair Repair missing data (default TRUE)
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @param proxy Optional proxy settings
#' @return A list of tibbles with historical market data for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_history <- get_tickers_history(tech_tickers, period = "1y")
#' }
get_tickers_history <- function(tickers_obj,
                                period = "1mo",
                                interval = "1d",
                                start_date = NULL,
                                end_date = NULL,
                                prepost = FALSE,
                                auto_adjust = TRUE,
                                back_adjust = TRUE,
                                repair = TRUE,
                                output = c("tibble", "response", "request"),
                                proxy = NULL) {
  output <- rlang::arg_match(output)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get history for each ticker
  history_list <- purrr::map(tickers_obj$tickers, function(ticker) {
    get_history(
      ticker,
      period = period,
      interval = interval,
      start_date = start_date,
      end_date = end_date,
      prepost = prepost,
      auto_adjust = auto_adjust,
      back_adjust = back_adjust,
      repair = repair,
      output = output,
      proxy = proxy
    )
  })

  # Name the list with ticker symbols
  names(history_list) <- tickers_obj$symbols

  return(history_list)
}

#' Get income statement for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start_timestamp Start timestamp (default EOY 2016)
#' @param end_timestamp End timestamp (default current timestamp)
#' @param income_keys Vector of income statement keys (default all)
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return A list of tibbles with income statement data for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_income <- get_tickers_income_statement(tech_tickers)
#' }
#' @export
get_tickers_income_statement <- function(tickers_obj, freq = c("annual", "quarterly"),
                                         start_timestamp = NULL, end_timestamp = NULL,
                                         income_keys = NULL, pretty = TRUE, wide = TRUE,
                                         proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get income statement for each ticker
  income_list <- purrr::map(
    tickers_obj$tickers,
    function(ticker) {
      get_income_statement(
        ticker,
        freq = freq,
        start_timestamp = start_timestamp,
        end_timestamp = end_timestamp,
        income_keys = income_keys,
        pretty = pretty,
        wide = wide,
        proxy = proxy,
        output = output
      )
    }
  )

  # Name the list with ticker symbols
  names(income_list) <- tickers_obj$symbols

  return(income_list)
}

#' Get balance sheet for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start_timestamp Start timestamp (default EOY 2016)
#' @param end_timestamp End timestamp (default current timestamp)
#' @param balance_keys Balance sheet keys to retrieve (default all)
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return A list of tibbles with balance sheet data for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_balance <- get_tickers_balance_sheet(tech_tickers)
#' }
#' @export
get_tickers_balance_sheet <- function(tickers_obj, freq = c("annual", "quarterly"),
                                      start_timestamp = NULL, end_timestamp = NULL,
                                      balance_keys = NULL, pretty = TRUE, wide = TRUE,
                                      proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get balance sheet for each ticker
  balance_list <- purrr::map(
    tickers_obj$tickers,
    function(ticker) {
      get_balance_sheet(
        ticker,
        freq = freq,
        start_timestamp = start_timestamp,
        end_timestamp = end_timestamp,
        balance_keys = balance_keys,
        pretty = pretty,
        wide = wide,
        proxy = proxy,
        output = output
      )
    }
  )

  # Name the list with ticker symbols
  names(balance_list) <- tickers_obj$symbols

  return(balance_list)
}

#' Get cash flow statement for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start_timestamp Start timestamp (default EOY 2016)
#' @param end_timestamp End timestamp (default current timestamp)
#' @param cashflow_keys Vector of cash flow statement keys (default all)
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return A list of tibbles with cash flow statement data for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_cashflow <- get_tickers_cashflow(tech_tickers)
#' }
#' @export
get_tickers_cashflow <- function(tickers_obj, freq = c("annual", "quarterly"),
                                 start_timestamp = NULL, end_timestamp = NULL,
                                 cashflow_keys = NULL, pretty = TRUE, wide = TRUE,
                                 proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get cash flow statement for each ticker
  cashflow_list <- purrr::map(
    tickers_obj$tickers,
    function(ticker) {
      get_cashflow(
        ticker,
        freq = freq,
        start_timestamp = start_timestamp,
        end_timestamp = end_timestamp,
        cashflow_keys = cashflow_keys,
        pretty = pretty,
        wide = wide,
        proxy = proxy,
        output = output
      )
    }
  )

  # Name the list with ticker symbols
  names(cashflow_list) <- tickers_obj$symbols

  return(cashflow_list)
}

#' Get all financial statements for multiple tickers
#'
#' @param tickers_obj A tickers object created with get_tickers()
#' @param freq Frequency of data: "annual" or "quarterly" (default "annual")
#' @param start_timestamp Start timestamp for data
#' @param end_timestamp End timestamp for data
#' @param cash_flow_keys Cash flow statement keys (default all)
#' @param balance_keys Balance sheet keys (default all)
#' @param income_keys Income statement keys (default all)
#' @param pretty Format column names to be more readable (default TRUE)
#' @param wide Return data in wide format (default TRUE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", or "request" (default "tibble")
#' @return A nested list containing financial statements for each ticker
#'
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_financials <- get_tickers_financials(tech_tickers)
#' }
#' @export
get_tickers_financials <- function(tickers_obj, freq = c("annual", "quarterly"),
                                   start_timestamp = NULL, end_timestamp = NULL,
                                   cash_flow_keys = NULL, balance_keys = NULL,
                                   income_keys = NULL, pretty = TRUE, wide = TRUE,
                                   proxy = NULL, output = c("tibble", "response", "request")) {
  output <- rlang::arg_match(output)
  freq <- rlang::arg_match(freq)

  if (!inherits(tickers_obj, "yf_tickers")) {
    rlang::abort("tickers_obj must be a yf_tickers object created with get_tickers()")
  }

  # Get financial statements for each ticker
  financials_list <- purrr::map(
    tickers_obj$tickers,
    function(ticker) {
      get_financials(
        ticker,
        freq = freq,
        start_timestamp = start_timestamp,
        end_timestamp = end_timestamp,
        cash_flow_keys = cash_flow_keys,
        balance_keys = balance_keys,
        income_keys = income_keys,
        pretty = pretty,
        wide = wide,
        proxy = proxy,
        output = output
      )
    }
  )

  # Name the list with ticker symbols
  names(financials_list) <- tickers_obj$symbols

  return(financials_list)
}
