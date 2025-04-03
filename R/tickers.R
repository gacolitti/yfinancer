# This file contains functions for retrieving and processing data for multiple tickers.
# Many of the functions are wrappers for the corresponding single ticker functions.

#' Format ticker object print output
#'
#' @param x The ticker object
#' @param ... Additional arguments passed to print
#' @return The ticker object (invisibly)
#' @export
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
  cat("  - get_cashflow(): Get cash flow statement\n")

  invisible(x)
}

#' Format tickers object print output
#'
#' @param x The tickers object
#' @param ... Additional arguments passed to print
#' @return The tickers object (invisibly)
#' @export
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

#' Get Ticker Objects
#'
#' Creates one or more ticker objects for accessing data for ticker symbols.
#' This function handles both single and multiple ticker symbols and validates
#' the provided ticker symbols.
#'
#' @section Rate Limiting:
#' Yahoo Finance does not provide official API documentation or rate limits. Based on community
#' observations, there are approximate limits of a few hundred requests per day from a single IP
#' address before throttling may occur. When working with multiple tickers, consider:
#' \itemize{
#'   \item Batching requests when possible
#'   \item Adding delays between requests using `Sys.sleep()`
#'   \item Caching results for frequently accessed tickers
#'   \item Using the batch functions (e.g., `get_tickers_history()`) instead of individual calls
#' }
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

    ticker_obj
  })

  # Name the list with ticker symbols
  names(ticker_objects) <- valid_symbols

  tickers_obj <- list(
    symbols = valid_symbols,
    tickers = ticker_objects,
    proxy = proxy
  )

  class(tickers_obj) <- "yf_tickers"

  tickers_obj
}

#' Get information for multiple tickers
#'
#' Retrieves company information from Yahoo Finance for multiple specified ticker symbols.
#'
#' See `get_info` for more details on the company information.
#'
#' @inheritParams get_info
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A list of information for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_info <- get_tickers_info(tech_tickers)
#' }
#' @export
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

  info_list
}

#' Get historical data for multiple tickers
#'
#' Retrieves historical market data from Yahoo Finance for multiple specified ticker symbols.
#'
#' See `get_history` for more details on the historical market data.
#'
#' @inheritParams get_history
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A list of tibbles with historical market data for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_history <- get_tickers_history(tech_tickers, period = "1y")
#' }
#' @export
get_tickers_history <- function(tickers_obj,
                                period = "1mo",
                                interval = "1d",
                                start = NULL,
                                end = NULL,
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
      start = start,
      end = end,
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

  history_list
}

#' Get income statement for multiple tickers
#'
#' Retrieves income statement data from Yahoo Finance for multiple specified ticker symbols.
#' Income statements show a company's revenues, expenses, and profits over a specific period.
#'
#' See `get_income_statement` for more details on the income statement.
#'
#' @inheritParams get_income_statement
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A list of tibbles with income statement data for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_income <- get_tickers_income_statement(tech_tickers)
#' }
#' @export
get_tickers_income_statement <- function(tickers_obj, freq = c("annual", "quarterly"),
                                         start = NULL, end = NULL,
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
        start = start,
        end = end,
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

  income_list
}

#' Get balance sheet for multiple tickers
#'
#' Retrieves balance sheet data from Yahoo Finance for multiple specified ticker symbols.
#' Balance sheets show a company's assets, liabilities, and shareholders' equity
#' at a specific point in time.
#'
#' See `get_balance_sheet` for more details on the balance sheet.
#'
#' @inheritParams get_balance_sheet
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A list of tibbles with balance sheet data for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_balance <- get_tickers_balance_sheet(tech_tickers)
#' }
#' @export
get_tickers_balance_sheet <- function(tickers_obj, freq = c("annual", "quarterly"),
                                      start = NULL, end = NULL,
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
        start = start,
        end = end,
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

  balance_list
}

#' Get cash flow statement for multiple tickers
#'
#' Retrieves cash flow statement data from Yahoo Finance for multiple specified ticker symbols.
#' Cash flow statements show how changes in balance sheet accounts and income affect
#' cash and cash equivalents, breaking the analysis down to operating, investing, and
#' financing activities.
#'
#' See `get_cashflow` for more details on the cash flow statement.
#'
#' @inheritParams get_cashflow
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A list of tibbles with cash flow statement data for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_cashflow <- get_tickers_cashflow(tech_tickers)
#' }
#' @export
get_tickers_cashflow <- function(tickers_obj, freq = c("annual", "quarterly"),
                                 start = NULL, end = NULL,
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
        start = start,
        end = end,
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

  cashflow_list
}

#' Get all financial statements for multiple tickers
#' @inheritParams get_financials
#' @param tickers_obj A tickers object created with get_tickers()
#' @return A nested list containing financial statements for each ticker
#' @examples
#' \dontrun{
#' tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
#' tech_financials <- get_tickers_financials(tech_tickers)
#' }
#' @export
get_tickers_financials <- function(tickers_obj, freq = c("annual", "quarterly"),
                                   start = NULL, end = NULL,
                                   cashflow_keys = NULL, balance_keys = NULL,
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
        start = start,
        end = end,
        cashflow_keys = cashflow_keys,
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

  financials_list
}
