#' Validate Yahoo Finance Ticker Symbols
#'
#' This function validates whether given ticker symbols are valid
#' by making an API request to Yahoo Finance's validation endpoint.
#'
#' @param symbols A character vector of ticker symbols to validate
#' @return A tibble with validation results for each symbol
#' @export
validate_tickers <- function(symbols = NULL) {
  if (all(!nzchar(symbols))) {
    rlang::abort("Ticker symbols must be a non-empty character vector")
  }

  symbols <- parse_ticker_symbols(symbols)

  # Return early if no symbols provided
  if (is.null(symbols) || length(symbols) == 0) {
    return(dplyr::tibble(
      symbol = character(0),
      isValid = logical(0)
    ))
  }

  # Build and perform the request using httr2
  request <- httr2::request(yf_base_url) |>
    httr2::req_url_path_append("v6/finance/quote/validate") |>
    httr2::req_url_query(symbols = paste(symbols, collapse = ","))

  # Try to perform the request and handle potential errors
  result <- tryCatch(
    {
      resp <- httr2::req_perform(request)

      # Parse the response
      parsed <- jsonlite::fromJSON(
        httr2::resp_body_string(resp),
        simplifyVector = FALSE
      )

      # Extract validation results
      extract_validation_results(parsed, symbols)
    },
    error = function(e) {
      message(paste0("Yahoo Finance API request failed\nError: ", e$message))
      # Return tibble with NA values on error
      dplyr::tibble(symbol = symbols, isValid = FALSE)
    }
  )

  return(result)
}

#' Parse Ticker Symbols
#'
#' @param symbols A character vector of ticker symbols
#' @return A character vector of parsed ticker symbols
#' @keywords internal
parse_ticker_symbols <- function(symbols) {
  if (!is.character(symbols) || length(symbols) == 0) {
    rlang::abort("Ticker symbols must be a non-empty character vector")
  }

  # Remove any whitespace and convert to uppercase
  symbols <- purrr::map_chr(symbols, function(symbol) {
    toupper(stringr::str_trim(symbol))
  })

  symbols
}

#' Extract validation results from Yahoo Finance API response
#'
#' @param parsed_response The parsed JSON response from Yahoo Finance
#' @param original_symbols The original symbols that were requested
#' @return A tibble with validation results
#' @keywords internal
extract_validation_results <- function(parsed_response, original_symbols) {
  # Initialize result tibble with all requested symbols
  result <- dplyr::tibble(
    symbol = original_symbols,
    isValid = NA
  )

  # Fast path for invalid responses
  validation_path <- parsed_response$symbolsValidation$result
  if (is.null(validation_path) || length(validation_path) == 0) {
    return(result)
  }

  # Extract validation results
  validation_results <- validation_path[[1]]

  # Create vectors for mapping
  api_symbols <- toupper(names(validation_results))
  api_results <- vapply(validation_results, as.logical, logical(1))

  if (length(api_symbols) > 0) {
    # Create mapping of original symbols to uppercase for efficient matching
    upper_orig_symbols <- toupper(original_symbols)

    # Process matches in a vectorized way
    for (i in seq_along(api_symbols)) {
      # Get indices of matches in original symbols vector (should be just one match normally)
      match_indices <- which(upper_orig_symbols == api_symbols[i])

      # Update result for all matches
      if (length(match_indices) > 0) {
        result$isValid[match_indices] <- api_results[i]
      }
    }
  }

  result
}

#' Check if a variable is a logical value
#'
#' @param x The variable to check
#' @return The variable `x`
#' @keywords internal
check_is_lgl <- function(x) {
  x_name <- rlang::as_string(rlang::ensym(x))
  if (!is.logical(x)) {
    rlang::abort(glue::glue("`{x_name}` must be a logical value"))
  }
  x
}

#' Validate date period
#'
#' @param period A character string specifying the period (1d, 5d, 1mo, 3mo, 6mo, 1y, 2y, 5y, 10y, ytd, max)
#' @return The validated period
#' @keywords internal
validate_period <- function(period) {
  valid_periods <- c("1d", "5d", "1mo", "3mo", "6mo", "1y", "2y", "5y", "10y", "ytd", "max")

  if (!period %in% valid_periods) {
    rlang::abort(sprintf("Invalid period. Must be one of: %s", paste(valid_periods, collapse = ", ")))
  }

  period
}

#' Validate date interval
#'
#' @param interval A character string specifying the interval (1m, 2m, 5m, 15m, 30m, 60m, 90m, 1h, 1d, 5d, 1wk, 1mo, 3mo)
#' @return The validated interval
#' @keywords internal
validate_interval <- function(interval) {
  valid_intervals <- c("1m", "2m", "5m", "15m", "30m", "60m", "90m", "1h", "1d", "5d", "1wk", "1mo", "3mo")

  if (!interval %in% valid_intervals) {
    rlang::abort(sprintf("Invalid interval. Must be one of: %s", paste(valid_intervals, collapse = ", ")))
  }

  interval
}

#' Validate frequency
#'
#' @param frequency A character string specifying the frequency (annual, quarterly)
#' @return The validated frequency
#' @keywords internal
validate_frequency <- function(frequency) {
  valid_frequencies <- c("annual", "quarterly")

  if (!frequency %in% valid_frequencies) {
    rlang::abort(sprintf("Invalid frequency. Must be one of: %s", paste(valid_frequencies, collapse = ", ")))
  }

  frequency
}
