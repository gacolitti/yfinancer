#' Function to extract a field value based on its type
#'
#' @param field_data The field data to extract
#' @return The extracted field value or NULL if the field data is not present
#' @keywords internal
extract_field_value <- function(field_data) {
  if (is.null(field_data)) {
    return(NULL)
  } else if (is.list(field_data) && "raw" %in% names(field_data)) {
    return(field_data$raw)
  } else if (!is.list(field_data)) {
    return(field_data)
  }
  return(NULL)
}

#' Extract data from result modules
#'
#' Utility function that extracts data from nested module structures in Yahoo Finance API results.
#' Uses purrr functions to reduce complexity.
#'
#' @param result The API result containing modules
#' @return A list with extracted data
#' @keywords internal
extract_module_data <- function(result) {
  # Process each module
  all_modules <- result[[1]]

  # Only proceed if we have modules to process
  if (is.null(all_modules) || length(all_modules) == 0) {
    return(list())
  }

  # Extract fields from all modules and combine them
  fields <- all_modules |>
    purrr::map(function(module) {
      # Skip NULL modules
      if (is.null(module)) {
        return(list())
      }

      # Process each field in the module
      module |>
        purrr::imap(function(field_data, field_name) {
          # Extract value based on field type
          value <- extract_field_value(field_data)
          # Return NULL if we couldn't extract a value
          if (is.null(value)) {
            return(NULL)
          }
          # Return named value
          stats::setNames(list(value), field_name)
        }) |>
        purrr::compact() |>
        purrr::flatten()
    }) |>
    purrr::flatten()

  # Return the combined fields
  return(fields)
}

#' Get basic information about a ticker
#'
#' @param ticker A ticker name or ticker object created with get_tickers).
#' @param modules A character vector of modules to request from the API
#' @param output The output format for the request (tibble, list, response, request)
#' @param proxy A character string specifying the proxy URL
#' @return A list with company information
#'
#' @examples
#' \dontrun{
#' apple <- get_tickers("AAPL")
#' apple_info <- get_info(apple)
#' }
get_info <- function(ticker,
                     modules = "summaryProfile",
                     output = c("tibble", "list", "response", "request"),
                     proxy = NULL) {
  output <- rlang::arg_match(output)

  if (!inherits(ticker, "yf_ticker")) {
    ticker <- get_tickers(ticker, proxy = proxy)
  }

  # Filter modules for selected ones
  modules <- modules[modules %in% valid_modules]

  if (length(modules) == 0) {
    rlang::abort("No valid modules selected.")
  }

  # Construct request object
  req <- httr2::request(yf_query2_url) |>
    httr2::req_url_path("v10/finance/quoteSummary", ticker$symbol) |>
    httr2::req_url_query(modules = modules, .multi = "comma") |>
    httr2::req_proxy(proxy) |>
    req_add_auth(proxy = proxy) |>
    req_add_headers()

  if (output == "request") {
    return(req)
  }

  # Make request with tryCatch to handle potential errors
  resp <- httr2::req_perform(req)

  if (output == "response") {
    return(resp)
  }

  resp_json <- httr2::resp_body_json(resp)

  if (output == "list") {
    return(resp_json)
  }

  # Return empty list if response is NULL
  if (is.null(resp_json)) {
    return(list())
  }

  # Extract the data
  result <- resp_json$quoteSummary$result

  if (length(result) == 0) {
    return(list())
  }

  # Combine all modules into a single list
  info <- list()

  # Extract data from modules using utility function
  info <- extract_module_data(result)

  return(info)
}
