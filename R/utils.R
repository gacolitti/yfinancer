#' Convert Unix timestamp to date
#'
#' @param timestamp Unix timestamp in seconds (can be a vector)
#' @return A Date object or vector of Date objects
#' @keywords internal
unix_to_date <- function(timestamp) {
  if (is.null(timestamp)) {
    return(NA)
  }

  # Handle vectors with NA values
  if (length(timestamp) > 1) {
    return(lubridate::as_datetime(as.numeric(timestamp)))
  }

  # Handle single value
  if (is.na(timestamp)) {
    return(NA)
  }

  lubridate::as_datetime(as.numeric(timestamp))
}

#' Format date for API requests
#'
#' @param date Date object or string in YYYY-MM-DD format
#' @return Unix timestamp in seconds
#' @keywords internal
format_date <- function(date) {
  if (is.null(date)) {
    return(NULL)
  }

  if (inherits(date, "Date") || inherits(date, "POSIXt")) {
    return(as.integer(as.numeric(date)))
  }

  # If string, convert to Date then to timestamp
  if (is.character(date)) {
    date <- as.Date(date)
    return(as.integer(as.numeric(date)))
  }

  rlang::abort("Invalid date format. Please provide a Date object or YYYY-MM-DD string.")
}

#' Clean table or vector of names
#'
#' @return Returns vector if vector or data.frame if data.frame.
#' @param .data Data.frame or vector of column names.
#' @param unique Should the variable names be unique?
#' @param minus_to_underscore By default `-` is replaced with `minus`.
#'   This argument replaces the hyphen with `_` (underscore) instead.
#'
#' @export
clean_names <- function(.data,
                        unique = FALSE,
                        minus_to_underscore = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data

  # If Non-ASCII characters found, return original strings
  if (any(grepl("[^ -~]", n))) {
    warning("Found non-ASCII characters. Returning original names...", call. = FALSE)
    return(n)
  }

  n <- gsub("%+", "_per_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)

  if (minus_to_underscore) {
    n <- gsub("-+", "_", n)
  } else {
    n <- gsub("-+", "_minus_", n)
  }

  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)


  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  n <- gsub("(^_+|_+$)", "", n)
  n <- gsub("_+", "_", n)

  if (unique) n <- make.unique(n, sep = "_")

  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

#' Process Timestamp Argument
#' Converts a character string, timestamp, or date to a unix timestamp
#' @param timestamp A character string that can be converted to a unix timestamp, timestamp, or date
#' @param default A default value to return if timestamp is NULL
#' @return A unix timestamp. Default is current time.
#' @keywords internal
process_timestamp_arg <- function(timestamp = NULL, default = NULL) {
  if (is.numeric(timestamp)) {
    return(timestamp)
  }

  if (is.null(default)) default <- as.numeric(Sys.time())

  if (is.null(timestamp)) {
    return(default)
  }

  if (is.character(timestamp)) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", timestamp)) {
      timestamp <- lubridate::as_datetime(timestamp)
      return(as.integer(as.numeric(timestamp)))
    }

    if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", timestamp)) {
      timestamp <- lubridate::as_datetime(timestamp)
      return(as.integer(as.numeric(timestamp)))
    }
  }

  rlang::abort("Invalid timestamp format. Please provide a unix timestamp, a Date object, or a YYYY-MM-DD string.")
}

#' Null coalescing operator
#'
#' @name null-coalesce-operator
#' @param x First value
#' @param y Fallback value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
