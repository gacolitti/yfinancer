#' Get A1 cookie from Yahoo Finance
#'
#' Uses curl_chrome110 to get the A1 cookie from Yahoo Finance
#'
#' @return A1 cookie string or NULL if not found
#' @keywords internal
get_a1_cookie <- function() {
  # Check if curl_chrome110 is available
  if (system("which curl_chrome110", ignore.stdout = TRUE) != 0) {
    rlang::abort("curl_chrome110 command not found. Please install it to use this feature.")
  }

  # Use curl_chrome110 to get the A1 token cookie
  cmd_output <- system("curl_chrome110 -v -s 'https://finance.yahoo.com' 2>&1 1> /dev/null", intern = TRUE)

  # Extract set-cookie headers that contain "A1="
  a1_lines <- cmd_output[grepl("set-cookie:", tolower(cmd_output)) & grepl("A1=", cmd_output, fixed = TRUE)]

  # If no A1 cookie was found, return NULL
  if (length(a1_lines) == 0) {
    rlang::warn("No A1 cookie found in Yahoo Finance response")
    return(NULL)
  }

  # Extract the A1 token
  a1_token <- gsub(".*set-cookie: A1=([^;]+).*", "\\1", a1_lines[1], ignore.case = TRUE)
  a1_token
}

#' Get crumb using A1 cookie
#'
#' @param a1_cookie A1 cookie string
#' @param proxy Optional proxy settings
#' @return Crumb string or NULL if not found
#' @keywords internal
get_crumb <- function(a1_cookie, proxy = NULL) {
  req_crumb <- httr2::request("https://query2.finance.yahoo.com/v1/test/getcrumb") |>
    httr2::req_headers(
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36",
      "Accept" = "*/*",
      "Accept-Language" = "en-US,en;q=0.9",
      "Referer" = "https://finance.yahoo.com/",
      "Origin" = "https://finance.yahoo.com",
      "Sec-Fetch-Site" = "same-site",
      "Sec-Fetch-Mode" = "cors",
      "Sec-Fetch-Dest" = "empty",
      "Cookie" = paste0("A1=", a1_cookie)
    ) |>
    httr2::req_timeout(30)

  if (!is.null(proxy)) {
    req_crumb <- req_crumb |> httr2::req_proxy(proxy)
  }

  tryCatch(
    {
      resp_crumb <- req_crumb |> httr2::req_perform()
      crumb <- httr2::resp_body_string(resp_crumb)
      crumb
    },
    error = function(e) {
      rlang::warn(sprintf("Failed to get crumb: %s", e$message))
      NULL
    }
  )
}

#' Read Auth File
#'
#' @param path Path to the authentication file
#' @param refresh Whether to refresh the auth file
#' @return NULL
#' @keywords internal
read_auth_file <- function(path = NULL, refresh = FALSE) {
  # Check if the file exists
  if (!is.null(path) && !file.exists(path)) {
    rlang::warn(paste0("No authentication file found at ", path))
    return(NULL)
  }

  if (is.null(path)) {
    # Use R's standard location for package config files
    config_dir <- tools::R_user_dir("yfinancer", which = "cache")
    path <- file.path(config_dir, "auth")
  }

  if (refresh) {
    file.remove(path)
  }

  # Check if the file is valid JSON
  json <- tryCatch(
    {
      jsonlite::fromJSON(path)
    },
    error = function(e) {
      NULL
    }
  )

  # Check if the file is complete if it exists
  if (!is.null(json)) {
    if (all(c("a1_cookie", "crumb") %in% names(json))) {
      should_message("Using existing authentication file.", timestamp_file = "yfinancer_auth_message_timestamp")
      return(json)
    }
  }

  # Fetch A1 cookie and crumb
  a1_cookie <- get_a1_cookie()
  crumb <- get_crumb(a1_cookie)

  auth_data <- list(a1_cookie = a1_cookie, crumb = crumb)

  # Write the authentication details to the file
  rlang::inform("Creating authentication file at ", path)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(auth_data, path)
  auth_data
}

#' Get Yahoo Finance authentication (crumb)
#'
#' Tries multiple methods to get a crumb:
#' 1. Check YFINANCE_CRUMB and YFINANCE_A1 environment variables
#' 2. If environment variables not available, get auth from ~/.yfinance/auth file
#'
#' @param req Request object
#' @param proxy Optional proxy settings
#' @param refresh Logical. If TRUE, force a refresh of the crumb.
#' @param path Path to authentication file. Default is ~/.yfinance/auth
#' @return Request object with added authentication
#' @keywords internal
req_add_auth <- function(req, proxy = NULL, refresh = FALSE, path = NULL) {
  # First check for environment variables
  env_crumb <- Sys.getenv("YFINANCE_CRUMB", NA_character_)
  env_a1 <- Sys.getenv("YFINANCE_A1", NA_character_)

  # If both environment variables are available, use them
  if (!is.na(env_crumb) && !is.na(env_a1)) {
    should_message("Using environment variables for authentication.",
      timestamp_file = "yfinancer_env_message_timestamp"
    )
    return(
      req |>
        httr2::req_url_query("crumb" = env_crumb) |>
        httr2::req_headers("Cookie" = paste0("A1=", env_a1))
    )
  }

  # Otherwise, check for A1 and crumb in ~/.yfinance/auth
  auth <- read_auth_file(path, refresh)

  # Add cookies to the request
  req |>
    httr2::req_url_query("crumb" = auth$crumb) |>
    httr2::req_headers("Cookie" = paste0("A1=", auth$a1_cookie))
}

#' Control message frequency
#'
#' Shows a message only once every `interval` hours
#'
#' @param msg The message to display
#' @param interval Time interval in hours between showing messages
#' @param timestamp_file Name used to ID the timestamp file (defaults to a tempfile)
#' @return NULL invisibly
#' @keywords internal
should_message <- function(msg, interval = 8, timestamp_file = NULL) {
  # Use a tempfile for tracking message timing
  # This creates a unique temporary file that persists across R sessions
  if (is.null(timestamp_file)) timestamp_file <- "yfinancer_message_timestamp"
  temp_dir <- tempdir()
  timestamp_file <- file.path(temp_dir, timestamp_file)

  current_time <- Sys.time()
  can_message <- TRUE

  # Check if the timestamp file exists
  if (file.exists(timestamp_file)) {
    tryCatch(
      {
        # Read the last message time
        last_message_time <- as.POSIXct(readLines(timestamp_file)[1])

        # Only message if more than 'interval' hours have passed
        time_diff <- difftime(current_time, last_message_time, units = "hours")
        if (time_diff < interval) {
          can_message <- FALSE
        }
      },
      error = function(e) {
        # If there's an error reading the file, allow messaging
        can_message <- TRUE
      }
    )
  }

  # If we can show a message, update the timestamp file and display the message
  if (can_message) {
    writeLines(as.character(current_time), timestamp_file)
    rlang::inform(msg)
  }

  invisible(NULL)
}
