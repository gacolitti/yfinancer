#' Add headers to a request
#'
#' @param req Request object
#' @return Request object with added headers
#' @keywords internal
req_add_headers <- function(req) {
  req |>
    httr2::req_headers(
      "User-Agent" = get_user_agent(),
      "Accept" = "*/*",
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept-Encoding" = "gzip, deflate, br",
      "Referer" = "https://finance.yahoo.com/",
      "Origin" = "https://finance.yahoo.com",
      "Sec-Fetch-Site" = "same-site",
      "Sec-Fetch-Mode" = "cors",
      "Sec-Fetch-Dest" = "empty"
    )
}
