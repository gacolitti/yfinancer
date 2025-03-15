#' Search for ticker symbols, companies, and news from Yahoo Finance
#'
#' This function allows you to search for ticker symbols, companies, ETFs, etc.
#' using the Yahoo Finance search API. It can also return related news articles.
#'
#' @param query Search query string
#' @param limit Maximum number of results to return (default 10)
#' @param quotes_only Return only quotes/tickers, not news (default TRUE)
#' @param fuzzy_query Enable fuzzy search for typos (default FALSE)
#' @param lists_count Number of lists to retrieve (default 0)
#' @param enable_research Include research reports (default FALSE)
#' @param proxy Optional proxy settings
#' @param output Object to return. Can be "tibble", "response", "request", or "all" (default "tibble")
#' @return Depending on output parameter:
#'   - "tibble": quotes data (or a list with quotes and news if quotes_only is FALSE)
#'   - "response": raw response from the API
#'   - "request": the request object
#'   - "all": a named list containing all results: quotes, news, lists, research, and raw response
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for Apple
#' apple_results <- search_tickers("Apple")
#'
#' # Search for tech companies with more results
#' tech_results <- search_tickers("tech", limit = 20)
#'
#' # Get both quotes and news articles
#' apple_with_news <- search_tickers("Apple", quotes_only = FALSE)
#'
#' # Get all data including lists and research reports
#' all_apple_data <- search_tickers(
#'   "Apple",
#'   quotes_only = FALSE,
#'   lists_count = 5,
#'   enable_research = TRUE,
#'   output = "all"
#' )
#' }
search_tickers <- function(
    query,
    limit = 10,
    quotes_only = TRUE,
    fuzzy_query = FALSE,
    lists_count = 0,
    enable_research = FALSE,
    proxy = NULL,
    output = c("tibble", "response", "request", "all")) {
  # Validate arguments
  output <- rlang::arg_match(output)

  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) {
    rlang::abort("Query must be a non-empty character string")
  }

  # Construct request object for search
  req <- httr2::request(yf_base_url) |>
    httr2::req_url_path("v1/finance/search") |>
    httr2::req_url_query(
      q = query,
      quotesCount = as.integer(limit),
      newsCount = if (quotes_only) 0 else as.integer(limit),
      enableFuzzyQuery = if (fuzzy_query) "true" else "false",
      quotesQueryId = "tss_match_phrase_query",
      newsQueryId = "news_cie_vespa",
      listsCount = as.integer(lists_count),
      enableCb = "true",
      enableNavLinks = "false",
      enableEnhancedTrivialQuery = "true",
      enableResearchReports = if (enable_research) "true" else "false",
      enableCulturalAssets = "false"
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

  # Parse response
  resp_json <- httr2::resp_body_json(resp)

  # Handle error response or empty data
  if (is.null(resp_json) || "Will be right back" %in% resp_json) {
    rlang::warn("Yahoo Finance search API returned an error or empty response.")
    if (output == "all") {
      return(list(
        quotes = tibble::tibble(),
        news = tibble::tibble(),
        lists = list(),
        research = list(),
        response = resp_json
      ))
    }
    return(tibble::tibble())
  }

  # Extract components
  quotes <- resp_json$quotes %||% list()
  news <- resp_json$news %||% list()
  lists <- resp_json$lists %||% list()
  research <- resp_json$researchReports %||% list()

  # Filter quotes to only include entries with symbol
  quotes <- purrr::keep(quotes, ~ !is.null(.x$symbol))

  # Process quotes
  if (length(quotes) > 0) {
    quotes_df <- purrr::map_df(quotes, function(quote) {
      # Extract relevant fields
      tibble::tibble(
        symbol = quote$symbol %||% NA_character_,
        name = quote$shortname %||% quote$longname %||% NA_character_,
        exchange = quote$exchange %||% NA_character_,
        quoteType = quote$quoteType %||% NA_character_,
        score = quote$score %||% NA_real_,
        typeDisp = quote$typeDisp %||% NA_character_,
        market = quote$market %||% NA_character_,
        sector = quote$sector %||% NA_character_,
        industry = quote$industry %||% NA_character_
      )
    })
  } else {
    quotes_df <- tibble::tibble()
  }

  # Process news if present
  if (length(news) > 0) {
    news_df <- purrr::map_df(news, function(item) {
      tibble::tibble(
        title = item$title %||% NA_character_,
        publisher = item$publisher %||% NA_character_,
        link = item$link %||% NA_character_,
        publish_time = unix_to_date(item$providerPublishTime %||% NA_integer_),
        type = "news"
      )
    })
  } else {
    news_df <- tibble::tibble()
  }

  # Return result based on output parameter
  if (output == "all") {
    return(list(
      quotes = quotes_df,
      news = news_df,
      lists = lists,
      research = research,
      response = resp_json
    ))
  } else if (!quotes_only && length(news) > 0) {
    # Return both quotes and news if requested and news exists
    return(list(
      quotes = quotes_df,
      news = news_df
    ))
  } else {
    # Default: return only quotes
    return(quotes_df)
  }
}
