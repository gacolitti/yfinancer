% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{get_info}
\alias{get_info}
\title{Get basic information about a ticker}
\usage{
get_info(
  ticker,
  modules = "summaryProfile",
  output = c("tibble", "list", "response", "request"),
  proxy = NULL
)
}
\arguments{
\item{ticker}{A ticker name or ticker object created with get_tickers).}

\item{modules}{A character vector of modules to request from the API}

\item{output}{The output format for the request (tibble, list, response, request)}

\item{proxy}{A character string specifying the proxy URL}
}
\value{
A tibble, list, response, or request object containing the requested information.
If multiple modules are requested, returns a list of tibbles. Some individual modules
may return a list of tibbles even when output is set to "tibble" because the underlying
data contains multiple datasets.
}
\description{
Get basic information about a ticker
}
\examples{
\dontrun{
# Get a single module
apple <- get_tickers("AAPL")
apple_profile <- get_info(apple, modules = "assetProfile")

# Get multiple modules as a list of tibbles
apple_financials <- get_info(apple,
  modules = c("incomeStatementHistory", "balanceSheetHistory")
)
}
}
