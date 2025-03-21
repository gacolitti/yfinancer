% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tickers.R
\name{get_tickers_balance_sheet}
\alias{get_tickers_balance_sheet}
\title{Get balance sheet for multiple tickers}
\usage{
get_tickers_balance_sheet(
  tickers_obj,
  freq = c("annual", "quarterly"),
  start_timestamp = NULL,
  end_timestamp = NULL,
  balance_keys = NULL,
  pretty = TRUE,
  wide = TRUE,
  proxy = NULL,
  output = c("tibble", "response", "request")
)
}
\arguments{
\item{tickers_obj}{A tickers object created with get_tickers()}

\item{freq}{Frequency of data: "annual" or "quarterly" (default "annual")}

\item{start_timestamp}{Start timestamp (default EOY 2016)}

\item{end_timestamp}{End timestamp (default current timestamp)}

\item{balance_keys}{Balance sheet keys to retrieve (default all)}

\item{pretty}{Format column names to be more readable (default TRUE)}

\item{wide}{Return data in wide format (default TRUE)}

\item{proxy}{Optional proxy settings}

\item{output}{Object to return. Can be "tibble", "response", or "request" (default "tibble")}
}
\value{
A list of tibbles with balance sheet data for each ticker
}
\description{
Get balance sheet for multiple tickers
}
\examples{
\dontrun{
tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
tech_balance <- get_tickers_balance_sheet(tech_tickers)
}
}
