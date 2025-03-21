% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tickers.R
\name{get_tickers_history}
\alias{get_tickers_history}
\title{Get historical data for multiple tickers}
\usage{
get_tickers_history(
  tickers_obj,
  period = "1mo",
  interval = "1d",
  start_date = NULL,
  end_date = NULL,
  prepost = FALSE,
  auto_adjust = TRUE,
  back_adjust = TRUE,
  repair = TRUE,
  output = c("tibble", "response", "request"),
  proxy = NULL
)
}
\arguments{
\item{tickers_obj}{A tickers object created with get_tickers()}

\item{period}{The period to download data for (default "1mo")}

\item{interval}{The interval between data points (default "1d")}

\item{start_date}{Start date for custom date range (format: "YYYY-MM-DD")}

\item{end_date}{End date for custom date range (format: "YYYY-MM-DD")}

\item{prepost}{Include pre and post market data (default FALSE)}

\item{auto_adjust}{Adjust all OHLC automatically (default TRUE)}

\item{back_adjust}{Adjust data to reflect splits and dividends (default TRUE)}

\item{repair}{Repair missing data (default TRUE)}

\item{output}{Object to return. Can be "tibble", "response", or "request" (default "tibble")}

\item{proxy}{Optional proxy settings}
}
\value{
A list of tibbles with historical market data for each ticker
}
\description{
Get historical data for multiple tickers
}
\examples{
\dontrun{
tech_tickers <- get_tickers(c("AAPL", "MSFT", "GOOG"))
tech_history <- get_tickers_history(tech_tickers, period = "1y")
}
}
