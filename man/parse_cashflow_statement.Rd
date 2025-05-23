% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{parse_cashflow_statement}
\alias{parse_cashflow_statement}
\title{Parse cash flow statement module data from Yahoo Finance API}
\usage{
parse_cashflow_statement(result_data)
}
\arguments{
\item{result_data}{The raw cash flow statement data from Yahoo Finance API}
}
\value{
A tibble containing the parsed cash flow data with financial metrics
}
\description{
Transforms the nested cash flow statement data from Yahoo Finance API into a tidy tibble.
This function handles both annual and quarterly cash flow statements.
}
\keyword{internal}
