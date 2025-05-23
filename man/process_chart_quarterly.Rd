% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{process_chart_quarterly}
\alias{process_chart_quarterly}
\title{Process quarterly data from earnings or financials chart}
\usage{
process_chart_quarterly(
  chart_data,
  field_name,
  result_name,
  result_tibbles = list()
)
}
\arguments{
\item{chart_data}{The chart data containing quarterly information}

\item{field_name}{The field name to extract from the chart data}

\item{result_name}{The name to use in the result tibbles list}

\item{result_tibbles}{The list of result tibbles to update}
}
\value{
Updated result_tibbles list with processed data
}
\description{
Process quarterly data from earnings or financials chart
}
\keyword{internal}
