# Info.R - Retrieve and parsing Yahoo Finance quoteSummary data
#
# This file contains functions for parsing Yahoo Finance quoteSummary API data
# into tidy tibble formats using tidyverse functions to handle nested data.
# The main parsing function uses a dispatch table to route each module type
# to its appropriate parsing function, greatly reducing code complexity.

#' Parse asset profile module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_asset_profile <- function(result_data) {
  tbl <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data")

  # If company officers exist, extract them as an attribute
  if ("companyOfficers" %in% names(tbl)) {
    officers_data <- tbl$companyOfficers
    if (!is.null(officers_data) && length(officers_data) > 0) {
      officers_tibble <- dplyr::tibble(data = officers_data[[1]]) |>
        tidyr::unnest_wider("data")

      # Attempt to unnest any raw/fmt structures
      officer_cols <- names(officers_tibble)
      for (col in officer_cols) {
        if (is.list(officers_tibble[[col]]) && all(purrr::map_lgl(officers_tibble[[col]], is.list))) {
          officers_tibble <- officers_tibble |>
            tidyr::unnest_wider(dplyr::all_of(col), names_sep = "")
        }
      }

      tbl$companyOfficers <- list(officers_tibble)
    }
  }
  tbl
}

#' Parse balance sheet module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_balance_sheet <- function(result_data) {
  if (!is.null(result_data$balanceSheetStatements)) {
    tbl <- dplyr::tibble(data = result_data$balanceSheetStatements) |>
      tidyr::unnest_wider("data") |>
      process_nested_cols()
  } else {
    tbl <- dplyr::tibble()
  }
  tbl
}

#' Parse cash flow statement module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_cashflow_statement <- function(result_data) {
  if (!is.null(result_data$cashflowStatements)) {
    tbl <- dplyr::tibble(data = list(result_data$cashflowStatements)) |>
      tidyr::unnest_longer("data") |>
      tidyr::unnest_wider("data") |>
      process_nested_cols()
  } else {
    tbl <- dplyr::tibble()
  }
  tbl
}

#' Parse default key statistics module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_default_key_statistics <- function(result_data) {
  if (!is.null(result_data)) {
    tbl <- dplyr::tibble(data = list(result_data)) |>
      tidyr::unnest_wider("data") |>
      process_nested_cols()
  } else {
    tbl <- dplyr::tibble()
  }
  tbl
}

#' Process a nested field and add it to the tibble
#'
#' @param tbl The tibble to update
#' @param field_data The nested field data
#' @param prefix The prefix to use for column names
#' @param is_array Whether the field is an array (TRUE) or single value (FALSE)
#' @param index Optional index for array elements
#' @return Updated tibble with processed field data
#' @keywords internal
process_calendar_field <- function(tbl, field_data, prefix, is_array = FALSE,
                                   index = NULL) {
  if (!is.list(field_data)) {
    return(tbl)
  }

  # Create full prefix with index if provided
  full_prefix <- if (!is.null(index)) paste0(prefix, index) else prefix

  # Create a temporary tibble to pass to process_nested_cols
  temp_tbl <- dplyr::tibble(data = list(field_data))

  # Process using the prefix
  processed_tbl <- process_nested_cols(temp_tbl, prefix = full_prefix)

  # Merge the processed columns into the main tibble
  for (processed_col in setdiff(names(processed_tbl), "data")) {
    if (is_array) {
      tbl[[processed_col]] <- processed_tbl[[processed_col]][[1]]
    } else {
      tbl[[processed_col]] <- processed_tbl[[processed_col]]
    }
  }

  tbl
}

#' Process earnings data in calendar events
#'
#' @param tbl The tibble to update
#' @param earnings_data The earnings data to process
#' @return Updated tibble with processed earnings data
#' @keywords internal
process_calendar_earnings <- function(tbl, earnings_data) {
  # Process scalar fields with raw/fmt/longFmt structures
  scalar_fields <- c(
    "earningsAverage", "earningsLow", "earningsHigh",
    "revenueAverage", "revenueLow", "revenueHigh"
  )

  # Process each scalar field
  for (field in scalar_fields) {
    if (field %in% names(earnings_data)) {
      tbl <- process_calendar_field(tbl, earnings_data[[field]], field)
    }
  }

  # Process earnings dates (array of date objects)
  if ("earningsDate" %in% names(earnings_data)) {
    dates <- earnings_data$earningsDate
    # Process up to 2 dates (typical max in the API)
    for (i in seq_len(min(length(dates), 2))) {
      tbl <- process_calendar_field(tbl, dates[[i]], "earningsDate",
        is_array = TRUE, index = i
      )
    }
  }

  # Process earnings call date
  if ("earningsCallDate" %in% names(earnings_data)) {
    call_dates <- earnings_data$earningsCallDate
    tbl <- process_calendar_field(tbl, call_dates[[1]], "earningsCallDate",
      is_array = TRUE
    )
  }

  # Process simple fields
  if ("isEarningsDateEstimate" %in% names(earnings_data)) {
    tbl$isEarningsDateEstimate <- earnings_data$isEarningsDateEstimate[[1]]
  }

  tbl
}

#' Parse calendar events module data
#'
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_calendar_events <- function(result_data) {
  # Start with the base tibble from the result data
  tbl <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data")

  # Add maxAge if it exists
  if ("maxAge" %in% names(tbl)) {
    tbl$maxAge <- tbl$maxAge[[1]]
  }

  # Process earnings data if it exists
  if ("earnings" %in% names(tbl) && !is.null(tbl$earnings)) {
    tbl <- process_calendar_earnings(tbl, tbl$earnings[[1]])
  }

  # Process exDividendDate
  if ("exDividendDate" %in% names(tbl) && is.list(tbl$exDividendDate)) {
    tbl <- process_calendar_field(tbl, tbl$exDividendDate[[1]],
      "exDividendDate",
      is_array = TRUE
    )
    # Remove the original nested column
    tbl$exDividendDate <- NULL
  }

  # Process dividendDate
  if ("dividendDate" %in% names(tbl) && is.list(tbl$dividendDate)) {
    tbl <- process_calendar_field(tbl, tbl$dividendDate[[1]],
      "dividendDate",
      is_array = TRUE
    )
    # Remove the original nested column
    tbl$dividendDate <- NULL
  }

  # Remove original earnings column since we've flattened its contents
  tbl$earnings <- NULL

  tbl
}

#' Process quarterly data from earnings or financials chart
#'
#' @param chart_data The chart data containing quarterly information
#' @param field_name The field name to extract from the chart data
#' @param result_name The name to use in the result tibbles list
#' @param result_tibbles The list of result tibbles to update
#' @return Updated result_tibbles list with processed data
#' @keywords internal
process_chart_quarterly <- function(chart_data, field_name, result_name,
                                    result_tibbles = list()) {
  if (!is.null(chart_data) && field_name %in% names(chart_data)) {
    quarterly_data <- chart_data[[field_name]]

    if (length(quarterly_data) > 0) {
      # Create tibble from quarterly data
      quarterly_tbl <- dplyr::tibble(data = list(quarterly_data))

      # Apply appropriate unnesting based on data structure
      quarterly_tbl <- quarterly_tbl |>
        tidyr::unnest_longer("data") |>
        tidyr::unnest_wider("data")

      # Process nested columns and store in results list
      result_tibbles[[result_name]] <- quarterly_tbl |> process_nested_cols()
    }
  }

  result_tibbles
}

#' Process current quarter estimate data
#'
#' @param earnings_chart The earnings chart data
#' @param result_tibbles The list of result tibbles to update
#' @return Updated result_tibbles list with processed data
#' @keywords internal
process_current_quarter <- function(earnings_chart, result_tibbles = list()) {
  if (!is.null(earnings_chart) && "currentQuarterEstimate" %in% names(earnings_chart)) {
    # Extract relevant fields for current quarter
    fields <- c(
      "currentQuarterEstimate", "currentQuarterEstimateDate",
      "currentQuarterEstimateYear", "earningsDate"
    )

    # Create and process tibble
    current_quarter <- dplyr::tibble(data = list(earnings_chart[fields])) |>
      tidyr::unnest_wider("data") |>
      dplyr::rename("estimate" = "currentQuarterEstimate") |>
      dplyr::mutate(date = paste0(
        .data$currentQuarterEstimateDate,
        .data$currentQuarterEstimateYear
      )) |>
      dplyr::select(-dplyr::starts_with("currentQuarterEstimate")) |>
      tidyr::unnest_wider("earningsDate", names_sep = "") |>
      process_nested_cols()

    # Store in results list
    result_tibbles$current_quarter_earnings <- current_quarter
  }

  result_tibbles
}

#' Process metadata from earnings data
#'
#' @param base_tbl The base tibble containing metadata
#' @param result_tibbles The list of result tibbles to update
#' @return Updated result_tibbles list with metadata
#' @keywords internal
process_earnings_metadata <- function(base_tbl, result_tibbles = list()) {
  if ("maxAge" %in% names(base_tbl)) {
    result_tibbles$metadata <- dplyr::tibble(
      maxAge = base_tbl$maxAge,
      financialCurrency = base_tbl$financialCurrency
    )
  }

  result_tibbles
}

#' Parse earnings module data
#'
#' @param result_data The data to parse
#' @return A list of tibbles with parsed data
#' @keywords internal
parse_earnings <- function(result_data) {
  # Start with the base tibble from the result data
  base_tbl <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data")

  # Initiate list to hold tibbles
  result_tibbles <- list()

  # Process earnings chart data if it exists
  if ("earningsChart" %in% names(base_tbl) && !is.null(base_tbl$earningsChart)) {
    earnings_chart <- base_tbl$earningsChart[[1]]

    # Process quarterly earnings data
    result_tibbles <- process_chart_quarterly(
      earnings_chart, "quarterly", "quarterly_earnings", result_tibbles
    )

    # Process current quarter estimate
    result_tibbles <- process_current_quarter(earnings_chart, result_tibbles)
  }

  # Process financials chart data if it exists
  if ("financialsChart" %in% names(base_tbl) && !is.null(base_tbl$financialsChart)) {
    financials_chart <- base_tbl$financialsChart[[1]]

    # Process yearly financials data
    result_tibbles <- process_chart_quarterly(
      financials_chart, "yearly", "yearly_financials", result_tibbles
    )

    # Process quarterly financials data
    result_tibbles <- process_chart_quarterly(
      financials_chart, "quarterly", "quarterly_financials", result_tibbles
    )
  }

  # Extract metadata
  result_tibbles <- process_earnings_metadata(base_tbl, result_tibbles)

  # Return the list of tibbles
  result_tibbles
}

#' Parse earnings history module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_earnings_history <- function(result_data) {
  earnings_history <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("history") |>
    dplyr::select(-dplyr::any_of("maxAge")) |>
    tidyr::unnest_wider("history", names_repair = "minimal") |>
    process_nested_cols()
  earnings_history
}

#' Parse earnings trend module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_earnings_trend <- function(result_data) {
  earnings_trend <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("trend") |>
    dplyr::select(-dplyr::any_of("maxAge")) |>
    tidyr::unnest_wider("trend") |>
    dplyr::mutate(
      earningsEstimate = purrr::map(.data$earningsEstimate, function(est) {
        new_names <- capitalize(names(est))
        new_names <- gsub("earnings|Earnings", "", new_names)
        stats::setNames(est, new_names)
      }),
      revenueEstimate = purrr::map(.data$revenueEstimate, function(est) {
        new_names <- capitalize(names(est))
        new_names <- gsub("earnings|Earnings", "", new_names)
        stats::setNames(est, new_names)
      }),
      epsTrend = purrr::map(.data$epsTrend, function(est) {
        new_names <- capitalize(names(est))
        new_names <- gsub("epsTrend|EpsTrend", "", new_names)
        stats::setNames(est, new_names)
      }),
      epsRevisions = purrr::map(.data$epsRevisions, function(est) {
        new_names <- capitalize(names(est))
        new_names <- gsub("epsRevisions|EpsRevisions", "", new_names)
        stats::setNames(est, new_names)
      })
    ) |>
    tidyr::unnest_wider("earningsEstimate", names_sep = "") |>
    tidyr::unnest_wider("revenueEstimate", names_sep = "") |>
    tidyr::unnest_wider("epsTrend", names_sep = "") |>
    tidyr::unnest_wider("epsRevisions", names_sep = "") |>
    process_nested_cols()
  earnings_trend
}

#' Parse ESG scores module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_esg_scores <- function(result_data) {
  esg_scores <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    process_nested_cols()
  esg_scores
}

#' Parse fund profile module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_fund_profile <- function(result_data) {
  fund_profile <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_wider("feesExpensesInvestment") |>
    tidyr::unnest_wider("feesExpensesInvestmentCat", names_repair = "minimal") |>
    process_nested_cols()
  fund_profile
}

#' Parse futures chain module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_futures_chain <- function(result_data) {
  futures_chain <- dplyr::tibble(data = list(result_data$futuresChainDetails)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("data")
  futures_chain
}

#' Parse index trend module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_index_trend <- function(result_data) {
  index_trend <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("estimates") |>
    tidyr::unnest_wider("estimates") |>
    process_nested_cols()
  index_trend
}

#' Parse insider holders module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_insider_holders <- function(result_data) {
  parse_ownership_data(result_data, "holders")
}

#' Parse insider transactions module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_insider_transactions <- function(result_data) {
  parse_ownership_data(result_data, "transactions")
}

#' Parse institution ownership module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_institution_ownership <- function(result_data) {
  parse_ownership_data(result_data, "ownershipList")
}

#' Parse fund ownership module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_fund_ownership <- function(result_data) {
  parse_ownership_data(result_data, "ownershipList")
}

#' Parse recommendation trend module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_recommendation_trend <- function(result_data) {
  recommendation_trend <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("trend") |>
    tidyr::unnest_wider("trend")
  recommendation_trend
}

#' Parse upgrade downgrade history module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_up_down_history <- function(result_data) {
  up_down_history <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("history") |>
    tidyr::unnest_wider("history")
  up_down_history
}

#' Parse income statement module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_income_statement <- function(result_data) {
  income_statement_history <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("incomeStatementHistory") |>
    dplyr::select(-dplyr::any_of("maxAge")) |>
    tidyr::unnest_wider("incomeStatementHistory") |>
    process_nested_cols()
  income_statement_history
}

#' Parse ownership data module
#' @param result_data The data to parse
#' @param list_field The field containing the list of ownership data
#' @return A tibble with parsed data
#' @keywords internal
parse_ownership_data <- function(result_data, list_field = "ownershipList") {
  ownership_data <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer(list_field) |>
    dplyr::select(-dplyr::any_of("maxAge")) |>
    tidyr::unnest_wider(list_field) |>
    process_nested_cols()
  ownership_data
}

#' Parse SEC filings module data
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_sec_filings <- function(result_data) {
  sec_filings <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_longer("filings") |>
    dplyr::select(-dplyr::any_of("maxAge")) |>
    tidyr::unnest_wider("filings") |>
    tidyr::unnest_longer("exhibits") |>
    dplyr::mutate(exhibits = purrr::map(.data$exhibits, \(ex) {
      new_names <- capitalize(names(ex))
      names(ex) <- new_names
      ex
    })) |>
    tidyr::unnest_wider("exhibits", names_sep = "")
  sec_filings
}

#' Parse generic module data that requires simple unnesting
#' @param result_data The data to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_generic_module <- function(result_data) {
  if (is.null(result_data)) {
    return(dplyr::tibble())
  }

  tbl <- dplyr::tibble(data = list(result_data)) |>
    tidyr::unnest_wider("data") |>
    process_nested_cols()

  tbl
}

#' Parse a module's data into a tidy tibble format
#'
#' @param result_data The result data from Yahoo Finance API
#' @param module_name The name of the module to parse
#' @return A tibble with parsed data
#' @keywords internal
parse_module_data <- function(result_data, module_name) {
  if (is.null(result_data) || length(result_data) == 0) {
    return(dplyr::tibble())
  }

  # Module handler dispatch table
  module_handlers <- list(
    "assetProfile" = parse_asset_profile,
    "balanceSheetHistory" = parse_balance_sheet,
    "balanceSheetHistoryQuarterly" = parse_balance_sheet,
    "calendarEvents" = parse_calendar_events,
    "cashflowStatementHistory" = parse_cashflow_statement,
    "cashflowStatementHistoryQuarterly" = parse_cashflow_statement,
    "defaultKeyStatistics" = parse_default_key_statistics,
    "earnings" = parse_earnings,
    "earningsHistory" = parse_earnings_history,
    "earningsTrend" = parse_earnings_trend,
    "esgScores" = parse_esg_scores,
    "financialData" = parse_generic_module,
    "fundOwnership" = parse_fund_ownership,
    "fundProfile" = parse_fund_profile,
    "futuresChain" = parse_futures_chain,
    "incomeStatementHistory" = parse_income_statement,
    "incomeStatementHistoryQuarterly" = parse_income_statement,
    "indexTrend" = parse_index_trend,
    "insiderHolders" = parse_insider_holders,
    "insiderTransactions" = parse_insider_transactions,
    "institutionOwnership" = parse_institution_ownership,
    "majorDirectHolders" = parse_generic_module,
    "majorHoldersBreakdown" = parse_generic_module,
    "netSharePurchaseActivity" = parse_generic_module,
    "price" = parse_generic_module,
    "quoteType" = parse_generic_module,
    "recommendationTrend" = parse_recommendation_trend,
    "secFilings" = parse_sec_filings,
    "sectorTrend" = parse_generic_module,
    "summaryDetail" = parse_generic_module,
    "summaryProfile" = parse_generic_module,
    "upgradeDowngradeHistory" = parse_up_down_history
  )

  # Get the appropriate handler function or use the default generic handler
  handler_fn <- module_handlers[[module_name]]
  if (is.null(handler_fn)) {
    # Default to generic module parser if no specific handler exists
    return(parse_generic_module(result_data))
  }

  # Call the handler with the result data
  handler_fn(result_data)
}

#' Extract a specific field from nested structures
#'
#' @param col_data List column data
#' @param field_name Field name to extract
#' @param is_numeric Whether the field is numeric
#' @return Vector of extracted values
#' @keywords internal
extract_nested_field <- function(col_data, field_name,
                                 is_numeric = TRUE) {
  if (is_numeric) {
    # Extract numeric values
    values <- purrr::map_dbl(
      col_data,
      ~ if (is.null(.x[[field_name]]) || !field_name %in% names(.x)) {
        NA_real_
      } else {
        .x[[field_name]]
      }
    )
  } else {
    # Extract character values
    values <- purrr::map_chr(
      col_data,
      ~ if (is.null(.x[[field_name]]) || !field_name %in% names(.x)) {
        NA_character_
      } else {
        .x[[field_name]]
      }
    )
  }
  values
}

#' Check if a column contains nested structures with specific fields
#'
#' @param col_data List column data
#' @return Boolean indicating if the column contains nested structures
#' @keywords internal
has_nested_structure <- function(col_data) {
  if (length(col_data) == 0) {
    return(FALSE)
  }

  first_non_null <- purrr::detect(col_data, function(x) !is.null(x))

  if (is.null(first_non_null) || !is.list(first_non_null)) {
    return(FALSE)
  }

  # Check if it has any of the expected nested field patterns
  has_fmt_pattern <- any(c("raw", "fmt", "longFmt") %in% names(first_non_null))
  has_stat_pattern <- any(c("min", "avg", "max") %in% names(first_non_null))

  has_fmt_pattern || has_stat_pattern
}

#' Process a single nested column
#'
#' @param tibble The tibble containing the column
#' @param col The column name
#' @param col_prefix Prefix to use for new columns
#' @return Updated tibble with processed column
#' @keywords internal
process_nested_column <- function(tibble, col, col_prefix) {
  # Extract raw values if they exist
  # Check for raw values
  has_raw <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "raw" %in% names(x)
  ))
  if (has_raw) {
    tibble[[paste0(col_prefix, "Raw")]] <- extract_nested_field(tibble[[col]], "raw")
  }

  # Extract fmt values if they exist
  # Check for fmt values
  has_fmt <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "fmt" %in% names(x)
  ))
  if (has_fmt) {
    tibble[[paste0(col_prefix, "Fmt")]] <- extract_nested_field(tibble[[col]], "fmt", FALSE)
  }

  # Extract longFmt values if they exist
  # Check for longFmt values
  has_longfmt <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "longFmt" %in% names(x)
  ))
  if (has_longfmt) {
    tibble[[paste0(col_prefix, "LongFmt")]] <- extract_nested_field(tibble[[col]], "longFmt", FALSE)
  }

  # Extract min values if they exist
  # Check for min values
  has_min <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "min" %in% names(x)
  ))
  if (has_min) {
    tibble[[paste0(col_prefix, "Min")]] <- extract_nested_field(tibble[[col]], "min")
  }

  # Extract avg values if they exist
  # Check for avg values
  has_avg <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "avg" %in% names(x)
  ))
  if (has_avg) {
    tibble[[paste0(col_prefix, "Avg")]] <- extract_nested_field(tibble[[col]], "avg")
  }
  # Extract max values if they exist
  # Check for max values
  has_max <- any(purrr::map_lgl(
    tibble[[col]],
    function(x) !is.null(x) && "max" %in% names(x)
  ))
  if (has_max) {
    tibble[[paste0(col_prefix, "Max")]] <- extract_nested_field(tibble[[col]], "max")
  }
  # Remove original column
  tibble[[col]] <- NULL
  tibble
}

#' Process nested columns in a tibble
#'
#' @param tibble A tibble potentially containing nested column structures like
#'               format/raw/longFmt pairs or min/avg/max values
#' @param prefix Optional prefix for the created columns. If NULL, uses the column name
#'               followed by the appropriate suffix (Raw, Fmt, LongFmt, Min, Avg, Max)
#' @return A processed tibble with nested structures appropriately unnested
#' @keywords internal
process_nested_cols <- function(tibble, prefix = NULL) {
  if (nrow(tibble) == 0 || ncol(tibble) == 0) {
    return(tibble)
  }

  # Check each column
  for (col in names(tibble)) {
    if (is.list(tibble[[col]])) {
      # Check if this column contains nested structures
      if (has_nested_structure(tibble[[col]])) {
        # Determine the column name prefix
        col_prefix <- if (is.null(prefix)) col else prefix
        # Process the nested column
        tibble <- process_nested_column(tibble, col, col_prefix)
      }
    }
  }

  tibble
}

#' Get basic information about a ticker
#'
#' @param ticker A ticker name or ticker object created with get_tickers).
#' @param modules A character vector of modules to request from the API
#' @param output The output format for the request (tibble, list, response, request)
#' @param proxy A character string specifying the proxy URL
#' @return A tibble, list, response, or request object containing the requested information.
#'   If multiple modules are requested, returns a list of tibbles. Some individual modules
#'   may return a list of tibbles even when output is set to "tibble" because the underlying
#'   data contains multiple datasets.
#'
#' @examples
#' \dontrun{
#' # Get a single module
#' apple <- get_tickers("AAPL")
#' apple_profile <- get_info(apple, modules = "assetProfile")
#'
#' # Get multiple modules as a list of tibbles
#' apple_financials <- get_info(apple,
#'   modules = c("incomeStatementHistory", "balanceSheetHistory")
#' )
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

  # Extract the data
  result <- resp_json$quoteSummary$result

  if (output == "list") {
    return(result)
  }

  # Process each module and create a list of tibbles
  module_data_list <- list()

  # Process the result data
  if (length(result) > 0) {
    # Process modules in the first result
    all_modules <- result[[1]]

    # Extract and parse each module
    for (module_name in names(all_modules)) {
      module_data <- all_modules[[module_name]]
      parsed_data <- parse_module_data(module_data, module_name)
      module_data_list[[module_name]] <- parsed_data
    }
  }

  # If no modules were processed successfully, return an empty tibble
  if (length(module_data_list) == 0) {
    return(tibble::tibble())
  }

  # Return based on user preference and number of modules
  if (length(module_data_list) > 1) {
    return(module_data_list)
  } else {
    # Return a single tibble for a single module
    return(module_data_list[[1]])
  }
}
