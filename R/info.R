#' Info.R - Module for retrieving and parsing Yahoo Finance quoteSummary data
#'
#' This file contains functions for parsing Yahoo Finance quoteSummary API data
#' into tidy tibble formats using tidyverse functions to handle nested data.

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
  
  # Module-specific parsing logic

  # Asset Profile module
  if (module_name == "assetProfile") {
    # For asset profile, handle company officers separately
    tbl <- dplyr::tibble(data = list(result_data)) |>
      tidyr::unnest_wider(data)
    
    # If company officers exist, extract them as an attribute
    if ("companyOfficers" %in% names(tbl)) {
      officers_data <- tbl$companyOfficers
      if (!is.null(officers_data) && length(officers_data) > 0) {
        officers_tibble <- dplyr::tibble(data = officers_data[[1]]) |>
          tidyr::unnest_wider(data)
        
        # Attempt to unnest any raw/fmt structures
        officer_cols <- names(officers_tibble)
        for (col in officer_cols) {
          if (is.list(officers_tibble[[col]]) && all(purrr::map_lgl(officers_tibble[[col]], is.list))) {
            officers_tibble <- officers_tibble |>
              tidyr::unnest_wider(col, names_sep = "")
          }
        }
        
        tbl$companyOfficers <- list(officers_tibble)
      }
    }
    return(tbl)
    # Balance Sheet History module
  } else if (module_name == "balanceSheetHistory" || module_name == "balanceSheetHistoryQuarterly") {
    # For balance sheet data, extract statements
    if (!is.null(result_data$balanceSheetStatements)) {
      tbl <- dplyr::tibble(data = result_data$balanceSheetStatements) |> 
             tidyr::unnest_wider("data") |> 
             process_nested_cols()
      return(tbl)
    }
   # Calendar Events Module 
  } else if (module_name == "calendarEvents") {
    # Start with the base tibble from the result data
    tbl <- dplyr::tibble(data = list(result_data)) |>
      tidyr::unnest_wider(data)
    
    # Add maxAge if it exists
    if ("maxAge" %in% names(tbl)) {
      tbl$maxAge <- tbl$maxAge[[1]]
    }
    
    # Process earnings data if it exists
    if ("earnings" %in% names(tbl) && !is.null(tbl$earnings)) {
      earnings_data <- tbl$earnings[[1]]
      
      # Process scalar fields with raw/fmt/longFmt structures
      scalar_fields <- c("earningsAverage", "earningsLow", "earningsHigh", 
                        "revenueAverage", "revenueLow", "revenueHigh")
      
      for (field in scalar_fields) {
        if (field %in% names(earnings_data)) {
          field_data <- earnings_data[[field]]
          if (is.list(field_data)) {
            # Create a temporary tibble to pass to process_nested_cols
            temp_tbl <- dplyr::tibble(data = list(field_data))
            # Process using the updated function with field as the prefix
            processed_tbl <- process_nested_cols(temp_tbl, prefix = field)
            # Merge the processed columns into the main tibble
            for (processed_col in setdiff(names(processed_tbl), "data")) {
              tbl[[processed_col]] <- processed_tbl[[processed_col]]
            }
          }
        }
      }
      
      # Process earnings dates (array of date objects)
      if ("earningsDate" %in% names(earnings_data)) {
        dates <- earnings_data$earningsDate
        if (is.list(dates) && length(dates) > 0) {
          # Process up to 2 dates (typical max in the API)
          for (i in 1:min(length(dates), 2)) {
            date_obj <- dates[[i]]
            if (is.list(date_obj)) {
              # Create a temporary tibble to pass to process_nested_cols
              temp_tbl <- dplyr::tibble(data = list(date_obj))
              # Process using the updated function with earningsDate and i as the prefix
              processed_tbl <- process_nested_cols(temp_tbl, prefix = paste0("earningsDate", i))
              # Merge the processed columns into the main tibble
              for (processed_col in setdiff(names(processed_tbl), "data")) {
                tbl[[processed_col]] <- processed_tbl[[processed_col]][[1]]
              }
            }
          }
        }
      }
      
      # Process earnings call date
      if ("earningsCallDate" %in% names(earnings_data)) {
        call_dates <- earnings_data$earningsCallDate
        if (is.list(call_dates) && length(call_dates) > 0) {
          call_date <- call_dates[[1]]
          if (is.list(call_date)) {
            # Create a temporary tibble to pass to process_nested_cols
            temp_tbl <- dplyr::tibble(data = list(call_date))
            # Process using the updated function with earningsCallDate as the prefix
            processed_tbl <- process_nested_cols(temp_tbl, prefix = "earningsCallDate")
            # Merge the processed columns into the main tibble
            for (processed_col in setdiff(names(processed_tbl), "data")) {
              tbl[[processed_col]] <- processed_tbl[[processed_col]][[1]]
            }
          }
        }
      }
      
      # Process simple fields
      if ("isEarningsDateEstimate" %in% names(earnings_data)) {
        tbl$isEarningsDateEstimate <- earnings_data$isEarningsDateEstimate[[1]]
      }
    }
    
    # Process exDividendDate
    if ("exDividendDate" %in% names(tbl) && is.list(tbl$exDividendDate)) {
      ex_div <- tbl$exDividendDate[[1]]
      if (is.list(ex_div)) {
        # Create a temporary tibble to pass to process_nested_cols
        temp_tbl <- dplyr::tibble(data = list(ex_div))
        # Process using the updated function with exDividendDate as the prefix
        processed_tbl <- process_nested_cols(temp_tbl, prefix = "exDividendDate")
        # Merge the processed columns into the main tibble
        for (processed_col in setdiff(names(processed_tbl), "data")) {
          tbl[[processed_col]] <- processed_tbl[[processed_col]][[1]]
        }
      }
      # Remove the original nested column
      tbl$exDividendDate <- NULL
    }
    
    # Process dividendDate
    if ("dividendDate" %in% names(tbl) && is.list(tbl$dividendDate)) {
      div_date <- tbl$dividendDate[[1]]
      if (is.list(div_date)) {
        # Create a temporary tibble to pass to process_nested_cols
        temp_tbl <- dplyr::tibble(data = list(div_date))
        # Process using the updated function with dividendDate as the prefix
        processed_tbl <- process_nested_cols(temp_tbl, prefix = "dividendDate")
        # Merge the processed columns into the main tibble
        for (processed_col in setdiff(names(processed_tbl), "data")) {
          tbl[[processed_col]] <- processed_tbl[[processed_col]][[1]]
        }
      }
      # Remove the original nested column
      tbl$dividendDate <- NULL
    }
    
    # Remove original earnings column since we've flattened its contents
    tbl$earnings <- NULL
    
    return(tbl)
  } else if (module_name == "cashflowStatementHistory" || module_name == "cashflowStatementHistoryQuarterly") {
    # For cash flow statements, extract statements
    if (!is.null(result_data$cashflowStatements)) {
      tbl <- dplyr::tibble(data = list(result_data$cashflowStatements)) |> 
      tidyr::unnest_longer("data") |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
      return(tbl)
    }
  } else if (module_name == "defaultKeyStatistics") {
    # For default key statistics, extract statistics
    if (!is.null(result_data)) {
      tbl <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
      return(tbl)
    }
  } else if (module_name == "earnings") {
    # For earnings module, handle the complex nested structure
    # Start with the base tibble from the result data
    base_tbl <- dplyr::tibble(data = list(result_data)) |>
      tidyr::unnest_wider("data")

    # Initiate list to hold tibbles
    result_tibbles <- list()
    
    # Process earningsChart quarterly data
    if ("earningsChart" %in% names(base_tbl) && 
        !is.null(base_tbl$earningsChart) && 
        "quarterly" %in% names(base_tbl$earningsChart[[1]])) {
      
      quarterly_data <- base_tbl$earningsChart[[1]]$quarterly
      
      if (length(quarterly_data) > 0) {
        # Create tibble from quarterly earnings data
        earnings_quarterly <- dplyr::tibble(data = list(quarterly_data)) |> 
          tidyr::unnest_longer(data) |> 
          tidyr::unnest_wider(data) |> 
          process_nested_cols()
        
        # Store in our results list
        result_tibbles$quarterly_earnings <- earnings_quarterly
      }
    }
    
    # Process current quarter estimate
    if ("earningsChart" %in% names(base_tbl) && 
        !is.null(base_tbl$earningsChart) && 
        "currentQuarterEstimate" %in% names(base_tbl$earningsChart[[1]])) {

      current_quarter <- dplyr::tibble(
        data = list(
          base_tbl$earningsChart[[1]][c("currentQuarterEstimate", "currentQuarterEstimateDate", "currentQuarterEstimateYear", "earningsDate")]
        )
        ) |> 
        tidyr::unnest_wider("data") |> 
        dplyr::rename("estimate" = "currentQuarterEstimate") |> 
        dplyr::mutate(date = paste0(.data$currentQuarterEstimateDate, .data$currentQuarterEstimateYear)) |> 
        dplyr::select(-dplyr::starts_with("currentQuarterEstimate")) |> 
        tidyr::unnest_wider("earningsDate", names_sep = "") |> 
        process_nested_cols()
      
      # Store in our results list
      result_tibbles$current_quarter_earnings <- current_quarter
    }
    
    # Process financialsChart yearly data
    if ("financialsChart" %in% names(base_tbl) && 
        !is.null(base_tbl$financialsChart) && 
        "yearly" %in% names(base_tbl$financialsChart[[1]])) {
      
      yearly_data <- base_tbl$financialsChart[[1]]$yearly
      
      if (length(yearly_data) > 0) {
        # Create tibble from yearly financials data
        financials_yearly <- dplyr::tibble(data = yearly_data) |> 
          tidyr::unnest_wider(data) |> 
          process_nested_cols()
        
        # Store in our results list
        result_tibbles$yearly_financials <- financials_yearly
      }
    }
    
    # Process financialsChart quarterly data
    if ("financialsChart" %in% names(base_tbl) && 
        !is.null(base_tbl$financialsChart) && 
        "quarterly" %in% names(base_tbl$financialsChart[[1]])) {
      
      quarterly_data <- base_tbl$financialsChart[[1]]$quarterly
      
      if (length(quarterly_data) > 0) {
        # Create tibble from quarterly financials data
        financials_quarterly <- dplyr::tibble(data = quarterly_data) |> 
          tidyr::unnest_wider(data) |> 
          process_nested_cols()
        
        # Store in our results list
        result_tibbles$quarterly_financials <- financials_quarterly
      }
    }
    
    # Extract maxAge if it exists
    if ("maxAge" %in% names(base_tbl)) {
      result_tibbles$metadata <- dplyr::tibble(maxAge = base_tbl$maxAge, financialCurrency = base_tbl$financialCurrency)
    }

    # Return the list of tibbles
    return(result_tibbles)
  } else if (module_name == "earningsHistory") {
    # Process earnings history data
    earnings_history <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("history") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("history", names_repair = "minimal") |> 
      process_nested_cols()
    return(earnings_history)
  } else if (module_name == "earningsTrend") {
    # Process earnings trend data
    earnings_trend <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data")  |> 
      tidyr::unnest_longer("trend")  |> 
      dplyr::select(-"maxAge") |> 
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
    return(earnings_trend)
  } else if (module_name == "esgScores") {
    # Process ESG scores data
    esg_scores <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(esg_scores)
  } else if (module_name == "financialData") {
    # Process financial data
    financial_data <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(financial_data)
  } else if (module_name == "fundOwnership") {
    # Process fund ownership data
    fund_ownership <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("ownershipList") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("ownershipList") |> 
      process_nested_cols()
    return(fund_ownership)
  } else if (module_name == "fundProfile") {
    # Process fund profile data
    fund_profile <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_wider("feesExpensesInvestment") |> 
      tidyr::unnest_wider("feesExpensesInvestmentCat", names_repair = "minimal") |> 
      process_nested_cols()
    return(fund_profile)
  } else if (module_name == "futuresChain") {
    # Process futures chain data
    futures_chain <- dplyr::tibble(data = list(result_data$futuresChainDetails)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("data") 
    return(futures_chain)
  } else if (module_name == "incomeStatementHistory" || module_name == "incomeStatementHistoryQuarterly") {
    # Process income statement history data
    income_statement_history <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("incomeStatementHistory") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("incomeStatementHistory") |> 
      process_nested_cols()
    return(income_statement_history)
  } else if (module_name == "indexTrend") {
    # Process index trend data
    index_trend <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("estimates") |> 
      tidyr::unnest_wider("estimates") |> 
      process_nested_cols()
    return(index_trend)
  } else if (module_name == "insiderHolders") {
    # Process insider holders data
    insider_holders <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("holders") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("holders") |> 
      process_nested_cols()
    return(insider_holders)
  } else if (module_name == "insiderTransactions") {
    # Process insider transactions data
    insider_transactions <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("transactions") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("transactions") |> 
      process_nested_cols()
    return(insider_transactions)
  } else if (module_name == "institutionOwnership") {
    # Process institution ownership data
    institution_ownership <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("ownershipList") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("ownershipList") |> 
      process_nested_cols()
    return(institution_ownership)
  } else if (module_name == "majorDirectHolders") {
    # Process major direct holders data
    major_direct_holders <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(major_direct_holders)
  } else if (module_name == "majorHoldersBreakdown") {
    # Process major holders breakdown data
    major_holders_breakdown <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(major_holders_breakdown)
  } else if (module_name == "netSharePurchaseActivity") {
    # Process net share purchase activity data
    net_share_purchase_activity <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(net_share_purchase_activity)
  } else if (module_name == "price") {
    # Process price data
    price <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(price)
  } else if (module_name == "quoteType") {
    # Process quote type data
    quote_type <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(quote_type)
  } else if (module_name == "recommendationTrend") {
    # Process recommendation trend data
    recommendation_trend <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("trend") |> 
      tidyr::unnest_wider("trend") 
    return(recommendation_trend)
  } else if (module_name == "secFilings") {
    # Process SEC filings data
    sec_filings <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("filings") |> 
      dplyr::select(-"maxAge") |> 
      tidyr::unnest_wider("filings") |> 
      tidyr::unnest_longer("exhibits") |> 
      dplyr::mutate(exhibits = purrr::map(exhibits, \(ex) {
        new_names <- capitalize(names(ex))
        names(ex) <- new_names
        ex
      })) |> 
      tidyr::unnest_wider(exhibits, names_sep = "")
    return(sec_filings)
  } else if (module_name == "sectorTrend") {
    # Process sector trend data
    sector_trend <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(sector_trend)
  } else if (module_name == "summaryDetail") {
    # Process summary detail data
    summary_detail <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(summary_detail)
  } else if (module_name == "summaryProfile") {
    # Process summary profile data
    summary_profile <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      process_nested_cols()
    return(summary_profile)
  } else if (module_name == "upgradeDowngradeHistory") {
    # Process upgrade/downgrade history data
    upgrade_downgrade_history <- dplyr::tibble(data = list(result_data)) |> 
      tidyr::unnest_wider("data") |> 
      tidyr::unnest_longer("history") |> 
      tidyr::unnest_wider("history") 
    return(upgrade_downgrade_history)
  }
  # Return empty tibble if no valid data found
  return(dplyr::tibble())
}

#' Parse nested column structures in a tibble
#'
#' @param tibble A tibble potentially containing nested column structures like format/raw/longFmt pairs
#'               or min/avg/max values
#' @param prefix Optional prefix for the created columns. If NULL, uses the column name followed by 
#'               the appropriate suffix (Raw, Fmt, LongFmt, Min, Avg, Max)
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
      is_nested <- FALSE
      if (length(tibble[[col]]) > 0) {
        first_non_null <- purrr::detect(tibble[[col]], function(x) !is.null(x))
        if (!is.null(first_non_null) && is.list(first_non_null) && 
            (any(c("raw", "fmt", "longFmt") %in% names(first_non_null)) ||
             any(c("min", "avg", "max") %in% names(first_non_null)))) {
          is_nested <- TRUE
        }
      }
      
      if (is_nested) {
        # Determine the column name prefix
        col_prefix <- if (is.null(prefix)) col else prefix
        
        # Extract raw values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "raw" %in% names(x)))) {
          raw_values <- purrr::map_dbl(tibble[[col]], ~if(is.null(.x$raw) || !"raw" %in% names(.x)) NA_real_ else .x$raw)
          tibble[[paste0(col_prefix, "Raw")]] <- raw_values
        }
        
        # Extract fmt values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "fmt" %in% names(x)))) {
          fmt_values <- purrr::map_chr(tibble[[col]], ~if(is.null(.x$fmt) || !"fmt" %in% names(.x)) NA_character_ else .x$fmt)
          tibble[[paste0(col_prefix, "Fmt")]] <- fmt_values
        }
        
        # Extract longFmt values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "longFmt" %in% names(x)))) {
          longfmt_values <- purrr::map_chr(tibble[[col]], ~if(is.null(.x$longFmt) || !"longFmt" %in% names(.x)) NA_character_ else .x$longFmt)
          tibble[[paste0(col_prefix, "LongFmt")]] <- longfmt_values
        }
        
        # Extract min values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "min" %in% names(x)))) {
          min_values <- purrr::map_dbl(tibble[[col]], ~if(is.null(.x$min) || !"min" %in% names(.x)) NA_real_ else .x$min)
          tibble[[paste0(col_prefix, "Min")]] <- min_values
        }
        
        # Extract avg values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "avg" %in% names(x)))) {
          avg_values <- purrr::map_dbl(tibble[[col]], ~if(is.null(.x$avg) || !"avg" %in% names(.x)) NA_real_ else .x$avg)
          tibble[[paste0(col_prefix, "Avg")]] <- avg_values
        }
        
        # Extract max values
        if (any(purrr::map_lgl(tibble[[col]], function(x) !is.null(x) && "max" %in% names(x)))) {
          max_values <- purrr::map_dbl(tibble[[col]], ~if(is.null(.x$max) || !"max" %in% names(.x)) NA_real_ else .x$max)
          tibble[[paste0(col_prefix, "Max")]] <- max_values
        }
        
        # Remove original column
        tibble[[col]] <- NULL
      }
    }
  }
  
  return(tibble)
}

#' Convert timestamps to dates in a tibble
#'
#' @param tibble A tibble potentially containing timestamp columns
#' @return A tibble with timestamps converted to Date objects
#' @keywords internal
convert_timestamps <- function(tibble) {
  if (nrow(tibble) == 0 || ncol(tibble) == 0) {
    return(tibble)
  }
  
  # Identify potential date columns
  date_pattern <- "(date|Date|quarter|endDate|startDate|epochDate)"
  date_cols <- grep(date_pattern, names(tibble), value = TRUE)
  
  # Convert numeric columns matching the pattern to dates
  for (col in date_cols) {
    if (is.numeric(tibble[[col]]) || (is.list(tibble[[col]]) && all(purrr::map_lgl(tibble[[col]], is.numeric)))) {
      # Convert list of numbers to vector if needed
      if (is.list(tibble[[col]])) {
        numeric_values <- purrr::map_dbl(tibble[[col]], ~if(is.null(.x)) NA_real_ else as.numeric(.x))
        tibble[[col]] <- numeric_values
      }
      
      # Convert timestamp to date
      tibble[[col]] <- as.Date(as.POSIXct(tibble[[col]], origin = "1970-01-01"))
    }
  }
  
  return(tibble)
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
#'                              modules = c("incomeStatementHistory", "balanceSheetHistory"))
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
