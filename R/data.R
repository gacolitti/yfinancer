#' Data file containing possible balance sheet keys from Yahoo Finance API
#'
#' A dataset containing the possible keys that can be found in balance sheet data
#' retrieved from Yahoo Finance's API. These keys are used to identify and extract
#' specific balance sheet line items.
#'
#' @format A character vector containing 147 possible balance sheet keys
#'
#' @examples
#' # View all possible balance sheet keys
#' data(valid_balance_keys)
#' head(valid_balance_keys)
#'
#' # Check if a specific key exists
#' "TotalAssets" %in% valid_balance_keys
#'
#' @source Yahoo Finance API
"valid_balance_keys"

#' Data file containing possible cash flow statement keys from Yahoo Finance API
#'
#' A dataset containing the possible keys that can be found in cash flow statement data
#' retrieved from Yahoo Finance's API. These keys are used to identify and extract
#' specific cash flow line items.
#'
#' @format A character vector containing 131 possible cash flow statement keys
#'
#' @examples
#' # View all possible cash flow keys
#' data(valid_cashflow_keys)
#' head(valid_cashflow_keys)
#'
#' # Check if a specific key exists
#' "OperatingCashFlow" %in% valid_cashflow_keys
#'
#' @source Yahoo Finance API
"valid_cashflow_keys"

#' Data file containing possible income statement keys from Yahoo Finance API
#'
#' A dataset containing the possible keys that can be found in income statement data
#' retrieved from Yahoo Finance's API. These keys are used to identify and extract
#' specific income statement line items.
#'
#' @format A character vector containing 103 possible income statement keys
#'
#' @examples
#' # View all possible income statement keys
#' data(valid_income_keys)
#' head(valid_income_keys)
#'
#' # Check if a specific key exists
#' "NetIncome" %in% valid_income_keys
#'
#' @source Yahoo Finance API
"valid_income_keys"

#' Data file containing possible Yahoo Finance API modules
#'
#' A dataset containing the possible modules that can be requested from the Yahoo Finance API.
#' Each module represents a specific type of financial data that can be retrieved for a ticker.
#'
#' @format A character vector containing 33 possible API modules with descriptions
#'
#' @details
#' The modules include:
#' \itemize{
#'   \item assetProfile - Summary profile and company officers
#'   \item balanceSheetHistory - Annual balance sheet data
#'   \item balanceSheetHistoryQuarterly - Quarterly balance sheet data
#'   \item calendarEvents - Future earnings dates
#'   \item cashFlowStatementHistory - Annual cash flow statement data
#'   \item cashFlowStatementHistoryQuarterly - Quarterly cash flow statement data
#'   \item defaultKeyStatistics - Key performance indicators (PE, enterprise value, EPS, etc.)
#'   \item earnings - Earnings history
#'   \item earningsHistory - Historical earnings data
#'   \item earningsTrend - Earnings trend data
#'   \item esgScores - Environmental, social, and governance scores
#'   \item financialData - Financial KPIs (revenue, margins, cash flow, etc.)
#'   \item institutionOwnership - Institutional ownership data
#'   \item insiderHolders - Insider holdings data
#'   \item insiderTransactions - Insider transaction data
#'   \item and more...
#' }
#'
#' @examples
#' # View all possible modules
#' data(valid_modules)
#' head(valid_modules)
#'
#' # Check if a specific module exists
#' "financialData" %in% valid_modules
#'
#' @source Yahoo Finance API
"valid_modules"
