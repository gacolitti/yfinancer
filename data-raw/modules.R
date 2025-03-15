valid_modules <- c(
  "assetProfile", # summaryProfile + company officers
  "balanceSheetHistory",
  "balanceSheetHistoryQuarterly",
  "calendarEvents", # future earnings date
  "cashFlowStatementHistory",
  "cashFlowStatementHistoryQuarterly",
  "defaultKeyStatistics", # KPIs (PE, enterprise value, EPS, EBITA, and more)
  "earnings", # earnings history
  "earningsHistory",
  "earningsTrend", # earnings trend
  "esgScores", # Environmental, social, and governance (ESG) scores, sustainability and ethical performance of companies
  "financialData", # Financial KPIs (revenue, gross margins, operating cash flow, free cash flow, and more)
  "fundOwnership", # mutual fund ownership, holders and shares outstanding
  "fundProfile",
  "futuresChain",
  "incomeStatementHistory",
  "incomeStatementHistoryQuarterly",
  "indexTrend",
  "insiderHolders", # insider holders, such as the number of shares held by company executives
  "insiderTransactions", # insider transactions, such as the number of shares bought and sold by company executives
  "institutionOwnership", # institutional ownership, holders and shares outstanding
  "majorDirectHolders",
  "majorHoldersBreakdown",
  "netSharePurchaseActivity", # net share purchase activity, such as the number of shares bought and sold by company executives
  "price", # current prices
  "quoteType", # quoteType
  "recommendationTrend",
  "secFilings", # SEC filings, such as 10K and 10Q reports
  "sectorTrend",
  "summaryDetail", # prices + volume + market cap + etc
  "summaryProfile", # contains general information about the company
  "upgradeDowngradeHistory" # upgrades and downgrades that analysts have given a company's stock
)

usethis::use_data(valid_modules, overwrite = TRUE)
