#' ---
#' title: "Check Pricing Data"
#' author: "Kevin Lu"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document: 
#'     theme: default 
#'     highlight: tango
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 8 
#'     fig_height: 5 
#' ---

#' # 1. Source Functions 
source("./Mean Reversion/TMR.003 Data Wrangling Functions.R")
source("./Mean Reversion/TMR.004 Coin Selection Functions.R")
source("./Mean Reversion/TMR.005 Model Functions.R")
source("./Mean Reversion/TMR.006 Backtesting Functions.R")
source("./Mean Reversion/TMR.007 Plot Functions.R")
source("./Mean Reversion/TMR.008 Generate Predictions Functions.R")

#' # 2. Query Data 
pricing_data <- load_data(source = "csv", time_resolution = "300", start_unix = "1504224000")

#' # 3. Prepare Data 
check <- prepare_data(pricing_data, "2017-11-01", "2018-01-01", list(time_resolution = "300"))


