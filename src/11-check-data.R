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

#' # 1. Source Pairs Trading Functions 
source("./src/01-load-packages.R")
source("./src/03-set-parameters.R")
source("./src/04-data-functions.R")
source("./src/05-coin-selection-functions.R")
source("./src/06-setup-strategy-functions.R")
source("./src/07-model-functions.R")
source("./src/08-backtesting-functions.R")
source("./src/09-plot-functions.R")
source("./src/10-generate-predictions-functions.R")

#' # 2. Check Data 
#' Description  
#' Checks the integrity of the pricing data.  
#' 
#' Arguments  
#' source: A string indicating whether to use a csv or mongodb as the source. Takes value "csv" and "mongodb".  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' start_unix: The start_unix to query the database from.  
#' start_date: The start date of the time window.  
#' end_date: The end date of the time window.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#' 
#' Return  
#' A dataframe containing the pricing data.  
check_data <- function(source, time_resolution, start_unix, start_date, end_date, params) { 
  
  # Query data
  pricing_data <- load_data(source = source, time_resolution = time_resolution, start_unix = start_unix)
  
  # Prepare data
  check <- prepare_data(pricing_data = pricing_data, start_date = start_date, end_date = end_date, params = params)
  
  # To do: Add more check data logic here
  
  # Return check 
  return(check)
}


