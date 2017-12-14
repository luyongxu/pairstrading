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

#' # 2. Query Data 
pricing_data <- load_data(source = "csv", time_resolution = "300", start_unix = "1504224000")

#' # 3. Prepare Data 
check <- prepare_data(pricing_data, "2017-11-01", "2018-01-01", list(time_resolution = "300"))


