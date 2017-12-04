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

#' # 1. Load Pairs Trading Functions 
source("./Mean Reversion/TMR.003 Pairs Trading Functions.R") 

#' # 2. Query Data 
pricing_data <- load_data(source = "mongodb", time_resolution = "300", start_unix = "1504224000")

#' # 3. Prepare Data 
check <- prepare_data(pricing_data, "2017-11-01", "2018-01-01", list(time_resolution = "300"))


