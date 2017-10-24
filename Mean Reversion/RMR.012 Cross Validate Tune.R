#' ---
#' title: "Cross Validate Tune"
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

#' # 1. Source Cross Validate Functions 
source("./Mean Reversion/RMR.010 Cross Validate Functions.R") 

#' # 2. Load Data 
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) 

#' # 3. Tune
