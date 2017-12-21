#' ---
#' title: "Call Set Parameters"
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

#' # 1. Capture Command Line Arguments 
#' This script when called through the command line using Rscript has the option of including ar arguments that contains 
#' the path to set the working directory. 
args_params <- commandArgs(trailingOnly = TRUE)
if (length(args_params) == 1) { 
  wd <- args_params[1]
  setwd(wd) 
}
commandArgs <- function(...) NULL

#' # 2. Load Packages
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")

#' # 3. Set Parameters 
params <- set_params(time_resolution = 300, 
                     quote_currency = "BTC", 
                     cointegration_test = "eg", 
                     adf_threshold = -4.0, 
                     distance_threshold = 0.00, 
                     train_window = days(30), 
                     test_window = days(20), 
                     model_type = "raw", 
                     regression_type = "ols", 
                     spread_type = "rolling", 
                     rolling_window = 1440, 
                     signal_logic = "scaled", 
                     signal_scaled_enter = 3.0, 
                     signal_discrete_enter = 3.0, 
                     signal_discrete_exit = 0.2, 
                     signal_stop = 4.5, 
                     signal_reenter = TRUE, 
                     signal_reenter_threshold = 2.00, 
                     pair_allocation = "equal", 
                     pair_allocation_scaling = 1.00, 
                     return_calc = "maximum")

#' # 4. Save Params 
save_params(params = params, filename = "./output/params/params.csv")

#' # 5. Load Params
params <- load_params("./output/params/params.csv")

