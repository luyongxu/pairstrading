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
                     adf_threshold = -3.60, 
                     distance_threshold = 0.31, 
                     train_window = days(24), 
                     test_window = days(6), 
                     model_type = "log", 
                     regression_type = "ols", 
                     spread_type = "rolling", 
                     rolling_window = 1152, 
                     signal_logic = "scaled", 
                     signal_scaled_enter = 3.0, 
                     signal_discrete_enter = 2.20, 
                     signal_discrete_exit = 0.25, 
                     signal_stop = 4.70, 
                     signal_reenter = TRUE, 
                     signal_reenter_threshold = 1.90, 
                     pair_allocation = "equal", 
                     pair_allocation_scaling = 1.00, 
                     return_calc = "maximum")

#' # 4. Save Params 
save_params(params = params, filename = "./output/params/params.csv")

#' # 5. Load Params
params <- load_params("./output/params/params.csv")

