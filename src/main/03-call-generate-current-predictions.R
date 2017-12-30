#' ---
#' title: "Call Generate Current Predictions"
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

#' # 2. Source Pairs Trading Functions 
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")
source("./src/util/04-data-functions.R")
source("./src/util/05-coin-selection-functions.R")
source("./src/util/06-setup-strategy-functions.R")
source("./src/util/07-model-functions.R")
source("./src/util/08-backtesting-functions.R")
source("./src/util/09-plot-functions.R")
source("./src/util/10-generate-predictions-functions.R")
source("./src/util/11-check-data.R")
source("./src/util/12-generate-current-predictions.R")

#' # 3. Load Params
params <- load_params("./output/params/params.csv")

#' # 4. Generate Current Predictions
predictions_current <- generate_current_predictions(source = "mongodb", 
                                                    time_resolution = "300", 
                                                    params = params, 
                                                    initial_date = "2017-11-01")

#' # 5. Write Current Predictions 
write_feather(predictions_current, "./output/predictions/current-predictions.feather")
