#' ---
#' title: "Generate Predictions"
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
#' This script when called through the command line using Rscript has the option of including one argument that takes the 
#' following values: predictions.   
#' 
#' When the script is called with the predictions argument, only predictions are generated and exported to feather. This 
#' is designed to be called on a regular cadence to generate real-time predictions quickly.  
#' 
#' When the script is called without an argument, both predictions and plots are generated. This is designed to be called 
#' using render to generate notebooks for diagnostic purposes.  
args_prediciton <- commandArgs(trailingOnly = TRUE)
if (length(args_prediciton) == 0) { 
  args_prediciton <- "none" 
}
commandArgs <- function(...) NULL

#' # 2. Source Pairs Trading Functions 
source("./src/01-load-packages.R")
source("./src/03-set-parameters.R")
source("./src/04-data-functions.R")
source("./src/05-coin-selection-functions.R")
source("./src/06-setup-strategy-functions.R")
source("./src/07-model-functions.R")
source("./src/08-backtesting-functions.R")
source("./src/09-plot-functions.R")
source("./src/10-generate-predictions-functions.R")

#' # 3. Set Parameters 
params <- set_params()

#' # 4. Load Data 
#' Query the mongo database if the script is called using the predictions argument. Otherwise, load the csv file.  
if (args_prediciton == "none") { 
  pricing_data <- load_data(source = "csv", time_resolution = "300", start_unix = "1504224000")
}
if (args_prediciton == "predictions") {
  pricing_data <- load_data(source = "mongodb", time_resolution = "300", start_unix = "1504224000")
}

#' # 5. Initialize Cutoff Date 
cutoff_date <- set_cutoff_date(initial_date = "2017-11-01", params = params)

#' # 6. Generate Predictions 
predictions <- generate_predictions(pricing_data = pricing_data, 
                                    cutoff_date = cutoff_date, 
                                    params = params)

#' # 7. Extract Current Predictions 
predictions_current <- predictions %>% 
  group_by(coin_y_name, coin_x_name) %>% 
  filter(row_number() == n()) %>% 
  select(date_unix, date_time, coin_pair_id, coin_y_name, coin_x_name, coin_y_price, coin_x_price, signal, hedge_ratio, 
         intercept, coin_y_position, coin_x_position, change_y_position, change_x_position)

#' # 8. Print Predictions 
options(width = 180)
print(predictions_current %>% select(date_time, coin_y_name, coin_x_name, change_y_position, change_x_position))

#' # 9. Export Predictions 
if (args_prediciton == "predictions") { 
  write_feather(predictions_current, "./Mean Reversion/Output/Feather/predictions.feather") 
}

