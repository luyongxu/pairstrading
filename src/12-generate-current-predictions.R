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

#' # 2. Generate Current Predictions 
generate_current_predictions <- function(source, time_resolution, params, initial_date) {  
  
  # Initialize start_unix. Load data over past three months for performance reasons on AWS instance. 
  start_unix <- as.numeric(Sys.time()) - (86400 * 90)
  
  # Load data
  pricing_data <- load_data(source = source, time_resolution = time_resolution, start_unix = start_unix)

  # Initialize cutoff date 
  cutoff_date <- set_cutoff_date(initial_date = initial_date, params = params)
  
  # Generate predictions
  predictions <- generate_predictions(pricing_data = pricing_data, cutoff_date = cutoff_date, params = params)
  
  # Extract current predictions
  predictions_current <- predictions %>% 
    group_by(coin_y_name, coin_x_name) %>% 
    filter(row_number() == n()) %>% 
    select(date_unix, date_time, coin_pair_id, coin_y_name, coin_x_name, coin_y_price, coin_x_price, signal, 
           hedge_ratio, intercept, coin_y_position, coin_x_position, change_y_position, change_x_position)
  
  # Print Predictions 
  options(width = 180)
  print(predictions_current %>% select(date_time, coin_y_name, coin_x_name, coin_y_position, coin_x_position))
  
  # Save predictions 
  write_feather(predictions_current, "./output/predictions/current-predictions.feather")

}


