#' ---
#' title: "Generate Current Predictions"
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
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")
source("./src/util/04-data-functions.R")
source("./src/util/05-coin-selection-functions.R")
source("./src/util/06-setup-strategy-functions.R")
source("./src/util/07-model-functions.R")
source("./src/util/08-backtesting-functions.R")
source("./src/util/09-plot-functions.R")
source("./src/util/10-generate-predictions-functions.R")

#' # 2. Generate Current Predictions  
#' Description  
#' Generates and saves current predictions to a feather file. Designed to be used in live trading.  
#' 
#' Arguments  
#' source: A string indicating whether to use a csv or mongodb as the source. Takes value "csv" and "mongodb".  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#' initial_date: An arbitrary date indicating when the strategy first started to trade.  
#' 
#' Value  
#' Does not return a value.  
generate_current_predictions <- function(source, time_resolution, params, initial_date) {  
  
  # Initialize start_unix. Load data over past 45 days for performance reasons on AWS instance. 
  start_unix <- as.numeric(Sys.time()) - (86400 * 45)
  
  # Load data
  print(str_c("Generating predictions started at ", Sys.time(), "."))
  print("Loading data.")
  pricing_data <- load_data(source = source, time_resolution = time_resolution, start_unix = start_unix)

  # Initialize cutoff date 
  print("Initializing cutoff date.")
  cutoff_date <- set_cutoff_date(initial_date = initial_date, params = params)
  
  # Generate predictions 
  print("Generating predictions.")
  predictions <- generate_predictions(pricing_data = pricing_data, cutoff_date = cutoff_date, params = params)
  
  # Extract current predictions
  predictions_current <- predictions %>% 
    group_by(coin_y_name, coin_x_name) %>% 
    filter(row_number() == n()) %>% 
    select(date_unix, date_time, coin_pair_id, coin_y_name, coin_x_name, coin_y_price, coin_x_price, signal, 
           hedge_ratio, intercept, coin_y_position, coin_x_position, change_y_position, change_x_position, 
           coin_y_position_base, coin_x_position_base, change_y_position_base, change_x_position_base) %>% 
    ungroup()
  
  # Print Predictions 
  options(width = 180)
  print(predictions_current %>% select(date_time, coin_y_name, coin_x_name, coin_y_position, coin_x_position))
  
  # Return predictions 
  print(str_c("Predictions successfully generated at ", Sys.time(), "."))
  return(predictions_current)

}


