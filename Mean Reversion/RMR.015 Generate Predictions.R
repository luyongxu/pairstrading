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

#' # 1. Source Cross Validate Functions 
source("./Mean Reversion/RMR.010 Cross Validate Functions.R")

#' # 2. Set Parameters 
params <- list(time_resolution = 300, 
               quote_currency = "USDT", 
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
               pair_allocation_scaling = 1.00) 

#' # 3. Query Data 
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) %>% 
  filter(date_time <= "2017-11-17")
# mongo_connection <- mongo(collection = str_c("pricing_data_", params[["time_resolution"]], 
#                           db = "poloniex_ohlc", 
#                           url = "mongodb://localhost") 
# pricing_data <- mongo_connection$find(query = '{}') 

#' # 3. Generate Predictions 
temp <- generate_predictions(pricing_data = pricing_data, 
                             cutoff_date = "2017-11-01", 
                             params = params)
