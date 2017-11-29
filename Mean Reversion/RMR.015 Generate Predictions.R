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

#' # 2. Source Cross Validate Functions 
#' Sets the command line arguments to NULL so that the command line arguments intended for this script do not get passed to
#' the load packages script. 
commandArgs <- function(...) NULL
source("./Mean Reversion/RMR.010 Cross Validate Functions.R")

#' # 3. Set Parameters 
params <- list(time_resolution = 300, 
               quote_currency = "USDT", 
               cointegration_test = "eg", 
               adf_threshold = -5.0, 
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

#' # 4. Query Data 
#' Query the mongo database if the script is called using the predictions argument. Otherwise, load the csv file.  
if (args_prediciton == "predictions") { 
  mongo_connection <- mongo(collection = str_c("pricing_data_", params[["time_resolution"]],
                                               db = "poloniex_ohlc",
                                               url = "mongodb://localhost"))
  pricing_data <- mongo_connection$find(query = '{}') %>% 
    as_tibble() %>% 
    filter(date_time <= "2017-11-17") 
}
if (args_prediction == "none") { 
  pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) %>% 
    filter(date_time <= "2017-11-17")
}


                            
#' # 5. Initialize Cutoff Date 
#' Initialize the cutoff date to split the data into a training and test set where the training set is used to 
#' select cointegrated coin pairs and predictions are made over the test set. The cutoff date is initialized to 
#' an arbitrary date which represents the date. The cutoff date is then moved forward in time so that current 
#' predictions can be made.  
cutoff_date <- as.Date("2017-11-01")
test_window <- days(as.numeric(str_match(params[["test_window"]], "(\\d*)d*")[, 2]))
while (Sys.Date() - test_window > cutoff_date) { 
  cutoff_date <- cutoff_date + test_window
}
print(str_c("Coin pair selection last occured on ", cutoff_date, "."))

#' # 6. Generate Predictions 
predictions <- generate_predictions(pricing_data = pricing_data, 
                                    cutoff_date = "2017-11-01", 
                                    params = params)

#' # 7. Extract Current Predictions
predictions_current <- predictions %>% 
  group_by(coin_y_name, coin_x_name) %>% 
  filter(row_number() == n()) %>% 
  select(date_unix, date_time, coin_pair_id, coin_y_name, coin_x_name, coin_y_price, coin_x_price, signal, hedge_ratio, intercept, 
         coin_y_position, coin_x_position, change_y_position, change_x_position)

#' # 8. Print Predictions 
options(width = 160)
print(predictions_current %>% select(date_time, coin_y_name, coin_x_name, change_y_position, change_x_position))

#' # 8. Export Predictions 
if (args_prediciton == "predictions") { 
  write_feather(predictions_current, "./Mean Reversion/Output/Feather/predictions.feather") 
}

#' # 9. Generate Diagnostic Plots 
if (args_prediciton == "none") { 
  plot_many(pricing_data = pricing_data, 
            cutoff_date = cutoff_date, 
            params = params, 
            number_pairs = 100)
}

