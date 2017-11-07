#' ---
#' title: "Export Feather"
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

#' # 3. Set Parameters 
params <- list(time_resolution = 300, 
               quote_currency = "USDT", 
               cointegration_test = "eg", 
               adf_threshold = -3.70, 
               distance_threshold = 0.19, 
               train_window = days(3), 
               test_window = days(41), 
               model_type = "raw", 
               regression_type = "ols", 
               spread_type = "rolling", 
               rolling_window = 864, 
               signal_logic = "scaled", 
               signal_scaled_enter = 4.0, 
               signal_discrete_enter = 2.20, 
               signal_discrete_exit = 0.85, 
               signal_stop = 5.40, 
               signal_reenter = TRUE, 
               signal_reenter_threshold = 0.50, 
               pair_allocation = "weighted", 
               pair_allocation_scaling = 1.95) 
number_pairs <- 10
cutoff_date <- "2017-09-01" 
coin_y <- "USDT_REP"
coin_x <- "USDT_XMR"

#' # 4. Create Test and Train
train <- prepare_data(pricing_data = pricing_data, 
                      start_date = as.Date(cutoff_date) - params[["train_window"]], 
                      end_date = as.Date(cutoff_date), 
                      params = params) 
test <- prepare_data(pricing_data = pricing_data, 
                     start_date = as.Date(cutoff_date), 
                     end_date = as.Date(cutoff_date) + params[["test_window"]], 
                     params = params) 
model <- train_model(train = train, 
                     test = test, 
                     coin_y = coin_y, 
                     coin_x = coin_x, 
                     params = params)
signals <- generate_signals(train = train, 
                            test = test, 
                            coin_y = coin_y, 
                            coin_x = coin_x, 
                            model = model, 
                            params = params)
backtest <- backtest_pair(train = train, 
                          test = test, 
                          coin_y = coin_y, 
                          coin_x = coin_x, 
                          params = params, 
                          feather = TRUE)
write_feather(backtest, "./Mean Reversion/Output/Feather/backtest.feather")
