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
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) %>% 
  filter(date_time < "2017-10-09")

#' # 3. Describe Parameters 
#' Arguments   
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' cutoff_date: A data representing the cutoff date between the train and test sets.  
#' train_window: A period object from lubridate representing the length of time the train set covers.  
#' test_window: A period object from lubridate representing the length of time the the test set covers. 
#' quote_currency: A string indicating the quote currency of the currency pairs. Can take values USDT or BTC.  
#' adf_threshold: The threshold for the ADF test statistic. Pairs below this threshold are selected.   
#' rolling_window: The number of observations used in each iteration of a rolling linear regression.  
#' stop_threshold: A threshold for the spread z-score beyond which the strategy stops trading the coin pair.  
#' signal_logic: A string indicating which logic to use to generate signals.  
#' model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' number_pairs: The number of pairs to generate plots for.  

#' # 4. Tune Parameters 
set.seed(5) 
results_all <- list()
for (i in 1:10) { 
  print(str_c("Testing iteration ", i, "."))
  time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), 1) 
  train_window <- days(sample(2:60, 1)) 
  test_window <- days(sample(2:60, 1)) 
  quote_currency <- sample(c("USDT", "BTC"), 1)  
  adf_threshold <- sample(seq(-1, -4, -0.01), 1)
  rolling_window <- 86400 / time_resolution * min(as.numeric(days(sample(2:60, 1))) / 86400, as.numeric(train_window) / 86400)
  stop_threshold <- sample(seq(3, 7, 0.01), 1) 
  signal_logic <- sample(c("scaled", "discrete"), 1) 
  model_type <- sample(c("log", "raw"), 1) 
  return <- backtest_strategy_full(pricing_data = pricing_data, 
                                   time_resolution = time_resolution, 
                                   train_window = train_window, 
                                   test_window = test_window, 
                                   quote_currency = quote_currency, 
                                   adf_threshold = adf_threshold, 
                                   rolling_window = rolling_window, 
                                   stop_threshold = stop_threshold, 
                                   signal_logic = signal_logic, 
                                   model_type = model_type)  
  results <- list(time_resolution = time_resolution, 
                  train_window = train_window, 
                  test_window = test_window, 
                  quote_currency = quote_currency, 
                  adf_threshold = adf_threshold, 
                  rolling_window = rolling_window, 
                  stop_threshold = stop_threshold, 
                  signal_logic = signal_logic, 
                  model_type = model_type, 
                  overall_return = return[["return_strategy_cumulative"]][nrow(return)], 
                  return_df = return) 
  results_all <- c(results_all, results)
} 

