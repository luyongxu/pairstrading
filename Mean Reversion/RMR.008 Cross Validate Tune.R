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
source("./Mean Reversion/TMR.003 Pairs Trading Functions.R") 

#' # 2. Load Data 
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) %>% 
  filter(date_time < "2017-10-09")

#' # 2. Parameter List 
#' Description  
#' A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#' 
#' Arguments  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, 
#'   and 86400.   
#' quote_currency: A string indicating the quote currency of the currency pairs. Takes values "USDT" or "BTC".  
#' cointegration_test: A string indicating whether the Engle-Granger method or distance method is used to test for 
#'   cointegration. Takes values "eg", "tls", or "distance".  
#' adf_threshold: The threshold for the ADF test statistic. Pairs below this threshold are selected when using 
#'   the Engle-Granger method.  
#' distance_threshold: The threshold for the rmse of the coins normalized prices. Pairs below this threshold are 
#'   selected when using the distance method.  
#' train_window: A lubridate period object representing the length of time the train set covers.  
#' test_window: A lubridate period object representing the length of time the the test set covers.  
#' model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' regression_type: A string indicating whether OLS, TLS, or a non-parametric regression should be used. Takes values 
#'   "ols", "tls", or "non-parametric".    
#' spread_type: A string indicating whether the regression uses a rolling or fixed window. Takes value "rolling" or 
#'   "fixed".  
#' rolling_window: The number of observations used in the lookback window of a rolling linear regression.  
#' signal_logic: A string indicating which logic to use to generate signals. Takes values "scaled" or "discrete".  
#' signal_scaled_enter: The z-score threshold indicating the z-score that the signal is fully scaled in when the 
#'   signal logic is scaled.  
#' signal_discrete_enter: The z-score threshold for entering a position when the signal logic is discrete.  
#' signal_discrete_exit: The z-score threshold for exiting a position when the signal logic is discrete.  
#' signal_stop: A threshold for the spread z-score beyond which the strategy stops trading the coin pair.  
#' signal_reenter: A boolean indicating whether the strategy should reenter positions after exceeding the 
#'     signal_stop threshold once the spread z-score returns to a reasonable range.    
#' signal_reenter_threshold: The z-score threshold for reentering a position if signal_reenter is TRUE. 
#' pair_allocation: A string indicating whether the capital allocation to the coin pairs should be equal or weighted. 
#'   Takes values "equal", "weighted", and "scaled".     
#'  pair_allocation_scaling: A double indicating the volatility scaling applied to the cointegration stat when the pair 
#'    allocation is scaled. Higher numbers are associated with greater weight being placed on coin pairs with a high 
#'    cointegration stat.    
#'   

#' # 4. Tune Parameters 
set.seed(5553) 
results <- tibble()
return <- list()
for (i in 1:1) { 
  print(str_c("Testing iteration ", i, ".")) 
  time_resolution <- 300  
  train_window <- days(sample(3:60, 1)) 
  params <- list(time_resolution = time_resolution, 
                 quote_currency = sample(c("USDT", "BTC"), 1), 
                 cointegration_test = sample(c("eg", "distance"), 1), 
                 adf_threshold = sample(seq(-2, -4, -0.05), 1), 
                 distance_threshold = sample(seq(0.05, 0.40, 0.01), 1), 
                 train_window = train_window, 
                 test_window = days(sample(3:60, 1)), 
                 model_type = sample(c("log", "raw"), 1), 
                 regression_type = "ols", 
                 spread_type = sample(c("rolling"), 1), 
                 rolling_window = 86400 / time_resolution * 
                   min(as.numeric(days(sample(2:60, 1))) / 86400, as.numeric(train_window) / 86400), 
                 signal_logic = sample(c("scaled"), 1), 
                 signal_scaled_enter = sample(c(2, 3, 4), 1), 
                 signal_discrete_enter = sample(seq(1, 3, 0.05), 1), 
                 signal_discrete_exit = sample(seq(0, 1, 0.05), 1), 
                 signal_stop = sample(seq(3, 7, 0.05), 1), 
                 signal_reenter = sample(c(TRUE, FALSE), 1), 
                 signal_reenter_threshold = sample(seq(0, 2, 0.05), 1), 
                 pair_allocation = sample(c("equal", "weighted", "scaled"), 1), 
                 pair_allocation_scaling = sample(seq(1, 2, 0.05), 1))
  return[[i]] <- backtest_strategy_full(pricing_data = pricing_data, 
                                        params = params) 
  results_temp <- bind_cols(params %>% as_tibble(), 
                            overall_return = return[[i]][["return_strategy_cumulative"]][nrow(return[[i]])]) %>% 
    mutate(train_window = as.character(train_window), 
           test_window = as.character(test_window))
  results <- bind_rows(results, results_temp)
} 

#' # 5. Examine Results
print(results)

