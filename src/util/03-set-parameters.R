#' ---
#' title: "Set Parameters"
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

#' # 1. Load Packages 
source("./src/util/01-load-packages.R")

#' # 2. Set Parameters Function 
#' Description  
#' Generates a list of parameters that describes the pairs trading strategy. 
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
#'  return_calc: A string indicating the position calculation used in calculating the return of the strategy. Can take values 
#'    maximum or actual which refer to setting the denominator in the return calculation to the maximum capital allocation 
#'    to the strategy or the actual amount that was used at the time.  
#'    
#' Value  
#' Returns a list of parameters.  
set_params <- function(time_resolution, quote_currency, cointegration_test, adf_threshold, 
                       distance_threshold, train_window, test_window, model_type, regression_type, 
                       spread_type, rolling_window, signal_logic, signal_scaled_enter, 
                       signal_discrete_enter, signal_discrete_exit, signal_stop, signal_reenter, 
                       signal_reenter_threshold, pair_allocation, pair_allocation_scaling, return_calc) { 
  params <- list(time_resolution = time_resolution, 
                 quote_currency = quote_currency, 
                 cointegration_test = cointegration_test, 
                 adf_threshold = adf_threshold, 
                 distance_threshold = distance_threshold, 
                 train_window = train_window, 
                 test_window = test_window, 
                 model_type = model_type, 
                 regression_type = regression_type, 
                 spread_type = spread_type, 
                 rolling_window = rolling_window, 
                 signal_logic = signal_logic, 
                 signal_scaled_enter = signal_scaled_enter, 
                 signal_discrete_enter = signal_discrete_enter, 
                 signal_discrete_exit = signal_discrete_exit, 
                 signal_stop = signal_stop, 
                 signal_reenter = signal_reenter, 
                 signal_reenter_threshold = signal_reenter_threshold, 
                 pair_allocation = pair_allocation, 
                 pair_allocation_scaling = pair_allocation_scaling, 
                 return_calc = return_calc) 
  return(params)
}

#' # 3. Save Params 
#' Description  
#' Saves a list of parameters from a file that describes the pairs trading strategy. 
#' 
#' Arguments  
#' params: A list of params.  
#' filename: The filename of the saved file.    
#' 
#' Value  
#' Returns a list of parameters.  
save_params <- function(params, filename) { 
  df_params <- params %>% as_tibble()
  write_csv(df_params, filename)
}

#' # 4. Load Params
#' Description  
#' Loads a list of parameters from a file that describes the pairs trading strategy. 
#' 
#' Arguments  
#' filename: A file containing the parameters in csv format.  
#' 
#' Value  
#' Returns a list of parameters.  
load_params <- function(filename) { 
  df_params <- read_csv(filename)
  list_params <- df_params %>% 
    mutate(train_window = days(str_match(train_window, "([0-9]*)d.*")[, 2]), 
           test_window = days(str_match(test_window, "([0-9]*)d.*")[, 2])) %>% 
    as.list()
  return(list_params)
}


