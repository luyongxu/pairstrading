#' ---
#' title: "Cross Validate Plots"
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

#' # 3. Parameter List 
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

#' # 4. Set Parameters 
params <- list(time_resolution = 300, 
               quote_currency = "USDT", 
               cointegration_test = "eg", 
               adf_threshold = -3.4, 
               distance_threshold = 0.38, 
               train_window = days(36), 
               test_window = days(51), 
               model_type = "raw", 
               regression_type = "ols", 
               spread_type = "rolling", 
               rolling_window = 1440, 
               signal_logic = "scaled", 
               signal_scaled_enter = 3.0, 
               signal_discrete_enter = 2.9, 
               signal_discrete_exit = 0.2, 
               signal_stop = 4.5, 
               signal_reenter = TRUE, 
               signal_reenter_threshold = 2.00, 
               pair_allocation = "scaled", 
               pair_allocation_scaling = 1.25) 
number_pairs <- 8 

#' # 5. Cross Validation September 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-09-01",
          params = params, 
          number_pairs = number_pairs)

#' # 6. Cross Validation August 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-08-01",
          params = params, 
          number_pairs = number_pairs)

#' # 7. Cross Validation July 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-07-01",
          params = params, 
          number_pairs = number_pairs)

#' # 8. Cross Validation June 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-06-01",
          params = params, 
          number_pairs = number_pairs)

#' # 9. Cross Validation May 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-05-01",
          params = params, 
          number_pairs = number_pairs)

#' # 10. Cross Validation April 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-04-01",
          params = params, 
          number_pairs = number_pairs)

#' # 11. Cross Validation March 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-03-01",
          params = params, 
          number_pairs = number_pairs)

#' # 12. Cross Validation February 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-02-01",
          params = params, 
          number_pairs = number_pairs)

#' # 13. Cross Validation January 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-01-01",
          params = params, 
          number_pairs = number_pairs)

#' # 14. Cross Validation Full 
results <- backtest_strategy_full(pricing_data = pricing_data, 
                                  params = params)  
ggplot(results, aes(x = date_time)) + 
  geom_line(aes(y = return_strategy_cumulative), colour = "blue", size = 1) + 
  geom_hline(yintercept = 1, colour = "black") + 
  labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return") 
print(results[["return_strategy_cumulative"]][nrow(results)]) 
