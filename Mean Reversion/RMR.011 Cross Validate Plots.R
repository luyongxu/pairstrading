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
source("./Mean Reversion/RMR.010 Cross Validate Functions.R")  %>% 
  filter(date_time < "2017-10-09")

#' # 2. Load Data 
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) 

#' # 3. Set Parameters 
#' Arguments  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' train_window: A lubridate period object representing the length of time the train set covers.  
#' test_window: A lubridate period object representing the length of time the the test set covers. 
#' model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' quote_currency: A string indicating the quote currency of the currency pairs. Can take values "USDT" or "BTC".  
#' adf_threshold: The threshold for the ADF test statistic. Pairs below this threshold are selected.   
#' rolling_window: The number of observations used in each window of a rolling linear regression.  
#' stop_threshold: A threshold for the spread z-score beyond which the strategy stops trading the coin pair.  
#' signal_logic: A string indicating which logic to use to generate signals. Can take values "scaled" or "discrete". 
#' number_pairs: The number of pairs to generate plots for.  
params <- list(time_resolution = 300, 
               train_window = days(16), 
               test_window = days(10), 
               quote_currency = "USDT", 
               adf_threshold = -2.96, 
               rolling_window = 4608, 
               stop_threshold = 3.51, 
               signal_logic = "scaled", 
               model_type = "raw")
number_pairs <- 8 

#' # 4. Cross Validation September 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-09-01",
          params = params, 
          number_pairs = number_pairs)

#' # 5. Cross Validation August 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-08-01",
          params = params, 
          number_pairs = number_pairs)

#' # 6. Cross Validation July 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-07-01",
          params = params, 
          number_pairs = number_pairs)

#' # 7. Cross Validation June 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-06-01",
          params = params, 
          number_pairs = number_pairs)

#' # 8. Cross Validation May 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-05-01",
          params = params, 
          number_pairs = number_pairs)

#' # 9. Cross Validation April 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-04-01",
          params = params, 
          number_pairs = number_pairs)

#' # 10. Cross Validation March 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-03-01",
          params = params, 
          number_pairs = number_pairs)

#' # 11. Cross Validation February 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-02-01",
          params = params, 
          number_pairs = number_pairs)

#' # 12. Cross Validation January 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-01-01",
          params = params, 
          number_pairs = number_pairs)

#' # 13. Cross Validation Full 
results <- backtest_strategy_full(pricing_data = pricing_data, 
                                  params = params)  
ggplot(results, aes(x = date_time)) + 
  geom_line(aes(y = return_strategy_cumulative), colour = "blue", size = 1) + 
  geom_hline(yintercept = 1, colour = "black") + 
  labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return") 
print(results[["return_strategy_cumulative"]][nrow(results)]) 
