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
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("iTdddddddci")) 

#' # 3. Set Parameters 
time_resolution <- 900
train_window <- days(32) 
test_window <- days(16) 
quote_currency <- "BTC" 
adf_threshold <- -3.43 
rolling_window <- 86400 / time_resolution * as.numeric(days(2)) / 86400 
stop_threshold <- 7 
signal_logic <- "scaled"
number_pairs <- 3 

#' # 4. Cross Validation September 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-09-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 5. Cross Validation August 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-08-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 6. Cross Validation July 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-07-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 7. Cross Validation June 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-06-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 8. Cross Validation May 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-05-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 9. Cross Validation April 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-04-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 10. Cross Validation March 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-03-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 11. Cross Validation February 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-02-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 12. Cross Validation January 2017
plot_many(pricing_data = pricing_data,
          time_resolution = time_resolution,
          cutoff_date = "2017-01-01",
          train_window = train_window,
          test_window = test_window,
          quote_currency = quote_currency, 
          adf_threshold = adf_threshold, 
          rolling_window = rolling_window, 
          stop_threshold = stop_threshold, 
          signal_logic = signal_logic, 
          number_pairs = number_pairs)

#' # 13. Cross Validation Full 
results <- backtest_strategy_full(pricing_data = pricing_data, 
                                  time_resolution = time_resolution, 
                                  train_window = train_window, 
                                  test_window = test_window, 
                                  quote_currency = quote_currency, 
                                  adf_threshold = adf_threshold, 
                                  rolling_window = rolling_window, 
                                  stop_threshold = stop_threshold, 
                                  signal_logic = signal_logic) 
ggplot(results, aes(x = date_time)) + 
  geom_line(aes(y = return_strategy_cumulative), colour = "blue", size = 1) + 
  geom_hline(yintercept = 1, colour = "black") + 
  labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return") 
print(results[["return_strategy_cumulative"]][nrow(results)]) 
