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

#' # 2. Load Data 
pricing_data <- load_data(source = "csv", time_resolution = "300", start_unix = "0000000000")

#' # 3. Tune Parameters 
set.seed(1) 
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
                 pair_allocation_scaling = sample(seq(1, 2, 0.05), 1), 
                 return_calc = sample(c("actual", "maximum"), 1))
  return[[i]] <- backtest_strategy_full(pricing_data = pricing_data, 
                                        params = params) 
  results_temp <- bind_cols(params %>% as_tibble(), 
                            overall_return = return[[i]][["return_strategy_cumulative"]][nrow(return[[i]])]) %>% 
    mutate(train_window = as.character(train_window), 
           test_window = as.character(test_window))
  results <- bind_rows(results, results_temp)
} 

#' # 4. Examine Results
print(results)

#' # 5. Save Results 
write_csv(results, str_c("./output/tuning/parameter-tuning-", format(Sys.Date(), "%Y%m%d")), ".csv")