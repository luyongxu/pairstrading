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
params_results <- bind_rows(read_csv("./output/tuning/parameter-tuning-20171101.csv"), 
                            read_csv("./output/tuning/parameter-tuning-20171102.csv"), 
                            read_csv("./output/tuning/parameter-tuning-20171106.csv"), 
                            read_csv("./output/tuning/parameter-tuning-20171113.csv")) %>% 
  mutate(return_calc = "maximum")

#' # 3. Set Parameters 
#' Take the top 50 best performing parameter sets. First set the train window, test window, cointegration test, 
#' and adf threshold to be identical so that each strategy will select the same coin pairs. 
params_results <- params_results %>% 
  arrange(desc(overall_return)) %>% 
  filter(row_number() <= 1) %>% 
  mutate(train_window = rep(days(20), nrow(.)), 
         test_window = rep(days(30), nrow(.)), 
         cointegration_test = "eg", 
         adf_threshold = -3.10, 
         regression_type = "ols", 
         overall_return = round(overall_return, 2), 
         params_id = row_number()) %>% 
  select(params_id, everything())
cutoff_date <- "2017-09-01"

#' # 4. Loop Over Parameters 
for (i in 1:nrow(params_results)) { 
  
  # Print 
  print(str_c("Creating backtesting results for parameter set ", i, "."))
  
  # Set parameter set 
  params <- params_results[i, ] %>% as.list() 
  
  # Create train and test sets 
  train <- prepare_data(pricing_data = pricing_data, 
                        start_date = as.Date(cutoff_date) - params[["train_window"]], 
                        end_date = as.Date(cutoff_date), 
                        params = params) 
  test <- prepare_data(pricing_data = pricing_data, 
                       start_date = as.Date(cutoff_date), 
                       end_date = as.Date(cutoff_date) + params[["test_window"]], 
                       params = params) 
  
  # Select coin pairs 
  coin_pairs <- create_pairs(params = params) 
  selected_pairs <- select_pairs(train = train, 
                                 coin_pairs = coin_pairs, 
                                 params = params) 
  
  # Generate backtest results 
  backtest <- backtest_strategy(train = train, 
                                test = test, 
                                selected_pairs = selected_pairs, 
                                params = params) %>% 
    mutate(params_id = i, 
           params = paste(map2_chr(names(params), params, str_c, sep = " "), collapse = ", ")) 
  
  # Output feather file 
  feather_name <- paste(map_chr(params, str_c, sep = " "), collapse = ", ")
  write_feather(backtest, str_c("./output/backtest/backtest ", feather_name, ".feather"))
}

#' # 5. Output Feather of Parameter File 
write_feather(params_results, "./output/backtest/params-results.feather")
