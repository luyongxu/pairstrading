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
params_results <- bind_rows(read_csv("./Mean Reversion/Output/Parameter Tuning/parameter tuning 20171101.csv"), 
                            read_csv("./Mean Reversion/Output/Parameter Tuning/parameter tuning 20171102.csv"), 
                            read_csv("./Mean Reversion/Output/Parameter Tuning/parameter tuning 20171106.csv")) 

#' # 3. Set Parameters 
#' Take the top 50 best performing parameter sets. First set the train window, test window, cointegration test, 
#' and adf threshold to be identical so that each strategy will select the same coin pairs. 
params_results <- params_results %>% 
  arrange(desc(overall_return)) %>% 
  filter(row_number() <= 3) %>% 
  mutate(train_window = rep(days(24), nrow(.)), 
         test_window = rep(days(30), nrow(.)), 
         cointegration_test = "eg", 
         adf_threshold = -3.10, 
         regression_type = "ols")
cutoff_date <- "2017-09-01"

#' # 4. Loop Over Parameters 
backtest_strategy_results <- tibble()
for (i in 1:nrow(params_results)) { 
  
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
                                params = params, 
                                feather = TRUE) %>% 
    mutate(params_id = i, 
           params = paste(map2_chr(names(params), params, str_c, sep = " "), collapse = ", ")) 
  
  # Append results 
  backtest_strategy_results <- bind_rows(backtest_strategy_results, backtest)
}

#' # 5. Export to feather
write_feather(backtest_strategy_results, "./Mean Reversion/Output/Feather/backtest multiple.feather")
