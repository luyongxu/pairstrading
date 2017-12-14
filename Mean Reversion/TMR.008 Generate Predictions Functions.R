#' ---
#' title: "Generate Predictions Functions"
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
source("./Mean Reversion/TMR.001 Load Packages.R") 

#' # 2. Generate Predictions Function   
#' Description  
#' Generate predictions on the test set given a cutoff date to split the train and test sets and a list of parameters. 
#' 
#' Arguments   
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' cutoff_date: A data representing the cutoff date between the train and test sets.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'     
#' Value  
#' A dataframe containing the position, change in position, signal, and hedge ratio for the coin pairs selected by the 
#' strategy.  
generate_predictions <- function(pricing_data, cutoff_date, params) { 
  
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
  predictions <- backtest_strategy(train = train, 
                                   test = test, 
                                   selected_pairs = selected_pairs, 
                                   params = params)
  
  # Return predictions 
  return(predictions) 
}
