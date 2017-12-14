#' ---
#' title: "Prepare Objects"
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
source("./src/01-load-packages.R") 

#' # 2. Set Cutoff Date Function 
#' Description  
#' Initialize the cutoff date to split the data into a training and test set where the training set is used to 
#' select cointegrated coin pairs and predictions are made over the test set. The cutoff date is initialized to 
#' an arbitrary date which represents the date that the strategy first began to trade. The cutoff date is then 
#' moved forward in time so new coin selection occurs at the proper interval as determined by the parameter set.    
#' 
#' Arguments  
#' initial_date: An arbitrary date indicating when the strategy first started to trade.  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#'   test_window: A period object from lubridate representing the length of time the the test set covers.  
#' 
#' Value  
#' Returns the most recent cutoff date.  
set_cutoff_date <- function(initial_date, params) { 
  cutoff_date <- as.Date(initial_date)
  while (Sys.Date() - days(as.numeric(str_match(params[["test_window"]], "(\\d*)d*")[, 2])) > cutoff_date) { 
    cutoff_date <- cutoff_date + days(as.numeric(str_match(params[["test_window"]], "(\\d*)d*")[, 2]))
  }
  print(str_c("Coin pair selection last occured on ", cutoff_date, "."))
  return(cutoff_date)
}

#' # 2. Setup Strategy Function 
#' Description  
#' Initializes the train, test, and selected coin pair dataframes given a cutoff date and list of parameters. 
#' 
#' Arguments  
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format. 
#' cutoff_date: A date indicating the cutoff date between the train and test sets.   
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   train_window: A period object from lubridate representing the length of time the train set covers.  
#'   test_window: A period object from lubridate representing the length of time the the test set covers.  
#' 
#' Value  
#' A list containing the train, test, and selected_pairs dataframes.  
setup_strategy <- function(pricing_data, cutoff_date, params) { 
  
  # Convert cutoff date to date 
  cutoff_date <- as.Date(cutoff_date) 
  
  # Create train and test sets 
  train <- prepare_data(pricing_data = pricing_data, 
                        start_date = cutoff_date - params[["train_window"]], 
                        end_date = cutoff_date, 
                        params = params) 
  test <- prepare_data(pricing_data = pricing_data, 
                       start_date = cutoff_date, 
                       end_date = cutoff_date + params[["test_window"]], 
                       params = params) 
  
  # Select cointegrated coin pairs 
  coin_pairs <- create_pairs(params = params) 
  selected_pairs <- select_pairs(train = train, 
                                 coin_pairs = coin_pairs, 
                                 params = params) 
  
  # Return dataframes 
  return(list(train = train, 
              test = test, 
              selected_pairs = selected_pairs))
  
}