#' ---
#' title: "Generate Trades"
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
#' Contains functions that generate trades based on the output from the generate_current_predictions() function.  

#' # 1. Capture Command Line Arguments 
#' This script when called through the command line using Rscript has the option of including arguments that contains 
#' the path to set the working directory. 
args_params <- commandArgs(trailingOnly = TRUE)
if (length(args_params) == 1) { 
  wd <- args_params[1]
  setwd(wd) 
}
commandArgs <- function(...) NULL

#' # 2. Load Packages and Functions
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")
source("./src/util/04-data-functions.R")
source("./src/util/05-coin-selection-functions.R")
source("./src/util/06-setup-strategy-functions.R")
source("./src/util/07-model-functions.R")
source("./src/util/08-backtesting-functions.R")
source("./src/util/09-plot-functions.R")
source("./src/util/10-generate-predictions-functions.R")
source("./src/util/11-check-data.R")
source("./src/util/12-generate-current-predictions.R")

#' # 3. Parse Predicted Positions 
#' Description  
#' Parses the output from the generate_current_predictions() function to create aggregate positions for each 
#' coin. This is a necessary step before generating the actual trades.  
#' 
#' Arguments  
#' predictions_current: A dataframe that is returned by the generate_current_predictions() function.  
#'  
#' Value  
#' A dataframe containing the aggregate positions for each coin.  
parse_predicted_positions <- function(predictions_current) { 
  
  # Some coins are selected across multiple coin pairs. This section gathers the data into long format 
  # to create agregate positions. 
  df <- bind_rows(
    predictions_current %>% 
      select(date_unix, 
             date_time, 
             coin = coin_y_name, 
             price = coin_y_price, 
             predicted_position = coin_y_position_base), 
    predictions_current %>% 
      select(date_unix, 
             date_time, 
             coin = coin_x_name, 
             price = coin_x_price, 
             predicted_position = coin_x_position_base)) %>% 
    group_by(coin) %>% 
    summarise(date_unix = mean(date_unix), 
              date_time = mean(date_time), 
              price = mean(price), 
              predicted_position = sum(predicted_position)) %>% 
    ungroup() %>% 
    select(date_unix, date_time, coin, price, predicted_position)
  
  # Return parsed current predictions
  return(df)
  
}

#' # 4. Generate Trades 
#' Description  
#' Generates trades by diffing the current Poloniex margin account positions against the predicted positions.  
#' 
#' Arguments  
#' margin_position: A dataframe generated by the poloniex_margin_position() function. 
#' predictions_current_parsed: A dataframe generated by the parse_predicted_positions() function.  
#' diff_threshold: A double indicating the percent difference threshold between actual and predicted 
#' positions in order to execute a trade.  
#' 
#' Value  
#' Returns a dataframe containing the necessary trades to execute.  
generate_trades <- function(margin_position, predictions_current_parsed, diff_threshold) {
  
  # Query real-time prices from ticker endpoint 
  ticker <- poloniex_return_ticker() %>% 
    select(coin, trade_rate = close)
  
  # Join predictions and real-time prices on to the positions 
  trades <- margin_position %>% 
    full_join(predictions_current_parsed) %>% 
    left_join(ticker)
  
  # Diff actual positions to predicted predictions to generate desired trades 
  # The difference must exceed the trade threshold in order to execute a trade 
  # Set predicted position to 0 if no predictions are generated for that coin 
  # This means that new coin selection occurred and need to close out positions 
  trades <- trades %>% 
    mutate(predicted_position = ifelse(is.na(predicted_position), 0, predicted_position), 
           trade_amount = predicted_position - amount, 
           trade_amount = ifelse(abs(trade_amount) / abs(predicted_position) < diff_threshold | predicted_position == 0, 0, trade_amount), 
           trade_type = case_when(trade_amount > 0 ~ "buy", 
                                  trade_amount == 0 ~ "hold", 
                                  trade_amount < 0 ~ "sell")) %>% 
    select(-lendingFees, -date_unix, -date_time)
  print(trades)
  
  # Only keep trades that need to be executed
  trades <- trades %>% 
    filter(trade_type != "hold")
    
  # Return trades
  return(trades)
  
}





