#' ---
#' title: "Execute Trades"
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
source("./trade-execution/poloniex-commands.R")
source("./trade-execution/generate-trades.R")

#' # 3. Load Params
params <- load_params("./output/params/params.csv")

#' # 4. Generate Current Predictions
predictions_current <- generate_current_predictions(
  source = "mongodb", 
  time_resolution = "300", 
  params = params, 
  initial_date = "2017-11-01"
)

#' # 5. Temporary Testing Code 
#' Temporarily reduce coin pairs to just one coin pair and temporarily reduce position size by a factor of 10
predictions_current <- predictions_current %>% 
  filter(row_number() == 1) %>% 
  mutate(coin_y_position_base = coin_y_position_base / 10, 
         coin_x_position_base = coin_x_position_base / 10)

#' # 6. Parse Predicted Predictions 
predictions_current_parsed <- parse_predicted_positions(
  predictions_current = predictions_current
)

#' # 7. Get Margin Position 
print("Querying Poloniex margin account positions.")
poloniex_conn <- poloniex_generate_conn(
  key = Sys.getenv("poloniex_pairs_trading_key"), 
  secret = Sys.getenv("poloniex_pairs_trading_secret")
)
margin_position <- poloniex_margin_position(
  poloniex_conn = poloniex_conn, 
  currencyPair = "all"
)
print(margin_position)

#' # 8. Generate Trades 
print("Generating trades.")
trades <- generate_trades(
  margin_position = margin_position, 
  predictions_current_parsed = predictions_current_parsed, 
  diff_threshold = 0.01
)
print(trades)

#' # 9. Execute Trades 
print("Executing trades.")
if (nrow(trades) == 0) print("No trades are necessary to reach desired positions.")
trades_logs <- tibble()
if (nrow(trades) != 0) { 
  
  # Loop through each trade and execute it. Save the output to a dataframe.  
  for (i in 1:nrow(trades)) { 
    
    # Buy
    if (trades[["trade_type"]][i] == "buy") { 
      print(str_c("Buying ", trades[["trade_amount"]][i], " units of ", trades[["coin"]][i], "."))
      trade <- poloniex_margin_buy(
        poloniex_conn = poloniex_conn, 
        currencyPair = trades[["coin"]][i], 
        rate = trades[["trade_rate"]][i], 
        slippage = 0.01, 
        amount = trades[["trade_amount"]][i]
      )
    }
      
    # Sells 
    if (trades[["trade_type"]][i] == "sell") { 
      print(str_c("Selling ", abs(trades[["trade_amount"]][i]), " units of ", trades[["coin"]][i], "."))
      trade <- poloniex_margin_sell(
        poloniex_conn = poloniex_conn, 
        currencyPair = trades[["coin"]][i], 
        rate = trades[["trade_rate"]][i], 
        slippage = 0.01, 
        amount = trades[["trade_amount"]][i]
      )
    }
    
    # Append logs
    trades_logs <- bind_rows(trades_logs, trade)
  }
  
  # Print trades logs
  print(trades_logs)
}

#' # 10. Save Trades
print(str_c("Saving trade logs to mongo database at ", Sys.time(), "."))
mongo_connection <- mongo(collection = "POLOINIEX_TRADES", 
                          db = "ExchangeAccountDB", 
                          url = "mongodb://localhost") 
mongo_connection$insert(trades)







