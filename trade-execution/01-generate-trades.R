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

#' # 1. Load Packages and Functions
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

#' # 2. Load Params
params <- load_params("./output/params/params.csv")

#' # 3. Generate Current Predictions
predictions_current <- generate_current_predictions(source = "mongodb", 
                                                    time_resolution = "300", 
                                                    params = params, 
                                                    initial_date = "2017-11-01")

#' # 4. Create Aggregate Predicted Balances 
#' The output from generate_current_predictions lists all coin pairs. Since an individual coin may be used in 
#' multiple coin pairs and has predicted balances generated using different trading signals, this section 
#' aggregates the desired balances for each individual coin.  
predicted_positions <- bind_rows(
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
  mutate(pair = coin, 
         coin = str_replace(coin, "(BTC_)|(USDT_)", "")) %>% 
  select(date_unix, date_time, coin, pair, price, predicted_position)

#' # 5. Query Current Positions 
#' Establish connection to Poloniex API and query current balances.  
poloniex_conn <- PoloniexR::PoloniexTradingAPI(
  key = Sys.getenv("poloniex_pairs_trading_key"), 
  secret = Sys.getenv("poloniex_pairs_trading_secret")
)
poloniex_balances <- PoloniexR::ReturnCompleteBalances(poloniex_conn, all.balances = TRUE) %>% 
  mutate(coin = row.names(.)) %>% 
  as_tibble() %>% 
  select(coin, 
         balance = available, 
         pending_balance = on.orders, 
         total_balance = total)

#' # 6. Generate Trades 
trades <- predicted_positions %>% 
  full_join(poloniex_balances) %>% 
  mutate_at(vars(date_unix, date_time), funs(na.locf(.))) %>% 
  mutate_at(vars(balance, pending_balance, total_balance), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(pair = ifelse(is.na(pair), str_c(coin, "_", params[["quote_currency"]]), pair), 
         predicted_position = ifelse(is.na(predicted_position), 0, predicted_position)) %>% 
  mutate(exchange = "POLONIEX", 
         orderType = "MARKET", 
         buyOrSell = case_when( 
           predicted_position - balance > 0 ~ "BUY", 
           predicted_position == balance ~ "HOLD", 
           predicted_position - balance < 0 ~ "SELL"), 
         orderSize = abs(predicted_position - balance)) %>% 
  filter(coin != "BTC", coin != "BCH")

#' # 7. Insert Trades Into Mongo Database 
mongo_connection <- mongo(collection = "POLOINIEX_TRADES", 
                          db = "ExchangeAccountDB", 
                          url = "mongodb://localhost") 
mongo_connection$insert(trades)

