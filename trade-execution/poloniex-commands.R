#' ---
#' title: "Poloniex Commands"
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
#' Contains wrapper functions for the Poloniex API (https://poloniex.com/support/api/) using the PoloniexR package 
#' (https://github.com/VermeirJellen/PoloniexR).  

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

#' # 3. Generate Poloniex Connection 
#' Description 
#' A wrapper around the PoloniexR::PoloniexTradingAPI() function. Establishes connection to Poloniex API.  
#' 
#' Arguments  
#' key: Poloniex API key.  
#' secret: Poloniex secret hash value.  
#' 
#' Value 
#' Returns a Poloniex API object to be passed to PoloinexAPI functions.  
poloniex_generate_conn <- function(key = key, secret = secret) { 
  poloniex_conn <- PoloniexR::PoloniexTradingAPI(
    key = key, 
    secret = secret
  )
}

#' # 4. Get Ticker 
#' Description  
#' Returns the ticker for all markets. 
#' 
#' Arguments  
#' None.  
#' 
#' Value  
#' Returns a dataframe containing ticker information.  
poloniex_return_ticker <- function() { 
  df <- fromJSON("https://poloniex.com/public?command=returnTicker") %>% 
    map2_df(names(.), ~ as_tibble(.x) %>% mutate(ticker = .y)) %>% 
    mutate_at(vars(last, lowestAsk, highestBid, percentChange, baseVolume, 
                   quoteVolume, isFrozen, high24hr, low24hr), as.numeric) %>% 
    mutate(date_time = Sys.time(), 
           date_unix = as.numeric(as.POSIXct(date_time)), 
           coin = ticker, 
           close = last, 
           source = "return_ticker") %>% 
    select(date_unix, date_time, close, coin, source)
  return(df)
}

#' # 5. Get Margin Position 
#' Description  
#' Returns information about your margin position in a given market, specified by the "currencyPair" POST parameter. 
#' You may set "currencyPair" to "all" if you wish to fetch all of your margin positions at once. If you have no 
#' margin position in the specified market, "type" will be set to "none". "liquidationPrice" is an estimate, and 
#' does not necessarily represent the price at which an actual forced liquidation will occur. If you have no 
#' liquidation price, the value will be -1. 
#' 
#' Arguments  
#' poloniex_conn: An object created by the generate_poloniex_conn() function.  
#' currencyPair:  A string indicating the currency pair. Can also take value "all" to query all positions at once.  
#'  
#' Sample output  
#' {"amount":"40.94717831","total":"-0.09671314",""basePrice":"0.00236190","liquidationPrice":-1,"pl":"-0.00058655", 
#' "lendingFees":"-0.00000038","type":"long"}
#' 
#' Value  
#' A dataframe containing the current margin positions.  
poloniex_margin_position <- function(poloniex_conn, currencyPair) { 
  
  # Query margin positionn
  margin_position <- PoloniexR::ProcessTradingRequest(
    theObject = poloniex_conn, 
    command = poloniex_conn@commands$getMarginPosition, 
    args = list(currencyPair = currencyPair)
  )
  
  # Parse margin position data into dataframe 
  df <- margin_position %>% 
    map(bind_rows) %>% 
    map(~mutate_if(., is.numeric, as.character)) %>% 
    bind_rows(.id = "coin") %>% 
    mutate_at(vars(amount, total, basePrice, liquidationPrice, pl, lendingFees), funs(as.numeric))
  
  # Return dataframe
  print(df)
  return(df) 
}

#' # 6. Return Margin Account Summary 
#' Description  
#' Returns a summary of your entire margin account. This is the same information you will find in the Margin 
#' Account section of the Margin Trading page, under the Markets list. 
#' 
#' Arguments  
#' poloniex_conn: An object created by the generate_poloniex_conn() function.  
#' 
#' Sample output  
#' {"totalValue": "0.00346561","pl": "-0.00001220","lendingFees": "0.00000000","netValue": "0.00345341",
#' "totalBorrowedValue": "0.00123220","currentMargin": "2.80263755"}
#' 
#' Value  
#' A dataframe containing the margin account summary information.  
poloniex_return_margin_account_summary <- function(poloniex_conn) { 
  
  # Query margin account summary
  margin_account_summary <- PoloniexR::ProcessTradingRequest(
    theObject = poloniex_conn, 
    command = poloniex_conn@commands$returnMarginAccountSummary
  )
  
  # Parse margin account summary data into dataframe 
  df <- margin_account_summary %>% 
    bind_rows()
  
  # Return dataframe
  print(df)
  return(df)

}

#' # 7. Margin Buy 
#' Description   
#' Places a margin buy order in a given market. Required POST parameters are "currencyPair", "rate", and "amount". 
#' You may optionally specify a maximum lending rate using the "lendingRate" parameter. If successful, the method 
#' will return the order number and any trades immediately resulting from your order. 
#' 
#' Arguments  
#' poloniex_conn: An object created by the generate_poloniex_conn() function.  
#' currencyPair: A string indicating the currency pair.  
#' rate: A double indicating the price of the order.  
#' slippage: A double indicating the amount of slippage in percent terms if a market order that takes liquidity is desired. 
#' Set to 0 if you want a limit order.  
#' amount: A double indicating the amount to buy in base currency.  
#' 
#' Sample Output  
#' {"success":1,"message":"Margin order placed.","orderNumber":"154407998","resultingTrades":{"BTC_DASH":
#' [{"amount":"1.00000000","date":"2015-05-10 
#' 22:47:05","rate":"0.01383692","total":"0.01383692","tradeID":"1213556","type":"buy"}]}}
#' 
#' Value 
#' Executes the trade and returns a dataframe containing the trade confirmation output.  
poloniex_margin_buy <- function(poloniex_conn, currencyPair, rate, slippage, amount) { 
  
  # Execute margin buy order
  buy <- PoloniexR::ProcessTradingRequest(
    theObject = poloniex_conn, 
    command = poloniex_conn@commands$marginBuy, 
    args = list(currencyPair = currencyPair, 
                rate = rate * (1 + slippage), 
                amount = amount)
  )
  
  # Parse trade confirmation data into dataframe
  df <- buy %>% 
    .[["resultingTrades"]] %>% 
    map(bind_rows) %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    mutate(currencyPair = currencyPair, 
           orderNumber = buy[["orderNumber"]], 
           message = buy[["message"]])
  
  # Return trade confirmation data 
  return(df)
  
}

#' # 8. Margin Sell 
#' Description   
#' Places a margin sell order in a given market. Parameters and output are the same as for the marginBuy method. 
#' 
#' Arguments  
#' poloniex_conn: An object created by the generate_poloniex_conn() function.  
#' currencyPair: A string indicating the currency pair.  
#' rate: A double indicating the price of the order.  
#' slippage: A double indicating the amount of slippage in percent terms if a market order that takes liquidity is desired. 
#' Set to 0 if you want a limit order.  
#' amount: A double indicating the amount to sell in base currency.  
#' 
#' Sample Output  
#' {"success":1,"message":"Margin order placed.","orderNumber":"154407998","resultingTrades":{"BTC_DASH":
#' [{"amount":"1.00000000","date":"2015-05-10 
#' 22:47:05","rate":"0.01383692","total":"0.01383692","tradeID":"1213556","type":"buy"}]}}
#' 
#' Value 
#' Executes the trade and returns a dataframe containing the trade confirmation output.  
poloniex_margin_sell <- function(poloniex_conn, currencyPair, rate, slippage, amount) { 
  
  # Execute margin sell order
  sell <- PoloniexR::ProcessTradingRequest(
    theObject = poloniex_conn, 
    command = poloniex_conn@commands$marginSell, 
    args = list(currencyPair = currencyPair, 
                rate = rate * (1 - slippage), 
                amount = abs(amount))
  )
  
  # Parse trade confirmation data into dataframe
  df <- sell %>% 
    .[["resultingTrades"]] %>% 
    map(bind_rows) %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    mutate(currencyPair = currencyPair, 
           orderNumber = sell[["orderNumber"]], 
           message = sell[["message"]])
  
  # Return trade confirmation data 
  return(df)
  
}

#' # 9. Close Margin Position 
#' Description  
#' Closes your margin position in a given market (specified by the "currencyPair" POST parameter) using a market order. 
#' This call will also return success if you do not have an open position in the specified market. 
#' 
#' Arguments  
#' poloniex_conn: An object created by the generate_poloniex_conn() function.  
#' currencyPair: A string indicating the currency pair.  
#' 
#' Sample output  
#' {"success":1,"message":"Successfully closed margin position.","resultingTrades":{"BTC_XMR":
#' [{"amount":"7.09215901","date":"2015-05-10 
#' 22:38:49","rate":"0.00235337","total":"0.01669047","tradeID":"1213346","type":"sell"},
#' {"amount":"24.00289920","date":"2015-05-10 
#' 22:38:49","rate":"0.00235321","total":"0.05648386","tradeID":"1213347","type":"sell"}]}}
#' 
#' Value  
#' A dataframe containing the trade confirmation data.  
poloniex_close_margin_position <- function(poloniex_conn, currencyPair) { 
  
  # Execute margin sell order
  close <- PoloniexR::ProcessTradingRequest(
    theObject = poloniex_conn, 
    command = poloniex_conn@commands$closeMarginPosition, 
    args = list(currencyPair = currencyPair)
  )
  
  # Parse trade confirmation data into dataframe and return dataframe
  if (close[["message"]] == "You do not have an open position in this market.") { 
    print(str_c("You do not have an open position in ", currencyPair, "."))
    return(NULL)
  }
  if (close[["message"]] == "Successfully closed margin position.") {
    df <- close %>% 
      .[["resultingTrades"]] %>% 
      map(bind_rows) %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      mutate(currencyPair = currencyPair, 
             success = close[["success"]], 
             message = close[["message"]])
    return(df)
  }

}





