#' ---
#' title: "Scrape Pricing Data"
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
#' This script when called through the command line using Rscript has the option of including one argument that takes the 
#' following values: 300, 900, 1800, 7200, 14400, 86400, update and rebuild.  
#' 
#' (1) When the script is called where the argument is a number (300, 900, 1800, 7200, 14400, and 86400), the argument indicates 
#' the time resolution to download the data for and this argument gets passed to the period parameter in the Poloenix API 
#' returnChartData end point. The past 24 hours of data are downloaded, compared to the exitsing documents in the mongo 
#' collection, and uses the new data to update existing documents or is upsert into the collection. This option is designed 
#' for cron jobs where new data is added to the database at regular intervals.  
#' 
#' (2) When the script is called using the update argument, new data for all time resolutions are downloaded and only data that 
#' is more recent than the most recent document in the collection is inserted into the collection. This option is designed 
#' to update the database in the event of some disruption in the Poloneix API or AWS instance hosting the script.  
#' 
#' (3) When the script is called using the rebuild argument, all collections and documents within those collections are dropped, 
#' the collections are recreated, and all historical data is re-inserted in the collections. This option is designed to 
#' initially populate the database or in the event of some major disruption. NB: THIS OPTION DROPS ALL EXISTING DOCUMENTS 
#' AND REPOPULATES THE COLLECTIONS SO USE THIS OPTION WITH CAUTION.  
args_period <- commandArgs(trailingOnly = TRUE)
if (length(args_period) == 0) { 
  args_period <- "none" 
}

#' # 2. Load Packages 
#' Sets the command line arguments to NULL so that the command line arguments intended for this script do not get passed to 
#' the load packages script. 
commandArgs <- function(...) NULL
source("./Mean Reversion/TMR.001 Load Packages.R") 

#' # 3. Query Poloniex returnTicker Endpoint 
#' Returns the current price for a given ticker.  
return_ticker <- function(ticker) { 
  df <- fromJSON("https://poloniex.com/public?command=returnTicker") %>% 
    map2_df(names(.), ~ as_tibble(.x) %>% mutate(ticker = .y)) %>% 
    mutate_at(vars(last, lowestAsk, highestBid, percentChange, baseVolume, 
                   quoteVolume, isFrozen, high24hr, low24hr), as.numeric) %>% 
    mutate(date_time = Sys.time(), 
           date_unix = as.numeric(as.POSIXct(date_time)), 
           currency_pair = ticker, 
           close = last) %>% 
    select(date_unix, date_time, close, currency_pair) %>%  
    filter(currency_pair == ticker)  
  return(df)
}
  
#' # 4. Query Poloniex returnChartData Endpoint   
#' Returns candlestick chart data. Required GET parameters are "currencyPair", "period" (candlestick period in seconds; 
#' valid values are 300, 900, 1800, 7200, 14400, and 86400), "start", and "end". "start" and "end" are given in unix 
#' timestamp format and are used to specify the date range for the data returned. This function is a wrapper around the 
#' returnChartData endpoint and adds the currency pair, period, and date time to the result. 
return_chartdata <- function(currency_pair, start_unix, end_unix, period) { 
  df <- fromJSON(str_c("https://poloniex.com/public?command=returnChartData", 
                       "&currencyPair=", currency_pair, 
                       "&start=", start_unix, 
                       "&end=", end_unix, 
                       "&period=", period)) %>% 
    mutate(period = period, 
           currency_pair = currency_pair, 
           date_time = as.POSIXct(date, origin = "1970-01-01")) %>% 
    select(date, date_time, high, low, open, close, volume, quoteVolume, weightedAverage, currency_pair, period) %>% 
    as_tibble()
  colnames(df) <- c("date_unix", "date_time", "high", "low", "open", "close", "volume", "quote_volume", 
                    "weighted_average", "currency_pair", "period")
  return(df)
} 

#' # 5. Create List of Tickers 
#' The following tickers are of interest: USDT_BTC, USDT_ETH, USDT_LTC, USDT_DASH, USDT_XMR, USDT_ZEC, USDT_REP, BTC_XEM, 
#' BTC_ETH, BTC_LTC, BTC_DASH, BTC_XMR, BTC_ZEC, BTC_REP, BTC_XEM, BTC_DCR, BTC_FCT, BTC_LSK.
tickers <- c("USDT_BTC", "USDT_ETH", "USDT_LTC", "USDT_DASH", "USDT_XMR", "USDT_ZEC", "USDT_REP", 
             "BTC_ETH", "BTC_LTC", "BTC_DASH", "BTC_XMR", "BTC_ZEC", "BTC_REP", "BTC_XEM", "BTC_DCR", "BTC_FCT", "BTC_LSK")

#' # 6. Create List of Periods
#' The following periods are of interest: 5-minute, 15-minute, 30-minute, 2-hour, 4-hour, 1-day. If the command line argument 
#' is update, rebuild, or is missing, all the periods and all historical data are downloaded. If the command line argument is 
#' one of the time resolutions, only data for that time resolution over the past 24 hours is downloaded. 
if (args_period %in% c("update", "rebuild", "none")) { 
  periods <- c("86400", "14400", "7200", "1800", "900", "300") 
  start_unix <- "0000000000"
} 
if (args_period %in% c("86400", "14400", "7200", "1800", "900", "300")) { 
  periods <- args_period
  start_unix <- as.character(round(as.numeric(Sys.time())) - 86400)
} 

#' # 7. Download Pricing Data 
#' Download pricing data for each ticker and period length combination. 
# Initialize a tibble for containing the results for all tickers and all period length combinations 
pricing_data <- tibble()

# Download data for each time resolution 
for (period in periods) { 
  print(str_c("Downloading data for period length ", period, "."))
  
  # Initialize a tibble for containing the results of all tickers within a period length 
  pricing_data_ticker <- tibble() 
  
  # Download data for each ticker within each time resolution and return results in a dataframe. 
  # Data from the returnChartdata endpoint is used for historical data and data from the returnTicker 
  # endpoint is used for the latest value. 
  for (ticker in tickers) { 
    print(str_c("Downloading data for currency pair ", ticker, ".")) 
    df_chartdata <- return_chartdata(currency_pair = ticker, 
                                     start_unix = start_unix, 
                                     end_unix = "9999999999", 
                                     period = period) %>% 
      bind_rows(return_ticker(ticker = ticker)) %>% 
      mutate(close = na.locf(close), 
             period = na.locf(period))
    pricing_data_ticker <- bind_rows(pricing_data_ticker, ) %>% bind_rows(df_chartdata)
  } 
  
  # Clean data 
  pricing_data_ticker <- pricing_data_ticker %>% 
    arrange(currency_pair, date_unix)
  
  # Establish connection to mongo database 
  if (args_period != "none") { 
    mongo_connection <- mongo(collection = str_c("pricing_data_", period), 
                              db = "poloniex_ohlc", 
                              url = "mongodb://localhost") 
  }

  # When the command line argument is rebuild, drop the collection and reinsert all historical data 
  if (args_period == "rebuild") { 
    mongo_connection$insert(tibble(name = "placeholder"))
    mongo_connection$drop() 
    mongo_connection$insert(pricing_data_ticker)
  }
  
  # When the command line argument is update, find the unix timestamp of the most recent observation in the 
  # collection and only insert observations that are new 
  if (args_period == "update") { 
    pricing_data_recent <- mongo_connection$find(query = '{}') 
    new_data <- pricing_data_ticker %>% 
      filter(date_unix > max(pricing_data_recent[["date_unix"]]))
    mongo_connection$insert(new_data)
  }

  # When the command line argument is a time resolution, query the observations from the past 24 hours in the 
  # database, remove them, compare them to the most recent data, and upsert the newest data into the collection.  
  if (args_period %in% c("86400", "14400", "7200", "1800", "900", "300")) { 
    pricing_data_recent <- mongo_connection$find(query = paste0('{ "date_unix" : { "$gt" : ', start_unix, ' } }'))
    mongo_connection$remove(query = paste0('{ "date_unix" : { "$gt" : ', start_unix, ' } }'))
    new_data <- pricing_data_ticker %>% 
      bind_rows(pricing_data_recent) %>% 
      filter(date_unix > start_unix) %>% 
      distinct() %>% 
      group_by(currency_pair, date_unix) %>% 
      filter(row_number() == 1)
    mongo_connection$insert(new_data)
  }
  
  # Append the data 
  pricing_data <- pricing_data %>% 
    bind_rows(pricing_data_ticker) 
}

#' # 8. Save Data to CSV 
#' Save data to csv only if no argument was passed in the command line.  
if (args_period == "none") { 
  write_csv(pricing_data, "./Mean Reversion/Raw Data/pricing data.csv")
} 

#' # 9. Summary
print(pricing_data)
glimpse(pricing_data)
summary(pricing_data) 

#' # 10. Clean
rm(return_ticker, period, periods, ticker, tickers, return_chartdata, args_period)
