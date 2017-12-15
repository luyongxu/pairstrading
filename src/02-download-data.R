#' ---
#' title: "Download Data"
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
#' This script when called through the command line using Rscript has the option of including two arguments. The first 
#' argument is a period argument that takes the following values: 300, 900, 1800, 7200, 14400, 86400, update and rebuild.  
#' The second argument contains the path to the working directory. 
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
#' 
args_download <- commandArgs(trailingOnly = TRUE)
if (length(args_download) == 0) { 
  args_download <- "none" 
}
if (length(args_download) == 2) { 
  setwd(args_download[2]) 
}
commandArgs <- function(...) NULL

#' # 2. Load Packages 
source("./src/01-load-packages.R")

#' # 3. Query Poloniex returnTicker Endpoint 
#' Returns the current price for all tickers.  
return_ticker <- function() { 
  df <- fromJSON("https://poloniex.com/public?command=returnTicker") %>% 
    map2_df(names(.), ~ as_tibble(.x) %>% mutate(ticker = .y)) %>% 
    mutate_at(vars(last, lowestAsk, highestBid, percentChange, baseVolume, 
                   quoteVolume, isFrozen, high24hr, low24hr), as.numeric) %>% 
    mutate(date_time = Sys.time(), 
           date_unix = as.numeric(as.POSIXct(date_time)), 
           currency_pair = ticker, 
           close = last, 
           source = "return_ticker") %>% 
    select(date_unix, date_time, close, currency_pair, source)
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
           date_time = as.POSIXct(date, origin = "1970-01-01"), 
           source = "return_chartdata") %>% 
    select(date, date_time, high, low, open, close, volume, quoteVolume, weightedAverage, currency_pair, period, source) %>% 
    as_tibble()
  colnames(df) <- c("date_unix", "date_time", "high", "low", "open", "close", "volume", "quote_volume", 
                    "weighted_average", "currency_pair", "period", "source")
  return(df)
} 

#' # 5. Set Start Unix 
#' Returns the start unix parameter used in the return_chartdata() function depending on the arguments that are passed 
#' in the command line.  
set_start_unix <- function(args_download) { 
  
  # If the command line argument is update, rebuild, or none, then all historical data is downloaded
  if (args_download[1] %in% c("update", "rebuild", "none")) 
    start_unix <- "0000000000"
  
  # If the command line argument only specifies one time resolution, then data over the past 24 hours is downloaded
  if (args_download[1] %in% c("86400", "14400", "7200", "1800", "900", "300")) 
    start_unix <- as.character(round(as.numeric(Sys.time())) - 86400)

  # Return start_unix
  return(start_unix)
}

#' # 6. Set Periods
#' Returns a vector of periods depending on the arguments that are passed in the command line. 
set_periods <- function(args_download) { 
  
  # If the command line argument is update, rebuild, or none, set the periods vector to all time resolutions 
  if (args_download[1] %in% c("update", "rebuild", "none")) 
    periods <- c("86400", "14400", "7200", "1800", "900", "300") 

  # If the command line argument only specifies one time resolution, set the periods vector to that time resolution 
  if (args_download[1] %in% c("86400", "14400", "7200", "1800", "900", "300")) 
    periods <- args_download[1]

  # Return periods
  return(periods)
}

#' # 7. Save Data 
#' Once the data has been downloaded, this function saves the data to a csv file or in the mongo database. 
save_data <- function(pricing_data, period, args_download) { 
  
  # If the command line argument is none, save the data as a csv file 
  if (args_download[1] == "none") { 
    write_csv(pricing_data, str_c("./data/pricing-data-", period, ".csv"))
    print("Saving data to csv file.")
  }
  
  # If the command line argument exists, establish connection to mongo database 
  if (args_download[1] != "none") { 
    mongo_connection <- mongo(collection = str_c("pricing_data_", period), 
                              db = "poloniex_ohlc", 
                              url = "mongodb://localhost") 
  }
  
  # If the command line argument is rebuild, drop the collection, and reinsert all historical data 
  if (args_download[1] == "rebuild") { 
    mongo_connection$insert(tibble(name = "placeholder"))
    mongo_connection$drop() 
    mongo_connection$insert(pricing_data)
    print("Rebuilding mongo collection.")
  }
  
  # If the command line argument is update, find the unix timestamp of the most recent observation in the 
  # collection and only insert observations that are new. Removes observations from the returnTicker endpoint 
  # first. 
  if (args_download[1] == "update") { 
    mongo_connection$remove(query = '{ "source" : "return_ticker" }')
    mongo_data <- mongo_connection$find(query = '{}') 
    new_data <- pricing_data %>% 
      filter(date_unix > max(mongo_data[["date_unix"]]))
    mongo_connection$insert(new_data)
    print("Updating mongo collection.")
  }
  
  # If the command line argument is a time resolution, query the observations from the past 24 hours in the 
  # database, remove them, compare them to the most recent data, and upsert the newest data into the collection.  
  if (args_download[1] %in% c("86400", "14400", "7200", "1800", "900", "300")) { 
    mongo_connection$remove(query = '{ "source" : "return_ticker" }')
    mongo_data <- mongo_connection$find(query = paste0('{ "date_unix" : { "$gt" : ', start_unix, ' } }'))
    mongo_connection$remove(query = paste0('{ "date_unix" : { "$gt" : ', start_unix, ' } }'))
    new_data <- pricing_data %>% 
      bind_rows(mongo_data) %>% 
      filter(date_unix > start_unix) %>% 
      distinct() %>% 
      group_by(currency_pair, date_unix) %>% 
      filter(row_number() == 1)
    mongo_connection$insert(new_data)
    print("Adding most recent data to mongo collection.")
  }
}

#' # 8. Download Data 
download_data <- function(tickers, args_download) { 
  
  # Set periods and start_unix
  periods <- set_periods(args_download = args_download)
  start_unix <- set_start_unix(args_download = args_download)
  
  # Download data for each time resolution 
  for (period in periods) { 
    print(str_c("Downloading data for period length ", period, "."))
    
    # Initialize a tibble for containing the results of all tickers within a period length 
    pricing_data <- tibble() 
    
    # Download data for each ticker within each time resolution and return results in a dataframe. 
    # Data from the returnChartdata endpoint is used for historical data and data from the returnTicker 
    # endpoint is used for the latest value. 
    for (ticker in tickers) { 
      print(str_c("Downloading data for currency pair ", ticker, ".")) 
      df_chartdata <- return_chartdata(currency_pair = ticker, 
                                       start_unix = start_unix, 
                                       end_unix = "9999999999", 
                                       period = period)
      pricing_data <- bind_rows(pricing_data, df_chartdata)
    } 
    
    # Add latest data from returnTicker endpoint 
    df_ticker <- return_ticker() %>% 
      filter(currency_pair %in% tickers)
    pricing_data <- bind_rows(pricing_data, df_ticker)
    
    # Clean data 
    pricing_data <- pricing_data %>% 
      arrange(currency_pair, date_unix) %>% 
      group_by(currency_pair) %>% 
      mutate(close = na.locf(close), 
             period = na.locf(period)) %>% 
      ungroup()
    
    # Save the data 
    save_data(pricing_data = pricing_data, 
              period = period, 
              args_download = args_download)
    
    # Print summary 
    cat(str_c("Successfully downloaded data for time resolution ", period, "."))
    cat("\n")
    cat("Observation counts of latest timestamps: \n")
    print(pricing_data %>% count(date_time) %>% filter(row_number() >= n() - 10))
    cat("\n")
  }
}

#' # 9. Run Download Data 
download_data(
  tickers = c("USDT_BTC", "USDT_ETH", "USDT_LTC", "USDT_DASH", "USDT_XMR", "USDT_ZEC", "USDT_REP",
              "BTC_ETH", "BTC_LTC", "BTC_DASH", "BTC_XMR", "BTC_ZEC", "BTC_REP", "BTC_XEM", "BTC_DCR", "BTC_FCT", "BTC_LSK"), 
  args_download = args_download
)