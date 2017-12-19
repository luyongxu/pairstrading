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

#' # 1. Load Packages 
source("./src/util/01-load-packages.R")

#' # 2. Query Poloniex returnTicker Endpoint 
#' Description  
#' Returns the current quotes for all tickers traded on Poloniex.  
#' 
#' Arguments  
#' None.  
#' 
#' Value  
#' Returns a dataframe containing the current quotes for all tickers.  
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
  
#' # 3. Query Poloniex returnChartData Endpoint   
#' Description  
#' Returns candlestick chart data. This function is a wrapper around the returnChartData endpoint and adds the 
#' currency pair, period, and date time to the result. 
#' 
#' Arguments  
#' start_unix: A string indicating the start unix timestamp.  
#' end_unix: A string indicating the end unix timestamp.  
#' period: A string indicating candlestick period in seconds. Valid values are 300, 900, 1800, 7200, 14400, and 86400.  
#' 
#' Value  
#' Returns a dataframe containing the chart data.  
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

#' # 4. Set Start Unix 
#' Description  
#' Returns the start unix parameter used in the return_chartdata() function depending on the download option.  
#' 
#' Arguments  
#' See documentation in download_data().  
#' 
#' Value  
#' Returns the start_unix parameter.  
set_start_unix <- function(option) { 
  
  # If the command line argument is update, rebuild, or none, then all historical data is downloaded
  if (option %in% c("update", "rebuild", "none")) 
    start_unix <- "0000000000"
  
  # If the command line argument only specifies one time resolution, then data over the past 24 hours is downloaded
  if (option %in% c("86400", "14400", "7200", "1800", "900", "300")) 
    start_unix <- as.character(round(as.numeric(Sys.time())) - 86400)

  # Return start_unix
  return(start_unix)
}

#' # 5. Set Periods 
#' Description  
#' Returns a vector of periods depending on the download option.  
#' 
#' Arguments  
#' See documentation in download_data().  
#' 
#' Value  
#' Returns a vector of periods.  
set_periods <- function(option) { 
  
  # If the command line argument is update, rebuild, or none, set the periods vector to all time resolutions 
  if (option %in% c("update", "rebuild", "none")) 
    periods <- c("86400", "14400", "7200", "1800", "900", "300") 

  # If the command line argument only specifies one time resolution, set the periods vector to that time resolution 
  if (option %in% c("86400", "14400", "7200", "1800", "900", "300")) 
    periods <- option

  # Return periods
  return(periods)
}

#' # 6. Save Data 
#' Description  
#' Once the data has been downloaded, this function saves the data to a csv file or in the mongo database. 
#' 
#' Arguments  
#' pricing_data: A dataframe containing the pricing data.  
#' period: A string indicating candlestick period in seconds. Valid values are 300, 900, 1800, 7200, 14400, and 86400.  
#' start_unix: A string indicating the start unix timestamp.  
#' option: See documentation in download_data().  
#' 
#' Value 
#' Does not return a value.  
save_data <- function(pricing_data, period, start_unix, option) { 
  
  # If the command line argument is none, save the data as a csv file 
  if (option == "none") { 
    write_csv(pricing_data, str_c("./data/pricing-data-", period, ".csv"))
    print("Saving data to csv file.")
  }
  
  # If the command line argument exists, establish connection to mongo database 
  if (option != "none") { 
    mongo_connection <- mongo(collection = str_c("pricing_data_", period), 
                              db = "poloniex_ohlc", 
                              url = "mongodb://localhost") 
  }
  
  # If the command line argument is rebuild, drop the collection, and reinsert all historical data 
  if (option == "rebuild") { 
    mongo_connection$insert(tibble(name = "placeholder"))
    mongo_connection$drop() 
    mongo_connection$insert(pricing_data)
    print("Rebuilding mongo collection.")
  }
  
  # If the command line argument is update, find the unix timestamp of the most recent observation in the 
  # collection and only insert observations that are new. Removes observations from the returnTicker endpoint 
  # first. 
  if (option == "update") { 
    mongo_connection$remove(query = '{ "source" : "return_ticker" }')
    mongo_data <- mongo_connection$find(query = '{}') 
    new_data <- pricing_data %>% 
      filter(date_unix > max(mongo_data[["date_unix"]]))
    mongo_connection$insert(new_data)
    print("Updating mongo collection.")
  }
  
  # If the command line argument is a time resolution, query the observations from the past 24 hours in the 
  # database, remove them, compare them to the most recent data, and upsert the newest data into the collection.  
  if (option %in% c("86400", "14400", "7200", "1800", "900", "300")) { 
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

#' # 7. Download Data 
#' Description  
#' Downloads and saves OHLC pricing data using Poloniex's returnChartData and returnTicker endpoints.  
#' 
#' Arguments  
#' tickers: a vector of tickers to download data for.  
#' option:  
#' 
#' (1) When the option a number (300, 900, 1800, 7200, 14400, and 86400), the option indicates the time resolution to download 
#' the data for and this argument gets passed to the period parameter in the Poloenix API returnChartData end point. The past 
#' 24 hours of data are downloaded, compared to the exitsing documents in the mongo collection, and uses the new data to update 
#' existing documents or is upsert into the collection. This option is designed for cron jobs where new data is added to the 
#' database at regular intervals.  
#' 
#' (2) When the option is update, new data for all time resolutions are downloaded and only data that is more recent than the 
#' most recent document in the collection is inserted into the collection. This option is designed to update the database in 
#' the event of some disruption in the Poloneix API or AWS instance hosting the script.  
#' 
#' (3) When the option is rebuild, all collections and documents within those collections are dropped, the collections are 
#' recreated, and all historical data is re-inserted in the collections. This option is designed to initially populate the 
#' database or in the event of some major disruption. NB: THIS OPTION DROPS ALL EXISTING DOCUMENTS AND REPOPULATES THE 
#' COLLECTIONS SO USE THIS OPTION WITH CAUTION.  
#' 
#' (4) When the option is none, all data for all periods are downloaded and saved to a csv file. Does not interface with the 
#' mongodb.  
#' 
#' Value  
#' Does not return a value.  
download_data <- function(tickers, option) { 
  
  # Set periods and start_unix
  periods <- set_periods(option = option)
  start_unix <- set_start_unix(option = option)
  
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
              start_unix = start_unix, 
              option = option)
    
    # Print summary 
    cat(str_c("Successfully downloaded data for time resolution ", period, "."))
    cat("\n")
    cat("Observation counts of latest timestamps: \n")
    print(pricing_data %>% count(date_time) %>% filter(row_number() >= n() - 10))
    cat("\n")
  }
}

