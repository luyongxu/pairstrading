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

#' # 1. Load Packages 
source("./Mean Reversion/RMR.001 Load Packages.R")
  
#' # 2. Query Poloniex returnTicker Endpoint 
#' Returns the ticker for all markets. 
return_ticker <- fromJSON("https://poloniex.com/public?command=returnTicker") %>% 
  map2_df(names(.), ~ as_tibble(.x) %>% mutate(ticker = .y)) %>% 
  mutate_at(vars(last, lowestAsk, highestBid, percentChange, baseVolume, quoteVolume, isFrozen, high24hr, low24hr), 
            as.numeric)

#' # 3. Query Poloniex returnChartData Endpoint 
#' Returns candlestick chart data. Required GET parameters are "currencyPair", "period" (candlestick period in seconds; 
#' valid values are 300, 900, 1800, 7200, 14400, and 86400), "start", and "end". "Start" and "end" are given in UNIX 
#' timestamp format and used to specify the date range for the data returned. This function is a wrapper around the 
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
    select(date, date_time, high, low, open, close, volume, 
           quoteVolume, weightedAverage, currency_pair, period) %>% 
    as_tibble()
  colnames(df) <- c("date_unix", "date_time", "high", "low", "open", "close", "volume", 
                    "quote_volume", "weighted_average", "currency_pair", "period")
  return(df)
} 

#' # 4. Create List of Tickers and Periods 
#' The following tickers are of interest: USDT_BTC, USDT_ETH, USDT_LTC, USDT_DASH, USDT_XMR, USDT_ZEC, USDT_REP, BTC_XEM, 
#' BTC_ETH, BTC_LTC, BTC_DASH, BTC_XMR, BTC_ZEC, BTC_REP, BTC_XEM.
#' The following periods are of interest: 5-minute, 15-minute, 30-minute, 2-hour, 4-hour, 1-day.
tickers <- c("USDT_BTC", "USDT_ETH", "USDT_LTC", "USDT_DASH", "USDT_XMR", "USDT_ZEC", "USDT_REP", 
             "BTC_ETH", "BTC_LTC", "BTC_DASH", "BTC_XMR", "BTC_ZEC", "BTC_REP", "BTC_XEM")
periods <- c("300", "900", "1800", "7200", "14400", "86400")

#' # 5. Download Pricing Data 
#' Download pricing data for each ticker and period length combination. Save in one dataframe. 
pricing_data <- tibble()
for (period in periods) { 
  print(str_c("Downloading data for period length ", period, "."))
  for (ticker in tickers) { 
    print(str_c("Downloading data for currency pair ", ticker, "."))
    pricing_data <- pricing_data %>% 
      bind_rows(return_chartdata(currency_pair = ticker, start_unix = "0000000000", end_unix = "9999999999", period = period)) 
    Sys.sleep(1)
  }
}

#' # 6. Write Data 
write_csv(pricing_data, "./Mean Reversion/Raw Data/pricing data.csv")

#' # 7. Summary
print(pricing_data)
summary(pricing_data)

#' # 8. Clean
rm(return_ticker, period, periods, ticker, tickers, return_chartdata)
