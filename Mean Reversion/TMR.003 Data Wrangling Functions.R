#' ---
#' title: "Cross Validate Strategy"
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

#' # 2. Load Data Function 
#' Description  
#' Loads Poloniex OHLC data.  
#' 
#' Arguments
#' source: A string indicating whether to use a csv or mongodb as the source. Takes value "csv" and "mongodb".  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' 
#' Value 
#' Returns a dataframe containing Poloniex OHLC for a selection of currency pairs.  
load_data <- function(source, time_resolution, start_unix = "0000000000") { 
  
  # Loads from csv file 
  if (source == "csv") { 
    pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv", col_types = c("dTdddddddcic")) %>% 
      filter(date_unix >= as.numeric(start_unix))
  }
  
  # Loads from mongodb 
  if (source == "mongodb") { 
    mongo_connection <- mongo(collection = str_c("pricing_data_", time_resolution), 
                              db = "poloniex_ohlc", 
                              url = "mongodb://localhost")
    pricing_data <- mongo_connection$find(query = paste0('{ "date_unix" : { "$gte" : ', start_unix, ' } }')) %>% 
      as_tibble()
  }
  
  # Return data
  return(pricing_data)
}

#' # 3. Prepare Data Function 
#' Description  
#' Spreads Poloneix pricing data into wide format and filters data to a specified time resolution and time window.  
#' 
#' Arguments  
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' start_date: The start date of the time window.  
#' end_date: The end date of the time window.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' 
#' Value  
#' Returns a dataframe consiting of the unix timestamp, date time, and the closing price of various currency pairs.  
prepare_data <- function(pricing_data, start_date, end_date, params) { 
  df <- pricing_data %>% 
    filter(period == params[["time_resolution"]], 
           date_time >= start_date, 
           date_time < end_date) %>% 
    select(date_unix, date_time, close, currency_pair) %>% 
    spread(currency_pair, close) %>% 
    mutate_all(na.locf)
  return(df)
} 

