#' ---
#' title: "Check Pricing Data"
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

#' # 2. Setup Mongo Connection
connect_mongo <- function (time_resolution) { 
  conn <- mongo(collection = str_c("pricing_data_", time_resolution),
                                   db = "poloniex_ohlc",
                                   url = "mongodb://localhost")
  return(conn)
}

#' # 3. Prepare Data 
prepare_data <- function(pricing_data, start_date, end_date, time_resolution) { 
  df <- pricing_data %>% 
    filter(period == time_resolution, 
           date_time >= start_date, 
           date_time < end_date) %>% 
    select(date_unix, date_time, close, currency_pair) %>% 
    spread(currency_pair, close) %>% 
    mutate_all(na.locf)
  return(df)
} 

#' # 4. Connect to Mongo 
time_resolution <- "300"
mongo_connection <- connect_mongo(time_resolution)

#' # 5. Query Data 
#' 1504224000 is the unix timestamp for 2017-09-01. 
pricing_data <- mongo_connection$find(query = '{ "date_unix" : { "$gt" : 1504224000 } }') %>% 
  as_tibble()
check <- prepare_data(pricing_data, "2017-11-01", "2017-12-01", time_resolution)
