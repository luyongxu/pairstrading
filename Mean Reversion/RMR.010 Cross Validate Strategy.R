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
source("./Mean Reversion/RMR.007 Load Packages.R")

#' # 2. Load Data 
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data clean.csv")

#' # 3. Prepare Data 
pricing_data <- pricing_data %>% 
  filter(period == 86400, 
         date_time >= "2017-04-01") %>% 
  select(date_unix, date_time, close, currency_pair) %>% 
  spread(currency_pair, close)
print(pricing_data)