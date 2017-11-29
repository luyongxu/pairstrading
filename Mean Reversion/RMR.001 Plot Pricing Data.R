#' ---
#' title: "Plot Pricing Data"
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

#' # 2. Load Data
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv")

#' # 3. Plot Data 
#' Plots were compared visually with data on CoinMarketCap.com. Overall data quality for daily time resolution is good. 
pricing_data %>% 
  ggplot(aes(x = date_time, y = close, colour = factor(period))) + 
  geom_line() + 
  facet_wrap(~ currency_pair, scales = "free_y")

#' # 4. Plot Data Individually 
#' There appear to be some data quality issues with intraday time resolutions. It is not clear if these prices are 
#' errors in the data or if they were actual trades. 
for (currency_pair_var in unique(pricing_data[["currency_pair"]])) { 
  print(pricing_data %>% 
          filter(currency_pair == currency_pair_var) %>% 
          ggplot(aes(x = date_time, y = close, colour = factor(period))) + 
          geom_line() + 
          scale_y_continuous(trans = "log2") + 
          facet_wrap(~ currency_pair))
}