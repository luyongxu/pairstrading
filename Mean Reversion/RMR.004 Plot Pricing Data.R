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
source("./Mean Reversion/RMR.001 Load Packages.R")

#' # 2. Load Data
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data clean.csv")

#' # 3. Plot Data 
#' Plots were compared visually with data on CoinMarketCap.com. Data quality looks good. 
pricing_data %>% 
  ggplot(aes(x = date_time, y = close, colour = factor(period))) + 
  geom_line() + 
  facet_wrap(~ currency_pair, scales = "free_y")

#' # 4. Plot Data Individually
for (currency_pair in unique(pricing_data[["currency_pair"]])) { 
  pricing_data %>% 
    filter(currency_pair == currency_pair) %>% 
    ggplot(aes(x = date_time, y = close, colour = factor(period))) + 
    geom_line() + 
    scale_y_continuous(trans = "log2") + 
    facet_wrap(~ currency_pair)
}