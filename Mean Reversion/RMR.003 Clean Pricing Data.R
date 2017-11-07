#' ---
#' title: "Clean Pricing Data"
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
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data.csv")

#' # 3. Convert Currency Pairs to USD 
#' This section converts the BTC_XEM currency pair to USDT_XEM. 
usdt_xem <- pricing_data %>% 
  filter(currency_pair == "BTC_XEM") %>% 
  left_join(pricing_data %>% filter(currency_pair == "USDT_BTC"), by = c("date_unix" = "date_unix", "period" = "period")) %>% 
  mutate(high.x = high.x * high.y, 
         low.x = low.x * low.y, 
         open.x = open.x * open.y, 
         close.x = close.x * close.y, 
         volume.x = volume.x * weighted_average.y, 
         quote_volume.x = quote_volume.x, 
         weighted_average.x = weighted_average.x * weighted_average.y, 
         currency_pair.x = "USDT_XEM") %>% 
  select(date_unix, 
         date_time = date_time.x, 
         high = high.x, 
         low = low.x, 
         open = open.x, 
         close = close.x, 
         volume = volume.x, 
         quote_volume = quote_volume.x, 
         weighted_average = weighted_average.x, 
         currency_pair = currency_pair.x, 
         period)

#' # 4. Combine Data 
#' Add currency pair USDT_XEM. 
pricing_data <- pricing_data %>% 
  bind_rows(usdt_xem)

#' # 5. Write Data 
write_csv(pricing_data, "./Mean Reversion/Raw Data/pricing data clean.csv")

#' # 6. Summary
print(pricing_data)
summary(pricing_data)

#' # 7. Clean 
rm(usdt_xem)
