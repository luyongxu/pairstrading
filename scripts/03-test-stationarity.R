#' ---
#' title: "Test Stationarity"
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
source("./src/01-load-packages.R")

#' # 2. Load Data
pricing_data <- bind_rows(read_csv("./data/pricing-data-300.csv"), 
                          read_csv("./data/pricing-data-900.csv"), 
                          read_csv("./data/pricing-data-1800.csv"), 
                          read_csv("./data/pricing-data-7200.csv"), 
                          read_csv("./data/pricing-data-14400.csv"), 
                          read_csv("./data/pricing-data-86400.csv"))

#' # 3. Filter Data 
pricing_data_a <- pricing_data %>% 
  filter(period == 86400, 
         date_time >= "2017-04-01")

#' # 3. Calculate Half Life of Mean Reversion of Individual Coins 
coin_single_a <- pricing_data_a %>% 
  group_by(currency_pair) %>% 
  do(model = lm(close - lag(close, 1) ~ lag(close, 1), data = .)) %>% 
  mutate(half_life = -log(2) / coef(model)[2]) %>% 
  select(-model)

#' # 4. Test for Stationarity of Individual Coins
coin_single_b <- pricing_data_a %>%  
  group_by(currency_pair) %>% 
  summarise(df_stat = ur.df(close, type = "drift", lags = 1)@testreg[["coefficients"]][2, 3], 
            crit_value_1pct = ur.df(close, type = "drift", lags = 1)@cval[1, 1], 
            crit_value_5pct = ur.df(close, type = "drift", lags = 1)@cval[1, 2], 
            crit_value_10pct = ur.df(close, type = "drift", lags = 1)@cval[1, 3]) %>% 
  left_join(coin_single_a) %>% 
  arrange(df_stat) 
knitr::kable(coin_single_b)

#' # 5. Plot Coins 
#' The following plot shows data from April 2017 to present ranked by the ADF test statitsic. The horizontal 
#' line indicates the mean price. 
pricing_data_a %>%  
  mutate(currency_pair = factor(currency_pair, levels = coin_single_b[["currency_pair"]])) %>% 
  ggplot(aes(x = date_time)) + 
  geom_line(aes(y = close), colour = "blue") + 
  facet_wrap(~ currency_pair, scale = "free_y")

#' Plots of the top four coins. 
pricing_data_a %>% 
  filter(currency_pair == "BTC_LTC") %>% 
  ggplot(aes(x = date_time)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_hline(aes(yintercept = mean(close)))
pricing_data_a %>% 
  filter(currency_pair == "BTC_DASH") %>% 
  ggplot(aes(x = date_time)) + 
  geom_line(aes(y = close), colour = "blue")  + 
  geom_hline(aes(yintercept = mean(close)))
pricing_data_a %>% 
  filter(currency_pair == "BTC_XEM") %>% 
  ggplot(aes(x = date_time)) + 
  geom_line(aes(y = close), colour = "blue")  + 
  geom_hline(aes(yintercept = mean(close)))
pricing_data_a %>% 
  filter(currency_pair == "USDT_REP") %>% 
  ggplot(aes(x = date_time)) + 
  geom_line(aes(y = close), colour = "blue")  + 
  geom_hline(aes(yintercept = mean(close)))