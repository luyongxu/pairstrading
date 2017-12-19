#' ---
#' title: "Test Cointegration"
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

#' # 2. Load Data
pricing_data <- bind_rows(read_csv("./data/pricing-data-300.csv"), 
                          read_csv("./data/pricing-data-900.csv"), 
                          read_csv("./data/pricing-data-1800.csv"), 
                          read_csv("./data/pricing-data-7200.csv"), 
                          read_csv("./data/pricing-data-14400.csv"), 
                          read_csv("./data/pricing-data-86400.csv"))

#' # 3. Spread Data 
#' Data was previous gathered in tidy format. For preliminary analysis, filter data to daily time resolution and data after 
#' April 1, 2017. Based on some exploratory analysis, the number of cointegrated coins rapidly increased after April 1, 2017. 
#' This time period coincides with an extremely rapid rise of bitcoin (starting from roughly $1100) and many other coins.
pricing_data_a <- pricing_data %>% 
  filter(period == 86400, 
         date_time >= "2017-04-01") %>% 
  select(date_unix, date_time, close, currency_pair) %>% 
  spread(currency_pair, close)
print(pricing_data_a)

#' # 4. Cointegration Function 
#' The Engle-Granger method is used to test for cointegration. This method is comprised of two steps:  (1) Perform a linear 
#' regression of coin_y on coin_x. (2) Perform an Augmented Dickey-Fuller test on the residuals from the linear regression 
#' estimated in (1). The ADF test assumes a non-zero mean but no time-based trend. The function returns the Dickey-Fuller 
#' test statistic, the critical values at 1 percent, 5 percent, and 10 percent, and the half-life of mean reversion assuming 
#' that the mean reversion follows a Ornstein–Uhlenbeck process. Note that the p-value is calculated from a distribution where 
#' the linear regression coefficients are not estimated from the data. Since the regression coefficients are estimated from 
#' the data, the p-value is biased downward, so interpretation of the p-value relative to critical values should be done with 
#' caution.  
#' 
#' Other methods considered were the Johansen test and the Phillips-Ouliaris cointegration test. Using different methods may 
#' lead to different conclusions when looking at test statistics relative to critical values. 
test_cointegration <- function(coin_y, coin_x) { 
  lm_model <- lm(coin_y ~ coin_x) 
  residuals <- lm_model[["residuals"]]
  adf_test <- ur.df(residuals, type = "drift", lags = 1) 
  ou_process <- lm(residuals - lag(residuals, 1) ~ lag(residuals, 1))
  return(list(df_stat = adf_test@testreg[["coefficients"]][2, 3], 
              crit_value_1pct = adf_test@cval[1, 1], 
              crit_value_5pct = adf_test@cval[1, 2], 
              crit_value_10pct = adf_test@cval[1, 3], 
              half_life = -log(2) / coef(ou_process)[2]))
} 

#' # 5. Create Coin Combinations 
#' Two sets of currency pairs are examined: currency pairs where USDT is the quote currency and currency pairs where BTC 
#' is the quote currency. All combinations of coins within each set of currency pairs are created. Combinations that consist 
#' of the coin with itself are removed. There are a total of 98 coin combinations. 
coins_usdt <- c("USDT_BTC", "USDT_DASH", "USDT_ETH", "USDT_LTC", "USDT_REP", "USDT_XMR", "USDT_ZEC")
coins_btc <- c("BTC_DASH", "BTC_ETH", "BTC_LTC", "BTC_REP", "BTC_XEM", "BTC_XMR", "BTC_ZEC", "BTC_DCR", "BTC_FCT", "BTC_LSK")
coin_pairs <- rbind(expand.grid(coins_usdt, coins_usdt), expand.grid(coins_btc, coins_btc)) %>% 
  as_tibble() %>% 
  rename(coin_y = Var1, coin_x = Var2) %>% 
  filter(coin_y != coin_x) %>% 
  mutate_if(is.factor, as.character) 
print(coin_pairs)

#' # 6. Test Cointegration on All Coin Pairs 
#' The Engle-Granger method is tested on all coin combinations generated above. The Dickey-Fuller statistic, critical values at 
#' the 1 percent, 5 percent, and 10 percent level, and the half-life of mean reversion for each coin combination are generated. 
#' Of the 98 coin combinations, 11 of the coins have a p-value of less than 10 percent. Most of these coins have a half-life of 
#' mean reversion of less than 10 days. However, many of the highly cointegrated coin pairs are comprised of coins that may 
#' individually exhiibt some stationarity. See BTC_DASH for example. If an individual coin is stationary, any trivial combination 
#' of that coin with another coin could be made stationary as well. 
df_stat <- c() 
crit_value_1pct <- c()
crit_value_5pct <- c()
crit_value_10pct <- c()
half_life <- c()
for (n in 1:nrow(coin_pairs)) { 
  coin_y <- coin_pairs[[n, "coin_y"]]
  coin_x <- coin_pairs[[n, "coin_x"]] 
  cointegration_results<- test_cointegration(pricing_data_a[[coin_y]], 
                                             pricing_data_a[[coin_x]])
  print(str_c("Testing cointegration of ", coin_y, " and ", coin_x, "."))
  df_stat <- c(df_stat, cointegration_results[["df_stat"]])
  crit_value_1pct <- c(crit_value_1pct, cointegration_results[["crit_value_1pct"]])
  crit_value_5pct <- c(crit_value_5pct, cointegration_results[["crit_value_5pct"]])
  crit_value_10pct <- c(crit_value_10pct, cointegration_results[["crit_value_10pct"]])
  half_life <- c(half_life, cointegration_results[["half_life"]])
}
coin_pairs <- coin_pairs %>% 
  mutate(df_stat = df_stat, 
         crit_value_1pct = crit_value_1pct, 
         crit_value_5pct = crit_value_5pct, 
         crit_value_10pct = crit_value_10pct, 
         half_life = half_life) %>% 
  arrange(df_stat)
rm(coin_x, coin_y, cointegration_results, df_stat, crit_value_1pct, crit_value_5pct, crit_value_10pct, half_life, n)
knitr::kable(coin_pairs)

#' # 7. Plot Cointegrated Coins 
#' For the top 40 coin combinations ranked by the Dickey-Fuller test statistic, the coin_y and the coin_x * hedge_ratio 
#' are plotted. The residuals of the linear regression of coin_y on coin_x are also plotted in a following plot. BTC_DASH 
#' appears to be itself stationary, so any trivial combination of BTC_DASH and another coin is likely to also be stationary. 
plot_coins <- function(coin_y, coin_x) { 
  m <- lm(coin_y ~ coin_x, data = pricing_data_a)
  print(summary(m))
  print(ggplot(pricing_data_a, aes(x = date_time)) + 
    geom_line(aes(y = coin_y), colour = "blue") + 
    geom_line(aes(y = coef(m)[1] + coin_x * coef(m)[2]), colour = "red"))
  print(ggplot(pricing_data_a, aes(x = date_time)) + 
    geom_line(aes(y = m[["residuals"]]), colour = "blue") + 
    geom_hline(yintercept = 0))
}
for (i in 1:40) { 
  coin_y <- coin_pairs[["coin_y"]][i] 
  coin_x <- coin_pairs[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(coin_y = pricing_data_a[[coin_y]], coin_x = pricing_data_a[[coin_x]])
}

