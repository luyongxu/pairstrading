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
source("./Mean Reversion/RMR.001 Load Packages.R")

#' # 2. Load Data
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data clean.csv")

#' # 3. Spread Data 
#' Data was previous gathered in tidy format. For preliminary analysis, filter data to daily time resolution and data after 
#' April 1, 2017. Based on some exploratory analysis, the number of cointegrated coins rapidly increased after April 1, 2017. 
#' This time period coincides with an extremely rapid rise of bitcoin (starting from roughly $1100) and many other coins.
pricing_data_a <- pricing_data %>% 
  filter(period == "86400", 
         date_time >= "2017-04-01") %>% 
  select(date_unix, date_time, close, currency_pair) %>% 
  spread(currency_pair, close)
print(pricing_data_a)

#' # 4. Cointegration Function 
#' The Engle-Granger method is used to test for cointegration. This method is comprised of two steps:  (1) Perform a linear 
#' regression of coin_y on coin_x. (2) Perform an Augmented Dickey-Fuller test on the residuals from the linear regression 
#' estimated in (1). The function returns both the Dickey-Fuller test statistic and the p-value. Note that the p-value is 
#' calculated from a distribution where the linear regression coefficients are not estimated from the data. Since the 
#' regression coefficients are estimated from the data, the p-value is biased downward, so interpretation of the p-value 
#' relative to critical values should be done with caution.  
#' 
#' Other methods considered were the Johansen test and the Phillips-Ouliaris cointegration test. Using different methods may 
#' lead to different conclusions when looking at test statistics relative to critical values. 
test_cointegration <- function(coin_y, coin_x) { 
  lm_model <- lm(coin_y ~ coin_x) 
  adf_test <- adf.test(lm_model[["residuals"]], alternative = "stationary", k = 1) 
  return(list(df_stat = adf_test[["statistic"]], 
              p_value = adf_test[["p.value"]]))
} 

#' # 5. Create Coin Combinations 
#' Two sets of currency pairs are examined: currency pairs where USDT is the quote currency and currency pairs where BTC 
#' is the quote currency. All combinations of coins within each set of currency pairs are created. Combinations that consist 
#' of the coin with itself are removed. There are a total of 98 coin combinations. 
coins_usdt <- c("USDT_BTC", "USDT_DASH", "USDT_ETH", "USDT_LTC", "USDT_REP", "USDT_XEM", "USDT_XMR", "USDT_ZEC")
coins_btc <- c("BTC_DASH", "BTC_ETH", "BTC_LTC", "BTC_REP", "BTC_XEM", "BTC_XMR", "BTC_ZEC")
coin_pairs <- rbind(expand.grid(coins_usdt, coins_usdt), expand.grid(coins_btc, coins_btc)) %>% 
  as_tibble() %>% 
  rename(coin_y = Var1, coin_x = Var2) %>% 
  filter(coin_y != coin_x)

#' # 6. Test Cointegration on All Coin Pairs 
#' The Engle-Granger method is tested on all coin combinations generated above. The Dickey-Fuller statistic and the p-value 
#' for each coin combination are generated. Of the 98 coin combinations, 13 of the coins have a p-value of less than 10 percent. 
df_stat <- c() 
p_value <- c()
for (n in 1:nrow(coin_pairs)) { 
  df_stat <- c(df_stat, 
               test_cointegration(pricing_data_a[[coin_pairs[[n, "coin_y"]]]], 
                                  pricing_data_a[[coin_pairs[[n, "coin_x"]]]]) %>% 
                 .[["df_stat"]])
  p_value <- c(p_value, 
               test_cointegration(pricing_data_a[[coin_pairs[[n, "coin_y"]]]], 
                                  pricing_data_a[[coin_pairs[[n, "coin_x"]]]]) %>% 
                 .[["p_value"]])
}
coin_pairs <- coin_pairs %>% 
  filter(coin_y != coin_x) %>%
  mutate(df_stat = df_stat, 
         p_value = p_value) %>% 
  arrange(df_stat)
rm(df_stat, p_value, n)
knitr::kable(coin_pairs)

#' # 7. Plot Cointegrated Coins 
#' For the 13 coin combinations with p-value of less than 10 percent, the coin_y and the fitted regression line are plotted. 
#' The residuals of the linear regression of coin_y on coin_x are also plotted. Most of the highly cointegrated coin combinations 
#' are from currency pairs that use USDT as the quote currency. 
plot_coins <- function(coin_y, coin_x){ 
  m <- lm(coin_y ~ coin_x, data = pricing_data_a)
  print(summary(m))
  print(ggplot(pricing_data_a, aes(x = date_time)) + 
    geom_line(aes(y = coin_y), colour = "blue") + 
    geom_line(aes(y = coin_x * coef(m)[2]), colour = "red"))
  print(ggplot(pricing_data_a, aes(x = date_time)) + 
    geom_line(aes(y = m[["residuals"]]), colour = "blue") + 
    geom_hline(yintercept = 0))
}
plot_coins(coin_y = pricing_data_a[["USDT_DASH"]], coin_x = pricing_data_a[["USDT_BTC"]])
plot_coins(coin_y = pricing_data_a[["USDT_BTC"]], coin_x = pricing_data_a[["USDT_DASH"]])
plot_coins(coin_y = pricing_data_a[["BTC_REP"]], coin_x = pricing_data_a[["BTC_XMR"]])
plot_coins(coin_y = pricing_data_a[["USDT_XEM"]], coin_x = pricing_data_a[["USDT_LTC"]])
plot_coins(coin_y = pricing_data_a[["USDT_ETH"]], coin_x = pricing_data_a[["USDT_XMR"]])
plot_coins(coin_y = pricing_data_a[["USDT_ETH"]], coin_x = pricing_data_a[["USDT_XEM"]])
plot_coins(coin_y = pricing_data_a[["USDT_BTC"]], coin_x = pricing_data_a[["USDT_ETH"]])
plot_coins(coin_y = pricing_data_a[["USDT_DASH"]], coin_x = pricing_data_a[["USDT_ETH"]])
plot_coins(coin_y = pricing_data_a[["USDT_ETH"]], coin_x = pricing_data_a[["USDT_BTC"]])
plot_coins(coin_y = pricing_data_a[["USDT_ETH"]], coin_x = pricing_data_a[["USDT_DASH"]])
plot_coins(coin_y = pricing_data_a[["BTC_XMR"]], coin_x = pricing_data_a[["BTC_REP"]])
plot_coins(coin_y = pricing_data_a[["USDT_ETH"]], coin_x = pricing_data_a[["USDT_REP"]])
plot_coins(coin_y = pricing_data_a[["BTC_XMR"]], coin_x = pricing_data_a[["BTC_DASH"]])