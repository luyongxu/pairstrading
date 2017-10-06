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

#' # 3. Subset and Spread Data Function
prepare_data <- function(time_resolution, start_date, end_date) { 
  df <- pricing_data %>% 
    filter(period == time_resolution, 
           date_time >= start_date, 
           date_time <= end_date) %>% 
    select(date_unix, date_time, close, currency_pair) %>% 
    spread(currency_pair, close) 
  return(df)
}

#' # 4. Test Cointegration Function 
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
create_coins <- function() { 
  coins_usdt <- c("USDT_BTC", "USDT_DASH", "USDT_ETH", "USDT_LTC", "USDT_REP", "USDT_XEM", "USDT_XMR", "USDT_ZEC")
  coins_btc <- c("BTC_DASH", "BTC_ETH", "BTC_LTC", "BTC_REP", "BTC_XEM", "BTC_XMR", "BTC_ZEC")
  coin_pairs <- rbind(expand.grid(coins_usdt, coins_usdt), expand.grid(coins_btc, coins_btc)) %>% 
    as_tibble() %>% 
    rename(coin_y = Var1, coin_x = Var2) %>% 
    filter(coin_y != coin_x) %>% 
    mutate_if(is.factor, as.character) 
  return(coin_pairs)
}

#' # 6. Create Summary Statistics
create_summary <- function(pricing_data, coin_pairs) { 
  df_stat <- c() 
  crit_value_1pct <- c()
  crit_value_5pct <- c()
  crit_value_10pct <- c()
  half_life <- c()
  for (n in 1:nrow(coin_pairs)) { 
    coin_y <- coin_pairs[[n, "coin_y"]]
    coin_x <- coin_pairs[[n, "coin_x"]] 
    cointegration_results<- test_cointegration(pricing_data[[coin_y]], 
                                               pricing_data[[coin_x]])
    df_stat <- c(df_stat, cointegration_results[["df_stat"]])
    crit_value_1pct <- c(crit_value_1pct, cointegration_results[["crit_value_1pct"]])
    crit_value_5pct <- c(crit_value_5pct, cointegration_results[["crit_value_5pct"]])
    crit_value_10pct <- c(crit_value_10pct, cointegration_results[["crit_value_10pct"]])
    half_life <- c(half_life, cointegration_results[["half_life"]])
  }
  summary <- coin_pairs %>% 
    mutate(df_stat = df_stat, 
           crit_value_1pct = crit_value_1pct, 
           crit_value_5pct = crit_value_5pct, 
           crit_value_10pct = crit_value_10pct, 
           half_life = half_life) %>% 
    arrange(df_stat) %>% 
    summarise(df_stat = mean(df_stat), 
              crit_value_1pct = mean(crit_value_1pct), 
              crit_value_5pct = mean(crit_value_5pct), 
              crit_value_10pct = mean(crit_value_10pct), 
              half_life = mean(half_life))
  return(summary)
}

#' # 7. Simulate 
results <- tibble() 
set.seed(5)
for (i in 1:10) { 
  time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
  start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
  end_date <- sample(seq(from = start_date, to = Sys.Date(), by = "1 day"), size = 1) 
  print(str_c("Testing for time resolution ", time_resolution, " from ", start_date, " to ", end_date, "."))
  df_a <- prepare_data(time_resolution = time_resolution, 
                       start_date = start_date, 
                       end_date = end_date) 
  df_b <- create_summary(pricing_data = df_a, 
                         coin_pairs = create_coins()) %>% 
    mutate(time_resolution = time_resolution, 
           start_date = start_date, 
           end_date = end_date, 
           length = start_date - end_date) 
  results <- bind_rows(results, df_b)
}

#' # 8. Results
knitr::kable(results)
