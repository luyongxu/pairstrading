#' ---
#' title: "Search Cointegration Combinations"
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

#' # 3. Subset and Spread Data Function 
#' Data was previous gathered in tidy format. This function spreads the data into wide format and filters the data based 
#' on the time_resolution parameter (which takes values 300, 900, 1800, 7200, 14400, and 86400 indicating the number of 
#' seconds per observation) and the date range based on the start_date and end_date parameters. 
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
#' The Engle-Granger method is used to test for cointegration. This method is comprised of two steps:  (1) Perform a linear 
#' regression of coin_y on coin_x. (2) Perform an Augmented Dickey-Fuller test on the residuals from the linear regression 
#' estimated in (1). The ADF test assumes a non-zero mean but no time-based trend. The function returns the Dickey-Fuller 
#' test statistic, the critical values at 1 percent, 5 percent, and 10 percent, and the half-life of mean reversion assuming 
#' that the mean reversion follows a Ornstein–Uhlenbeck process. Note that the p-value is calculated from a distribution where 
#' the linear regression coefficients are not estimated from the data. Since the regression coefficients are estimated from 
#' the data, the p-value is biased downward, so interpretation of the p-value relative to critical values should be done with 
#' caution. 
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

#' # 5. Create Coin Combinations Function 
#' Two sets of currency pairs are examined: currency pairs where USDT is the quote currency and currency pairs where BTC 
#' is the quote currency. All combinations of coins within each set of currency pairs are created. Combinations that consist 
#' of the coin with itself are removed. There are a total of 98 coin combinations. 
create_coins <- function() { 
  coins_usdt <- c("USDT_BTC", "USDT_DASH", "USDT_ETH", "USDT_LTC", "USDT_REP", "USDT_XMR", "USDT_ZEC")
  coins_btc <- c("BTC_DASH", "BTC_ETH", "BTC_LTC", "BTC_REP", "BTC_XEM", "BTC_XMR", "BTC_ZEC", "BTC_DCR", "BTC_FCT", "BTC_LSK")
  coin_pairs <- rbind(expand.grid(coins_usdt, coins_usdt), expand.grid(coins_btc, coins_btc)) %>% 
    as_tibble() %>% 
    rename(coin_y = Var1, coin_x = Var2) %>% 
    filter(coin_y != coin_x) %>% 
    mutate_if(is.factor, as.character) 
  return(coin_pairs)
}

#' # 6. Calculate Statistics For Each Coin Pair Function
#' The Engle-Granger method is tested on all coin combinations generated above. The Dickey-Fuller statistic, critical values at 
#' the 1 percent, 5 percent, and 10 percent level, and the half-life of mean reversion for each coin combination are generated. 
#' The mean values of all the statistics are then calculated and a dataframe is returned with those values. 
calculate_statistics <- function(pricing_data, coin_pairs) { 
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
  df <- coin_pairs %>% 
    mutate(df_stat = df_stat, 
           crit_value_1pct = crit_value_1pct, 
           crit_value_5pct = crit_value_5pct, 
           crit_value_10pct = crit_value_10pct, 
           half_life = half_life) %>% 
    arrange(df_stat)
  return(df)
} 

#' # 7. Plot Coins Function 
plot_coins <- function(df, coin_y, coin_x) { 
  m <- lm(coin_y ~ coin_x, data = df)
  print(summary(m))
  print(ggplot(df, aes(x = date_time)) + 
          geom_line(aes(y = coin_y), colour = "blue") + 
          geom_line(aes(y = coef(m)[1] + coin_x * coef(m)[2]), colour = "red"))
  print(ggplot(df, aes(x = date_time)) + 
          geom_line(aes(y = m[["residuals"]]), colour = "blue") + 
          geom_hline(yintercept = 0))
} 

#' # 8. Calculate Summary Statistics 
#' Given the coin pair statistics calculated in the calculate_statistics function, calculate the mean and median 
#' values of each statistic. 
calculate_summary <- function(statistics_df) { 
  df <- statistics_df %>% 
    summarise(df_stat_mean = mean(df_stat), 
              crit_value_1pct_mean = mean(crit_value_1pct), 
              crit_value_5pct_mean = mean(crit_value_5pct), 
              crit_value_10pct_mean = mean(crit_value_10pct), 
              half_life_mean = mean(half_life), 
              df_stat_median = median(df_stat), 
              crit_value_1pct_median = median(crit_value_1pct), 
              crit_value_5pct_median = median(crit_value_5pct), 
              crit_value_10pct_median = median(crit_value_10pct), 
              half_life_median = median(half_life))
}

#' # 9. Search Cointegration 
#' Given a random sampling of a time resolution and date range combination, generate the summary statistics generated by the 
#' create_summary function. THe idea behind this random search is to find the optimal time-resolution-date-range combination 
#' that exhiibts the most mean reversion. 
#' 
#' For rendering purposes, the number of iterations is set to 10 below. 
results <- tibble() 
set.seed(5)
for (i in 1:10) { 
  time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
  start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
  end_date <- sample(seq(from = start_date + 3, to = start_date + 50, by = "1 day"), size = 1) 
  print(str_c(i, ": Testing for time resolution ", time_resolution, " from ", start_date, " to ", end_date, "."))
  df_a <- prepare_data(time_resolution = time_resolution, 
                       start_date = start_date, 
                       end_date = end_date) 
  df_b <- calculate_statistics(pricing_data = df_a, 
                               coin_pairs = create_coins()) 
  df_c <- calculate_summary(statistics_df = df_b) %>% 
    mutate(time_resolution = time_resolution, 
           start_date = start_date, 
           end_date = end_date, 
           length = start_date - end_date) 
  results <- bind_rows(results, df_c)
}
print(results)

#' # 10. Results A 
#' An initial run of 18,600 iterations was completed using the following search parameters: 
# time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
# start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
# end_date <- sample(seq(from = start_date + 3, to = Sys.Date(), by = "1 day"), size = 1) 
#' These results were saved using the code below. 
# write_csv(results, "./output/tuning/search-cointegration-results-a.csv") 
results_a <- read_csv("./output/tuning/search-cointegration-results-a.csv")
print(results_a)

#' # 11. Results B 
#' A secondary run of 18,600 iterations was completed using the following search parameters: 
# time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
# start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
# end_date <- sample(seq(from = start_date + 3, to = start_date + 50, by = "1 day"), size = 1) 
#' These results were saved using the code below. 
# write_csv(results, "./output/tuning/search-cointegration-results-b.csv")
results_b <- read_csv("./output/tuning/search-cointegration-results-b.csv")
print(results_b)

#' # 12. Clean 
rm(i, time_resolution, end_date, start_date, df_a, df_b, df_c)
