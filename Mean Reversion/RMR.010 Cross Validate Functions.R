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
source("./Mean Reversion/RMR.001 Load Packages.R") 

#' # 2. Parameter List 
#' Description  
#' A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#' 
#' Arguments  
#' time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, 
#'   and 86400.   
#' quote_currency: A string indicating the quote currency of the currency pairs. Takes values "USDT" or "BTC".  
#' cointegration_test: A string indicating whether the Engle-Granger method or distance method is used to test for 
#'   cointegration. Takes values "eg" or "distance".  
#' adf_threshold: The threshold for the ADF test statistic. Pairs below this threshold are selected when using 
#'   the Engle-Granger method.  
#' distance_threshold: The number of coin pairs to select when using the distance method.  
#' train_window: A lubridate period object representing the length of time the train set covers.  
#' test_window: A lubridate period object representing the length of time the the test set covers.  
#' model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' spread_type: A string indicating whether the regression uses a rolling or fixed window. Takes value "rolling" or 
#'   "fixed".  
#' rolling_window: The number of observations used in each window of a rolling linear regression.  
#' signal_logic: A string indicating which logic to use to generate signals. Takes values "scaled" or "discrete".  
#' signal_scaled_enter: The z-score threshold indicating the z-score that the signal is fully scaled in when the 
#'   signal logic is scaled.  
#' signal_discrete_enter: The z-score threshold for entering a position when the signal logic is discrete.  
#' signal_discrete_exit: The z-score threshold for exiting a position when the signal logic is discrete.  
#' signal_stop: A threshold for the spread z-score beyond which the strategy stops trading the coin pair.  
#' signal_reenter: A string indicating whether the strategy should reenter positions after exceeding the 
#'   signal_stop threshold once the spread z-score returns to a reasonable range.    
#' pair_allocation: A string indicating whether the capital allocation to the coin pairs should be equal or weighted. Takes values 
#'   "equal" and "weighted".   

#' # 3. Prepare Data Function 
#' Description  
#' Spreads Poloneix pricing data into wide format and filters data to a specified time resolution and time window.  
#' 
#' Arguments  
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' start_date: The start date of the time window.  
#' end_date: The end date of the time window.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' 
#' Value  
#' Returns a dataframe consiting of the unix timestamp, date time, and the closing price of various currency pairs.  
prepare_data <- function(pricing_data, start_date, end_date, params) { 
  df <- pricing_data %>% 
    filter(period == params[["time_resolution"]], 
           date_time >= start_date, 
           date_time <= end_date) %>% 
    select(date_unix, date_time, close, currency_pair) %>% 
    spread(currency_pair, close) 
  return(df)
} 

#' # 3. Test Cointegration Function  
#' Description  
#' Cointegration is tested using either the distance method or the Engle-Granger two step method. The Engle-Granger consists 
#' of two steps:  (1) Perform a linear regression of log(coin_y) on log(coin_x). (2) Perform an Augmented Dickey-Fuller test 
#' on the residuals from the linear regression estimated in (1). The ADF test specification is of a non-zero mean, no 
#' time-based trend, and one autoregressive lag. The distance method calculates the sum of squared differences between the 
#' two coins's normalized prices.  
#' 
#' Arguments  
#' coin_y: A vector containing the pricing data for the dependent coin in the regression.  
#' coin_x: A vector containing the pricing data for the independent coin in the regression.  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#'   cointegration_test: A string indicating whether the Engle-Granger method or distance method is used to test for 
#'     cointegration. Takes values "eg" or "distance". 
#'   model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' 
#' Value  
#' Returns the cointegration test statistic for the given coin pair.  
test_cointegration <- function(coin_y, coin_x, params) { 
  
  # Test for cointegration using the Engle-Granger two step method
  if (params[["cointegration_test"]] == "eg") { 
    if (params[["model_type"]] == "raw") 
      lm_model <- lm.fit(y = coin_y, x = cbind(1, coin_x))   
    if (params[["model_type"]] == "log") 
      lm_model <- lm.fit(y = log(coin_y), x = cbind(1, log(coin_x))) 
    lm_residuals <- lm_model[["residuals"]] 
    adf_test <- ur.df(lm_residuals, type = "drift", lags = 1) 
    adf_stat = adf_test@testreg[["coefficients"]][2, 3]
    return(adf_stat) 
  } 
  
  # Test for cointegration using the distance method 
  if (params[["cointegration_test"]] == "distance") { 
    coin_y <- coin_y / coin_y[1] 
    coin_x <- coin_x / coin_x[1] 
    ssd = sum((coin_y - coin_x)^2) 
    return(ssd) 
  }
  
} 

#' # 4. Create Coin Pairs Function  
#' Description  
#' Two sets of currency pairs are examined: currency pairs where USDT is the quote currency and currency pairs where BTC 
#' is the quote currency. All combinations of coins are created within a given quote currency. Combinations that consist 
#' of the coin with itself are removed.  
#' 
#' Arguments  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#'   quote_currency: A string indicating the quote currency of the currency pairs. Can take values USDT or BTC.  
#' 
#' Value  
#' Returns a dataframe containing the coin pairs.  
create_pairs <- function(params) { 
  if (params[["quote_currency"]] == "USDT") 
    coin_list <- c("USDT_BTC", "USDT_DASH", "USDT_ETH", "USDT_LTC", "USDT_REP", "USDT_XMR", "USDT_ZEC")
  if (params[["quote_currency"]] == "BTC") 
    coin_list <- c("BTC_DASH", "BTC_ETH", "BTC_LTC", "BTC_REP", "BTC_XEM", "BTC_XMR", "BTC_ZEC")
  coin_pairs <- expand.grid(coin_list, coin_list) %>% 
    rename(coin_y = Var1, 
           coin_x = Var2) %>% 
    filter(coin_y != coin_x) %>% 
    mutate_if(is.factor, as.character) %>%
    as_tibble() 
  return(coin_pairs)
} 

#' # 5. Test Coin Pairs Function  
#' Description  
#' Test for cointegration between each coin pair generated by the create_pairs() function. The test for cointegration is
#' performed by test_cointegration(). 
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pairs.  
#' coin_pairs: A dataframe generated by create_pairs().  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#' 
#' Value  
#' Returns a dataframe containing the coin pairs and the ADF test statistic resulting from testing cointegration 
#' between each coin pair.  
test_pairs <- function(train, coin_pairs, params) { 
  cointegration_stat <- numeric(nrow(coin_pairs))  
  for (i in 1:nrow(coin_pairs)) { 
    coin_y <- coin_pairs[[i, "coin_y"]] 
    coin_x <- coin_pairs[[i, "coin_x"]] 
    cointegration_stat[i] <- test_cointegration(coin_y = train[[coin_y]], 
                                                coin_x = train[[coin_x]], 
                                                params = params)
  } 
  df <- coin_pairs %>% 
    mutate(cointegration_stat = cointegration_stat) %>% 
    arrange(cointegration_stat)
  return(df) 
} 

#' # 6. Select Coin Pairs Function 
#' Description  
#' Select cointegrated coin pairs to be used in a mean reversion strategy. Coin pairs are tested for cointegration 
#' using test_pairs(). Coin pairs below a certain threshold are selected.  
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pair.  
#' coin_pairs: A dataframe generated by create_pairs().  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#'   adf_threshold: The threshold for the ADF test statistic. Pairs below this threshold are selected when using 
#'     the Engle-Granger method. 
#'   distance_threshold: The number of coin pairs to select when using the distance method. 
#' 
#' Value  
#' Returns a dataframe containing the coin pairs that were selected.  
select_pairs <- function(train, coin_pairs, params) { 
  
  # Test for cointegration for all coin pairs
  df <- test_pairs(train = train, 
                   coin_pairs = coin_pairs, 
                   params = params)
  
  # If cointegration test uesd the Engle-Granger method, filter by adf threshold 
  if (params[["cointegration_test"]] == "eg") { 
    df <- df %>% 
      filter(cointegration_stat <= params[["adf_threshold"]]) 
    return(df) 
  }

  # If cointegration used the distance method, filter by number of coins to select 
  if (params[["cointegration_test"]] == "distance") { 
    df <- df %>% 
      filter(row_number() <= params[["distance_threshold"]]) 
    return(df)
  }
  
} 

#' # 7. Train Model Function 
#' Description   
#' Performs rolling linear regression of coin y on coin x using a defined rolling window length over 
#' the test set.  
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pair.  
#' test: A dataframe generated by prepare_data() that represents the test set for the coin pair.  
#' coin_y: A string indicating the dependent coin in the coin pair regression.  
#' coin_x: A string indicating the independent coin in the coin pair regression.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   rolling_window: The number of observations used in each iteration of a rolling linear regression.  
#'   model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log". 
#'   spread_type: A string indicating whether the regression uses a rolling or fixed window. Takes value "rolling" or "fixed".  
#' 
#' Value  
#' Returns a list containing the intercept, hedge ratio, spread, and spread z-score calculated from a rolling 
#' regression over the test set. 
train_model <- function(train, test, coin_y, coin_x, params) { 
  
  # If calculation of spread uses a rolling regression 
  if (params[["spread_type"]] == "rolling") { 
    
    # Set y and x depending on model type
    if (params[["model_type"]] == "raw") { 
      rolling_coef <- bind_rows(train, test) %>%  
        mutate(y = .[[coin_y]], 
               x = .[[coin_x]]) %>% 
        select(y, x)
    } 
    if (params[["model_type"]] == "log") { 
      rolling_coef <- bind_rows(train, test) %>%  
        mutate(y = log(.[[coin_y]]), 
               x = log(.[[coin_x]])) %>% 
        select(y, x)
    } 
    
    # Perform rolling linear regression over the test set  
    rolling_coef <- rolling_coef %>% 
      rollapply(data = ., 
                width = params[["rolling_window"]], 
                FUN = function(df) { 
                  df <- as_tibble(df)
                  model <- lm.fit(y = df[["y"]], x = cbind(1, df[["x"]]))
                  return(model[["coefficients"]])
                }, 
                by.column = FALSE, 
                fill = NA, 
                align = "right") %>% 
      as_tibble() %>% 
      rename(intercept = x1, 
             hedge_ratio = x2) %>% 
      filter(row_number() > nrow(train)) 
    
    # Calculate spread in training and test set  
    if (params[["model_type"]] == "raw") { 
      train <- train %>% 
        mutate(spread = lm.fit(y = train[[coin_y]], x = cbind(1, train[[coin_x]]))[["residuals"]])
      test <- test %>% 
        mutate(spread = test[[coin_y]] - test[[coin_x]] * rolling_coef[["hedge_ratio"]] - rolling_coef[["intercept"]]) 
    } 
    if (params[["model_type"]] == "log") { 
      train <- train %>% 
        mutate(spread = lm.fit(y = log(train[[coin_y]]), x = cbind(1, log(train[[coin_x]])))[["residuals"]]) 
      test <- test %>% 
        mutate(spread = log(test[[coin_y]]) - log(test[[coin_x]]) * rolling_coef[["hedge_ratio"]] - rolling_coef[["intercept"]]) 
    } 
    
    # Combine train and test to calculate rolling z-score for the test set  
    result <- bind_rows(train %>% mutate(source = "train"), 
                        test %>% mutate(source = "test")) %>% 
      mutate(rolling_mean = roll_mean(spread, n = params[["rolling_window"]], fill = NA, align = "right"), 
             rolling_sd = roll_sd(spread, n = params[["rolling_window"]], fill = NA, align = "right"), 
             spread_z = (spread - rolling_mean) / rolling_sd) %>% 
      filter(source == "test") 
    
    # Return list of statistics for the test set  
    return(list(intercept = rolling_coef[["intercept"]], 
                hedge_ratio = rolling_coef[["hedge_ratio"]], 
                spread = result[["spread"]], 
                spread_z = result[["spread_z"]]))
  }
  
  # If calculation of spread uses a model with fixed coefficients estimated over the training set 
  if (params[["spread_type"]] == "fixed") { 
    
    # If regression uses raw prices 
    if (params[["model_type"]] == "raw") { 
      model <- lm.fit(y = train[[coin_y]], x = cbind(1, train[[coin_x]])) 
      intercept <- coef(model)[1] 
      hedge_ratio <- coef(model)[2] 
      result <- test %>% 
        mutate(spread = test[[coin_y]] - test[[coin_x]] * hedge_ratio - intercept, 
               spread_z = (spread - mean(model[["residuals"]])) / sd(model[["residuals"]]))
    }
    
    # If regression uses log prices 
    if (params[["model_type"]] == "log") { 
      model <- lm.fit(y = log(train[[coin_y]]), x = cbind(1, log(train[[coin_x]]))) 
      intercept <- coef(model)[1] 
      hedge_ratio <- coef(model)[2] 
      result <- test %>% 
        mutate(spread = log(test[[coin_y]]) - log(test[[coin_x]]) * hedge_ratio - intercept, 
               spread_z = (spread - mean(model[["residuals"]])) / sd(model[["residuals"]]))
    }
    
    # Return list of statistics for the test set  
    return(list(intercept = intercept, 
                hedge_ratio = hedge_ratio, 
                spread = result[["spread"]], 
                spread_z = result[["spread_z"]]))
  }
} 

#' # 8. Generate Signals Function 
#' Description  
#' Generate trading signals that indicate the current position in the spread formed by a trained model generated by  
#' train_model(). The trained model generates the spread and z-score of the spread over the test set. A signal of +1 
#' indicates a long position in the spread, 0 indicates a flat position, and -1 indicates a short position in the 
#' spread. 
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pair.  
#' test: A dataframe generated by prepare_data() that represents the test set for the coin pair.  
#' coin_y: A string indicating the dependent coin in the coin pair regression.  
#' coin_x: A string indicating the independent coin in the coin pair regression.  
#' model: A trained model generated by train_model(). 
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   signal_logic: A string indicating which logic to use to generate signals. Takes values "scaled" or "discrete".   
#'   signal_scaled_enter: The z-score threshold indicating the z-score that the signal is fully scaled in when the 
#'     signal logic is scaled.  
#'   signal_discrete_enter: The z-score threshold for entering a position when the signal logic is discrete.   
#'   signal_discrete_exit: The z-score threshold for exiting a position when the signal logic is discrete.  
#'   signal_stop: A threshold for the spread z-score beyond which the strategy stops trading the coin pair. 
#'   signal_reenter: A boolean indicating whether the strategy should reenter positions after exceeding the 
#'     signal_stop threshold once the spread z-score returns to a reasonable range.    
#' 
#' Value  
#' Returns a vector containing the trading signal over the test set.  
generate_signals <- function(train, test, coin_y, coin_x, model, params) { 
  
  # Scaled signal logic in which signal can take continuous values depending on the spread z-score
  if (params[["signal_logic"]] == "scaled") { 
    
    # Create signal strength depending on signal_scaled_enter parameter. Creates a signal strength vector 
    # that represents the signal at every increment in the spread z-score below.  
    if (params[["signal_scaled_enter"]] == 2.0) ss <- c(0.25, 0.50, 0.75, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)
    if (params[["signal_scaled_enter"]] == 3.0) ss <- c(0.33, 0.33, 0.67, 0.67, 1.00, 1.00, 1.00, 1.00, 1.00)
    if (params[["signal_scaled_enter"]] == 4.0) ss <- c(0.25, 0.25, 0.50, 0.50, 0.75, 1.00, 1.00, 1.00, 1.00)
    
    # calculate signal 
    df_signals <- test %>% 
      mutate(spread = model[["spread"]], 
             spread_z = model[["spread_z"]], 
             lag_spread_z = lag(spread_z, 1, default = 0), 
             signal_long = if_else(lag_spread_z <=  0.0 & lag_spread_z > -0.5, ss[1], 0), 
             signal_long = if_else(lag_spread_z <= -0.5 & lag_spread_z > -1.0, ss[2], signal_long), 
             signal_long = if_else(lag_spread_z <= -1.0 & lag_spread_z > -1.5, ss[3], signal_long), 
             signal_long = if_else(lag_spread_z <= -1.5 & lag_spread_z > -2.0, ss[4], signal_long), 
             signal_long = if_else(lag_spread_z <= -2.0 & lag_spread_z > -3.0, ss[5], signal_long), 
             signal_long = if_else(lag_spread_z <= -3.0 & lag_spread_z > -4.0, ss[6], signal_long), 
             signal_long = if_else(lag_spread_z <= -4.0 & lag_spread_z > -5.0, ss[7], signal_long), 
             signal_long = if_else(lag_spread_z <= -5.0 & lag_spread_z > -6.0, ss[8], signal_long), 
             signal_long = if_else(lag_spread_z <= -6.0 & lag_spread_z > -7.0, ss[9], signal_long), 
             signal_long = if_else(lag_spread_z <= -params[["signal_stop"]], 0, signal_long), 
             signal_short = if_else(lag_spread_z >= 0.0 & lag_spread_z < 0.5, -ss[1], 0), 
             signal_short = if_else(lag_spread_z >= 0.5 & lag_spread_z < 1.0, -ss[2], signal_short), 
             signal_short = if_else(lag_spread_z >= 1.0 & lag_spread_z < 1.5, -ss[3], signal_short), 
             signal_short = if_else(lag_spread_z >= 1.5 & lag_spread_z < 2.0, -ss[4], signal_short), 
             signal_short = if_else(lag_spread_z >= 2.0 & lag_spread_z < 3.0, -ss[5], signal_short), 
             signal_short = if_else(lag_spread_z >= 3.0 & lag_spread_z < 4.0, -ss[6], signal_short), 
             signal_short = if_else(lag_spread_z >= 4.0 & lag_spread_z < 5.0, -ss[7], signal_short), 
             signal_short = if_else(lag_spread_z >= 5.0 & lag_spread_z < 6.0, -ss[8], signal_short), 
             signal_short = if_else(lag_spread_z >= 6.0 & lag_spread_z < 7.0, -ss[9], signal_short), 
             signal_short = if_else(lag_spread_z >= params[["signal_stop"]], 0, signal_short), 
             signal = signal_long + signal_short)
  } 
  
  # Discrete signal logic in which signal can only take discrete values depending on the spread z-score 
  if (params[["signal_logic"]] == "discrete") { 
    
    df_signals <- test %>% 
      mutate(spread = model[["spread"]], 
             spread_z = model[["spread_z"]], 
             lag_spread_z = lag(spread_z, 1, default = 0), 
             signal_long = if_else(lag_spread_z <= -params[["signal_discrete_enter"]], 1, NA_real_), 
             signal_long = if_else(lag_spread_z >= -params[["signal_discrete_exit"]] , 0, signal_long), 
             signal_long = if_else(lag_spread_z <= -params[["signal_stop"]], 0, signal_long), 
             signal_long = na.locf(signal_long, na.rm = FALSE), 
             signal_short = if_else(lag_spread_z >= params[["signal_discrete_enter"]], -1, NA_real_), 
             signal_short = if_else(lag_spread_z <= params[["signal_discrete_exit"]], 0, signal_short), 
             signal_short = if_else(lag_spread_z >= params[["signal_stop"]], 0, signal_short), 
             signal_short = na.locf(signal_short, na.rm = FALSE), 
             signal = signal_long + signal_short) 
  } 
  
  # Set signal stopping logic 
  if (params[["signal_reenter"]] == FALSE) { 
    df_signals <- df_signals %>% 
      mutate(signal = if_else(is.na(signal), 0, signal), 
             signal = if_else(cummin(lag_spread_z) <= -params[["signal_stop"]], 0, signal), 
             signal = if_else(cummax(lag_spread_z) >=  params[["signal_stop"]], 0, signal))
  } 
  if (params[["signal_reenter"]] == TRUE) { 
    df_signals <- df_signals %>% 
      mutate(reenter = if_else(cummax(abs(lag_spread_z)) >= params[["signal_stop"]] & abs(lag_spread_z) <= 1, 1, NA_real_), 
             reenter = na.locf(reenter, na.rm = FALSE), 
             signal = if_else(is.na(signal), 0, signal), 
             signal = if_else(cummin(lag_spread_z) <= -params[["signal_stop"]] & is.na(reenter), 0, signal), 
             signal = if_else(cummax(lag_spread_z) >=  params[["signal_stop"]] & is.na(reenter), 0, signal))   
  } 
  
  # Return signal over test set 
  return(df_signals[["signal"]])
} 

#' # 9. Backtest Pair Function 
#' Description  
#' Calculate the return of a cointegration-based mean reversion trading strategy using coin y and coin x. 
#' 
#' The current backtesting logic uses a model generated by train_model() and trading signals generated by 
#' generate_signals(). The coin_y_return and coin_x_return indicate the one period percentage return of 
#' each coin. The coin_y_position and coin_x_position indicate the market value in USD in each coin. 
#' coin_y_pnl and coin_x_pnl indicate the USD value of the profit and loss for each coin. The combined_position 
#' indicates the gross market value of the combined positions. The return is calculated relative to the maximum 
#' capital allocation to the given coin pair.  
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pair.  
#' test: A dataframe generated by prepare_data() that represents the test set for the coin pair.  
#' coin_y: A string indicating the dependent coin in the coin pair regression.  
#' coin_x: A string indicating the independent coin in the coin pair regression.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' 
#' Value  
#' Returns a vector containing the cumulative return of applying the trading strategy to the given coin pair.  
backtest_pair <- function(train, test, coin_y, coin_x, params) { 
  
  # Generate model for calculating spread z-score 
  model <- train_model(train = train, 
                       test = test, 
                       coin_y = coin_y, 
                       coin_x = coin_x, 
                       params = params)
  
  # Return calculations if model type uses raw prices to test for cointegration 
  if (params[["model_type"]] == "raw") { 
    df_backtest <- test %>% 
      mutate(signal = generate_signals(train = train, 
                                       test = test, 
                                       coin_y = coin_y, 
                                       coin_x = coin_x, 
                                       model = model, 
                                       params = params), 
             coin_y_return = test[[coin_y]] / lag(test[[coin_y]], 1) - 1, 
             coin_x_return = test[[coin_x]] / lag(test[[coin_x]], 1) - 1, 
             coin_y_position = test[[coin_y]] * signal * 1                      *  1, 
             coin_x_position = test[[coin_x]] * signal * model[["hedge_ratio"]] * -1,  
             coin_y_pnl = lag(coin_y_position, 1) * coin_y_return, 
             coin_x_pnl = lag(coin_x_position, 1) * coin_x_return, 
             combined_position = abs(coin_y_position) + abs(coin_x_position), 
             combined_pnl = coin_y_pnl + coin_x_pnl, 
             combined_return = combined_pnl / lag(combined_position, 1)) %>% 
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
      mutate(return_pair = cumprod(1 + combined_return)) 
  } 
  
  # Return calculations if model uses log prices to test for cointegration 
  if (params[["model_type"]] == "log") { 
    df_backtest <- test %>% 
      mutate(signal = generate_signals(train = train, 
                                       test = test, 
                                       coin_y = coin_y, 
                                       coin_x = coin_x, 
                                       model = model, 
                                       params = params), 
             coin_y_return = test[[coin_y]] / lag(test[[coin_y]], 1) - 1, 
             coin_x_return = test[[coin_x]] / lag(test[[coin_x]], 1) - 1, 
             coin_y_position = signal * 1                      *  1, 
             coin_x_position = signal * model[["hedge_ratio"]] * -1,  
             coin_y_pnl = lag(coin_y_position, 1) * coin_y_return, 
             coin_x_pnl = lag(coin_x_position, 1) * coin_x_return, 
             combined_position = abs(coin_y_position) + abs(coin_x_position), 
             combined_pnl = coin_y_pnl + coin_x_pnl, 
             combined_return = combined_pnl / (1 + abs(model[["hedge_ratio"]]))) %>% 
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
      mutate(return_pair = cumprod(1 + combined_return)) 
  } 
  
  # Return cumulative return of the trading strategy on a coin pair 
  return(df_backtest[["return_pair"]])
} 

#' # 10. Backtest Strategy Function 
#' Description  
#' Calculate the return of a cointegration-based mean reversion trading strategy using an equally weighted 
#' portfolio of cointegrated coin pairs. The cumulative return of each coin pair is calculated using 
#' return_pair() and the mean is taken. The backtest evaluates the performance over a single test set. 
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pairs.  
#' test: A dataframe generated by prepare_data() that represents the test set for the coin pairs.  
#' selected_pairs: A dataframe generated by select_coins() that represents a set of cointegrated coin pairs.   
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy. 
#'   pair_allocation: A string indicating whether the capital allocation to the coin pairs should be equal or weighted. Takes values 
#'     "equal" and "weighted".   
#'   cointegration_test: A string indicating whether the Engle-Granger method or distance method is used to test for 
#'     cointegration. Takes values "eg" or "distance". 
#' Value  
#' A vector containing the cumulative return of the overall trading strategy for a given train and test split.  
backtest_strategy <- function(train, test, selected_pairs, params) { 
  
  # Return cumulative return of 1 if no pairs are selected 
  if (nrow(selected_pairs) == 0) 
    return(1) 
  
  # Iterate through each coin pair and calculate the return of the strategy on a coin pair 
  df <- tibble()  
  for (i in 1:nrow(selected_pairs)) { 
    single_pair <- tibble(return_pair = backtest_pair(train = train, 
                                                      test = test, 
                                                      coin_y = selected_pairs[["coin_y"]][i], 
                                                      coin_x = selected_pairs[["coin_x"]][i], 
                                                      params = params), 
                          coin_y = selected_pairs[["coin_y"]][i], 
                          coin_x = selected_pairs[["coin_x"]][i], 
                          date_time = test[["date_time"]], 
                          cointegration_stat = selected_pairs[["cointegration_stat"]][i]) 
    df <- bind_rows(df, single_pair)
  } 
  
  # Calculate return of the strategy applied to a portfolio of coin pairs assuming equal capital allocation 
  # to each coin pair 
  if (params[["pair_allocation"]] == "equal") { 
    df <- df %>% 
      group_by(date_time) %>% 
      summarise(return_strategy = mean(return_pair)) 
  } 
  
  # Calculate return of the strategy applied to a portfolio of coin pairs assuming weighted capital allocation 
  # to each coin pair depending on how cointegrated they are 
  if (params[["pair_allocation"]] == "weighted") { 
    
    # Calculate weights if cointegration test used Engle-Grange method 
    if (params[["cointegration_test"]] == "eg") { 
      df <- df %>% 
        mutate(cointegration_stat = abs(cointegration_stat))
    }
    
    # Calculate weights if cointegration test used distance method 
    if (params[["cointegration_test"]] == "distance") { 
      df <- df %>% 
        mutate(cointegration_stat = 1 / cointegration_stat)
    }
    
    # Calculate weighted average of the strategy return 
    df <- df %>% 
      group_by(date_time) %>% 
      summarise(return_strategy = weighted.mean(x = return_pair, w = cointegration_stat)) 
  }
  
  # Return the stratwgy return
  return(df[["return_strategy"]])
} 

#' # 11. Backtest Strategy Full Function 
#' Description  
#' Calculate the return of a cointegration-based mean reversion trading strategy using an equally weighted 
#' portfolio of cointegrated coin pairs. The backtest evaluates the performance in a timeseries cross validation 
#' method in which train and test sets are created iteratively over time. 
#' 
#' Arguments  
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#'   train_window: A period object from lubridate representing the length of time the train set covers.  
#'   test_window: A period object from lubridate representing the length of time the the test set covers. 
#' 
#' Value   
#' A dataframe containing the cumulative return of the overall trading strategy calculated in using a timeseries 
#' cross validation method. 
backtest_strategy_full <- function(pricing_data, params) { 
  
  # Create vector of cutoff dates for train and test sets over time using a time series cross validation approach 
  cutoff_dates <- seq(ymd("2017-01-01"), ymd("2017-10-01"), by = str_c(day(params[["test_window"]]), " days"))
  
  # Iterate through each cutoff date and calculate the strategy return for each test set 
  results <- tibble() 
  for (cutoff_date in cutoff_dates) {  
    
    # Print dates that train and test sets cover 
    cutoff_date <- as.Date(cutoff_date) 
    print(str_c("Cross validating strategy."))
    print(str_c("Using train set from ", cutoff_date - params[["train_window"]] , " to ", cutoff_date, ".")) 
    print(str_c("Using test set from ", cutoff_date, " to ", cutoff_date + params[["test_window"]], "."))  
    
    # Create train and test sets 
    train <- prepare_data(pricing_data = pricing_data, 
                          start_date = cutoff_date - params[["train_window"]], 
                          end_date = cutoff_date, 
                          params = params) 
    test <- prepare_data(pricing_data = pricing_data, 
                         start_date = cutoff_date, 
                         end_date = cutoff_date + params[["test_window"]], 
                         params = params) 
    
    # Select cointegrated coin pairs 
    coin_pairs <- create_pairs(params = params) 
    selected_pairs <- select_pairs(train = train, 
                                   coin_pairs = coin_pairs, 
                                   params = params) 
    
    # Calculate strategy return over single test set 
    test <- test %>% 
      mutate(return_strategy = backtest_strategy(train = train, 
                                                 test = test, 
                                                 selected_pairs = selected_pairs, 
                                                 params = params), 
             return_strategy_change = return_strategy / lag(return_strategy, 1) - 1) %>% 
      mutate_all(funs(ifelse(is.na(.), 0, .)))
    results <- bind_rows(results, test) 
  } 
  
  # Calculate strategy return using all test sets 
  results <- results %>% 
    mutate(return_strategy_cumulative = cumprod(1 + return_strategy_change), 
           date_time = as.POSIXct(date_time, origin = "1970-01-01")) %>% 
    select(date_time, return_strategy_cumulative)
  return(results)
} 

#' # 12. Plot Single Function 
#' Description  
#' Create plots of a cointegration-based mean reversion trading strategy of a single coin pair conprised of 
#' coin y and coin x. There are two plots created by this function. The first plot displays the spread transformed 
#' into z-score with three red lines at -2, 0, and 2. A green line indicates the signal which can take values -1, 
#' 0, and +1. The second plot displays the cumulative return of the model in blue. Two additional lines show the buy 
#' and hold return of coin y and coin x as red and green lines, respectively. 
#' 
#' Arguments  
#' train: A dataframe generated by prepare_data() that represents the training set for the coin pair.  
#' test: A dataframe generated by prepare_data() that represents the test set for the coin pair.  
#' coin_y: A string indicating the dependent coin in the coin pair regression.  
#' coin_x: A string indicating the independent coin in the coin pair regression. 
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#' 
#' Value  
#' Prints the plots described above.  
plot_single <- function(train, test, coin_y, coin_x, params) { 
  
  # Generate model for calculating spread z-score 
  model <- train_model(train = train, 
                       test = test, 
                       coin_y = coin_y, 
                       coin_x = coin_x, 
                       params = params) 
  
  # Calculate signal and strategy return for the coin pair 
  df_plot <- test %>% 
    mutate(spread = model[["spread"]], 
           spread_z = model[["spread_z"]], 
           signal = generate_signals(train = train, 
                                     test = test, 
                                     coin_y = coin_y, 
                                     coin_x = coin_x, 
                                     model = model, 
                                     params = params), 
           return_pair = backtest_pair(train = train, 
                                       test = test, 
                                       coin_y = coin_y, 
                                       coin_x = coin_x, 
                                       params = params), 
           return_buyhold_y = test[[coin_y]] / test[[coin_y]][1], 
           return_buyhold_x = test[[coin_x]] / test[[coin_x]][1]) 
  
  # This plot plots the spread z-score and signal 
  print(ggplot(df_plot, aes(x = date_time)) + 
          geom_line(aes(y = spread_z, colour = "Spread Z"), size = 1) + 
          geom_line(aes(y = signal, colour = "Signal"), size = 0.5) + 
          geom_hline(yintercept = 0, colour = "red", alpha = 0.5) + 
          geom_hline(yintercept = 2, colour = "red", alpha = 0.5) + 
          geom_hline(yintercept = -2, colour = "red", alpha = 0.5) + 
          scale_color_manual(name = "Series", values = c("Spread Z" = "blue", "Signal" = "green")) + 
          labs(title = "Spread vs Trading Signal", subtitle = str_c(coin_y, " and ", coin_x), 
               x = "Date", y = "Spread and Signal")) 
  
  # This plot plots the return of the strategy versus the buy-and-hold return of each coin 
  print(ggplot(df_plot, aes(x = date_time)) + 
          geom_line(aes(y = return_pair, colour = "Model"), size = 1) + 
          geom_line(aes(y = return_buyhold_y, colour = "Coin Y"), size = 0.5, alpha = 0.4) + 
          geom_line(aes(y = return_buyhold_x, colour = "Coin X"), size = 0.5, alpha = 0.4) + 
          geom_hline(yintercept = 1, colour = "black") + 
          scale_color_manual(name = "Return", values = c("Model" = "darkblue", "Coin Y" = "darkred", "Coin X" = "darkgreen")) + 
          labs(title = "Model Return vs Buy Hold Return", subtitle = str_c(coin_y, " and ", coin_x), 
               x = "Date", y = "Cumulative Return"))
} 

#' # 13. Plot Many Function 
#' Description  
#' Create many plots by calling the plot_single() function multiple times. Also creates a plot showing the results of the 
#' overall strategy using backtest_strateg(). Creates a train and test set surrounding a cutoff date and creates plot for 
#' the top n coins ranked by their ADF statistic. 
#' 
#' Arguments   
#' pricing_data: A dataframe containing pricing data from Poloneix gathered in tidy format.  
#' cutoff_date: A data representing the cutoff date between the train and test sets.  
#' params: A list of parameters passed to the functions below that describe the mean reversion pairs trading strategy.  
#'   time_resolution: The number of seconds that each observation spans. Takes values 300, 900, 1800, 7200, 14400, and 86400.  
#' number_pairs: The number of pairs to generate plots for.  
#' 
#' Value  
#' Prints the plots described above.  
plot_many <- function(pricing_data, cutoff_date, params, number_pairs) { 
  
  # Create train and test sets based on the cutoff date 
  train <- prepare_data(pricing_data = pricing_data, 
                        start_date = as.Date(cutoff_date) - params[["train_window"]], 
                        end_date = as.Date(cutoff_date), 
                        params = params) 
  test <- prepare_data(pricing_data = pricing_data, 
                       start_date = as.Date(cutoff_date), 
                       end_date = as.Date(cutoff_date) + params[["test_window"]], 
                       params = params) 
  
  # Select coin pairs 
  coin_pairs <- create_pairs(params = params)
  selected_pairs <- select_pairs(train = train, 
                                 coin_pairs = coin_pairs, 
                                 params = params) 
  
  # Create no plots if no coin pairs are selected 
  if (nrow(selected_pairs) == 0) 
    return("No coin pairs selected.")
  
  # For each coin pair, generate plots by calling plot_single()
  print(selected_pairs) 
  for (i in 1:min(number_pairs, nrow(selected_pairs))) { 
    plot_single(train = train, 
                test = test, 
                coin_y = selected_pairs[["coin_y"]][i], 
                coin_x = selected_pairs[["coin_x"]][i], 
                params = params)
  } 
  
  # Calculate overall return of strategy over this single test set and plot the cumulative return 
  test <- test %>% 
    mutate(return_strategy = backtest_strategy(train = train, 
                                               test = ., 
                                               selected_pairs = selected_pairs, 
                                               params = params)) 
  print(ggplot(test, aes(x = date_time)) + 
          geom_line(aes(y = return_strategy, colour = "Strategy"), size = 1) + 
          geom_line(aes(y = USDT_BTC / USDT_BTC[1], colour = "USDT_BTC"), size = 0.5, alpha = 0.4) + 
          geom_hline(yintercept = 1, colour = "black") + 
          scale_color_manual(name = "Return", values = c("Strategy" = "darkblue", "USDT_BTC" = "darkred")) + 
          labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return")) 
} 


