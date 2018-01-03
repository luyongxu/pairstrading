#' ---
#' title: "Coin Selection Functions"
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

#' # 2. Test Cointegration Function  
#' Description  
#' Cointegration is tested using either the distance method, the Engle-Granger two step method, or total least squares 
#' method. The distance method calculates the root mean squared difference between the two coins's normalized prices.  
#' The Engle-Granger consists of two steps:  (1) Perform a linear regression of coin_y on coin_x. (2) Perform an 
#' Augmented Dickey-Fuller test on the residuals from the linear regression estimated in (1). The ADF test specification 
#' is of a non-zero mean, no time-based trend, and one autoregressive lag. The total least squares method is similar to 
#' the Engle-Granger method except it uses a total least squares regression in step 1 instead of ordinary least squares. 
#' 
#' Arguments  
#' coin_y: A vector containing the pricing data for the dependent coin in the regression.  
#' coin_x: A vector containing the pricing data for the independent coin in the regression.  
#' params: A list of parameters that describe the mean reversion pairs trading strategy.  
#'   cointegration_test: A string indicating whether the Engle-Granger method or distance method is used to test for 
#'     cointegration. Takes values "eg", "tls", or "distance".  
#'   model_type: A string indicating whether raw prices or log prices should be used. Takes value "raw" or "log".  
#' 
#' Value  
#' Returns the cointegration test statistic for the given coin pair.  
test_cointegration <- function(coin_y, coin_x, params) { 
  
  # Use either raw or log prices
  if (params[["model_type"]] == "raw") { 
    coin_y <- coin_y 
    coin_x <- coin_x
  } 
  if (params[["model_type"]] == "log" & params[["cointegration_test"]] != "distance") { 
    coin_y <- log(coin_y)
    coin_x <- log(coin_x) 
  } 
  
  # Test for cointegration using the Engle-Granger two step method
  if (params[["cointegration_test"]] == "eg") { 
    lm_model <- lm.fit(y = coin_y, x = cbind(1, coin_x))   
    lm_residuals <- lm_model[["residuals"]] 
    adf_test <- ur.df(lm_residuals, type = "drift", lags = 1) 
    adf_stat <- adf_test@testreg[["coefficients"]][2, 3]
    return(adf_stat) 
  } 
  
  # Test for cointegration using the total least squares method
  if (params[["cointegration_test"]] == "tls") { 
    pca_model <- prcomp(formula = ~ coin_y + coin_x) 
    pca_beta <- pca_model[["rotation"]][1, 1] / pca_model[["rotation"]][2, 1] 
    pca_intercept <- pca_model[["center"]][1] - pca_beta * pca_model[["center"]][2] 
    pca_residuals <- coin_y - pca_beta * coin_x - pca_intercept 
    adf_test <- ur.df(pca_residuals, type = "drift", lags = 1) 
    adf_stat = adf_test@testreg[["coefficients"]][2, 3]
    return(adf_stat) 
  } 
  
  # Test for cointegration using the distance method 
  if (params[["cointegration_test"]] == "distance") { 
    coin_y <- coin_y / coin_y[1] 
    coin_x <- coin_x / coin_x[1] 
    rmse = mean((coin_y - coin_x)^2)^0.5 
    return(rmse) 
  } 
} 

#' # 3. Create Coin Pairs Function  
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
  
  # Create USDT or BTC quote denominated coin list 
  if (params[["quote_currency"]] == "USDT") {
    coin_list <- c("USDT_BTC", "USDT_ETH", "USDT_LTC", "USDT_DASH", "USDT_XMR", "USDT_ZEC", "USDT_REP")
  }
  if (params[["quote_currency"]] == "BTC") {
    coin_list <- c("BTC_ETH", "BTC_STR", "BTC_LTC", "BTC_BTS", "BTC_XMR", "BTC_FCT", "BTC_DOGE", "BTC_DASH", "BTC_MAID", "BTC_CLAM") 
  } 
  
  # Create coin pairs, filter out pairs consisting of the same coin, and create a unique identifier to identify coin pairs 
  # that are identical but in a different order 
  coin_pairs <- expand.grid(coin_list, coin_list) %>% 
    rename(coin_y = Var1, 
           coin_x = Var2) %>% 
    filter(coin_y != coin_x) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(id = ifelse(coin_y < coin_x, str_c(coin_y, " ", coin_x), str_c(coin_x, " ", coin_y))) %>% 
    as_tibble() 
  
  # Return coin pairs
  return(coin_pairs)
} 

#' # 4. Test Coin Pairs Function  
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
#' Returns a dataframe containing the coin pairs and the cointegration test statistic resulting from testing each coin pair.  
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

#' # 5. Select Coin Pairs Function 
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
#'   distance_threshold: The threshold for the rmse of the coins normalized prices. Pairs below this threshold are 
#'     selected when using the distance method. 
#' 
#' Value  
#' Returns a dataframe containing the coin pairs that were selected.  
select_pairs <- function(train, coin_pairs, params) { 
  
  # Test for cointegration for all coin pairs
  df <- test_pairs(train = train, 
                   coin_pairs = coin_pairs, 
                   params = params)
  
  # If cointegration test uses the Engle-Granger method, filter by adf threshold 
  if (params[["cointegration_test"]] == "eg") 
    df <- df %>% filter(cointegration_stat <= params[["adf_threshold"]]) 
  
  # If cointegration test uses the total least squares method, filter by adf threshold 
  if (params[["cointegration_test"]] == "tls") 
    df <- df %>% filter(cointegration_stat <= params[["adf_threshold"]]) 

  # If cointegration uses the distance method, filter by rmse distance threshold  
  if (params[["cointegration_test"]] == "distance") 
    df <- df %>% filter(cointegration_stat <= params[["distance_threshold"]]) 

  # Select the top performing unique coin pair if both the coin-y-coin-x and coin-x-coin-y coin pairs are selected. 
  df <- df %>% 
    group_by(id) %>% 
    filter(row_number() == 1) %>%
    ungroup()
  
  # Return selected coin pairs 
  return(df)
} 

