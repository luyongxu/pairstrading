#' ---
#' title: "Examine Tuning Parameters"
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
params <- read_csv("./Mean Reversion/Output/parameter tuning 20171030.csv")

#' # 3. Remove Bug and Outliers
#' This set of parameters had a bug that affected runs where the pair allocation was set to scaled. 
params <- params %>% 
  filter(pair_allocation != "scaled", 
         overall_return <= 5, 
         overall_return >= 0) %>% 
  mutate(train_window = as.numeric(str_match(train_window, "(\\d*)d*")[, 2]), 
         test_window = as.numeric(str_match(test_window, "(\\d*)d*")[, 2]), 
         model_spread_type = str_c(model_type, " ", spread_type), 
         rolling_window_days = rolling_window * time_resolution / 60 / 60 / 24, 
         time_resolution = factor(time_resolution), 
         signal_scaled_enter = factor(signal_scaled_enter))

#' # 4. Plot Function 
plot_boxplot <- function(df, x) { 
  ggplot(df, aes_string(x = x, fill = x)) + 
    geom_boxplot(aes(y = overall_return)) + 
    coord_cartesian(ylim = c(0, 2))
} 
plot_scatter <- function(df, x) { 
  ggplot(df, aes_string(x = x)) + 
    geom_point(aes(y = overall_return), colour = "blue") + 
    geom_smooth(aes(y = overall_return), colour = "red") + 
    coord_cartesian(ylim = c(0, 2))
} 

#' # 4. Time Resolution 
#' Time resolution of 300 is the best performing time resolution. 
plot_boxplot(params, "time_resolution")

#' # 5. Quote Currency 
#' Similar cumulative return for BTC quote-denominated and USDT quote-denominated coin pairs except that 
#' BTC coin pairs have a return relative to the buy-and-hold return of holindg BTC. 
plot_boxplot(params, "quote_currency")

#' # 6. Cointegration Test 
#' Similar cumulative return for both types of cointegration tests. 
plot_boxplot(params, "cointegration_test")

#' # 7. ADF and Distance Threshold 
#' Higher returns associated with more restrictive ADF threshold.  
#' Higher returns associated with more restrictive distance threshold.
params %>% 
  filter(cointegration_test == "eg") %>% 
  plot_scatter("adf_threshold")
params %>% 
  filter(cointegration_test == "distance") %>% 
  plot_scatter("distance_threshold")

#' # 9. Train and Test Window 
#' Slightly better performance with shorter test window.  
plot_scatter(params, "train_window")
plot_scatter(params, "test_window")

#' # 10. Model and Spread Type 
#' Higher returns associated with log model type.  
#' Higher returns associated with rolling spread type.  
plot_boxplot(params, "model_type")
plot_boxplot(params, "spread_type")
plot_boxplot(params, "model_spread_type")

#' # 11. Rolling Window 
#' Higher returns associated with shorter rolling windows.  
params %>% 
  filter(spread_type == "rolling") %>% 
  plot_scatter("rolling_window_days")

#' # 12. Signal Logic 
#' Higher returns associated with scaled signal logic.  
#' Slightly higher returns associated with signal scaled enter of 3.  
#' Slightly higher returns associated with higher signal discrete enter. 
#' Slightly higher returns associated with higher signal discrete exit.  
#' No pattern associated with signal stop.  
#' Higher returns associated with signal reenter of FALSE.  
#' Slightly higher returns associated with a lower signal reenter threshold.  
plot_boxplot(params, "signal_logic")
params %>% 
  filter(signal_logic == "scaled") %>% 
  plot_boxplot("signal_scaled_enter")
params %>% 
  filter(signal_logic == "discrete") %>% 
  plot_scatter("signal_discrete_enter")
params %>% 
  filter(signal_logic == "discrete") %>% 
  plot_scatter("signal_discrete_exit")
plot_scatter(params, "signal_stop") 
plot_boxplot(params, "signal_reenter") 
params %>% 
  filter(signal_reenter == TRUE) %>% 
  plot_scatter("signal_reenter_threshold")

#' # 13. Pair Allocation 
#' Slightly higher returns associated with a weighted pair allocation.  
plot_boxplot(params, "pair_allocation") 
