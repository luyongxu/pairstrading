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
source("./src/01-load-packages.R")

#' # 2. Load Data 
params <- read_csv("./output/parameters/parameter tuning 20171030.csv")

#' # 3. Remove Bug and Outliers
#' This set of parameters had a bug that affected runs where the pair allocation was set to scaled. These 
#' observations are removed. 
single <- params %>% 
  filter(pair_allocation != "scaled", 
         overall_return <= 5, 
         overall_return >= 0) %>% 
  mutate(train_window = as.numeric(str_match(train_window, "(\\d*)d*")[, 2]), 
         test_window = as.numeric(str_match(test_window, "(\\d*)d*")[, 2]), 
         model_spread_type = str_c(model_type, " ", spread_type), 
         rolling_window_days = rolling_window * time_resolution / 60 / 60 / 24, 
         time_resolution = factor(time_resolution), 
         signal_scaled_enter = factor(signal_scaled_enter))

#' # 4. Plot Functions
plot_boxplot <- function(df, x, ylim_low, ylim_high) { 
  ggplot(df, aes_string(x = x, fill = x)) + 
    geom_boxplot(aes(y = overall_return)) + 
    coord_cartesian(ylim = c(ylim_low, ylim_high)) + 
    facet_wrap(~ quote_currency, ncol = 1) + 
    geom_hline(yintercept = 1)
} 

plot_scatter <- function(df, x, ylim_low, ylim_high, smooth) { 
  if (smooth == TRUE) { 
    return(ggplot(df, aes_string(x = x)) + 
             geom_point(aes(y = overall_return), colour = "blue") + 
             geom_smooth(aes(y = overall_return), colour = "red") + 
             coord_cartesian(ylim = c(ylim_low, ylim_high)) + 
             facet_wrap(~ quote_currency, ncol = 1) + 
             geom_hline(yintercept = 1))
  } 
  if (smooth == FALSE) { 
    return(ggplot(df, aes_string(x = x)) + 
             geom_point(aes(y = overall_return), colour = "blue") + 
             coord_cartesian(ylim = c(ylim_low, ylim_high)) + 
             facet_wrap(~ quote_currency, ncol = 1) + 
             geom_hline(yintercept = 1)) 
  }
} 

#' # 5. Examine Single Parameters
#' This section creates plots that examine only one parameter at a time.  

#' ## 5.1 Time Resolution 
#' Time resolution of 300 is the best performing time resolution. 
plot_boxplot(single, "time_resolution", 0, 2)

#' ## 5.2 Quote Currency 
#' Similar cumulative return for BTC quote-denominated and USDT quote-denominated coin pairs except that 
#' BTC coin pairs have a return relative to the buy-and-hold return of holindg BTC. 
plot_boxplot(single, "quote_currency", 0, 2)

#' ## 5.3 Cointegration Test 
#' Similar cumulative return for both types of cointegration tests. 
plot_boxplot(single, "cointegration_test", 0, 2)

#' ## 5.4 ADF and Distance Threshold 
#' Higher returns associated with more restrictive ADF threshold.  
#' Higher returns associated with more restrictive distance threshold.
single %>% 
  filter(cointegration_test == "eg") %>% 
  plot_scatter("adf_threshold", 0, 2, TRUE)
single %>% 
  filter(cointegration_test == "distance") %>% 
  plot_scatter("distance_threshold", 0, 2, TRUE)

#' ## 5.5 Train and Test Window 
#' Slightly better performance with shorter test window.  
plot_scatter(single, "train_window", 0, 2, TRUE)
plot_scatter(single, "test_window", 0, 2, TRUE)

#' ## 5.6 Model and Spread Type 
#' Higher returns associated with log model type.  
#' Higher returns associated with rolling spread type.  
plot_boxplot(single, "model_type", 0, 2)
plot_boxplot(single, "spread_type", 0, 2)

#' ## 5.7 Rolling Window 
#' Higher returns associated with shorter rolling windows.  
single %>% 
  filter(spread_type == "rolling") %>% 
  plot_scatter("rolling_window_days", 0, 2, TRUE)

#' ## 5.8 Signal Logic 
#' Higher returns associated with scaled signal logic.  
#' Slightly higher returns associated with signal scaled enter of 3.  
#' Slightly higher returns associated with higher signal discrete enter. 
#' Slightly higher returns associated with higher signal discrete exit.  
#' No pattern associated with signal stop.  
#' Higher returns associated with signal reenter of FALSE.  
#' Slightly higher returns associated with a lower signal reenter threshold.  
plot_boxplot(single, "signal_logic", 0, 2)
single %>% 
  filter(signal_logic == "scaled") %>% 
  plot_boxplot("signal_scaled_enter", 0, 2)
single %>% 
  filter(signal_logic == "discrete") %>% 
  plot_scatter("signal_discrete_enter", 0, 2, TRUE)
single %>% 
  filter(signal_logic == "discrete") %>% 
  plot_scatter("signal_discrete_exit", 0, 2, TRUE)
plot_scatter(single, "signal_stop", 0, 2, TRUE) 
plot_boxplot(single, "signal_reenter", 0, 2)
single %>% 
  filter(signal_reenter == TRUE) %>% 
  plot_scatter("signal_reenter_threshold", 0, 2, TRUE)

#' ## 5.9 Pair Allocation 
#' Slightly higher returns associated with a weighted pair allocation.  
plot_boxplot(single, "pair_allocation", 0, 2) 

#' # 6. Examine Multiple Parameters 
#' This section creates plots that examines multiple parameters at a time for iterations where the 
#' time resolution is 300, the spread type is rolling, and the signal logic is scaled. These parameters 
#' have shown to have produced good results.   
#' 
params <- bind_rows(read_csv("./output/parameters/parameter tuning 20171101.csv"), 
                    read_csv("./output/parameters/parameter tuning 20171102.csv"), 
                    read_csv("./output/parameters/parameter tuning 20171106.csv"), 
                    read_csv("./output/parameters/parameter tuning 20171113.csv")) 

#' ## 6.1 Filter Results 
#' Iterations with an overall return greater than 20 are removed. This removes 49 out of the 1146 iterations 
#' but makes the smoothing much more interpretable. 
multiple <- params %>% 
  filter(time_resolution == 300, 
         spread_type == "rolling", 
         signal_logic == "scaled") %>% 
  mutate(train_window = as.numeric(str_match(train_window, "(\\d*)d*")[, 2]), 
         test_window = as.numeric(str_match(test_window, "(\\d*)d*")[, 2]), 
         model_spread_type = str_c(model_type, " ", spread_type), 
         rolling_window_days = rolling_window * time_resolution / 60 / 60 / 24, 
         time_resolution = factor(time_resolution), 
         signal_scaled_enter = factor(signal_scaled_enter)) %>% 
  filter(overall_return <= 20)
print(multiple)

#' ## 6.2 Cointegration Test 
#' Higher returns associated with the engle-granger cointegration test.  
plot_boxplot(multiple, "cointegration_test", 0, 10) 

#' ## 6.3 ADF and Distance Threshold 
plot_scatter(multiple, "adf_threshold", 0, 10, TRUE) 
plot_scatter(multiple, "distance_threshold", 0, 10, TRUE) 
multiple %>% 
  filter(cointegration_test == "eg") %>% 
  ggplot(aes(x = adf_threshold, y = overall_return, colour = pair_allocation)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
multiple %>% 
  filter(cointegration_test == "distance") %>% 
  ggplot(aes(x = adf_threshold, y = overall_return, colour = pair_allocation)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)


#' ## 6.4 Train and Test Window
plot_scatter(multiple, "train_window", 0, 10, TRUE)
plot_scatter(multiple, "test_window", 0, 10, TRUE) 
multiple %>% 
  ggplot(aes(x = train_window, y = test_window, colour = overall_return)) + 
  geom_point() + 
  scale_colour_gradient(low = "white", high = "black") +
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.5 Model Type and ADF Threshold and Distance Threshold 
plot_boxplot(multiple, "model_type", 0, 10) 
multiple %>% 
  filter(cointegration_test == "eg") %>% 
  ggplot(aes(x = adf_threshold, y = overall_return, colour = model_type)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
multiple %>% 
  filter(cointegration_test == "distance") %>% 
  ggplot(aes(x = distance_threshold, y = overall_return, colour = model_type)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.6 Rolling Window 
plot_scatter(multiple, "rolling_window", 0, 10, TRUE) 
multiple %>% 
  ggplot(aes(x = rolling_window, y = overall_return, colour = model_type)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1) 
multiple %>% 
  ggplot(aes(x = rolling_window, y = train_window, colour = overall_return)) + 
  geom_point() + 
  scale_colour_gradient(low = "white", high = "black") +
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1) 
multiple %>% 
  ggplot(aes(x = rolling_window, y = test_window, colour = overall_return)) + 
  geom_point() + 
  scale_colour_gradient(low = "white", high = "black") +
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1) 
multiple %>% 
  ggplot(aes(x = rolling_window, y = signal_stop, colour = overall_return)) + 
  geom_point() + 
  scale_colour_gradient(low = "white", high = "black") +
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1) 
multiple %>% 
  ggplot(aes(x = rolling_window, y = overall_return, colour = signal_scaled_enter)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)


#' ## 6.7 Signal Scaled Enter  
plot_boxplot(multiple, "signal_scaled_enter", 0, 10)
ggplot(multiple, aes(x = signal_scaled_enter, fill = cointegration_test)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
ggplot(multiple, aes(x = signal_scaled_enter, fill = model_type)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.8 Signal Stop 
plot_scatter(multiple, "signal_stop", 0, 10, TRUE)
multiple %>% 
  ggplot(aes(x = signal_stop, y = overall_return, colour = cointegration_test)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
multiple %>% 
  ggplot(aes(x = signal_stop, y = overall_return, colour = model_type)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.9 Signal Reenter 
plot_boxplot(multiple, "signal_reenter", 0, 10)
ggplot(multiple, aes(x = signal_reenter, fill = cointegration_test)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
ggplot(multiple, aes(x = signal_reenter, fill = model_type)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.10 Signal Reenter Threshold
plot_scatter(multiple, "signal_reenter_threshold", 0, 10, TRUE)
multiple %>% 
  ggplot(aes(x = signal_reenter_threshold, y = overall_return, colour = cointegration_test)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
multiple %>% 
  ggplot(aes(x = signal_reenter_threshold, y = overall_return, colour = model_type)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)

#' ## 6.11 Pair Allocation 
plot_boxplot(multiple, "pair_allocation", 0, 10)
ggplot(multiple, aes(x = pair_allocation, fill = cointegration_test)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
ggplot(multiple, aes(x = pair_allocation, fill = model_type)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
ggplot(multiple, aes(x = pair_allocation, fill = cointegration_test)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
ggplot(multiple, aes(x = pair_allocation, fill = signal_scaled_enter)) + 
  geom_boxplot(aes(y = overall_return)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~ quote_currency, ncol = 1)
