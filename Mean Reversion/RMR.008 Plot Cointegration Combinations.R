#' ---
#' title: "Explore Cointegration Combinations"
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
#' An initial run of 18,600 iterations was completed using the following search parameters: 
# time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
# start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
# end_date <- sample(seq(from = start_date + 3, to = Sys.Date(), by = "1 day"), size = 1)  
results_a <- read_csv("./Mean Reversion/Output/Parameter Tuning/search cointegration results A.csv")

#' A secondary run of 18,600 iterations was completed using the following search parameters: 
# time_resolution <- sample(c(300, 900, 1800, 7200, 14400, 86400), size = 1) 
# start_date <- sample(seq(from = ymd("2017-01-01"), to = ymd("2017-10-01"), by = "1 day"), size = 1)
# end_date <- sample(seq(from = start_date + 3, to = start_date + 50, by = "1 day"), size = 1) 
results_b <- read_csv("./Mean Reversion/Output/Parameter Tuning/search cointegration results B.csv")

#' # 3. Summary Plots 
#' The following plots show the distribution of the average ADF test statistic of the 98 coin pairs of interest 
#' faceted by time resolution. The critical value at the 1 percent value to reject the null of a unit root is 
#' around -3.50 depending on the size of the population. A time resolution of 5-minute observations (300 seconds) 
#' has the most number of observations where the average ADF test statistic is significant at the 1 percent level. 
ggplot(results_a, aes(x = df_stat)) + 
  geom_histogram(binwidth = 0.05, fill = "blue") + 
  coord_cartesian(xlim = c(-5, 0)) + 
  geom_vline(xintercept = -3.50) + 
  facet_wrap(~ time_resolution)
ggplot(results_b, aes(x = df_stat_median)) + 
  geom_histogram(binwidth = 0.05, fill = "blue") + 
  coord_cartesian(xlim = c(-5, 0)) + 
  geom_vline(xintercept = -3.50) + 
  facet_wrap(~ time_resolution)
ggplot(results_b, aes(x = df_stat_mean)) + 
  geom_histogram(binwidth = 0.05, fill = "blue") + 
  coord_cartesian(xlim = c(-5, 0)) + 
  geom_vline(xintercept = -3.50) + 
  facet_wrap(~ time_resolution) 

#' The following plots show the distribution of the start_date faceted by time resolution. Good sampling of different 
#' start dates across time resolution. 
ggplot(results_a, aes(x = start_date)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 
ggplot(results_b, aes(x = start_date)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 

#' The following plots show the distribution of the end_date faceted by time resolution. Good sampling of different 
#' end dates across time resolution. 
ggplot(results_a, aes(x = end_date)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 
ggplot(results_b, aes(x = end_date)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 

#' The following plot show the distribution of the length of the window in days. Good sampling of length across 
#' time resolution. 
ggplot(results_a, aes(x = length * -1)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 
ggplot(results_b, aes(x = length * -1)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  facet_wrap(~ time_resolution) 

#' # 4. Explore Optimal Combination 
#' The following plots show the distribution of the average ADF test statistic across time resolution. A time resolution 
#' of 5-minute observations (300 seconds) has the lowest average of the average ADF statistic although there are some 
#' very negative outliers at the 1-day time resolution. 
ggplot(results_a, aes(x = factor(time_resolution))) + 
  geom_boxplot(aes(y = df_stat), colour = "blue") + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0))
ggplot(results_b, aes(x = factor(time_resolution))) + 
  geom_boxplot(aes(y = df_stat_median), colour = "blue") + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0))
ggplot(results_b, aes(x = factor(time_resolution))) + 
  geom_boxplot(aes(y = df_stat_mean), colour = "blue") + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0))

#' The folliowing plots show the individual average ADF test statistics by length of window faceted by time resolution. 
#' Shorter window lengths are associated with more negative ADF test statistics. 
ggplot(results_a, aes(x = length * -1)) + 
  geom_point(aes(y = df_stat), colour = "blue", alpha = 0.2) + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0)) + 
  geom_smooth(aes(y = df_stat), colour = "red") + 
  facet_wrap(~ time_resolution) 

#' This plot is identical to above but focuses on a window length of 0 to 50 days. The conclusion is that a time resolution 
#' of 5-minute observations (300 seconds) along with a window length of between 5 to 20 days exhibits the most mean reversion 
#' out of the combinations studied. 
ggplot(results_a, aes(x = length * -1)) + 
  geom_point(aes(y = df_stat), colour = "blue", alpha = 0.2) + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0), xlim = c(0, 50)) + 
  geom_smooth(aes(y = df_stat), colour = "red") + 
  facet_wrap(~ time_resolution) 
ggplot(results_b, aes(x = length * -1)) + 
  geom_point(aes(y = df_stat_median), colour = "blue", alpha = 0.2) + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0)) + 
  geom_smooth(aes(y = df_stat_median), colour = "red") + 
  facet_wrap(~ time_resolution) 
ggplot(results_b, aes(x = length * -1)) + 
  geom_point(aes(y = df_stat_mean), colour = "blue", alpha = 0.2) + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-6, 0)) + 
  geom_smooth(aes(y = df_stat_median), colour = "red") + 
  facet_wrap(~ time_resolution) 

#' Mean and of the mean ADF test statistic and median of the median ADF test statistic by length. 
results_b %>% 
  group_by(time_resolution, length) %>% 
  summarise(df_stat_mean = mean(df_stat_mean), 
            df_stat_median = median(df_stat_median)) %>% 
  ggplot(aes(x = length * - 1)) + 
  geom_point(aes(y = df_stat_mean), colour = "blue") + 
  geom_point(aes(y = df_stat_median), colour = "red") + 
  geom_hline(yintercept = -3.50) + 
  coord_cartesian(ylim = c(-4, -1)) + 
  facet_wrap(~ time_resolution)
