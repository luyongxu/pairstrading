#' ---
#' title: "Cross Validate Plots"
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

#' # 1. Source Pairs Trading Functions 
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")
source("./src/util/04-data-functions.R")
source("./src/util/05-coin-selection-functions.R")
source("./src/util/06-setup-strategy-functions.R")
source("./src/util/07-model-functions.R")
source("./src/util/08-backtesting-functions.R")
source("./src/util/09-plot-functions.R")
source("./src/util/10-generate-predictions-functions.R")

#' # 2. Load Data 
pricing_data <- load_data(source = "csv", time_resolution = "300")

#' # 3. Set Parameters 
params <- load_params("./output/params/params.csv")
number_pairs <- 1

#' # 4. Cross Validation September 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-09-01",
          params = params, 
          number_pairs = number_pairs)

#' # 5. Cross Validation August 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-08-01",
          params = params, 
          number_pairs = number_pairs)

#' # 6. Cross Validation July 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-07-01",
          params = params, 
          number_pairs = number_pairs)

#' # 7. Cross Validation June 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-06-01",
          params = params, 
          number_pairs = number_pairs)

#' # 8. Cross Validation May 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-05-01",
          params = params, 
          number_pairs = number_pairs)

#' # 9. Cross Validation April 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-04-01",
          params = params, 
          number_pairs = number_pairs)

#' # 10. Cross Validation March 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-03-01",
          params = params, 
          number_pairs = number_pairs)

#' # 11. Cross Validation February 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-02-01",
          params = params, 
          number_pairs = number_pairs)

#' # 12. Cross Validation January 2017
plot_many(pricing_data = pricing_data,
          cutoff_date = "2017-01-01",
          params = params, 
          number_pairs = number_pairs)

#' # 13. Cross Validation Full 
results <- backtest_strategy_full(pricing_data = pricing_data, params = params)  
ggplot(results, aes(x = date_time)) + 
  geom_line(aes(y = return_strategy_cumulative), colour = "blue", size = 1) + 
  geom_hline(yintercept = 1, colour = "black") + 
  labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return") 
print(results[["return_strategy_cumulative"]][nrow(results)]) 
