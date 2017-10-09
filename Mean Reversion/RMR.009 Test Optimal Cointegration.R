#' ---
#' title: "Test Optimal Combination"
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

#' # 1. Source Search Cointegration Combinations 
source("./Mean Reversion/RMR.007 Search Cointegration Combinations.R", echo = FALSE, print.eval = FALSE)

#' # 2. Prepare Data and Calculate Statistics 
#' For each time resolution, prepare the pricing data and test for cointegration for all 98 coin pairs. The date of study is 
#' 2017-09-01 to 2017-09-30. This period has exhibited strong mean reversion. 

pricing_data_300 <- prepare_data(time_resolution = 300, start_date = "2017-09-01", end_date = "2017-09-30")
pricing_data_900 <- prepare_data(time_resolution = 900, start_date = "2017-09-01", end_date = "2017-09-30")
pricing_data_1800 <- prepare_data(time_resolution = 1800, start_date = "2017-09-01", end_date = "2017-09-30")
pricing_data_7200 <- prepare_data(time_resolution = 7200, start_date = "2017-09-01", end_date = "2017-09-30")
pricing_data_14400 <- prepare_data(time_resolution = 14400, start_date = "2017-09-01", end_date = "2017-09-30")
pricing_data_86400 <- prepare_data(time_resolution = 86400, start_date = "2017-09-01", end_date = "2017-09-30")

coin_pairs_300 <- calculate_statistics(pricing_data = pricing_data_300, coin_pairs = create_coins())
coin_pairs_900 <- calculate_statistics(pricing_data = pricing_data_900, coin_pairs = create_coins())
coin_pairs_1800 <- calculate_statistics(pricing_data = pricing_data_1800, coin_pairs = create_coins())
coin_pairs_7200 <- calculate_statistics(pricing_data = pricing_data_7200, coin_pairs = create_coins())
coin_pairs_14400 <- calculate_statistics(pricing_data = pricing_data_14400, coin_pairs = create_coins())
coin_pairs_86400 <- calculate_statistics(pricing_data = pricing_data_86400, coin_pairs = create_coins())

#' # 3. Plot Time Resolution 300 
#' For each time resolution, plot the top 10 coins ranked by the ADF test statistic. 
for (i in 1:10) { 
  coin_y <- coin_pairs_300[["coin_y"]][i] 
  coin_x <- coin_pairs_300[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_300, coin_y = pricing_data_300[[coin_y]], coin_x = pricing_data_300[[coin_x]])
}

#' # 4. Plot Time Resolution 900 
for (i in 1:10) { 
  coin_y <- coin_pairs_900[["coin_y"]][i] 
  coin_x <- coin_pairs_900[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_900, coin_y = pricing_data_900[[coin_y]], coin_x = pricing_data_900[[coin_x]])
}

#' # 5. Plot Time Resolution 1800 
for (i in 1:10) { 
  coin_y <- coin_pairs_1800[["coin_y"]][i] 
  coin_x <- coin_pairs_1800[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_1800, coin_y = pricing_data_1800[[coin_y]], coin_x = pricing_data_1800[[coin_x]])
}

#' # 6. Plot Time Resolution 7200 
for (i in 1:10) { 
  coin_y <- coin_pairs_7200[["coin_y"]][i] 
  coin_x <- coin_pairs_7200[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_7200, coin_y = pricing_data_7200[[coin_y]], coin_x = pricing_data_7200[[coin_x]])
}

#' # 7. Plot Time Resolution 14400 
for (i in 1:10) { 
  coin_y <- coin_pairs_14400[["coin_y"]][i] 
  coin_x <- coin_pairs_14400[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_14400, coin_y = pricing_data_14400[[coin_y]], coin_x = pricing_data_14400[[coin_x]])
}

#' # 8. Plot Time Resolution 86400 
for (i in 1:10) { 
  coin_y <- coin_pairs_86400[["coin_y"]][i] 
  coin_x <- coin_pairs_86400[["coin_x"]][i] 
  print(str_c("Generating plots for ", coin_y, " and ", coin_x, "."))
  plot_coins(df = pricing_data_86400, coin_y = pricing_data_86400[[coin_y]], coin_x = pricing_data_86400[[coin_x]])
}