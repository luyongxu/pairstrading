#' ---
#' title: "Pairs Trading Shiny App UI"
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
setwd(here::here())
source("./src/util/01-load-packages.R")
source("./src/util/03-set-parameters.R")
source("./src/util/04-data-functions.R")
source("./src/util/05-coin-selection-functions.R")
source("./src/util/06-setup-strategy-functions.R")
source("./src/util/07-model-functions.R")
source("./src/util/08-backtesting-functions.R")
source("./src/util/09-plot-functions.R")
source("./src/util/10-generate-predictions-functions.R")

#' # 2. Server 
server <- function(input, output, session) { 
  
  # 2.1 Set Parameters 
  params <- reactive({
    params <- load_params("./output/params/params.csv")
    params[["quote_currency"]] <- input[["select_quote_currency"]]
    return(params)
  })
  
  # 2.2 Query Data 
  pricing_data <- reactive({
    withProgress(value = 0.5, message = "Querying data.", expr = { 
      if (input[["select_autorefresh"]] == "On") 
        invalidateLater(300000, session)
      start_unix <- as.character(as.numeric(Sys.time()) - 86400 * 90)
      df <- load_data(source = "mongodb", time_resolution = params()[["time_resolution"]], start_unix = start_unix)
      setProgress(value = 1, message = "Querying data complete.")
      return(df)
    })
  })

  # 2.3 Initialize Cutoff Date 
  cutoff_date <- reactive({
    withProgress(value = 0.5, message = "Finding latest cutoff date.", expr = {
      cutoff_date <- set_cutoff_date(initial_date = "2017-11-01", params = params())
      return(cutoff_date)
    })
  })
  
  # 2.4 Create Train and Test Sets 
  train <- reactive({
    withProgress(value = 0.5, message = "Creating train set.", expr = {
      train <- prepare_data(pricing_data = pricing_data(), 
                            start_date = as.Date(cutoff_date()) - params()[["train_window"]], 
                            end_date = as.Date(cutoff_date()), 
                            params = params()) 
      setProgress(value = 1, message = "Creating train set complete.")
      return(train)
    })
  })
  test <- reactive({
    withProgress(value = 0.5, message = "Creating test set.", expr = {
      test <- prepare_data(pricing_data = pricing_data(), 
                           start_date = as.Date(cutoff_date()), 
                           end_date = as.Date(cutoff_date()) + params()[["test_window"]], 
                           params = params()) 
      setProgress(value = 1, message = "Creating test set complete.")
      return(test)
    })
  })
  
  # 2.5 Create Coin Pairs  
  coin_pairs <- reactive({
    withProgress(value = 0.5, message = "Creating coin pairs.", expr = {
      coin_pairs <- create_pairs(params = params()) 
      setProgress(value = 1, message = "Creating coin pairs complete.")
      return(coin_pairs)
    })
  })
  
  # 2.6 Select Coin Pairs
  selected_pairs <- reactive({
    withProgress(value = 0.5, message = "Selecting coin pairs.", expr = {
      selected_pairs <- select_pairs(train = train(), 
                                     coin_pairs = coin_pairs(), 
                                     params = params())
      if (nrow(selected_pairs) > 0) { 
        selected_pairs <- selected_pairs %>% 
          mutate(text = str_c("(", row_number(), ") ", coin_y, ", ", coin_x)) 
      }
      setProgress(value = 1, message = "Selecting coin pairs complete.")
      return(selected_pairs)
    })
  })
  
  # 2.7 Create Selected Pairs List 
  selected_pairs_list <- reactive({
    list <- selected_pairs()[["text"]] %>% as.list()
    return(list)
  })
  
  # 2.8 Select coin pair select menu in the sidebar 
  output[["select_pair"]] <- renderUI({
    input <- selectInput("coin_pair", label = "Select a coin pair: ", choices = selected_pairs_list())
    return(input)
  })
  
  # 2.9 Current time in the sidebar 
  output[["text_current_time"]] <- renderUI({
    invalidateLater(1000, session)
    helpText("Current time: ", Sys.time())
  })
  
  # 2.9 Generate Predictions 
  predictions <- reactive({
    withProgress(value = 0.5, message = "Generating latest predictions.", expr = {
      predictions <- generate_predictions(pricing_data(), cutoff_date(), params())
      setProgress(value = 1, message = "Predictions generated.")
      return(predictions)
    })
  })

  # 2.10. Current Predictions 
  current_predictions <- reactive({
    df <- predictions() %>% 
      group_by(coin_pair_id) %>% 
      filter(row_number() == n()) %>% 
      mutate(date = as.character(as.Date(date_time)), 
             time = as.character(strftime(date_time, format="%H:%M:%S", tz = "GMT")))
    return(df)
  })
  
  # 2.11 Calculate Strategy Return
  return_strategy <- reactive({
    df <- calculate_return(df_strategy = predictions(), params = params())
    return(df)
  })
  test_return <- reactive({
    df <- test() %>% 
      mutate(return_strategy = return_strategy()[["cumulative_return"]])
    return(df)
  })  
  
  # 2.12 Selected Coin Pair
  selected_coin_y <- reactive({ 
    coin_y <- input[["coin_pair"]] %>% 
      str_match("(\\(\\d*\\)\\s)([A-Z_]*)(\\,\\s)([A-Z_]*)") %>% 
      .[3]
    return(coin_y)
  })
  selected_coin_x <- reactive({
    coin_x <- selected_coin_x <- input[["coin_pair"]] %>% 
      str_match("(\\(\\d*\\)\\s)([A-Z_]*)(\\,\\s)([A-Z_]*)") %>% 
      .[5]
    return(coin_x)
  })
  
  # 2.13 Generate plots 
  plots <- reactive({
    withProgress(value = 0.5, message = "Creating plots.", expr = {
      plots <- plot_single(train = train(), 
                           test = test(), 
                           coin_y = selected_coin_y(), 
                           coin_x = selected_coin_x(), 
                           params = params(), 
                           print = FALSE)
      setProgress(value = 1, message = "Plots created.")
      return(plots)
    })
  })  
  
  # 2.14 Plot in the overview tab 
  output[["plot_backtest"]] <- renderPlot({ 
    test_return <- test_return() %>% 
      mutate(return_USDT_BTC = USDT_BTC / USDT_BTC[1], 
             return_strategy_USD = return_strategy * return_USDT_BTC)
    if(params()[["quote_currency"]] == "USDT") { 
      plot_strategy <- ggplot(test_return, aes(x = date_time)) +
        geom_line(aes(y = return_strategy, colour = "Strategy"), size = 1) +
        geom_line(aes(y = USDT_BTC / USDT_BTC[1], colour = "USDT_BTC"), size = 0.5, alpha = 0.4) +
        geom_hline(yintercept = 1, colour = "black") +
        scale_color_manual(name = "Return", values = c("Strategy" = "darkblue", "USDT_BTC" = "darkred")) +
        labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return")
    }
    if(params()[["quote_currency"]] == "BTC") { 
      plot_strategy <- ggplot(test_return, aes(x = date_time)) +
        geom_line(aes(y = return_strategy, colour = "Strategy in BTC"), size = 1) + 
        geom_line(aes(y = return_strategy_USD, colour = "Strategy in USD"), size = 1) + 
        geom_line(aes(y = return_USDT_BTC, colour = "USDT_BTC in USD"), size = 0.5, alpha = 0.4) +
        geom_hline(yintercept = 1, colour = "black") +
        scale_color_manual(name = "Return", values = c("Strategy in BTC" = "gray", 
                                                       "Strategy in USD" = "darkblue", 
                                                       "USDT_BTC in USD" = "darkred")) +
        labs(title = "Strategy Return vs Buy Hold Return", x = "Date", y = "Cumulative Return")
    }
    return(plot_strategy)
  })
  
  # 2.15 Table in the overview tab 
  output[["table_backtest"]] <- renderFormattable({ 
    df <- current_predictions()  %>% 
      mutate(coin_y_price = round(coin_y_price, 4), 
             coin_x_price = round(coin_x_price, 4), 
             cointegration_stat = round(cointegration_stat, 2), 
             spread_z = round(spread_z, 2), 
             hedge_ratio = round(hedge_ratio, 2), 
             intercept = round(intercept, 2), 
             coin_y_position = round(coin_y_position, 4), 
             coin_x_position = round(coin_x_position, 4), 
             cumulative_return = round(cumulative_return, 4)) %>% 
      select("ID" = coin_pair_id, "Date" = date, "Time" = time, "Coin Y" = coin_y_name, "Coin X" = coin_x_name, 
             "Coin Y Price" = coin_y_price, "Coin X Price" = coin_x_price, "ADF Stat" = cointegration_stat, 
             "Hedge Ratio" = hedge_ratio, "Intercept" = intercept, "Signal" = signal, "Spread Z-Score" = spread_z, 
             "Coin Y Position" = coin_y_position, "Coin X Position" = coin_x_position, "Cumulative Return" = cumulative_return)
    sign_formatter <- formatter("span", style = x ~ style(color = ifelse(x > 0, "blue", ifelse(x < 0, "red", "black"))))
    formattable(df, list(
      "Signal" = color_tile("lightcoral", "lightblue"), 
      "Spread Z-Score" = sign_formatter, 
      area(col = c("Coin Y Position", "Coin X Position")) ~ sign_formatter, 
      "Cumulative Return" = color_bar("lightgreen")
    ))
  })
  
  # 2.16. Information boxes in the plots tab 
  output[["infoBox_zscore"]] <- renderInfoBox({ 
    value <- current_predictions() %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      .[["spread_z"]] %>% 
      round(2)
    infoBox(title = "Spread Z-Score", value = value, color = "blue", fill = TRUE, icon = icon("asterisk"))
  })
  output[["infoBox_signal"]] <- renderInfoBox({ 
    value <- current_predictions() %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      .[["signal"]] %>% 
      round(2)
    infoBox(title = "Signal", value = value, color = "blue", fill = TRUE, icon = icon("signal"))
  })
  output[["infoBox_coin_y_position"]] <- renderInfoBox({ 
    value <- current_predictions() %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      .[["coin_y_position"]] %>% 
      round(4)
    color <- ifelse(value > 0, "green", "red")
    color <- ifelse(value == 0, "black", color)
    infoBox(title = str_c(selected_coin_y(), " Position (Y)"), 
            value = value, color = color, fill = TRUE, icon = icon("circle"))
  })
  output[["infoBox_coin_x_position"]] <- renderInfoBox({ 
    value <- current_predictions() %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      .[["coin_x_position"]] %>% 
      round(4)
    color <- ifelse(value > 0, "green", "red")
    color <- ifelse(value == 0, "black", color)
    infoBox(title = str_c(selected_coin_x(), " Position (X)"), 
            value = value, color = color, fill = TRUE, icon = icon("circle-o"))
  })
  
  # 2.17 Plots in the plots tab 
  output[["plot_return"]] <- renderPlot({
    plots()[["plot_return"]]
  })
  output[["plot_signal"]] <- renderPlot({
    plots()[["plot_signal"]]
  })
  output[["plot_prices"]] <- renderPlot({
    plots()[["plot_prices"]]
  })
  output[["plot_coefficients"]] <- renderPlot({
    plots()[["plot_coefficients"]]
  })
  output[["plot_distribution"]] <- renderPlot({ 
    plots()[["plot_distribution"]]
  })
  
  # 2.18 Table in trades tab 
  output[["table_trades"]] <- renderDataTable({
    df_trades <- predictions() %>% 
      mutate(coin_y_price = round(coin_y_price, 4), 
             coin_x_price = round(coin_x_price, 4), 
             cointegration_stat = round(cointegration_stat, 2), 
             spread_z = round(spread_z, 2), 
             hedge_ratio = round(hedge_ratio, 2), 
             intercept = round(intercept, 2), 
             coin_y_position = round(coin_y_position, 4), 
             coin_x_position = round(coin_x_position, 4), 
             change_y_position = round(change_y_position, 4), 
             change_x_position = round(change_x_position, 4), 
             cumulative_return = round(cumulative_return, 4)) %>% 
      arrange(desc(date_time)) %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      mutate(date = as.character(as.Date(date_time)), 
             time = as.character(strftime(date_time, format="%H:%M:%S", tz = "GMT"))) %>%
      select("Date" = date, "Time" = time, "Coin Y" = coin_y_name, "Coin X" = coin_x_name, "Coin Y Price" = coin_y_price, 
             "Coin X Price" = coin_x_price, "Signal" = signal, "Coin Y Position" = coin_y_position, 
             "Coin X Position" = coin_x_position, "Change Y Position" = change_y_position, 
             "Change X Position" = change_x_position)
    return(head(df_trades, 100))
  })
  
  # 2.19 Text in logs tab 
  display_log <- function(log) { 
    invalidateLater(1000, session)
    text <- read_lines(log) %>% tail(38)
    return(cat(text, sep = "\n"))
  }
  output[["text_generate_current_predictions"]] <- renderPrint({
    display_log("./logs/generate_current_predictions.log")
  })
  output[["text_download_data_300"]] <- renderPrint({
    display_log("./logs/download_data_300.log")
  })
  output[["text_download_data_900"]] <- renderPrint({
    display_log("./logs/download_data_900.log")
  })
  output[["text_download_data_1800"]] <- renderPrint({
    display_log("./logs/download_data_1800.log")
  })
  output[["text_download_data_7200"]] <- renderPrint({
    display_log("./logs/download_data_7200.log")
  })
  output[["text_download_data_14400"]] <- renderPrint({
    display_log("./logs/download_data_14400.log")
  })
  output[["text_download_data_86400"]] <- renderPrint({
    display_log("./logs/download_data_86400.log")
  })
  output[["text_launch_shiny"]] <- renderPrint({
    display_log("./logs/launch_shiny.log")
  })
  output[["text_send_notifications"]] <- renderPrint({
    display_log("./logs/send_notifications.log")
  })
  
  # 2.20 Table in the parameters tab
  output[["table_parameters"]] <- renderTable({
    params <- params()
    params[["train_window"]] <- as.character(params[["train_window"]])
    params[["test_window"]] <- as.character(params[["test_window"]])
    df_params <- tibble(Parameter = names(params), Value = params %>% unlist())
  }, hover = TRUE, spacing = "m", align = "l", digits = 2)
}
