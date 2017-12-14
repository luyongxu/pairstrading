#' ---
#' title: "Pairs Trading Dashboard"
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

#' # 1. Load Pairs Trading Functions 
setwd("..")
source("./Mean Reversion/TMR.003 Data Wrangling Functions.R")
source("./Mean Reversion/TMR.004 Coin Selection Functions.R")
source("./Mean Reversion/TMR.005 Model Functions.R")
source("./Mean Reversion/TMR.006 Backtesting Functions.R")
source("./Mean Reversion/TMR.007 Plot Functions.R")
source("./Mean Reversion/TMR.008 Generate Predictions Functions.R")

#' # 2. UI Header 
ui_header <- dashboardHeader(
  title = "Pairs Trading Dashboard"
)

#' # 3. UI Sidebar 
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "tab_overview", icon = icon("dashboard")), 
    menuItem("Plots", tabName = "tab_plots", icon = icon("line-chart")), 
    menuItem("Trades", tabName = "tab_trades", icon = icon("table")), 
    menuItem("Logs", tabName = "tab_logs", icon = icon("database")), 
    menuItem("Parameters", tabName = "tab_parameters", icon = icon("gear")), 
    selectInput("select_quote_currency", label = "Select a quote currency: ", choices = c("BTC", "USDT")), selected = "BTC", 
    uiOutput("select_pair") 
  )
)

#' # 4. UI Body 
ui_body <- dashboardBody(
  
  # 4.1 Tabs 
  tabItems(
    
    # 4.2 Tab Overview 
    tabItem(
      tabName = "tab_overview", 
      fluidPage(
        box(plotOutput("plot_backtest"), title = "Strategy Return vs Buy and Hold", collapsible = TRUE, width = 12), 
        box(formattableOutput("table_backtest"), title = "Current Positions", collapsible = TRUE, width = 12)
      )
    ), 
    
    # 4.3 Tab Plots 
    tabItem(
      tabName = "tab_plots", 
      fluidPage(
        infoBoxOutput("infoBox_zscore", width = 3), 
        infoBoxOutput("infoBox_signal", width = 3), 
        infoBoxOutput("infoBox_coin_y_position", width = 3), 
        infoBoxOutput("infoBox_coin_x_position", width = 3), 
        box(plotOutput("plot_return", height = 360), title = "Model Return vs Buy Hold Return", collapsible = TRUE), 
        box(plotOutput("plot_signal", height = 360), title = "Spread vs Trading Signal", collapsible = TRUE), 
        box(plotOutput("plot_prices", height = 360), title = "Prices Over the Train and Test Sets", collapsible = TRUE), 
        box(plotOutput("plot_coefficients", height = 360), title = "Hedge Ratio and Intercept", collapsible = TRUE)
      )
    ), 
    
    # 4.4 Tab Trades 
    tabItem(
      tabName = "tab_trades", 
      fluidPage(
        box(dataTableOutput("table_trades"), title = "Historical Trades", collapsible = TRUE, width = 12)
      )
    ), 
    
    # 4.5 Tab Logs 
    tabItem(
      tabName = "tab_logs", 
      fluidPage(
        fluidRow(
          box(verbatimTextOutput("text_generate_predictions"), title = "generate_predictions.sh", collapsible = TRUE, width = 12)
        ), 
        fluidRow(
          box(verbatimTextOutput("text_scrape_data_300"), title = "scrape_data_300.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_scrape_data_900"), title = "scrape_data_900.sh", collapsible = TRUE, width = 6)
        ), 
        fluidRow(
          box(verbatimTextOutput("text_scrape_data_1800"), title = "scrape_data_1800.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_scrape_data_7200"), title = "scrape_data_7200.sh", collapsible = TRUE, width = 6)
        ),
        fluidRow(
          box(verbatimTextOutput("text_scrape_data_14400"), title = "scrape_data_14400.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_scrape_data_86400"), title = "scrape_data_86400.sh", collapsible = TRUE, width = 6) 
        )
      )
    ), 
    
    # 4.6 Tab Parameters
    tabItem(
      tabName = "tab_parameters", 
      fluidPage(
        box(tableOutput("table_parameters"), title = "Current Parameter Set", collapsible = TRUE, width = 4)
      )
    )
  )
)

#' # 5. UI Dashboard
ui <- dashboardPage(header = ui_header, 
                    sidebar = ui_sidebar, 
                    body = ui_body, 
                    title = "Pairs Trading Dashboard")

#' # 6. Server 
server <- function(input, output, session) { 
  
  # 6.1 Set Parameters 
  params <- reactive({
    list(time_resolution = 300, 
         quote_currency = input[["select_quote_currency"]], 
         cointegration_test = "eg", 
         adf_threshold = -5.03, 
         distance_threshold = 0.00, 
         train_window = days(30), 
         test_window = days(20), 
         model_type = "raw", 
         regression_type = "ols", 
         spread_type = "rolling", 
         rolling_window = 1440, 
         signal_logic = "scaled", 
         signal_scaled_enter = 3.0, 
         signal_discrete_enter = 3.0, 
         signal_discrete_exit = 0.2, 
         signal_stop = 4.5, 
         signal_reenter = TRUE, 
         signal_reenter_threshold = 2.00, 
         pair_allocation = "equal", 
         pair_allocation_scaling = 1.00, 
         return_calc = "maximum") 
  })
  
  # 6.2 Query Data 
  pricing_data <- reactive({
    withProgress(value = 0.5, message = "Querying data.", expr = {
      invalidateLater(300000, session)
      df <- load_data(source = "mongodb", time_resolution = "300", start_unix = "1504224000")
      setProgress(value = 1, message = "Querying data complete.")
      df
    })
  })
  
  # 6.3 Initialize Cutoff Date 
  cutoff_date <- reactive({
    withProgress(value = 0.5, message = "Finding latest cutoff date.", expr = {
      cutoff_date <- as.Date("2017-11-01")
      while (Sys.Date() - days(as.numeric(str_match(params()[["test_window"]], "(\\d*)d*")[, 2])) > cutoff_date) { 
        cutoff_date <- cutoff_date + days(as.numeric(str_match(params()[["test_window"]], "(\\d*)d*")[, 2]))
      }
      setProgress(value = 1, message = "Finding latest cutoff date complete.")
      cutoff_date
    })
  })
  
  # 6.4 Create Train and Test Sets 
  train <- reactive({
    withProgress(value = 0.5, message = "Creating train set.", expr = {
      train <- prepare_data(pricing_data = pricing_data(), 
                            start_date = as.Date(cutoff_date()) - params()[["train_window"]], 
                            end_date = as.Date(cutoff_date()), 
                            params = params()) 
      setProgress(value = 1, message = "Creating train set complete.")
      train
    })
  })
  test <- reactive({
    withProgress(value = 0.5, message = "Creating test set.", expr = {
      test <- prepare_data(pricing_data = pricing_data(), 
                           start_date = as.Date(cutoff_date()), 
                           end_date = as.Date(cutoff_date()) + params()[["test_window"]], 
                           params = params()) 
      setProgress(value = 1, message = "Creating test set complete.")
      test
    })
  })
    
  # 6.5 Select Coin Pairs 
  coin_pairs <- reactive({
    withProgress(value = 0.5, message = "Creating coin pairs.", expr = {
      coin_pairs <- create_pairs(params = params()) 
      setProgress(value = 1, message = "Creating coin pairs complete.")
      coin_pairs
    })

  })
  selected_pairs <- reactive({
    withProgress(value = 0.5, message = "Selecting coin pairs.", expr = {
      selected_pairs <- select_pairs(train = train(), 
                                     coin_pairs = coin_pairs(), 
                                     params = params()) %>% 
        mutate(text = str_c("(", row_number(), ") ", coin_y, ", ", coin_x)) 
      setProgress(value = 1, message = "Selecting coin pairs complete.")
      selected_pairs
    })
  })
  
  # 6.6 Create Selected Pairs List 
  selected_pairs_list <- reactive({
    selected_pairs()[["text"]] %>% as.list()
  })
    
  # 6.7 Select coin pair select menu
  output[["select_pair"]] <- renderUI({
    selectInput("coin_pair", label = "Select a coin pair: ", choices = selected_pairs_list())
  })
  
  # 6.8 Generate Predictions 
  predictions <- reactive({
    withProgress(value = 0.5, message = "Generating latest predictions.", expr = {
      predictions <- generate_predictions(pricing_data(), cutoff_date(), params()) %>% 
        mutate(coin_y_position = coin_y_position * 100, 
               coin_x_position = coin_x_position * 100) %>% 
        mutate(coin_y_price = round(coin_y_price, 4), 
               coin_x_price = round(coin_x_price, 4), 
               cointegration_stat = round(cointegration_stat, 2), 
               spread_z = round(spread_z, 2), 
               hedge_ratio = round(hedge_ratio, 2), 
               intercept = round(intercept, 2), 
               coin_y_position = round(coin_y_position, 2), 
               coin_x_position = round(coin_x_position, 2), 
               cumulative_return = round(cumulative_return, 4))
        setProgress(value = 1, message = "Predictions generated.")
      predictions
    })
  })
  
  # 6.9 Current Predictions 
  current_predictions <- reactive({
    predictions() %>% 
      group_by(coin_pair_id) %>% 
      filter(row_number() == n()) %>% 
      mutate(date = as.character(as.Date(date_time)), 
             time = as.character(strftime(date_time, format="%H:%M:%S", tz = "GMT")))
  })
   
  # 6.10 Calculate Strategy Return
  return_strategy <- reactive({
    calculate_return(df_strategy = predictions(), params = params())
  })
  test_return <- reactive({
    test() %>% 
      mutate(return_strategy = return_strategy()[["cumulative_return"]])
  })  
  
  # 6.11 Selected Coin Pair
  selected_coin_y <- reactive({ 
    selected_coin_y <- input[["coin_pair"]] %>% 
      str_match("(\\(\\d*\\)\\s)([A-Z_]*)(\\,\\s)([A-Z_]*)") %>% 
      .[3]
  })
  selected_coin_x <- reactive({
    selected_coin_x <- input[["coin_pair"]] %>% 
      str_match("(\\(\\d*\\)\\s)([A-Z_]*)(\\,\\s)([A-Z_]*)") %>% 
      .[5]
  })

  # 6.12 Generate plots 
  plots <- reactive({
    withProgress(value = 0.5, message = "Creating plots.", expr = {
      plots <- plot_single(train = train(), 
                           test = test(), 
                           coin_y = selected_coin_y(), 
                           coin_x = selected_coin_x(), 
                           params = params(), 
                           print = FALSE)
      setProgress(value = 1, message = "Plots created.")
      plots
    })
  })  
  
  # 6.13 Plot in the overview tab 
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
    plot_strategy
  })
  
  # 6.13 Table in the overview tab 
  output[["table_backtest"]] <- renderFormattable({ 
    df <- current_predictions()  %>% 
      select("ID" = coin_pair_id, "Date" = date, "Time" = time, "Coin Y" = coin_y_name, "Coin X" = coin_x_name, 
             "Coin Y Price" = coin_y_price, "Coin X Price" = coin_x_price, "ADF Stat" = cointegration_stat, 
             "Hedge Ratio" = hedge_ratio, "Intercept" = intercept, "Signal" = signal, "Spread Z-Score" = spread_z, 
             "Coin Y Position" = coin_y_position, "Coin X Position" = coin_x_position, "Cumulative Return" = cumulative_return)
    sign_formatter <- formatter("span", 
      style = x ~ style(color = ifelse(x > 0, "blue", ifelse(x < 0, "red", "black"))))
    formattable(df, list(
      "Signal" = color_tile("lightcoral", "lightblue"), 
      "Spread Z-Score" = sign_formatter, 
      area(col = c("Coin Y Position", "Coin X Position")) ~ sign_formatter, 
      "Cumulative Return" = color_bar("lightgreen")
    ))
  })
  
  # 6.14 Information boxes in the plots tab 
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
      round(2)
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
      round(2)
    color <- ifelse(value > 0, "green", "red")
    color <- ifelse(value == 0, "black", color)
    infoBox(title = str_c(selected_coin_x(), " Position (X)"), 
            value = value, color = color, fill = TRUE, icon = icon("circle-o"))
  })

  # 6.15 Plots in the plots tab 
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
  
  # 6.16 Table in trades tab 
  output[["table_trades"]] <- renderDataTable({
    df_trades <- predictions() %>% 
      arrange(desc(date_time)) %>% 
      filter(coin_y_name == selected_coin_y(), 
             coin_x_name == selected_coin_x()) %>% 
      mutate(date = as.character(as.Date(date_time)), 
             time = as.character(strftime(date_time, format="%H:%M:%S", tz = "GMT"))) %>%
      select("Date" = date, "Time" = time, "Coin Y" = coin_y_name, "Coin X" = coin_x_name, "Coin Y Price" = coin_y_price, 
             "Coin X Price" = coin_x_price, "Signal" = signal, "Coin Y Position" = coin_y_position, 
             "Coin X Position" = coin_x_position, "Change Y Position" = change_y_position, 
             "Change X Position" = change_x_position)
    head(df_trades, 100)
  })
  
  # 6.17 Text in logs tab 
  output[["text_generate_predictions"]] <- renderPrint({
    cat(str_c("generate_predictions started on ", Sys.time() - 55, ". \n"))
    cat(str_c("Coin selection last occurred on ", cutoff_date(), ". \n"))
    cat(str_c("generate_predictions successfully finished on ", Sys.time() - 5, ". \n"))
    cat("Printing latest predictions: \n")
    print(current_predictions())
  })
  output[["text_scrape_data_300"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_300.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  output[["text_scrape_data_900"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_900.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  output[["text_scrape_data_1800"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_1800.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  output[["text_scrape_data_7200"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_7200.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  output[["text_scrape_data_14400"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_14400.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  output[["text_scrape_data_86400"]] <- renderPrint({
    invalidateLater(1000, session)
    text <- read_lines("./Logs/scrape_data_86400.log") %>% tail(38)
    cat(text, sep = "\n")
  })
  
  # 6.18 Table in the parameters tab
  output[["table_parameters"]] <- renderTable({
    params <- params()
    params[["train_window"]] <- as.character(params[["train_window"]])
    params[["test_window"]] <- as.character(params[["test_window"]])
    df_params <- tibble(Parameter = names(params), 
                        Value = params %>% unlist())
  }, hover = TRUE, spacing = "m", align = "l", digits = 2)
}

#' # 7. shinyApp
shinyApp(ui = ui, server = server)