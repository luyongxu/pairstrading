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
source("./Mean Reversion/TMR.003 Pairs Trading Functions.R")

#' # 2. Set Parameters 
params <- list(time_resolution = 300, 
               quote_currency = "USDT", 
               cointegration_test = "eg", 
               adf_threshold = -6.0, 
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
               pair_allocation_scaling = 1.00) 

#' # 3. UI Header 
ui_header <- dashboardHeader(
  title = "Pairs Trading Dashboard", 
  dropdownMenuOutput("menu_notifications")
)

#' # 4. UI Sidebar 
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "tab_overview", icon = icon("dashboard")), 
    menuItem("Plots", tabName = "tab_plots", icon = icon("line-chart")), 
    menuItem("Trades", tabName = "tab_trades", icon = icon("table")), 
    menuItem("Data", tabName = "tab_data", icon = icon("database")), 
    selectInput("select_pair", label = "Select Coin Pair: ", choices = c("Placeholder")) 
  )
)

#' # 5. UI Body 
ui_body <- dashboardBody(
  
  # 5.1 Tabs 
  tabItems(
    
    # 5.2 Tab Overview 
    tabItem(
      tabName = "tab_overview", 
      fluidPage(
        box(plotOutput("plot_backtest"), title = "Strategy Return vs Buy and Hold", collapsible = TRUE, width = 12), 
        box(dataTableOutput("datatable_backtest"), title = "Current Positions", collapsible = TRUE, width = 12)
      )
    ), 
    
    # 5.3 Tab Plots 
    tabItem(
      tabName = "tab_plots", 
      fluidPage(
        infoBoxOutput("infoBox_cointegration_stat", width = 3), 
        infoBoxOutput("infoBox_signal", width = 3), 
        infoBoxOutput("infoBox_coin_y_position", width = 3), 
        infoBoxOutput("infoBox_coin_x_position", width = 3),
        box(plotOutput("plot_prices", height = 250), title = "Prices Over the Train and Test Sets", collapsible = TRUE), 
        box(plotOutput("plot_signal", height = 250), title = "Spread vs Trading Signal", collapsible = TRUE), 
        box(plotOutput("plot_return", height = 250), title = "Model Return vs Buy Hold Return", collapsible = TRUE), 
        box(plotOutput("plot_coefficients", height = 250), title = "Hedge Ratio and Intercept", collapsible = TRUE)
      )
    ), 
    
    # 5.4 Tab Trades 
    tabItem(
      tabName = "tab_trades", 
      tableOutput("table_trades")
    ), 
    
    # 5.5 Tab Data 
    tabItem(
      tabName = "tab_data", 
      verbatimTextOutput("text_data")
    )
  )
)

#' # 6. UI Dashboard
ui_dashboard <- dashboardPage(header = ui_header, 
                              sidebar = ui_sidebar, 
                              body = ui_body, 
                              title = "Pairs Trading Dashboard")

#' # 7. Server 
server <- function(input, output) { 
  
  # 7.1 Query Data 
  pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data test.csv") 
  
  # 7.2 Initialize Cutoff Date 
  cutoff_date <- as.Date("2017-11-01")
  while (Sys.Date() - days(as.numeric(str_match(params[["test_window"]], "(\\d*)d*")[, 2])) > cutoff_date) { 
    cutoff_date <- cutoff_date + days(as.numeric(str_match(params[["test_window"]], "(\\d*)d*")[, 2]))
  }
  
  # 7.3 Create Train and Test Sets 
  train <- prepare_data(pricing_data = pricing_data, 
                        start_date = as.Date(cutoff_date) - params[["train_window"]], 
                        end_date = as.Date(cutoff_date), 
                        params = params) 
  test <- prepare_data(pricing_data = pricing_data, 
                       start_date = as.Date(cutoff_date), 
                       end_date = as.Date(cutoff_date) + params[["test_window"]], 
                       params = params) 
  
  # 7.2 Generate Predictions 
  predictions <- generate_predictions(pricing_data, cutoff_date, params) 
  
  # 7.3 Current Predictions 
  current_predictions <- predictions %>% 
    group_by(coin_pair_id) %>% 
    filter(row_number() == n()) %>% 
    mutate_at(vars(coin_y_price, coin_x_price, cointegration_stat, signal, hedge_ratio, intercept, 
                   coin_y_position, coin_x_position, cumulative_return), round, 2) %>% 
    select("ID" = coin_pair_id, "Date" = date_time, "Coin Y" = coin_y_name, "Coin X" = coin_x_name, 
           "Coin Y Price" = coin_y_price, "Coin X Price" = coin_x_price, "Cointegration Stat" = cointegration_stat, 
           "Signal" = signal, "Hedge Ratio" = hedge_ratio, "Intercept" = intercept, "Coin Y Position" = coin_y_position, 
           "Coin X Position" = coin_x_position, "Cumulative Return" = cumulative_return)
  
  #' 7.4 Calculate Strategy Return
  return_strategy <- calculate_return(df_strategy = predictions, params = params)
  test <- test %>% mutate(return_strategy = return_strategy[["cumulative_return"]])
  
  # 7.4 Generate plots 
  plots <- plot_single(train = train, 
                       test = test, 
                       coin_y = current_predictions[["Coin Y"]][1], 
                       coin_x = current_predictions[["Coin X"]][1], 
                       params = params, 
                       print = FALSE)

  # 7.5 Notification Menu
  output[["menu_notifications"]] <- renderMenu({ 
    dropdownMenu(type = "notifications", notificationItem(text = "This is a placeholder.", status = "success"))
  })
  
  # 7.6 Select Coin Pair 
  
  
  # 7.6 Plot in the overview tab 
  output[["plot_backtest"]] <- renderPlot({
    ggplot(test, aes(x = date_time)) + 
      geom_line(aes(y = return_strategy, colour = "Strategy"), size = 1) +
      geom_line(aes(y = USDT_BTC / USDT_BTC[1], colour = "USDT_BTC"), size = 0.5, alpha = 0.4) +
      geom_hline(yintercept = 1, colour = "black") +
      scale_color_manual(name = "Return", values = c("Strategy" = "darkblue", "USDT_BTC" = "darkred")) +
      labs(x = "Date", y = "Cumulative Return")
  }) 
  
  # 7.7 Table in the overview tab 
  output[["datatable_backtest"]] <- renderDataTable({
    current_predictions 
  })
  
  # 7.8 Information boxes in the plots tab 
  output[["infoBox_cointegration_stat"]] <- renderInfoBox({ 
    infoBox(title = "ADF Stat", value = "-4.50", color = "blue", fill = TRUE)
  })
  output[["infoBox_signal"]] <- renderInfoBox({ 
    infoBox(title = "Signal", value = "2.0", color = "blue", fill = TRUE)
  })
  output[["infoBox_coin_y_position"]] <- renderInfoBox({ 
    infoBox(title = "Coin Y Position", value = "100", color = "green", fill = TRUE)
  })
  output[["infoBox_coin_x_position"]] <- renderInfoBox({ 
    infoBox(title = "Coin X Position", value = "-100", color = "red", fill = TRUE)
  })

  # 7.9 Plots in the plots tab 
  output[["plot_prices"]] <- renderPlot({
    plots[["plot_prices"]]
  })
  output[["plot_signal"]] <- renderPlot({
    plots[["plot_signal"]]
  })
  output[["plot_return"]] <- renderPlot({
    plots[["plot_return"]]
  })
  output[["plot_coefficients"]] <- renderPlot({
    plots[["plot_coefficients"]]
  })
}

#' # 8. shinyApp
shinyApp(ui = ui_dashboard, server = server)