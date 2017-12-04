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

#' # 2. Query Data 
# pricing_data <- load_data(source = "csv", time_resolution = "300", start_unix = "1504224000")
pricing_data <- read_csv("./Mean Reversion/Raw Data/pricing data test.csv")

#' # 3. Prepare Data 
pricing_data <- prepare_data(pricing_data, "2017-11-01", "2018-01-01", list(time_resolution = "300"))

#' # 4. Header 
ui_header <- dashboardHeader(
  title = "Pairs Trading Dashboard", 
  dropdownMenuOutput("menu_notifications")
)

#' # 5. Sidebar 
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "tab_overview", icon = icon("dashboard")), 
    menuItem("Plots", tabName = "tab_plots", icon = icon("line-chart")), 
    menuItem("Trades", tabName = "tab_trades", icon = icon("table")), 
    menuItem("Data", tabName = "tab_data", icon = icon("database"))
  )
)

#' # 6. Body 
ui_body <- dashboardBody(
  
  # Tabs 
  tabItems(
    
    # Tab Overview 
    tabItem(
      tabName = "tab_overview", 
      box(plotOutput("plot_prices_big"), 
          title = "Plot of Indexed Prices", 
          collapsible = TRUE, 
          width = 12)
    ), 
    
    # Tab Plots 
    tabItem(
      tabName = "tab_plots", 
      fluidPage(
        infoBoxOutput("infoBox_cointegration_stat", width = 3), 
        infoBoxOutput("infoBox_signal", width = 3), 
        infoBoxOutput("infoBox_coin_y_position", width = 3), 
        infoBoxOutput("infoBox_coin_x_position", width = 3),
        box(plotOutput("plot_prices_a", height = 250), 
            title = "Plot of Indexed Prices", 
            collapsible = TRUE), 
        box(plotOutput("plot_prices_b", height = 250), 
            title = "Plot of Indexed Prices", 
            collapsible = TRUE), 
        box(plotOutput("plot_prices_c", height = 250), 
            title = "Plot of Indexed Prices", 
            collapsible = TRUE), 
        box(plotOutput("plot_prices_d", height = 250), 
            title = "Plot of Indexed Prices", 
            collapsible = TRUE)
      )
    ), 
    
    # Tab Trades 
    tabItem(
      tabName = "tab_trades", 
      tableOutput("table_trades")
    ), 
    
    # Tab Data 
    tabItem(
      tabName = "tab_data", 
      verbatimTextOutput("text_data")
    )
  )
)

#' # 7. Dashboard
ui_dashboard <- dashboardPage(header = ui_header, 
                              sidebar = ui_sidebar, 
                              body = ui_body, 
                              title = "Pairs Trading Dashboard")

#' # 8. Server 
server <- function(input, output) { 
  
  # Notification Menu
  output[["menu_notifications"]] <- renderMenu({ 
    dropdownMenu(
      type = "notifications", 
      notificationItem(
        text = "This is a placeholder.", 
        status = "success"
      )
    )
  })
  
  # Plot in the overview tab 
  output[["plot_prices_big"]] <- renderPlot({
    ggplot(pricing_data, aes(x = date_time, y = USDT_BTC)) + 
      geom_line()
  })
  
  # Information boxes in the plots tab 
  output[["infoBox_cointegration_stat"]] <- renderInfoBox({ 
    infoBox(
      title = "ADF Stat", 
      value = "-4.50", 
      color = "blue", 
      fill = TRUE
    )
  })
  output[["infoBox_signal"]] <- renderInfoBox({ 
    infoBox(
      title = "Signal", 
      value = "2.0", 
      color = "blue", 
      fill = TRUE
    )
  })
  output[["infoBox_coin_y_position"]] <- renderInfoBox({ 
    infoBox(
      title = "Coin Y Position", 
      value = "100", 
      color = "green", 
      fill = TRUE
    )
  })
  output[["infoBox_coin_x_position"]] <- renderInfoBox({ 
    infoBox(
      title = "Coin X Position", 
      value = "-100", 
      color = "red", 
      fill = TRUE
    )
  })

  # Plots in the plots tab 
  output[["plot_prices_a"]] <- renderPlot({
    ggplot(pricing_data, aes(x = date_time, y = USDT_BTC)) + 
      geom_line()
  })
  output[["plot_prices_b"]] <- renderPlot({
    ggplot(pricing_data, aes(x = date_time, y = USDT_BTC)) + 
      geom_line()
  })
  output[["plot_prices_c"]] <- renderPlot({
    ggplot(pricing_data, aes(x = date_time, y = USDT_BTC)) + 
      geom_line()
  })
  output[["plot_prices_d"]] <- renderPlot({
    ggplot(pricing_data, aes(x = date_time, y = USDT_BTC)) + 
      geom_line()
  })
  

}

#' # 9. shinyApp
shinyApp(ui = ui_dashboard, server = server)