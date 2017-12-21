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
wd <- getwd() 
setwd("..")
source("./src/util/01-load-packages.R")
setwd(wd)

#' # 1. UI Header 
ui_header <- dashboardHeader(
  title = "Pairs Trading Dashboard"
)

#' # 2. UI Sidebar 
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "tab_overview", icon = icon("dashboard")), 
    menuItem("Plots", tabName = "tab_plots", icon = icon("line-chart")), 
    menuItem("Trades", tabName = "tab_trades", icon = icon("table")), 
    menuItem("Logs", tabName = "tab_logs", icon = icon("database")), 
    menuItem("Parameters", tabName = "tab_parameters", icon = icon("gear")), 
    selectInput("select_quote_currency", label = "Select a quote currency: ", choices = c("BTC", "USDT"), selected = "BTC"), 
    uiOutput("select_pair"), 
    selectInput("select_autorefresh", label = "Automatic refresh: ", choices = c("On", "Off"), selected = "On")
  )
)

#' # 3. UI Body 
ui_body <- dashboardBody(
  
  # 3.1 Tabs 
  tabItems(
    
    # 3.2 Tab Overview 
    tabItem(
      tabName = "tab_overview", 
      fluidPage(
        box(plotOutput("plot_backtest"), title = "Strategy Return vs Buy and Hold", collapsible = TRUE, width = 12), 
        box(formattableOutput("table_backtest"), title = "Current Positions", collapsible = TRUE, width = 12), 
        box(plotOutput("plot_backtest_distribution"), title = "Distribution of Returns and Summary Statistics", 
            collapsible = TRUE, width = 12), 
        box(uiOutput("text_last_update"), title = "Last Update", collapsible = TRUE, width = 12)
      )
    ), 
    
    # 3.3 Tab Plots 
    tabItem(
      tabName = "tab_plots", 
      fluidPage(
        fluidRow( 
          infoBoxOutput("infoBox_zscore", width = 3), 
          infoBoxOutput("infoBox_signal", width = 3), 
          infoBoxOutput("infoBox_coin_y_position", width = 3), 
          infoBoxOutput("infoBox_coin_x_position", width = 3)
        ), 
        fluidRow( 
          box(plotOutput("plot_return", height = 360), title = "Model Return vs Buy Hold Return", collapsible = TRUE), 
          box(plotOutput("plot_signal", height = 360), title = "Spread vs Trading Signal", collapsible = TRUE)
        ), 
        fluidRow( 
          box(plotOutput("plot_prices", height = 360), title = "Prices Over the Train and Test Sets", collapsible = TRUE), 
          box(plotOutput("plot_coefficients", height = 360), title = "Hedge Ratio and Intercept", collapsible = TRUE)
        ), 
        fluidRow( 
          box(plotOutput("plot_distribution", height = 480), title = "Distribution of Returns and Summary Statistics", 
              collapsible = TRUE, width = 12)
        )
      )
    ), 
    
    # 3.4 Tab Trades 
    tabItem(
      tabName = "tab_trades", 
      fluidPage(
        box(dataTableOutput("table_trades"), title = "Historical Trades", collapsible = TRUE, width = 12)
      )
    ), 
    
    # 3.5 Tab Logs 
    tabItem(
      tabName = "tab_logs", 
      fluidPage(
        fluidRow(
          box(verbatimTextOutput("text_generate_current_predictions"), title = "generate_current_predictions.sh", 
              collapsible = TRUE, width = 12)
        ), 
        fluidRow(
          box(verbatimTextOutput("text_download_data_300"), title = "download_data_300.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_download_data_900"), title = "downloaddata_900.sh", collapsible = TRUE, width = 6)
        ), 
        fluidRow(
          box(verbatimTextOutput("text_download_data_1800"), title = "download_data_1800.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_download_data_7200"), title = "download_data_7200.sh", collapsible = TRUE, width = 6)
        ),
        fluidRow(
          box(verbatimTextOutput("text_download_data_14400"), title = "download_data_14400.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_download_data_86400"), title = "download_data_86400.sh", collapsible = TRUE, width = 6) 
        ), 
        fluidRow(
          box(verbatimTextOutput("text_launch_shiny"), title = "launch_shiny.sh", collapsible = TRUE, width = 6), 
          box(verbatimTextOutput("text_send_notifications"), title = "send_notifications.sh", collapsible = TRUE, width = 6)
        )
      )
    ), 
    
    # 3.6 Tab Parameters
    tabItem(
      tabName = "tab_parameters", 
      fluidPage(
        box(tableOutput("table_parameters"), title = "Current Parameter Set", collapsible = TRUE, width = 4)
      )
    )
  )
)

#' # 4. UI Dashboard 
ui <- dashboardPage(header = ui_header, 
                    sidebar = ui_sidebar, 
                    body = ui_body, 
                    title = "Pairs Trading Dashboard")


