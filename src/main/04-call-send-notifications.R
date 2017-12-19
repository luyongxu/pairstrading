#' ---
#' title: "Call Send Notifications"
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
source("./src/util/13-send-notifications.R")

#' # 2. Notify Slack 
n <- 100
channel <- "#alertbot" 
username <- "pairs_trading_bot" 
api_token <- "xoxp-120817259600-121069189777-150562361286-48c9bb2f7cadb4f13176d6d2b6acbe51"
notify_slack(logfile = "./logs/scrape_data_300.log", n = n, channel = channel, username = username, api_token = api_token)
notify_slack(logfile = "./logs/scrape_data_900.log", n = n, channel = channel, username = username, api_token = api_token)
notify_slack(logfile = "./logs/scrape_data_1800.log", n = n, channel = channel, username = username, api_token = api_token)
notify_slack(logfile = "./logs/scrape_data_7200.log", n = n, channel = channel, username = username, api_token = api_token)
notify_slack(logfile = "./logs/scrape_data_14400.log", n = n, channel = channel, username = username, api_token = api_token)
notify_slack(logfile = "./logs/scrape_data_86400.log", n = n, channel = channel, username = username, api_token = api_token)
