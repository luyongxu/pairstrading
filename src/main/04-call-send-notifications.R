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

#' # 1. Capture Command Line Arguments 
#' This script when called through the command line using Rscript has the option of including ar arguments that contains 
#' the path to set the working directory. 
args_params <- commandArgs(trailingOnly = TRUE)
if (length(args_params) == 1) { 
  wd <- args_params[1]
  setwd(wd) 
}
commandArgs <- function(...) NULL

#' # 2. Source Pairs Trading Functions 
source("./src/util/01-load-packages.R")
source("./src/util/13-send-notifications.R")

#' # 3. Notify Slack 
n <- 100
channel <- "#alertbot" 
username <- "pairs_trading_bot" 
api_token <- Sys.getenv("slack_api_token")
notify_slack("./logs/download_data_300.log", n, channel, username, api_token)
notify_slack("./logs/download_data_900.log", n, channel, username, api_token)
notify_slack("./logs/download_data_1800.log", n, channel, username, api_token) 
notify_slack("./logs/download_data_7200.log", n, channel, username, api_token) 
notify_slack("./logs/download_data_14400.log", n, channel, username, api_token) 
notify_slack("./logs/download_data_86400.log", n, channel, username, api_token) 
notify_slack("./logs/generate_current_predictions.log", n, channel, username, api_token)
notify_slack("./logs/execute_trades.log", channel, username, api_token)
print(str_c("Checked logs for errors at ", Sys.time(), "."))
