#' ---
#' title: "Call Download Data"
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
#' This script when called through the command line using Rscript has the option of including two arguments. The first 
#' argument is an option argument that takes the following values: 300, 900, 1800, 7200, 14400, 86400, update and rebuild.  
#' The second argument contains the path to set the working directory. 
#' 
args_download <- commandArgs(trailingOnly = TRUE)
if (length(args_download) == 0) { 
  option <- "none" 
}
if (length(args_download) == 2) { 
  option <- args_download[1]
  wd <- args_download[2]
  setwd(wd) 
}
commandArgs <- function(...) NULL

#' # 2. Load Packages and Functions
source("./src/util/01-load-packages.R")
source("./src/util/02-download-data.R")

#' # 3. Download Data
tickers <- c("USDT_BTC", "USDT_ETH", "USDT_LTC", "USDT_DASH", "USDT_XMR", "USDT_ZEC", "USDT_REP", 
             "BTC_ETH", "BTC_STR", "BTC_LTC", "BTC_BTS", "BTC_XMR", "BTC_FCT", "BTC_DOGE", "BTC_DASH", "BTC_MAID", "BTC_CLAM")
download_data(tickers = tickers, option = option)
