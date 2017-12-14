#' ---
#' title: "Send Notifications"
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
args_period <- commandArgs(trailingOnly = TRUE)
if (length(args_period) == 0) { 
  args_period <- "none" 
}

#' # 2. Load Packages 
#' Sets the command line arguments to NULL so that the command line arguments intended for this script do not get passed to 
#' the load packages script. 
commandArgs <- function(...) NULL
if (args_period == "none") 
  source("./Mean Reversion/TMR.001 Load Packages.R") 
if (args_period != "none") 
  source("/home/rstudio/kevin_lu_basket_mr/Mean Reversion/TMR.001 Load Packages.R")

#' # 3. Load Logs 


#' # 4. Detect Errors 
#' Errors can occur when the Poloniex API is unresponsive or there are problems interfacing with the mongo database.  
#' An example of the Poloniex API being unresponsive is:  
#' ```
#' Error in open.connection(con, "rb") :
#' Could not resolve host: poloniex.com
#' Calls: return_chartdata ... fromJSON_string -> parseJSON -> parse_con -> open -> open.connection
#' Execution halted
#' ```
#' An example of not being able to connect to the mongo database is:  
#' ```
#' Error: No suitable servers found (`serverSelectionTryOnce` set): [connection timeout calling ismaster on 'localhost:27017']
#' Execution halted
#' ```
slackr_setup(channel = "#alertbot", 
             username = "pairs_trading_bot", 
             api_token = "xoxp-120817259600-121069189777-150562361286-48c9bb2f7cadb4f13176d6d2b6acbe51", 
             echo = TRUE)
slackr("This is a test of the pairs trading bot. Will notify this channel if there are any failures in 
       downloading data or generating predictions.")


