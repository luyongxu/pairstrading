#' ---
#' title: "Send Slack Notifications"
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

#' # 1. Load Packages 
source("./src/util/01-load-packages.R")

#' # 2. Detect Errors 
#' Description  
#' Errors can occur when the Poloniex API is unresponsive or there are problems interfacing with the mongo database.  
#' An example of an API request being unresponsive is:  
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
#' 
#' Arguments  
#' logfile: filename of the log file  
#' n: number of lines from the end to read in the file  
#' 
#' Value 
#' Returns TRUE if errors or warnings were detected. Otherwise returns FALSE.  
detect_errors <- function(logfile, n) { 
  
  # Load last n lines of log 
  print(str_c("Checking ", logfile, " for errors or warnings."))
  log <- read_lines(logfile) %>% tail(n)
  
  # Use regular expression to detect warning, error, or execution halted 
  detect <- logical()
  for (pattern in c("warning", "error", "execution halted", "Warning", "Error", "Execution halted")) { 
    detect <- c(detect, str_detect(log, pattern))
  }
  
  # Return TRUE if something is wrong or return FALSE if everything is fine
  detect_true <- TRUE %in% detect 
  return(detect_true)
}

#' # 3. Detect Operating System 
#' Description  
#' Checks the operating system of the current machine.  
#' 
#' Arguments  
#' None.  
#' 
#' Value  
#' Returns the operating system in a string.  
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf[["sysname"]]
    if (os == "Darwin")
      os <- "osx"
  } else { 
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "OS_X"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

#' # 4. Notify Slack 
#' Description  
#' Notifies a slack channel if an error has occurred within the past n lines of a logfile.  
#' 
#' Arguments  
#' logfile: filename of the log file  
#' n: number of lines from the end to read in the file  
#' channel: The name of the Slack channel. Must include the pound sign.  
#' username: Username of the bot  
#' api_token: Slack API token  
#' 
#' Value  
#' Sends a message to the Slack channel. Does not return a value in R.  
notify_slack <- function(logfile, n, channel, username, api_token) { 
  
  # Setup Slack
  slackr_setup(channel = channel, username = username, api_token = api_token, echo = FALSE)
  
  # Notify slack if errors or warnings are detected 
  detect <- detect_errors(logfile, n)
  if (detect == TRUE) { 
    nodename <- Sys.info()[["nodename"]]
    message <- str_c("An error or warning was detected in ", logfile, " on nodename ", nodename, ".")
    slackr(message)
  }
}

