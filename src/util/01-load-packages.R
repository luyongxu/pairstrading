#' ---
#' title: "Load Packages"
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

#' # 1. Load Libraries 
#' Description  
#' The RStudio Server Amazon Machine Image provided by Louis Aslett (http://www.louisaslett.com/RStudio_AMI/) provides 
#' an easy way to start an ec2 instance with RStudio Server and all major libraries installed. This machine image installs 
#' packages in the "/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4" directory. When running scripts using Rscript in the 
#' command line, however, this directory is not present in the library path. This section adds the folder to the library 
#' path.  
#' 
#' Each library may depend on certain dependencies are those dependencies are loaded first before loading the parent 
#' library. When library dependencies are loaded, they may also be looking in incorrect library paths and may cause 
#' the parent library to not load. If this happens, load the dependencies manually first before loading the parent 
#' library.  
#' 
#' The load_library() function will load a package and will install the package first if necessary.  
#' 
#' Arguments  
#' package_name: A string indicating the package name.  
#' 
#' Value  
#' Does not return a value.  
load_library <- function(package_name) { 
  library_path <- c(.libPaths(), "/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4")
  suppressWarnings(suppressMessages({
    if(require(package_name, character.only = TRUE, lib.loc = library_path) == FALSE) {
      install.packages(package_name, repos = "https://cloud.r-project.org/")
      require(package_name, character.only = TRUE, lib.loc = library_path)
    }
  }))
}
load_library("cli")
load_library("tidyr")
load_library("readr")
load_library("purrr")
load_library("dplyr")
load_library("forcats")
load_library("stringr")
load_library("tidyselect")
load_library("tidyverse")
load_library("lubridate")
load_library("jsonlite")
load_library("urca")
load_library("zoo")
load_library("bindrcpp")
load_library("RcppRoll")
load_library("feather")
load_library("mongolite")
load_library("plotly")
load_library("shiny")
load_library("shinydashboard")
load_library("DT")
load_library("formattable")
load_library("slackr")
load_library("here")

#' # 2. Options 
#' Turn off scientific notation.
options(scipen = 999)

#' # 3. Clean
rm(load_library)
