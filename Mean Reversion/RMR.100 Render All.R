#' ---
#' title: "Test Cointegration"
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

#' # 1. Render Functions
render_wrapper <- function(filename, output) { 
  if (output == "html_document") { 
    rmarkdown::render(input = paste0("./Mean Reversion/", filename, ".R"), 
                      output_file = paste0("./Mean Reversion/Notebooks/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = paste0("./Mean Reversion/", filename, ".R"), 
                      output_file = paste0("./Mean Reversion/Notebooks/", filename, ".pdf"), 
                      knit_root_dir = ".", 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = paste0("./Mean Reversion/", filename, ".Rmd"), 
                      output_file = paste0("./Mean Reversion/Notebooks/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "flexdashboard::flex_dashboard")
  }
} 

#' # 2. Render Research
render_wrapper("TMR.001 Load Packages", "html_document")
render_wrapper("TMR.002 Scrape Pricing Data", "html_document")
render_wrapper("TMR.003 Pairs Trading Functions", "html_document") 
render_wrapper("TMR.004 Check Pricing Data", "html_document") 
render_wrapper("TMR.005 Generate Predictions", "html_document") 

render_wrapper("RMR.001 Plot Pricing Data", "html_document")
render_wrapper("RMR.002 Test Cointegration", "html_document")
render_wrapper("RMR.003 Test Stationarity", "html_document")
render_wrapper("RMR.004 Search Cointegration Combinations", "html_document")
render_wrapper("RMR.005 Plot Cointegration Combinations", "html_document")
render_wrapper("RMR.006 Test Optimal Cointegration", "html_document")
render_wrapper("RMR.007 Cross Validate Plots", "html_document") 
render_wrapper("RMR.008 Cross Validate Tune", "html_document")
render_wrapper("RMR.009 Examine Tuning Parameters", "html_document")
render_wrapper("RMR.010 Export Feather", "html_document")

#' # 3. Beep 
beepr::beep(1)
