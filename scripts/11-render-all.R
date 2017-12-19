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
render_wrapper <- function(filename, folder, output) { 
  if (output == "html_document") { 
    rmarkdown::render(input = paste0("./", folder, "/", filename, ".R"), 
                      output_file = paste0(filename, ".html"), 
                      output_dir = paste0("./notebooks/", folder, "/"), 
                      knit_root_dir = here::here(), 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = paste0("./", folder, "/", filename, ".R"), 
                      output_file = paste0(filename, ".pdf"), 
                      output_dir = paste0("./notebooks/", folder, "/"), 
                      knit_root_dir = here::here(), 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = paste0("./", folder, "/", filename, ".Rmd"), 
                      output_file = paste0(filename, ".html"), 
                      output_dir = paste0("./notebooks/", folder, "/"), 
                      knit_root_dir = here::here(), 
                      output_format = "flexdashboard::flex_dashboard")
  }
} 

#' # 2. Render Scripts
render_wrapper("01-plot-pricing-data", "scripts", "html_document")
render_wrapper("02-test-cointegration", "scripts", "html_document")
render_wrapper("03-test-stationarity", "scripts", "html_document")
render_wrapper("04-search-cointegration", "scripts", "html_document")
render_wrapper("05-plot-cointegration", "scripts", "html_document")
render_wrapper("06-test-cointegration", "scripts", "html_document")
render_wrapper("07-cross-validate-plots", "scripts", "html_document") 
render_wrapper("08-cross-validate-tune", "scripts", "html_document")
render_wrapper("09-examine-tuning-parameters", "scripts", "html_document")
render_wrapper("10-export-feather", "scripts", "html_document")

#' # 3. Render Utils 
render_wrapper("01-load-packages", "src/util", "html_document")
render_wrapper("02-download-data", "src/util", "html_document")
render_wrapper("03-set-parameters", "src/util", "html_document")
render_wrapper("04-data-functions", "src/util", "html_document") 
render_wrapper("05-coin-selection-functions", "src/util", "html_document")
render_wrapper("06-setup-strategy-functions", "src/util", "html_document")
render_wrapper("07-model-functions", "src/util", "html_document") 
render_wrapper("08-backtesting-functions", "src/util", "html_document") 
render_wrapper("09-plot-functions", "src/util", "html_document") 
render_wrapper("10-generate-predictions-functions", "src/util", "html_document") 
render_wrapper("11-check-data", "src/util", "html_document")
render_wrapper("12-generate-current-predictions", "src/util", "html_document")
render_wrapper("13-send-notifications", "src/util", "html_document")

#' # 4. Render Main 
render_wrapper("01-call-download-data.R", "src/main", "html_document")
render_wrapper("02-call-set-parameters.R", "src/main", "html_document")
render_wrapper("03-call-generate-current-predictions.R", "src/main", "html_document")

#' # 5. Beep 
beepr::beep(1)
