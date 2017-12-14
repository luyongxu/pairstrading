#!/bin/bash

sleep 5s
Rscript "/home/rstudio/kevin_lu_basket_mr/src/02-download-data.R" 14400 | sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/scrape_data_14400.log"