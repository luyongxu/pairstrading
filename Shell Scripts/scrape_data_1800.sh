#!/bin/bash

sleep 5s
Rscript "/home/rstudio/kevin_lu_basket_mr/Mean Reversion/TMR.002 Scrape Pricing Data.R" 1800 | sudo tee -a "/home/rstudio/kevin_lu_basket_mr/Logs/scrape_data_1800.log"