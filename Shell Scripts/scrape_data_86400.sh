#!/bin/bash

sleep 5s
Rscript "/home/rstudio/kevin_lu_basket_mr/Mean Reversion/TMR.002 Scrape Pricing Data.R" 86400 >> "home/rstudio/kevin_lu_basket_mr/Logs/scrape_data_86400.log" 2>&1
