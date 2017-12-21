#!/bin/bash

sleep 210s
Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/01-call-download-data.R" 14400 /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/download_data_14400.log"