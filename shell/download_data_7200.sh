#!/bin/bash

sleep 180s
Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/01-call-download-data.R" 7200 /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/download_data_7200.log"