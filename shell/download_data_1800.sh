#!/bin/bash

sleep 5s
Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/01-call-download-data.R" 1800 /home/rstudio/kevin_lu_basket_mr | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/download_data_1800.log"