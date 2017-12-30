#!/bin/bash

sudo Rscript "/home/rstudio/kevin_lu_basket_mr/trade-execution/01-generate-trades.R" /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/generate_trades.log"