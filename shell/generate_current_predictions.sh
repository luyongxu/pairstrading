#!/bin/bash

sudo Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/03-call-generate-current-predictions.R" /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/generate_current_predictions.log"