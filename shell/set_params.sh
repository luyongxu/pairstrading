#!/bin/bash

sudo Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/02-call-set-parameters.R" /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/set_params.log"