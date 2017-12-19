#!/bin/bash

Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/02-call-set-params.R" /home/rstudio/kevin_lu_basket_mr | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/set_params.log"