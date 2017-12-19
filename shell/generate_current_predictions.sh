#!/bin/bash

Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/03-call-generate-current-predictions.R" /home/rstudio/kevin_lu_basket_mr | 
  sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/set_params.log"