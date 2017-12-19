#!/bin/bash

Rscript -e "shiny::runApp(appDir = '/home/rstudio/kevin_lu_basket_mr/app/', port = 8888)" 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/launch_shiny.log"