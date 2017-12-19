#!/bin/bash

Rscript "/home/rstudio/kevin_lu_basket_mr/src/main/04-call-send-notifications.R" /home/rstudio/kevin_lu_basket_mr 2>&1 | 
sudo tee -a "/home/rstudio/kevin_lu_basket_mr/logs/send_notifications.log"