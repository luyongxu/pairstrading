library(tidyverse)
library(zoo)

y <- rnorm(1000)
x <- rnorm(1000)
df <- tibble(y = y, x = x)
lm(y ~ x)
temp <- rollapply(data = df, 
                  width = 100, 
                  FUN = function(df) { 
                    model <- lm(y ~ x, data = as_tibble(df))
                    return(coef(model))
                  }, 
                  by.column = FALSE, 
                  fill = NA, 
                  align = "right") %>% 
  as_tibble()



          
        