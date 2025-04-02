library(tidyverse)
library(tidyquant)
library(dplyr)
library(tidyr)

#Crea vettore di assets
assets <- c("AAPL", "MSFT", "AMZN", "GOOG", "TSLA", 
            "META", "NVDA", "JPM", "V", "WMT") 

#Fetch dei dati da yahoo finance usando tidyquant
dati <- tq_get(assets,
        get = "stock.prices",
        from = "2020-01-01",
        to = Sys.Date()
)




