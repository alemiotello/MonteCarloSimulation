library(alphavantager)
library(tidyverse)
library(lubridate)

# Inserisci la tua API key
av_api_key("OXDGASU6MYVVT7LI")

# Elenco asset compatibili con Alpha Vantage 
assets <- c("AAPL", "MSFT", "JNJ", "VOO", "TLT", 
            "GLD", "XLE", "SH")  # escludo BRK-B e BTC-USD per ora

# Funzione per scaricare dati e adattare alle colonne attese
scarica_dati <- function(symbol) {
  tryCatch({
    av_get(symbol, av_fun = "TIME_SERIES_DAILY", outputsize = "full") %>%
      select(date = timestamp, adjusted = close) %>%
      mutate(symbol = symbol) %>%
      filter(date >= as.Date("2020-01-01")) %>%
      arrange(date)
  }, error = function(e) {
    message(paste("Errore con", symbol, ":", e$message))
    return(NULL)
  })
}
# Scarica dati per tutti
lista_dati <- lapply(assets, scarica_dati)

# Combina in un unico data frame
dati <- bind_rows(lista_dati) %>%
  filter(!is.na(adjusted))

