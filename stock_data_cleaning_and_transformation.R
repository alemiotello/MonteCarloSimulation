library(tidyverse)
library(tidyquant)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(xts)

#Crea file csv con dati
write.csv(dati, file = "stock_data.csv", row.names = FALSE)

#Carica file csv con dati
dati_caricati <- read.csv("stock_data.csv")

# Converti in formato tibble e pulisci i dati
dati_clean <- dati_caricati %>%
  select(symbol, date, adjusted) %>%  # Prendi solo le colonne necessarie
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Converti in formato xts
dati_xts <- xts(dati_clean[,-1], order.by = dati_clean$date)

# Calcola i rendimenti logaritmici
rendimenti <- na.omit(Return.calculate(dati_xts, method = "log"))

