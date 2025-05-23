library(logger)
library(corpcor)
library(riskParityPortfolio)

# Configurazione iniziale del logger
log_appender(appender_console)  # Output sulla console
log_threshold(DEBUG)  # Mostra TUTTI i messaggi (DEBUG, INFO, WARN, ERROR)

# Funzione principale
run_portfolio_analysis <- function() {
  log_info("Inizio analisi del portafoglio")
  
  # 1. Caricamento dati
  log_info("Caricamento dati da 'rendimenti_logaritmici.csv'")
  rendimenti <- tryCatch({
    read.csv("rendimenti_logaritmici.csv")[, sapply(read.csv("rendimenti_logaritmici.csv"), is.numeric)]
  }, error = function(e) {
    log_error("Errore nel caricamento dei dati: {e$message}")
    stop("Caricamento dati fallito")
  })
  
  if (any(is.na(rendimenti))) {
    log_warn("Il dataset contiene valori mancanti. Verifica la qualità dei dati.")
    stop("Dati mancanti non ammessi")
  } else {
    log_success("Dati caricati correttamente (N={nrow(rendimenti)} osservazioni, {ncol(rendimenti)} asset)")
  }

# 1. Caricamento dati
log_info("Caricamento dati da 'rendimenti_logaritmici.csv'")
rendimenti <- tryCatch({
  read.csv("rendimenti_logaritmici.csv")[, sapply(read.csv("rendimenti_logaritmici.csv"), is.numeric)]
}, error = function(e) {
  log_error("Errore nel caricamento dei dati: {e$message}")
  stop("Caricamento dati fallito")
})

if (any(is.na(rendimenti))) {
  log_warn("Il dataset contiene valori mancanti. Verifica la qualità dei dati.")
  stop("Dati mancanti non ammessi")
} else {
  log_success("Dati caricati correttamente (N={nrow(rendimenti)} osservazioni, {ncol(rendimenti)} asset)")
}

# 2. Calcolo matrice di covarianza
log_info("Calcolo matrice di covarianza con shrinkage")
sigma_shrinkage <- tryCatch({
  cov.shrink(rendimenti, verbose = FALSE)
}, error = function(e) {
  log_error("Errore nel calcolo della covarianza: {e$message}")
  stop("Calcolo covarianza fallito")
})

sigma_matrix <- as.matrix(sigma_shrinkage)

# 3. Verifica simmetria
if (!isTRUE(all.equal(sigma_matrix, t(sigma_matrix), tolerance = 1e-10))) {
  log_warn("Matrice non simmetrica. Differenza massima: {max(abs(sigma_matrix - t(sigma_matrix)))}")
  sigma_matrix <- (sigma_matrix + t(sigma_matrix)) / 2
  log_info("Applicata correzione di simmetria")
} else {
  log_debug("Matrice simmetrica (controllo superato)")
}

# 4. Generazione portafogli
n_portafogli <- 10000
log_info("Generazione di {n_portafogli} portafogli casuali")
mu <- colMeans(rendimenti)
n_assets <- length(mu)
rf <- 0.035   #risk-free rate

set.seed(42)
weights_matrix <- matrix(runif(n_portafogli * n_assets), ncol = n_assets)
weights_matrix <- weights_matrix / rowSums(weights_matrix)

# 5. Calcolo rendimento annualizzato, rischio annualizzato tramite decomposizione di Cholesky e Sharpe-Ratio annualizzato
log_info("Calcolo metriche di performance")
trading_days <- 252
port_returns <- c(weights_matrix %*% mu) * trading_days
L <- chol(sigma_matrix)
port_risks <- sqrt(rowSums((weights_matrix %*% L)^2)) * sqrt(trading_days)
sharpe_ratios <- (port_returns - rf) / port_risks

log_info("Creazione dataframe")
results_df <- data.frame(
  Return = port_returns,
  Risk = port_risks,
  Sharpe = sharpe_ratios
)

# 6. Identificazione portafogli ottimali
log_info("Identificazione portafogli ottimali")
idx_min_var <- which.min(results_df$Risk)
idx_max_sharpe <- which.max(results_df$Sharpe)
weights_min_var <- weights_matrix[idx_min_var, ]
weights_max_sharpe <- weights_matrix[idx_max_sharpe, ]
asset_names <- colnames(rendimenti)
weights_min_var_named <- setNames(weights_min_var, asset_names)
weights_max_sharpe_named <- setNames(weights_max_sharpe, asset_names)

log_info("\nRISULTATI PRINCIPALI:",
         "\n- Portafoglio a minima varianza: Return = {round(results_df$Return[idx_min_var], 4)}, Risk = {round(results_df$Risk[idx_min_var], 4)}",
         "\n  Pesi: {paste(names(weights_min_var_named), round(weights_min_var_named, 4), sep = ': ', collapse = ', ')}",
         "\n- Portafoglio a max Sharpe: Return = {round(results_df$Return[idx_max_sharpe], 4)}, Risk = {round(results_df$Risk[idx_max_sharpe], 4)}, Sharpe = {round(results_df$Sharpe[idx_max_sharpe], 4)}",
         "\n  Pesi: {paste(names(weights_max_sharpe_named), round(weights_max_sharpe_named, 4), sep = ': ', collapse = ', ')}")

# 7. Salvataggio dei pesi in CSV
log_info("Salvataggio informazioni sui pesi in 'portafogli_ottimali.csv'")

df_pesi <- rbind(
  data.frame(Tipo = "Minima Varianza", Asset = names(weights_min_var_named), Peso = weights_min_var_named),
  data.frame(Tipo = "Massimo Sharpe", Asset = names(weights_max_sharpe_named), Peso = weights_max_sharpe_named)
)

write.csv(df_pesi, file = "portafogli_ottimali.csv", row.names = FALSE)

log_success("Analisi completata con successo")


save(results_df, idx_min_var, idx_max_sharpe, weights_min_var_named, weights_max_sharpe_named, file = "risultati_analisi.RData")
}
# Esecuzione con gestione degli errori
tryCatch({
  analysis_results <- run_portfolio_analysis()
}, error = function(e) {
  log_error("ANALISI FALLITA: {e$message}")
  stop("Processo interrotto")
})

