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
log_info("Generazione di {n_portafogli} portafogli casuali")
mu <- colMeans(rendimenti)
n_assets <- length(mu)
n_portafogli <- 10000
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

log_info("\nRISULTATI PRINCIPALI:",
         "\n- Portafoglio a minima varianza: Return = {results_df$Return[idx_min_var]}, Risk = {results_df$Risk[idx_min_var]}",
         "\n- Portafoglio a max Sharpe: Return = {results_df$Return[idx_max_sharpe]}, Risk = {results_df$Risk[idx_max_sharpe]}, Sharpe = {results_df$Sharpe[idx_max_sharpe]}")


log_success("Analisi completata con successo")
}
# Esecuzione con gestione degli errori
tryCatch({
  analysis_results <- run_portfolio_analysis()
}, error = function(e) {
  log_error("ANALISI FALLITA: {e$message}")
  stop("Processo interrotto")
})

