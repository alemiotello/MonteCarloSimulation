library(corpcor)
library(riskParityPortfolio)
library(ggrepel)
library(ggplot2)


# Caricamento e preparazione dati
rendimenti <- read.csv("rendimenti_logaritmici.csv")
rendimenti <- rendimenti[, sapply(rendimenti ,is.numeric)]

# Verifica dati mancanti
if (any(is.na(rendimenti))) {
  stop("Il dataset contiene valori mancanti")
}

# Calcolo matrice di covarianza ottimizzata
sigma_shrinkage <- cov.shrink(rendimenti, verbose = FALSE)
sigma_matrix <- as.matrix(sigma_shrinkage)

# Verifica simmetria con all.equal() + stampa un messaggio
is_symmetric <- all.equal(sigma_matrix, t(sigma_matrix))
if (isTRUE(is_symmetric)) {
  cat("La matrice è simmetrica (controllo superato).\n")
} else {
  cat("La matrice NON è simmetrica. Differenza massima:", max(abs(sigma_matrix - t(sigma_matrix))), "\n")
  sigma_matrix <- (sigma_matrix + t(sigma_matrix)) / 2
  cat("Simmetria forzata con (M + t(M))/2.\n")
}

# Parametri portafoglio
mu <- colMeans(rendimenti)
n_assets <- length(mu)
n_portafogli <- 10000
rf <- 0

# Generazione pesi ottimizzata
set.seed(42)
weights_matrix <- matrix(runif(n_portafogli * n_assets), ncol = n_assets)
weights_matrix <- weights_matrix / rowSums(weights_matrix)

# Calcolo performance vettorizzato
port_returns <- c(weights_matrix %*% mu)

# Calcolo rischio tramite decomposizione di Cholesky
L <- chol(sigma_shrinkage)
temp <- weights_matrix %*% L
port_risks <- sqrt(rowSums(temp^2))

# Calcolo Sharpe ratio
sharpe_ratios <- (port_returns - rf) / port_risks

# Creazione dataframe
results_df <- data.frame(
  Return = port_returns,
  Risk = port_risks,
  Sharpe = sharpe_ratios
)

# Indice del portafoglio a minima varianza
idx_min_var <- which.min(results_df$Risk)

# Indice del portafoglio a massimo Sharpe
idx_max_sharpe <- which.max(results_df$Sharpe)

# Portafoglio a minima varianza (minimo rischio)
min_var_portfolio <- results_df[idx_min_var, ]

# Portafoglio a massimo Sharpe Ratio
max_sharpe_portfolio <- results_df[idx_max_sharpe, ]

# Visualizzazione avanzata (log-rendimenti)
ggplot(results_df, aes(x = Risk, y = Return, color = Sharpe)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(option = "plasma") +
  
  # Evidenzia minima varianza (triangolo blu)
  geom_point(
    data = min_var_portfolio,
    aes(x = Risk, y = Return),
    color = "blue",
    size = 5,
    shape = 17
  ) +
  
  # Evidenzia massimo Sharpe (rombo rosso)
  geom_point(
    data = max_sharpe_portfolio,
    aes(x = Risk, y = Return),
    color = "red",
    size = 5,
    shape = 18
  ) +
  
  # Aggiungi etichette
  geom_label_repel(
    data = min_var_portfolio,
    aes(label = "Minima Varianza"),
    color = "blue",
    box.padding = 1
  ) +
  geom_label_repel(
    data = max_sharpe_portfolio,
    aes(label = paste("Max Sharpe:", round(Sharpe, 2))),
    color = "red",
    box.padding = 1
  ) +
  
  labs(title = "Frontiera Efficiente con Portafogli Ottimali (da pesi esistenti)")

# Stampo caratteristiche portafogli speciali
nomi_asset <- colnames(rendimenti)

# Pesi minima varianza
pesi_min_var <- weights_matrix[idx_min_var, ]
names(pesi_min_var) <- nomi_asset  # Assegna nomi agli asset

# Pesi massimo Sharpe
pesi_max_sharpe <- weights_matrix[idx_max_sharpe, ]
names(pesi_max_sharpe) <- nomi_asset

cat("--- PORTAFOGLIO A MINIMA VARIANZA ---\n")
pesi_min_var <- weights_matrix[which.min(results_df$Risk), ]
names(pesi_min_var) <- nomi_asset  # Assegna i nomi
print((min_var_portfolio))
print(data.frame(Asset = nomi_asset, Peso = round(pesi_min_var, 4)))  # Tabella ordinata
cat("Somma pesi:", sum(pesi_min_var), "\n\n")

cat("--- PORTAFOGLIO A MASSIMO SHARPE ---\n")
pesi_max_sharpe <- weights_matrix[which.max(results_df$Sharpe), ]
names(pesi_max_sharpe) <- nomi_asset
print(max_sharpe_portfolio)
print(data.frame(Asset = nomi_asset, Peso = round(pesi_max_sharpe, 4)))
cat("Somma pesi:", sum(pesi_max_sharpe), "\n")

