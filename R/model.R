library(corpcor)
library(riskParityPortfolio)
library(ggplot2)

rendimenti_caricati <- read.csv("rendimenti_logaritmici.csv")                      #Carico il file dei rendimenti logaritmici

rendimenti_caricati <- rendimenti_caricati[, sapply(rendimenti_caricati, is.numeric)]     #Mi assicuro che ci siano solo valori numerici

#Calcolo la matrice di covarianza utilizzando il metodo Ledoit-Wolf

lambda_opt <- estimate.lambda(rendimenti_caricati)                                 #Stimo il valore di shrinkrage ottimale


sigma_sample <- cov(rendimenti_caricati)                                           #Calcolo la matrice campionaria e quella di target
target <- diag(diag(sigma_sample))


sigma_shrinkage <- (1 - lambda_opt) * sigma_sample + lambda_opt * target           #Uso la formula di shrinkrage per calcolare la matrice di covarianza

is.symmetric <- all.equal(sigma_shrinkage, t(sigma_shrinkage))                     #Verifico che la matrice sia simmetrica
print(is.symmetric)

#Preparazione dei dati

mu <- colMeans(rendimenti_caricati)                                                #Rendimento atteso per ciascun asset

n_portafogli <- 10000                                                              #Numero di simulazioni

n_assets <- ncol(rendimenti_caricati)                                              #Numero di assets

results <- matrix(NA, nrow = n_portafogli, ncol = 3)                               #Matrice per salvare i risultati
colnames(results) <- c("Rendimento Atteso", "Rischio", "Sharpe Ratio")

rf <- 0

#Metodo Montecarlo

set.seed(42)                                                                       # per riproducibilità

for (i in 1:n_portafogli) {                                                        # Genera pesi casuali che sommano a 1
  
  weights <- runif(n_assets)
  weights <- weights / sum(weights)
                                                                               
  port_return <- sum(weights * mu)                                                 # Rendimento atteso del portafoglio
                                                                        
  port_risk <- sqrt(t(weights) %*% sigma_shrinkage %*% weights)                    # Rischio (deviazione standard) del portafoglio
                                                                          
  sharpe <- (port_return - rf) / port_risk                                         # Sharpe ratio
                                                                           
  results[i, ] <- c(port_return, port_risk, sharpe)                                # Salva i risultati
}

#Visualizzazione frontiera efficiente 

results_df <- as.data.frame(results)

ggplot(results_df, aes(x = Risk, y = Return, color = Sharpe)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Frontiera efficiente (Monte Carlo)",
       x = "Rischio (Dev. Std.)",
       y = "Rendimento atteso",
       color = "Sharpe Ratio")

