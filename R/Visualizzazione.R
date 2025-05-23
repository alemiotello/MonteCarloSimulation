library(ggrepel)
library(ggplot2)

load("risultati_analisi.RData")

# 7. Output grafico frontiera efficiente
g <- ggplot(results_df, aes(x = Risk, y = Return, color = Sharpe)) +
  geom_hex(bins = 50) +
  geom_point(data = results_df[idx_min_var, ], aes(x = Risk, y = Return), 
             color = "blue", size = 3, shape = 17) +
  geom_point(data = results_df[idx_max_sharpe, ], aes(x = Risk, y = Return), 
             color = "red", size = 3, shape = 18) +
  labs(title = "Frontiera Efficiente con Portafogli Ottimali",
       x = "Rischio (Dev. Std.)", y = "Rendimento Atteso") +
  theme_minimal()

print(g)

