set.seed(123)  # Reproducibilidad

# Simulación Monte Carlo basada en la distribución real de G3
g3_real <- datos$G3

# Simular 10,000 notas usando muestreo con reemplazo
simulaciones <- sample(g3_real, size = 10000, replace = TRUE)

# Calcular probabilidades estimadas
prob_aprueba <- mean(simulaciones >= 10)
prob_excelente <- mean(simulaciones >= 15)
prob_fracaso <- mean(simulaciones < 5)

# Mostrar resultados
cat("Probabilidad de aprobar (G3 ≥ 10):", round(prob_aprueba, 4), "\n")
cat("Probabilidad de excelencia (G3 ≥ 15):", round(prob_excelente, 4), "\n")
cat("Probabilidad de fracaso (G3 < 5):", round(prob_fracaso, 4), "\n")

hist(simulaciones,
     breaks = 20,
     col = "lightgreen",
     main = "Distribución simulada de notas finales (G3)",
     xlab = "Nota simulada (G3)")
abline(v = 10, col = "blue", lwd = 2, lty = 2)
abline(v = 15, col = "darkgreen", lwd = 2, lty = 2)
abline(v = 5, col = "red", lwd = 2, lty = 2)

