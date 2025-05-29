# Importamos nuestra base de datos
datos <- read.csv("student-mat-reduced.csv", header = TRUE, sep = ";")

# Cargar librerías necesarias
library(ggplot2)
library(readr)

# Histograma con curva normal superpuesta
ggplot(datos, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#56B4E9", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(datos$G3), sd = sd(datos$G3)), 
                color = "red") +
  labs(title = "Ajuste de Distribución Normal a G3", x = "G3", y = "Densidad") +
  theme_minimal()


#Pruebas de normalidad para G3
# Shapiro-Wilk (solo si n < 5000)
shapiro.test(datos$G3)
# Kolmogorov-Smirnov
ks.test(datos$G3, "pnorm", mean = mean(datos$G3), sd = sd(datos$G3))


# Histograma de absences con distribución de Poisson
ggplot(datos, aes(x = absences)) +
  geom_histogram(binwidth = 1, fill = "#F0E442", color = "black") +
  stat_function(fun = function(x) dpois(x, lambda = mean(datos$absences)) * length(datos$absences),
                color = "red") +
  labs(title = "Ajuste de Poisson a Ausencias", x = "Ausencias", y = "Frecuencia") +
  theme_minimal()


# Calcular frecuencia observada de ausencias
observado <- table(datos$absences)

# Calcular frecuencia esperada con distribución de Poisson
lambda <- mean(datos$absences)
valores <- as.numeric(names(observado))
esperado <- dpois(valores, lambda) * length(datos$absences)

# Prueba de Chi-cuadrado
chisq.test(x = observado, p = esperado / sum(esperado))


