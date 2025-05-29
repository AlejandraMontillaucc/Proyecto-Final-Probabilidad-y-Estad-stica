# Importamos nuestra base de datos
datos <- read.csv("student-mat-reduced.csv", header = TRUE, sep = ";")

# Seleccionar solo las columnas numéricas del dataset
numericas <- datos[sapply(datos, is.numeric)]

# Función para calcular la moda (el valor más frecuente)
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Crear una tabla resumen con todas las medidas
resumen <- data.frame(
  Variable = names(numericas),
  Media = sapply(numericas, mean),
  Mediana = sapply(numericas, median),
  Moda = sapply(numericas, moda),
  Rango = sapply(numericas, function(x) max(x) - min(x)),
  Varianza = sapply(numericas, var),
  Desviacion_Estandar = sapply(numericas, sd)
)
print(resumen)

# Instalar y cargar la librería ggplot2 si no está instalada
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Histograma de la nota final G3
ggplot(datos, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribución de las notas finales (G3)",
    x = "Nota final (G3)",
    y = "Cantidad de estudiantes"
  ) +
  theme_minimal()

# Histograma de ausencias
ggplot(datos, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") +
  labs(title = "Histograma de la Nota Final (G3)", x = "G3", y = "Frecuencia") +
  theme_minimal()

# Boxplot de G3
ggplot(datos, aes(y = G3)) +
  geom_boxplot(fill = "#009E73") +
  labs(title = "Boxplot de Nota Final (G3)", y = "G3") +
  theme_minimal()

# Boxplot de G3 por sexo
ggplot(datos, aes(x = sex, y = G3, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot de G3 por Sexo", x = "Sexo", y = "G3") +
  theme_minimal()


# Dispersión entre G2 y G3
ggplot(datos, aes(x = G2, y = G3)) +
  geom_point(color = "#CC79A7") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Diagrama de Dispersión G2 vs G3", x = "G2", y = "G3") +
  theme_minimal()

# Dispersión entre ausencias y G3
ggplot(datos, aes(x = absences, y = G3)) +
  geom_point(color = "#E69F00") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Ausencias vs Nota Final (G3)", x = "Ausencias", y = "G3") +
  theme_minimal()










