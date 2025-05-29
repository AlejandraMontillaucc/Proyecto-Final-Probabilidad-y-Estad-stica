
cor(datos$G2, datos$G3, method = "pearson")     # Correlación entre G2 y G3
cor(datos$studytime, datos$G3, method = "pearson") # Tiempo de estudio y nota final

# Crear modelo de regresión lineal múltiple
modelo_multip <- lm(G3 ~ studytime + failures + absences + goout + Dalc + Walc + age, data = datos)

# Mostrar resumen del modelo
summary(modelo_multip)

