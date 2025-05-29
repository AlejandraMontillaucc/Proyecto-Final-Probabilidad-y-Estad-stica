# Calcular la media
media <- mean(datos$G3)

# Calcular el error estándar
error <- qt(0.975, df = length(datos$G3) - 1) * sd(datos$G3) / sqrt(length(datos$G3))

# Calcular los límites inferior y superior del intervalo
LI <- media - error
LS <- media + error

# Mostrar el intervalo
c(LI, LS)


# Prueba t para comparar medias de G3 según el sexo
t.test(G3 ~ sex, data = datos)

#Pruebas de Hipótesis (usando wilcox.test)

#Apoyo familiar (famsup)
wilcox.test(G3 ~ famsup, data = datos)

#Sexo (sex)
wilcox.test(G3 ~ sex, data = datos)

#Internet en casa (internet)
wilcox.test(G3 ~ internet, data = datos)

# Tiene pareja (romantic)
wilcox.test(G3 ~ romantic, data = datos)

