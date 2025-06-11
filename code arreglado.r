data <- read.table("C:\\Users\\mijail\\Desktop\\TXT.txt", header = TRUE)

n <- length(data$datos)
R <- max(data$datos) - min(data$datos)    # ojo: max(data) estaba mal
C <- round(3.3 * log10(n) + 1)
W <- ceiling(R / C)
PM <- round((W * C - R) / 2)
Finf <- min(data$datos) - PM

# Construcción de tabla de frecuencia (ya la tenías bien)
RR2 <- transform(
  as.data.frame(table(cut(
    data$datos,
    breaks = seq(Finf, Finf + C*W, by = W),
    right = FALSE
  ))),
  Fa = cumsum(Freq),
  fr = round(Freq / n, 3),
  Fr = round(cumsum(Freq) / n, 3),
  fr1 = round(Freq / n, 3) * 100,
  Fr1 = round(cumsum(Freq) / n, 3) * 100,
  MC = seq(
    (Finf + Finf + W) / 2,
    (Finf + Finf + W) / 2 + (C-1)*W,
    by = W
  )
)
print(RR2)

Int <- seq(Finf, (Finf + (C-1)*W), by = W)
SInt <- Int + W

par(ps = 7, mex = 0.3, mfrow = c(1, 3))

# Ojiva
plot(SInt, RR2$Fa, type = "o",
     main = "Ojiva",
     ylab = "Frecuencia Absoluta Acumulada",
     xlab = "Fronteras Superiores Clases")

# Histograma
hist(data$datos,
     breaks = seq(Finf, Finf + C*W, by = W),
     labels = TRUE,
     col = rainbow(C),
     right = FALSE,
     ylab = "Frecuencias Absolutas",
     xlab = "Intervalos")

# Polígono de frecuencia: primero guarda hist(), luego grafica
yy <- hist(data$datos,
           breaks = seq(Finf, Finf + C*W, by = W),
           plot = FALSE)  # no dibujar aún

# Obtener límites dinámicos
xlim_range <- c(min(yy$mids) - 1.5 * W, max(yy$mids) + 1.5 * W)
ylim_range <- c(0, max(yy$counts) * 1.2)

# Ahora sí dibuja el histograma con los límites
plot(yy,
     col = rainbow(C),
     right = FALSE,
     ylab = "Frecuencias Absolutas",
     xlab = "Intervalos",
     main = "Polígono de Frecuencia",
     xlim = xlim_range,
     ylim = ylim_range)

# Añadir líneas del polígono
lines(x = c(yy$mids[1] - W, yy$mids, yy$mids[length(yy$mids)] + W),
      y = c(0, yy$counts, 0),
      type = "l")

# Resumen de datos
summary(data$datos)
