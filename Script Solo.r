data<-read.table("C:\\Users\\dell\\Desktop\\TXT.txt",header=T);
#data;

n <- length(data$datos)

R <- max(data) - min(data)

C <- round(3.3 * log10(n) + 1)

W <- ceiling(R / C)

PM <- round((W * C - R) / 2)

Finf <- min(data$datos) - PM

Fsup <- Finf + W

RR <- data.frame(table(cut(data$datos, seq(Finf, (Finf + (C) * W), by = W), right = FALSE)))

RR2 <- transform(RR, 
                 Fa = cumsum(Freq), 
                 fr = round(Freq / n, 3), 
                 Fr = round(cumsum(Freq) / n, 3),  
                 fr1 = round(Freq / n, 3) * 100, 
                 Fr1 = round(cumsum(Freq) / n, 3) * 100,  
                 MC = seq((Finf + Fsup) / 2, (Finf + Fsup) / 2 + (C - 1) * W, by = W))

print(RR2)

Int <- seq(Finf, (Finf + (C - 1) * W), by = W)

SInt <- Int + W

par(ps = 7, mex = 0.3, mfrow = c(1, 3))

ojiva <- plot(SInt, RR2$Fa, type = "o", main = "Ojiva", ylab = "Frecuencia Absoluta Acumulada", xlab = "Fronteras Superiores Clases")

histograma <- hist(data$datos, breaks = seq(Finf, (Finf + (C) * W), by = W), labels = TRUE, col = rainbow(C), right = FALSE, ylab = "Frecuencias Absollutas", xlab = "Intervalos")

yy = hist(data$datos,
          breaks = seq(Finf, (Finf + (C) * W), by = W),
          labels = T,
          col = rainbow(C),
          right = F,
          ylab = "Frecuencias Absolutas",
          xlab = "Intervalos",
          main = "Polígono de Frecuencia",
          xlim = c(min(yy$mids) - 1.5 * W, max(yy$mids) + 1.5 * W),
          ylim = c(0, max(yy$counts) * 1.2))


lines(
  x = c(yy$mids[1] - W, yy$mids, yy$mids[length(yy$mids)] + W),
  y = c(0, yy$counts, 0),
  type = "l"
)

summary(data$datos)
