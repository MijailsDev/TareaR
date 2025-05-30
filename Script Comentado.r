#****SCRIPD CON COMENTARIOS****


data<-read.table("C:\\Users\\dell\\Desktop\\TXT.txt",header=T);
# Aquí le decimos a R que agarre el archivo que tienes guardado en la compu,
# y que lea los datos para que podamos usarlos. El "header=TRUE" es para decirle
# que la primera fila son los títulos, como los nombres de las columnas.

#data;
# Esto está comentado, pero si lo activas te muestra los datos que acabas de cargar.

n <- length(data$datos)
# Aquí contamos cuántos números o datos tienes en la columna que se llama "datos".
# O sea, cuántos números vas a usar para hacer los cálculos.

R <- max(data) - min(data)
# Sacamos la diferencia entre el número más grande y el más chico,
# eso nos dice qué tan “grande” es el rango de tus datos.

C <- round(3.3 * log10(n) + 1)
# Esta fórmula nos ayuda a decidir en cuántos grupos o "clases" dividir tus datos.
# Es como pensar en cuántos "cajones" necesitamos para poner todos los datos ordenados.

W <- ceiling(R / C)
# Aquí calculamos qué tan anchos tienen que ser esos cajones, para que todos entren.
# Usamos ceiling para redondear hacia arriba, así no dejamos nada fuera.

PM <- round((W * C - R) / 2)
# A veces, al redondear, se genera un pequeño error en el tamaño total,
# así que calculamos este ajuste para compensar y que quede todo bien parejito.

Finf <- min(data$datos) - PM
# Ahora definimos dónde empieza el primer intervalo o cajón, restando ese ajuste.

Fsup <- Finf + W
# Y acá sacamos dónde termina ese primer intervalo, sumando el ancho del cajón.

RR <- data.frame(table(cut(data$datos, seq(Finf, (Finf + (C) * W), by = W), right = FALSE)))
# Ahora sí, partimos los datos en esos grupos o intervalos que calculamos,
# y contamos cuántos datos hay en cada grupo. Esto es clave para hacer el histograma.

RR2 <- transform(RR, 
                 Fa = cumsum(Freq), 
                 fr = round(Freq / n, 3), 
                 Fr = round(cumsum(Freq) / n, 3),  
                 fr1 = round(Freq / n, 3) * 100, 
                 Fr1 = round(cumsum(Freq) / n, 3) * 100,  
                 MC = seq((Finf + Fsup) / 2, (Finf + Fsup) / 2 + (C - 1) * W, by = W))
# Aquí le agregamos más info a la tabla: 
# Fa es la suma de frecuencias hasta ese punto (como ir guardando cuántos vas juntando),
# fr es la proporción de cada grupo respecto al total,
# Fr es la proporción acumulada hasta ese grupo,
# fr1 y Fr1 son esos mismos datos pero en porcentaje, para que se entiendan más fácil,
# y MC es el punto medio de cada grupo, que nos ayuda a graficar bien.

print(RR2)
# Aquí simplemente imprimimos esa tabla chida para verla en pantalla y entender qué hay.

Int <- seq(Finf, (Finf + (C - 1) * W), by = W)
# Creamos una lista con los límites inferiores de cada grupo, para usarlos en el gráfico.

SInt <- Int + W
# Y acá sacamos los límites superiores de cada grupo, sumándole el ancho a los inferiores.

par(ps = 7, mex = 0.3, mfrow = c(1, 3))
# Configuramos la ventana para poner 3 gráficos uno al lado del otro,
# y ajustamos tamaños para que no se vea gigante ni muy pequeño el texto.

ojiva <- plot(SInt, RR2$Fa, type = "o", main = "Ojiva", ylab = "Frecuencia Absoluta Acumulada", xlab = "Fronteras Superiores Clases")
# Dibujamos la ojiva, que es una línea que muestra cómo se van acumulando los datos
# según los intervalos que hicimos.

histograma <- hist(data$datos, breaks = seq(Finf, (Finf + (C) * W), by = W), labels = TRUE, col = rainbow(C), right = FALSE, ylab = "Frecuencias Absollutas", xlab = "Intervalos")
# Hacemos el histograma, que es un gráfico con barras para ver cuántos datos hay en cada grupo.
# Lo pintamos de magenta para que se vea chido y ponemos etiquetas.

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

# Hacemos otra vez el histograma para agarrar los datos que necesitamos para el siguiente gráfico,
# que es el polígono de frecuencia.

lines(
  x = c(yy$mids[1] - W, yy$mids, yy$mids[length(yy$mids)] + W),
  y = c(0, yy$counts, 0),
  type = "l"
)
# Aquí dibujamos líneas conectando los puntos de medio de cada barra,
# para formar ese polígono que ayuda a ver la forma de la distribución.

summary(data$datos)
# Para acabar, mostramos un resumen con lo más importante: mínimo, máximo,
# promedio, mediana, y otros datos que ayudan a entender el comportamiento de los datos.


#****PARA QUE SIRVEN LAS FUNCIONES DEL SCRIPD****
#
#read.table() — Lee el archivo con los datos para poder usarlos en R.
#
#length() — Cuenta cuántos datos tienes en tu lista.
#
#max(), min() — Encuentra el número más alto y más bajo.
#
#round() — Redondea números para que no tengan decimales raros.
#
#log10() — Calcula el logaritmo (una operación matemática que ayuda en el cálculo de clases).
#
#ceiling() — Redondea hacia arriba para no pasarnos de largo.
#
#table() — Cuenta cuántas veces aparece cada cosa.
#
#cut() — Divide tus datos en grupos o intervalos.
#
#data.frame() — Arma una tabla organizada para mostrar los datos.
#
#transform() — Agrega o modifica columnas en esa tabla.
#
#cumsum() — Va sumando valores de forma acumulada.
#
#seq() — Crea una lista o secuencia de números.
#
#print() — Muestra algo en pantalla para que puedas verlo.
#
#par() — Ajusta cómo se muestran los gráficos.
#
#plot() — Dibuja gráficos, como líneas o puntos.
#
#hist() — Dibuja un histograma para ver cómo están distribuidos los datos.
#
#lines() — Dibuja líneas en el gráfico que ya hiciste.
#
#summary() — Muestra datos estadísticos básicos sobre tus datos.