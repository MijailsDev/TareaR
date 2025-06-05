# Aprendiendo R
https://drive.google.com/file/d/1CK4hYB8tdpU3eaWDJQdC3VlMbvzJ6Yrh/view?usp=sharing
## 🔹 PRIMERA PARTE: Cargar y preparar los datos

```r
data <- read.table("C:\\Users\\dell\\Desktop\\TXT.txt", header = T)
```

🔍 **¿Qué hace esta línea?**

* `read.table(...)`: es una función para **leer archivos de texto** como tablas (filas y columnas).
* `"C:\\Users\\dell\\Desktop\\TXT.txt"`: es la **ruta completa del archivo** que estás leyendo.
* `header = T`: indica que el archivo **tiene una fila de encabezados** (nombres de las columnas).
* `<-`: es el operador de **asignación** en R. Es como el `=` en otros lenguajes.

🟢 **Resultado:** Crea un objeto llamado `data` que contiene los datos del archivo de texto. Por ejemplo, si tu archivo tiene una columna llamada `datos`, ahora puedes acceder a ella con `data$datos`.

---

## 🔹 SEGUNDA PARTE: Parámetros básicos para distribución de frecuencias

```r
n <- length(data$datos)
```

* `length(data$datos)`: cuenta cuántos valores hay en la columna `datos`.
* `n <- ...`: guarda ese número en la variable `n`.

```r
R <- max(data) - min(data)
```

* `max(data)` y `min(data)`: buscan el valor máximo y mínimo en todo el *data frame*, **no solo en la columna `datos`**, lo cual puede ser un error si tienes más columnas.
* `R`: representa el **recorrido** o **rango total** de los datos.

⚠️ **RECOMENDACIÓN:** Es más seguro usar:

```r
R <- max(data$datos) - min(data$datos)
```

```r
C <- round(3.3 * log10(n) + 1)
```

* `log10(n)`: calcula el logaritmo base 10 de `n`.
* `3.3 * log10(n) + 1`: es una fórmula para estimar el **número de clases** (`C`) según **Sturges**.
* `round(...)`: redondea el resultado al número entero más cercano.

```r
W <- ceiling(R / C)
```

* `R / C`: calcula el **ancho del intervalo** (tamaño de clase).
* `ceiling(...)`: redondea hacia **arriba**, para que cubra todos los datos.

```r
PM <- round((W * C - R) / 2)
```

* `W * C`: el total de cobertura de los intervalos.
* `W * C - R`: el exceso de cobertura.
* `(...)/2`: divide el exceso entre los extremos (margen).
* `PM`: **punto medio de ajuste**, lo usa para ajustar los extremos inferior y superior.

```r
Finf <- min(data$datos) - PM
Fsup <- Finf + W
```

* `Finf`: frontera inferior inicial, ajustada restando `PM`.
* `Fsup`: calcula la primera frontera superior sumando el ancho `W`.

---

## 🔹 TERCERA PARTE: Construcción de la tabla de frecuencias

```r
RR <- data.frame(table(cut(data$datos, seq(Finf, (Finf + (C) * W), by = W), right = FALSE)))
```

🔍 **¿Qué hace esta línea?**

### Desglosando paso a paso:

* `data$datos`: es la columna con los valores numéricos.
* `seq(Finf, (Finf + (C) * W), by = W)`:

  * `seq(...)` genera una **secuencia de números** desde `Finf` hasta `Finf + (C * W)`, con saltos de tamaño `W`.
  * Esto crea los **límites de clase**.
* `cut(data$datos, ..., right = FALSE)`:

  * **Corta los datos** en **intervalos/clases** definidos por la secuencia.
  * `right = FALSE`: significa que los intervalos son **cerrados por la izquierda** y **abiertos por la derecha**: `[a, b)`.
* `table(...)`: cuenta cuántos valores caen en cada intervalo.
* `data.frame(...)`: convierte el resultado en una tabla de datos, con dos columnas: el intervalo y su frecuencia.

🔢 **Resultado:** `RR` es una tabla con los intervalos y cuántos datos hay en cada uno.

---

## 🔹 CUARTA PARTE: Agregando más columnas a la tabla

```r
RR2 <- transform(RR,
  Fa = cumsum(Freq),
  fr = round(Freq / n, 3),
  Fr = round(cumsum(Freq) / n, 3),
  fr1 = round(Freq / n, 3) * 100,
  Fr1 = round(cumsum(Freq) / n, 3) * 100,
  MC = seq((Finf + Fsup) / 2, (Finf + Fsup) / 2 + (C - 1) * W, by = W)
)
```

🔍 **¿Qué es `transform`?**

* Es una forma de **agregar nuevas columnas** a un `data.frame` sin tener que escribir `RR2$columna <- ...` cada vez.

### ¿Qué se agrega?

* `Fa = cumsum(Freq)`: **frecuencia absoluta acumulada**. Suma acumulativa de los valores de la columna `Freq`.
* `fr = round(Freq / n, 3)`: **frecuencia relativa** (proporción de cada clase). Redondeada a 3 decimales.
* `Fr = round(cumsum(Freq) / n, 3)`: **frecuencia relativa acumulada**.
* `fr1 = ... * 100`: frecuencia relativa como **porcentaje**.
* `Fr1 = ... * 100`: frecuencia relativa acumulada como **porcentaje**.
* `MC = seq(...)`: **marca de clase** (punto medio de cada clase).

🔢 **Resultado:** `RR2` ahora tiene todas las frecuencias y marcas de clase.

---

## 🔹 QUINTA PARTE: Imprimir la tabla

```r
print(RR2)
```

* Muestra en consola la tabla `RR2` completa con las columnas que acabamos de calcular.

---

## 🔹 SEXTA PARTE: Preparación de intervalos para los gráficos

```r
Int <- seq(Finf, (Finf + (C - 1) * W), by = W)
SInt <- Int + W
```

* `Int`: vector con los **límites inferiores** de las clases.
* `SInt`: vector con los **límites superiores** sumando `W`.

---

## 🔹 SÉPTIMA PARTE: Configuración de la ventana gráfica

```r
par(ps = 7, mex = 0.3, mfrow = c(1, 3))
```

* `ps = 7`: tamaño del texto (point size).
* `mex = 0.3`: margen de los ejes en la ventana gráfica.
* `mfrow = c(1, 3)`: divide la ventana gráfica en **1 fila y 3 columnas**, para mostrar 3 gráficos al mismo tiempo.

---

## 🔹 OCTAVA PARTE: Graficar la **Ojiva**

```r
ojiva <- plot(SInt, RR2$Fa, type = "o", main = "Ojiva", ylab = "Frecuencia Absoluta Acumulada", xlab = "Fronteras Superiores Clases")
```

* `plot(...)`: función para graficar.
* `SInt`: límites superiores.
* `RR2$Fa`: frecuencias absolutas acumuladas.
* `type = "o"`: puntos y líneas.
* `main`, `xlab`, `ylab`: títulos del gráfico y ejes.

---

## 🔹 NOVENA PARTE: Histograma

```r
histograma <- hist(data$datos, breaks = seq(Finf, (Finf + (C) * W), by = W), labels = TRUE, col = rainbow(C), right = FALSE, ylab = "Frecuencias Absollutas", xlab = "Intervalos")
```

* `hist(...)`: dibuja un histograma.
* `breaks`: límites de clase.
* `labels = TRUE`: muestra las frecuencias sobre cada barra.
* `col = rainbow(C)`: colores diferentes para cada barra.
* `right = FALSE`: intervalos cerrados por la izquierda.
* `xlab`, `ylab`: etiquetas de los ejes.

---

## 🔟 DÉCIMA PARTE: Polígono de frecuencia

```r
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
```

Este segundo `hist()` sirve para:

* Dibujar el polígono de frecuencia sobre un histograma vacío.
* `xlim`, `ylim`: ajustan los límites del gráfico para que el polígono no se corte.

```r
lines(
  x = c(yy$mids[1] - W, yy$mids, yy$mids[length(yy$mids)] + W),
  y = c(0, yy$counts, 0),
  type = "l"
)
```

* `yy$mids`: marcas de clase (centros de barra).
* `yy$counts`: frecuencias.
* Añade una **línea continua** (tipo polígono de frecuencia), partiendo del 0 antes del primer punto y terminando en 0 después del último.

---

## 🔚 ÚLTIMA PARTE: Resumen estadístico

```r
summary(data$datos)
```

* Muestra un resumen de estadísticos básicos:

  * **mínimo**, **1er cuartil (Q1)**, **mediana**, **media**, **3er cuartil (Q3)** y **máximo** de los datos.
