setwd("~/Git/iecd")

# Definir el directorio donde se encuentran los archivos de datos
dir_data <- "datos/"

# Ejercicio 3
# Cargar los datos de los archivos iridio.txt y rodio.txt en dos data frames separados
iridio <- read.table(paste0(dir_data, "iridio.txt"), header = TRUE)
rodio <- read.table(paste0(dir_data, "rodio.txt"), header = TRUE)

iridio <- iridio$iridio
rodio <- rodio$rodio

# (a) Comparar los dos conjuntos de datos mediante histogramas y boxplots, graficando los boxplots en paralelo.
hist(iridio, main = "Histograma de temperatura de sublimación del iridio")
boxplot(iridio, main = "Boxplot de temperatura de sublimación del iridio")

hist(rodio, main = "Histograma de temperatura de sublimación del rodio")
boxplot(rodio, main = "Boxplot de temperatura de sublimación del rodio")

par(mfrow = c(1, 2))
boxplot(iridio, sub = "Iridio")
boxplot(rodio, sub = "Rodio")

rango <- range(c(rodio, iridio))
par(mfrow = c(1, 2))
boxplot(iridio, sub = "Iridio", ylim = rango)
boxplot(rodio, sub = "Rodio", ylim = rango)
mtext("Boxplots de temperatura de sublimación", outer = TRUE, side =3 , line=-2)

# (b) Hallar las medias, las medianas y las medias podadas al 10% y 20% muestrales. Comparar.
media_iridio <- mean(iridio)
media_rodio <- mean(rodio)

mediana_iridio <- median(iridio)
mediana_rodio <- median(rodio)

?mean
# Ese segundo argumento, `trim`, es lo que buscamos
media_podadas_10_iridio <- mean(iridio, trim = 0.1)
# Los argumentos son posicionales, así que si no nos salteamos ninguno, podemos obviar el nombre del argumento
mean(iridio, trim = 0.1) == mean(iridio, 0.1)
media_podadas_10_rodio <- mean(rodio, 0.1)
# En general, es buena idea no repetirse: si una variable se usará mas de una vez, démosle nombre
alfa <- 0.2

# (c) Hallar los desvíıos estándares, las distancias intercuartiles y las MAD muestrales como medidas de dispersión.

# Esto se está volviendo verborrágico. Tal vez convenga usar un vector nombrado
desvios <- c(iridio = sd(iridio), rodio = sd(rodio))
# Hacer click en el nombre de una función mientras se sostiene Ctrl abre el código fuente de la función - siempre y cuando esté escrita en R, es muy útil
IQRs <- c(iridio = IQR(iridio), rodio = IQR(rodio))
MADs <- c(iridio = mad(iridio), rodio = mad(rodio))

# Hallar los cuantiles 0.90, 0.75, 0.50, 0.25 y 0.10.
qs <- c(0.90, 0.75, 0.50, 0.25, 0.10)
# También podemos construir una lista de vectores (un "data frame" es esencialmente esto)
quantile(iridio, qs)
quantile(rodio, qs)
cuantiles <- list(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, qs)
)
cuantiles$rodio
cuantiles$rodio[3] == cuantiles$rodio["50%"]
# Pero como los dos vectores tienen los mismos índices (los cuantiles), podemos armar un data.frame
df_cuantiles <- data.frame(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, qs)
)
all(df_cuantiles == data.frame(cuantiles))
# No es un problema que los vectores de una lista tengan distinta longitud, pero sí lo es que suceda en un dataframe
list(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, c(qs, 0.99))
)
data.frame(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, c(qs, 0.99))
)
# Ojo que al data.frame no le calienta que los índices de los vectores sean distintos, simplemente tomará los del primer vector.
distintos_cuantiles <- list(
  iridio = quantile(iridio, qs),
  rodio = quantile(rodio, c(0.01, 0.02, 0.03, 0.04, 0.05))
)
data.frame(distintos_cuantiles)

# Un truco más. La asignación en R devuelve el valor asignado, y se puede usar dentro de otra expresión
x <- 2 * (y <- 3)
c(x, y)
# En particular, esto permite ver el resultado de una expresión (en la consola) al mismo tiempo que se asigna
(x <- 1)
