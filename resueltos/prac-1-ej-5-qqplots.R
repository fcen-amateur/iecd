setwd("~/Git/iecd")

# Definir el directorio donde se encuentran los archivos de datos
dir_data <- "datos/"

estudiantes <- read.table(paste0(dir_data, "estudiantes.txt"), header = TRUE)

# (a) Estudiar si la distribucio ́n de los conjuntos de datos para ambos grupos es normal, realizando los correspondientes histogramas y superponiendo la curva normal. Adema ́s dibujar los qqplots para cada conjunto de datos superponiendo, en otro color, la recta mediante el comando qqline.
# TIP: En el menú de RStudio, "Code" -> "Soft wrap long lines" para que el texto no se pase de largo de los márgenes como en la línea anterior y esta

hist(estudiantes$GRUPO1)
attach(estudiantes)
hist(GRUPO1)

# Ctrl-click en estos métodos complejos no es tan útil
?hist
par(mfrow=c(1,2))
hist(GRUPO1, breaks = 10)
hist(GRUPO2, breaks = 20)

# Hmmm, e que hay menos de 10 valores únicos?
?table
(t1 <- table(GRUPO1))
length(t1)
t2 <- table(GRUPO2)
length(t2)

# Agrego curva normal al histograma
hist(GRUPO1)
xs <- seq(-4, 4, length.out=100)
plot(x=xs, y=dnorm(xs), type="l")
curve(dnorm, from=-4, to=4)
curve(dnorm(x, mean = mean(GRUPO1), sd = sd(GRUPO1)), add = TRUE, col = "blue")
?hist
hist(GRUPO1, freq=FALSE)
curve(dnorm(x, mean = mean(GRUPO1), sd = sd(GRUPO1)), add = TRUE, col = "blue")

# Tip: reiniciar R seguido, en particular si los gráficos empiezan a volverse loquitos
# RStudio Menu -> "Session" -> "Restart R"
for (grupo in c(GRUPO1, GRUPO2)) {
  hist(grupo, freq=FALSE)
  curve(dnorm(x, mean = mean(grupo), sd = sd(grupo)), add = TRUE, col = "blue")
}
# Oh no. Concatenar os vectores devuelve un vector de nuevo, y entonces el for-loop itera sobre cada elemento escalar del vector. Lo que queremos es una lista de vectores
grupos <- list(GRUPO1, GRUPO2)
for (grupo in grupos) {
  hist(grupo, freq=FALSE)
  curve(dnorm(x, mean = mean(grupo), sd = sd(grupo)), add = TRUE, col = "blue")
}

# Sería lindo darle un título representativo, no? Implícitamente los elementos de la lsita tienen índices numéricos
par(mfrow=c(1,2))
for (num in seq_along(grupos)) {
  grupo <- grupos[[num]]
  hist(grupo, freq=FALSE, main=paste("Grupo", num))
  curve(dnorm(x, mean = mean(grupo), sd = sd(grupo)), add = TRUE, col = "blue")
}

# Creo un QQ plot - para la distribucion normal - for each group
names(grupos)
grupos <- list("G1"=GRUPO1, "G2"=GRUPO2)
# Las claves pueden ir sin comillas dobles, aunque no recomiendo, there be dragons.
grupos_bis <- list(G1=GRUPO1, "G2"=GRUPO2)
grupos == grupos_bis
?identical
identical(grupos, grupos_bis)
# O en forma de aseveración
stopifnot(identical(grupos, grupos_bis))

for (key in names(grupos)) {
  value <- grupos[[key]]
  normalizado <- (value - mean(value)) / sd(value)
  qqnorm(value, main = key)
  qqline(value, col = "red")
}
qnorm(seq.int(5)/5)
qnorm((seq.int(5) - 0.5)/5)
# Con ggplot2
# Load the ggplot2 package
library(ggplot2)

ggplot(data.frame(sample = GRUPO1), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color="red")

GRUPO1 <- GRUPO1 + runif(n=length(GRUPO1), min=-0.005, max=0.005)
# Y a mano?
GRUPO1_ordenado <- sort(GRUPO1)

n <- length(GRUPO1)
cuantiles <- qnorm((seq_len(n) - 0.5)/ n)
dnorm(cuantiles)
par(mfrow=c(1,2))
plot(cuantiles, GRUPO1_ordenado, main = "A mano")
qqnorm(GRUPO1, main = "Con qqnorm")
qqline(GRUPO1, col = "red")

GRUPO1 <- runif(n=2000)
GRUPO1_ordenado <- sort(GRUPO1)

n <- length(GRUPO1)
cuantiles <- qnorm((seq_len(n) - 0.5)/ n)
dnorm(cuantiles)
par(mfrow=c(1,2))
plot(cuantiles, GRUPO1_ordenado, main = "A mano")
qqplot(GRUPO1, GRUPO2, main = "Con qqnorm")
qqline(GRUPO1, GRUPO2, col = "red")

# Tarea para el hogar: cómo agregarían la qqline a mano?