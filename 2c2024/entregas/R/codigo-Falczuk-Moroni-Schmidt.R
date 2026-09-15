#Ejercicio 1
## ------------------------------------------------------------------------------------------------------------------------------
a = c(T, F)
b = c(T, F, 1)
c = c(T, F, 1, '1')

vec <- list(a, b, c)

for (v in vec) {
  cat("El vector: ", v, " pertenece a la clase: ", class(v), "\n")
}

#Lo que sucede es para guardar en una estructura que tiene que tener mismo valor, intenta castear los elementos del menos generico, en este caso los booleanos, al más generico que es el character. Entonces en estos casos:
# 1. c(T,F) como son del mismo tipo, guarda a los valores en una lista del tipo logical.
# 2. c(T,F,1) en este caso, hay dos clases distintas, numeric y logical, y como logical puede ser casteada a numeric, entonces T es 1 y F es 0 es términos de numeric.
# 3. c(T,F, 1, "1")  en este caso character es la clase mas genérica para poder castear, entonces T -> 'True', F -> 'False' y el 1 es "1".

#Ejercicio 2
## ------------------------------------------------------------------------------------------------------------------------------
cat('Density tiene clase: ', class(density), "\n")
cat('Density(1:500) tiene clase: ', class(density(1:500)))

# Vemos que las clases son distintas, la diferencia radica en que density es una función,
# pero si nosotros llamamos a la función con el vector 1:500, lo que estamos haciendo es viendo
# que clase tiene el resultado de llamar a la función density. En este caso el resultado son varios conjuntos de estructuras:

print(str(density(1:500)))

#Ejercicio 3
## ------------------------------------------------------------------------------------------------------------------------------
library(sloop)
print(summary(s3_methods_generic("print")))
cat('los métodos de density son: \n')
cat('la cantidad de métodos sin contar print son: ', length(methods(class = "density")) -
      1)

# Vemos que el método print tiene para despachar 266 clases distintas, pero esto también depende realmente de cuantos
# paquetes tenga instalados en mi version de R, podria suceder que algun compañero de TP corra esta misma celda y
# le de un valor distinto.
# Vemos que en este caso tenemos 6 métodos que tiene la clase density.

#Ejercicio 4
## ------------------------------------------------------------------------------------------------------------------------------
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))

mu0 <- 0
alfa <- 0.05
test_t <- t.test(X,
                 alternative = "two.sided",
                 mu = mu0,
                 conf.level = 1 - alfa)
print(unclass(test_t))
print(class(unclass(test_t)))
print(class(test_t))

# Al correr la funcion unclass sobre test_t lo que estamos haciendo es justamente remover la clase de esta estructura,
# por lo cual veremos reflajada la estructura de datos limpia.

#Ejercicio 10
## ------------------------------------------------------------------------------------------------------------------------------
todos_los_subconjuntos_de_5_elementos <- function() {
  elements <- 1:5
  subcojuntos <- list()
  for (k in 0:5) {
    subconjuntos_de_k <- combn(elements, k, simplify = FALSE)
    subcojuntos <- c(subcojuntos, subconjuntos_de_k)
  }
  return(subcojuntos)
}

df <-
  data.frame(
    T = numeric(),
    S = numeric(),
    Cantidad = numeric(),
    Probabilidad = numeric()
  )

crear_tabla_observacion_6 <- function() {
  subconjuntos <- todos_los_subconjuntos_de_5_elementos()
  for (t in 0:15) {
    subconjuntos_que_suman_t <- c()
    cantidad <- 0
    for (subconjunto in subconjuntos) {
      if (sum(subconjunto) == t) {
        subset_str <-
          if (length(sub) > 0)
            paste(subconjunto, collapse = ", ")
        else
          ""
        string_del_subconjunto <- c("{", subset_str, "}")
        subconjuntos_que_suman_t <-
          c(subconjuntos_que_suman_t, string_del_subconjunto)
        cantidad <- cantidad + 1
      }
    }
    df <- rbind(df,
                data.frame(
                  T = t,
                  S = paste(subconjuntos_que_suman_t, collapse = " "),
                  Cantidad = cantidad,
                  Probabilidad = cantidad / 32
                ))
  }
  return(df)
}
print(crear_tabla_observacion_6())

#Ejercicio 12
## ------------------------------------------------------------------------------------------------------------------------------
particiones <- function(t, n) {
  if (n == 0) {
    if (t == 0)
      return(1)
    return(0)
  }
  
  if (t < 0 || t > n * (n + 1) / 2)
    return(0)
  return(particiones(t, n - 1) + particiones(t - n, n - 1))
}

probabilidad <- function(t, n) {
  return(particiones(t, n) / (2 ** n))
}

stopifnot(
  particiones(t = 3, n = 4) == 2,
  particiones(t = 24, n = 12) == 67,
  particiones(t = 55, n = 10) == 1,
  particiones(t = 45, n = 30) == 1938
)

# Ejercicio 13
## ------------------------------------------------------------------------------------------------------------------------------
# proba puntual

dTmas <- function(ts, n) {
  res <- vector(mode = "numeric", length = length(ts))
  for (j in seq_along(ts)) {
    res[j] <- probabilidad(ts[j], n)
  }
  return(res)
}

# acumulada

pTmas <- function(ts, n) {
  res <- vector(mode = "numeric", length = length(ts))
  for (j in seq_along(ts)) {
    res[j] <- sum(dTmas((0:ts[j]), n))
  }
  return(res)
}

n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)

#Ejercicio 14
## ------------------------------------------------------------------------------------------------------------------------------
library(stats)
set.seed(125)
n <- 15
X <- rnorm(n)
theta0 <- 2


estadistico <- function(x) {
  signos <- ifelse(x >= 0, 1,-1)
  modx <- abs(x)
  rango <- rank(modx)
  producto <- signos * rango
  return(sum(producto))
}

mi.wilcox.test <- function(x, alternative, mu) {
  n <- length(x)
  alternative <-
    match.arg(alternative, c("two.sided", "less", "greater"))
  x <- x - mu
  tobs <- estadistico(x)
  tMasObs <- (tobs / 2) + n * (n + 1) / 4
  if (alternative == 'greater') {
    p.value <- 1 - pTmas(tMasObs - 1, length(x))
  } else if (alternative == 'two.sided') {
    p.value <-
      (1 - pTmas(abs(tobs) / 2 + (n * (n + 1) / 4) - 1, length(x))) *
      2
  } else {
    p.value <- pTmas(tMasObs, length(x))
  }
  
  resultado <- list(statistic = tMasObs,
                    p.value = p.value,
                    alternative = alternative)
  
  class(resultado) <- "htest"
  return(resultado)
}

R_wilcox <- wilcox.test(X, alternative = "two.sided", mu = theta0)
mi_wilcox <-
  mi.wilcox.test(X, alternative = "two.sided", mu = theta0)
print(mi_wilcox$p.value)
print(R_wilcox$p.value)

stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest",
  abs (mi_wilcox$p.value - R_wilcox$p.value) < 1E-6
)


# Test adicional
## ------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)
n <- 15
X <- rnorm(n)
theta0 <- 1
R_wilcox <- wilcox.test(X, alternative = "two.sided", mu = theta0)
mi_wilcox <-
  mi.wilcox.test(X, alternative = "two.sided", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  abs(mi_wilcox$p.value - R_wilcox$p.value) < 1E-10,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

# Ejercicio 17
## ------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

grafico_de_distribuciones <- function(n) {
  valores_Tmas <- 0:(n * (n + 1) / 2)
  probabilidades <- dTmas(valores_Tmas, n)
  
  media_asintotica <- n * (n + 1) / 4
  desv_asintotica <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  x_asintotico <-
    seq(min(valores_Tmas), max(valores_Tmas), length.out = 100)
  y_asintotico <-
    dnorm(x_asintotico, mean = media_asintotica, sd = desv_asintotica)
  
  data <- data.frame(valores_Tmas = valores_Tmas,
                     probabilidades = probabilidades)
  
  data_asintotico <- data.frame(x_asintotico = x_asintotico,
                                y_asintotico = y_asintotico)
  separacion <- 2
  if (n == 20) {
    separacion <- 8
  }
  ggplot() +
    geom_bar(
      data = data,
      aes(x = valores_Tmas, y = probabilidades),
      stat = "identity",
      fill = "skyblue",
      alpha = 0.8
    ) +
    geom_line(
      data = data_asintotico,
      aes(x = x_asintotico, y = y_asintotico),
      color = "red",
      size = 1
    ) +
    labs(
      title = sprintf("Grafico de distribuciones para n = %d", n),
      x = "Valores de T+",
      y = "Probabilidades",
      fill = "Legend",
      color = "Legend"
    ) +
    scale_x_continuous(
      breaks = seq(min(valores_Tmas), max(valores_Tmas), by = separacion),
      labels = as.character(seq(
        min(valores_Tmas), max(valores_Tmas), by = separacion
      )),
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.box.spacing = unit(1, "cm"),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
grafico_de_distribuciones(4)
grafico_de_distribuciones(10)
grafico_de_distribuciones(20)


# Ejercicio 18
# Primero vamos a calcular el valor de k* para maximizar la potencia del test.
## ------------------------------------------------------------------------------------------------------------------------------
# Buscar k* desde el valor máximo posible hasta 1

calculo_cuantil  <- function(n, alfa) {
  T_max <- n * (n + 1) / 2
  suma_prob <- 0
  k <- 1
  iteraciones <- 0
  for (t in T_max:0) {
    prob_t <- probabilidad(t, n)
    suma_prob <- suma_prob + prob_t
    if (suma_prob > alfa) {
      k <- t + 1
      break
    }
    
  }
  return(k)
}

print(calculo_cuantil(12, 0.05))
print(1 - pTmas(60, 12)) #Esto es P(T+ >= 61), asi que esta bien el k
print(1 - pTmas(59, 12)) #Esto es P(T+ >= 60), Vemos que nos pasamos del valor alpha = 0.05

# Una vez calculado el k para n = 12 , ahora vamos a realizar los pasos que aparecen en el TP.
# Comenzamos generando las m muestra de Yi calcualmos el valor de T+(Yi).

calculo_estadisticoT_mas <- function(x) {
  solo_positivos_filter <- ifelse(x >= 0, 1, 0)
  modx <-
    abs(x) #No es necesario pq a las negativas no las vamos a considerar
  rango <- rank(modx)
  producto <- solo_positivos_filter * rango
  return(sum(producto))
}
m = 10000
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
vector_Ys = c()
vector_Ys <- replicate(m, {
  X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  calculo_estadisticoT_mas(X)
}) #Esta forma es como un "for", pero en cada indice se guarda un valor del estadistico
k <- calculo_cuantil(12, 0.05)
vector_resultado <-
  ifelse(vector_Ys >= k, 1, 0) #IMPORTANTE el igual ya que es discreta
potencia_estimada = (1 / m) * sum(vector_resultado)
cat("La potencia estimada para el test de wilcoxon es",
    potencia_estimada)

#La estimación de la potencia del test es de 0.9271. Con lo cual la
# P(Error II) es 1-0.9271 = 0.0729 que es muy baja para este valor de tita fijo. Por lo tanto a
# partir de bootstrap podemos decir que el test es bastante bueno.

# Ejercicio 19
# Primero debemos computar el test t de Student para esta muestra.
# Lo primero que hacemos es calcular el valor de k, donde el test debe ser de nivel 0.05
## ------------------------------------------------------------------------------------------------------------------------------
theta <- 1
sigma <- 1
n <- 12
alfa <- 0.05
nu <- n - 1
centralidad <- theta / (sigma / sqrt(n))
k <- qt(1 - alfa, df = nu)

potencia <- 1 - (pt(k, df = nu, ncp = centralidad))
cat("Potencia para el t-test unilateral para theta = 1: ", potencia)

#Calculamos la potencia para el test de la normal para mu con sigma conocido.
k_alpha <- qnorm(0.95, 0, 1)
n <- 12
potencia_del_test_normal <- 1 - pnorm(k_alpha - sqrt(n), 0 , 1)
cat("El valor de la potencia para el test normal es:",
    potencia_del_test_normal,
    "\n")


# Luego pasamos a computar el test de signos
# El test de signos va a tener una distribución Bi(n,1/2) bajo H0, ya que si tita = 0,
# la distribucuión va a ser simétrica y centrada en 0, por lo que la probabilidad de ser mayor o menor que cero es la misma.
# Por lo tanto solo nos falta encontrar el k, tal que la potencia del test bajo H0 es menor a 0.05.

calculo_cuantil_binomial  <- function(n, alfa) {
  T_max <- n   # Valor máximo posible de T^+
  suma_prob <- 0  # Suma acumulada de probabilidades
  k <- 1
  iteraciones <- 0
  for (t in T_max:0) {
    prob_t <- dbinom(t, n, 0.5)  # Probabilidad puntual para T^+ = t
    suma_prob <- suma_prob + prob_t  # Acumular la probabilidad
    if (suma_prob > alfa) {
      k <- t + 1  # Guardar el valor k+1
      break
    }
    
  }
  return(k)
}


k = calculo_cuantil_binomial(12, 0.05)
print(k)

#Cálculo potencia via bootstrap.
calculo_estadistico_signo <- function(x) {
  signos <- ifelse(x >= 0, 1, 0)
  return(sum(signos))
}
m = 10000
set.seed(1984)
n <- 12
theta1 <- 1

sigma_sq <- 1
vector_Ys = c()
vector_Ys <- replicate(m, {
  X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  calculo_estadistico_signo(X)
}) #Esta forma es como un "for", pero en cada índice se guarda un valor del estadistico
k <- calculo_cuantil_binomial(12, 0.05)
vector_resultado_bin <-
  ifelse(vector_Ys >= k, 1, 0) # Importante el "="
potencia_estimada_bin = (1 / m) * sum(vector_resultado_bin)
cat("La potencia estimada para el test de signo es:",
    potencia_estimada_bin)
