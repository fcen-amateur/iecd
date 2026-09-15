######################################
#             PREGUNTA 1             #
######################################
objs <- list(c(T,F), c(T,F,1), c(T,F,1,"1"))
for (obj in objs) { print(class(obj)) }
# Se observa que lo que está sucediendo es que, en los vectores, R tiene una jerarquía para las clases, 
# donde si todos los elementos son de la misma clase, el vector tiene esa clase, y sino convierte a todos 
# los elementos a un tipo común, que sea compatible con todos.

######################################
#             PREGUNTA 2             #
######################################
install.packages("sloop")
library(sloop)

dens = density
dens2 = density(1:500)

cat("Class density:", class(dens), "\n")
cat("Class density(1:500): ", class(dens2))
# La diferencia radica en que cuando se busca la clase de "density", es una función que todavía no fue evaluada, 
# entonces es un objeto que toma un argumento. En cambio "density(1:500)" ya fue evaluada, por lo que es un objeto 
# "resultado", una densidad de una muestra en este caso.

######################################
#             PREGUNTA 3             #
######################################
cant = length(methods("print"))
cat("El método genérico print sabe despachar para", cant, "clases.", "\n")
methods(class="density")
# El método "density" cuenta con 6 métodos: "coerce", "initialize", "plot", "print", "show", "slotsFromS3".

######################################
#             PREGUNTA 4             #
######################################
# Tal como se explica en help(unclass), "`unclass` returns (a copy of) its argument with its class attribute removed."
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
mu_0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu_0,
  conf.level = 1 - alfa
)

unclass(test_t)
class(unclass(test_t))
# test_t es un objeto de tipo "htest" que contiene 10 elementos. Al aplicar unclass, se obtiene una copia 
# con la clase eliminada, lo que lo convierte en una lista básica.

######################################
#            PREGUNTA 12             #
######################################
particiones <- function(t, n) {
  p = (n*(n+1))/2
  if (n == 0){
    if (t == 0){
      return(1)
    } else {
      return(0)
    }
  } else if (t < 0 || t > p) {
    return(0)
  } else {
    return(particiones(t, n - 1) + particiones(t - n, n - 1))
  }
}

stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

######################################
#            PREGUNTA 13             #
######################################
dTmas <- function(x, n){
  ret <- vector(mode = "numeric", length = length(x))
  den = 2^n
  for (i in seq_along(x)) {
    num = particiones(x[i], n)
    res = num / den
    ret[i] = res
  }
  return(ret)
}

pTmas <- function(x, n){
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    res = 0
    t = 0
    while (t <= x[i]) {
      res = res + dTmas(t, n)
      t = t + 1
    }
    ret[i] = res
  }
  return(ret)
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

######################################
#            PREGUNTA 14             #
######################################
calculo_t <- function(X) {
  en_modulo_ordenado = sort(abs(X))
  t_obs = 0
  for (i in seq_along(X)) {
    if (X[i] > 0) {
      rango = which(en_modulo_ordenado == X[i])
      t_obs = t_obs + rango
    }
  }
  return(t_obs)
}

mi.wilcox.test <- function(X, alternative, mu) {
  n <- length(X)
  X_prima = X - mu
  print(X_prima)
  t_obs = calculo_t(X_prima)
  print(t_obs)
  if (alternative == "two.sided") {
    if (t_obs < (n * (n + 1)) / 4) {
      p_valor = 2 * pTmas(t_obs, n)
    } else {
      p_valor = 2 * (1 - pTmas(t_obs, n))
    }
  } else if (alternative == "greater") {
    p_valor = 1 - pTmas(t_obs - 1, n)
  } else {
    p_valor = pTmas(t_obs, n)
  }
  rv <- list(
    statistic = t_obs,
    alternative = alternative,
    p.value = p_valor
  )
  structure(rv, class = "htest")
}

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1

R_wilcox = wilcox.test(X, alternative="greater", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="greater", mu = theta0)
print(mi_wilcox$p.value)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)
######################################
#            PREGUNTA 17             #
######################################
install.packages("ggplot2")
library(ggplot2)


N <- c(4, 10, 20)

for (n in N) {  
  x <- 0:(n * (n + 1) / 2)
  res_1 <- dTmas(x, n)
  punto_x <- n * (n + 1) / 4
  # Crear un dataframe para ggplot
  df <- data.frame(
    x = x,
    probabilidad = res_1,
    asintotica = dnorm(x, mean = n * (n + 1) / 4, sd = sqrt(n*(n + 1) * (2 * n + 1) / (24)))
  )
  
  # Crear el gráfico
  p <- ggplot(df, aes(x = x)) +
    geom_bar(aes(y = probabilidad), stat = "identity", fill = "skyblue", alpha = 0.7) +
    geom_line(aes(y = asintotica), color = "red", linewidth = 1) +
    geom_point(aes(x = punto_x, y = 0), color = "blue", size = 3) + 
    labs(
      title = paste("Gráfico de barras y distribución asintótica para n =", n),
      x = "Valores de x",
      y = "Probabilidad"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Ajusta la orientación de las etiquetas
    )
  
  # Imprimir el gráfico
  print(p)
}


######################################
#            PREGUNTA 18             #
######################################
#Dos maneras. Con p-valor:
set.seed(1984)
t_mas = c()
m = 10000
suma = 0
n <- 12
theta1 <- 1
sigma_sq <- 1
for (i in 1:m) {
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  test = mi.wilcox.test(X, alternative="greater", mu = 0)
  #print(test$p.value)
  t_mas = c(t_mas, test$statistic)
  suma = suma + as.integer(test$p.value < 0.05)
}
potencia_estimada_wilc_1 = suma / m

#Redefino el test y busco k*
RR <- function(d,alpha,n,izquierda=TRUE) {
  t = 0
  p = 0
  if(izquierda) {
    while(p < alpha) {
      p = p + d(t,n)
      t = t+1
    }
    t = t - 1 # para que sea menor al valor de significacion
  } else {
    while(1-p >= alpha) {
      p = p + d(t,n)
      t = t + 1
    }
  }
  return(t)
}

mi.wilcox.test_2 <- function(X, alternative, mu,alpha) {
  n <- length(X)
  X_prima = X - mu
  t_obs = calculo_t(X_prima)
  alt = alternative
  rv <- list(
    statistic = t_obs,
    alternative = alt,
    decision = 0
  )
  if (alt == "two.sided"){
    #aca la region de techazo es una union de intervalos, los valores mas grandes que un k1 y los mas chicos que k2
    rv$r1_alpha <- RR(dTmas,n,alpha/2,FALSE)
    rv$r2_alpha <- RR(dTmas,n,alpha/2,TRUE)
    
    if(t_obs > rv$r1_alpha || t_obs < rv$r2_alpha) {
      rv$decision = 1
    }
    
    if (t_obs < (n*(n+1))/4){
      p_valor = 2*pTmas(t_obs, n)
    } else {
      p_valor = 2*(1 - pTmas(t_obs, n))
    }
  }else if (alt == "greater"){
    rv$k_alpha <- RR(dTmas,alpha,n,FALSE)
    if(t_obs > rv$k_alpha) {
      rv$decision = 1
    }
    p_valor = 1 - pTmas(t_obs - 1, n)
  }else {
    rv$k_alpha <- RR(dTmas,alpha,n,TRUE)
    
    if(t_obs < rv$k_alpha) {
      rv$decision = 1
    }
    p_valor = pTmas(t_obs, n)
  }
  rv$p.value <- p_valor
  structure(rv, class = "htest")
}

set.seed(1984)
t_mas = c()
m = 10000
suma = 0
n <- 12
theta1 <- 1
sigma_sq <- 1

for (i in 1:m){
  #print(suma)
  X = rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  test = mi.wilcox.test_2(X, alternative="greater", mu = 0,alpha = 0.05)
  t_mas = c(t_mas, test$statistic)
  suma = suma + test$decision
}

potencia_estimada_wilc_2 = suma / m
######################################
#            PREGUNTA 19             #
######################################
RR <- function(alpha,n,p0,izquierda=TRUE) {
  t = 0
  p = 0
  if(izquierda) {
    while(p < alpha) {
      p = p + dbinom(t,n,p0)
      t = t+1
    }
    t = t - 2 # para que sea menor al valor de significacion
  } else {
    while(1-p >= alpha) {
      p = p + dbinom(t,n,p0)
      t = t + 1
    }
  }
  return(t)
}
testsigno <- function(X,alpha,n) {
  t = sum(as.integer(X>0))
  k_alpha2 = RR(alpha,n,0.5,FALSE)
  if(t >= k_alpha2) {
    return(1)
  }
  return(0)
}

testnormal <- function(X,alpha,sigma,n) {
  t = sqrt(n)*mean(X)/sigma
  if(t> qnorm(1-alpha,0,sigma)) {
    return(1)
  }
  return(0)
}

set.seed(1984)
t_mas = c()
m = 10000
suma = 0
n <- 12
theta1 <- 1
sigma_sq <- 1
testnormales = c()
for (i in 1:m){
  #print(suma)
  X = rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  testnormales = c(testnormales,testnormal(X,0.05,sqrt(sigma_sq),n))
  suma = suma + testsigno(X,0.05,n)
}
potencia_estimada_normal = sum(testnormales)/m
potencia_estimada_signo = suma / m
print(potencia_estimada_normal)
print(potencia_estimada_signo)




