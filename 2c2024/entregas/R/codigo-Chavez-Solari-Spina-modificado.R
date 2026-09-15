# Importamos la librería sloop

library(sloop)

# Ejercicio 1: estudiamos la clase de algunos vectores

class<-c(T,F)
class<-c(T,F,1)
class<-c(T,F,1,"1")

# Ahora estudiamos los métodos genéricos

methods(print)

# Ejercicio 2

class(density)
class(density(1:500))

# Ejercicio 3

metodo_print <- sloop::s3_methods_generic("print")
metodos_density <- sloop::s3_methods_class("density")

# Ejercicio 4

help(unclass)

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

inherits(test_t,"htest")
test_t <- unclass(test_t)
inherits(test_t,"htest")
class(test_t)

# Ejercicio 12

particiones <- function(t, n) {
  if (n == 0) {
    if (t == 0) {
      return(1)
    } 
    else {
      return(0)
    }
  }
  else if (t < 0 || t > (n * (n + 1)) / 2) {
    return(0)
  }
  return(particiones(t, n - 1) + particiones(t - n, n - 1))
}

# Tests

particiones(3,4)
particiones(24,12)
particiones(55,10)
particiones(45,30)

# Ejercicio 13

# dTmas devuelve la función de probabilidad puntual de Tmas

dTmas<-function(x,n){
  ret <- vector(mode = "numeric", length = length(x))
  total <- 2^n
  
  for (i in seq_along(x)) {
    ret[i] <- particiones(x[i], n) / total
  }
  
  return(ret)
}

# pTmas devuelve la función de distribución acumulada de Tmas

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  prob <- dTmas(0:(n * (n + 1) / 2), n)
  
  for (i in seq_along(x)) {
    ret[i] <- sum(prob[0:x[i] + 1])
  }
  
  return(ret)
}

# Tests

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

# Ejercicio 14

# Implementamos el test de Wilcoxon de rango signado

mi.wilcox.test <- function(x, alternative = c("two.sided","greater","less"), mu=0){
  
  alternative <- match.arg(alternative)
  
  # Restamos el valor de mu para ajustar la hipótesis nula
  
  diferencias <- x - mu
  
  # Calculamos el estadístico de Wilcoxon
  
  rangos <- rank(abs(diferencias))
  
  tMas <- sum(rangos[diferencias > 0])
  
  n <- sum(diferencias != 0)
  
  if (alternative == "two.sided") { # Revisar los cálculos del p valor
    
    p_value <- 2 *  min(1-pTmas(tMas-1, n), pTmas(tMas,n))
    
  } else if (alternative == "greater") {
    
    p_value <- 1-pTmas(tMas-1, n)
    
  } else if (alternative == "less") {
    
    p_value <- pTmas(tMas, n)
    
  }
  
  result <- list(
    statistic = tMas,
    p.value = p_value,
    alternative = alternative
  )
  
  class(result) <- "htest"
  
  return(result)
  
}

# Test del enunciado

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1
R_wilcox <- wilcox.test(X, alternative="greater", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="greater", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

# Tests armados por nosotros, chequeamos que los p-valores están bien calculados para las otras alternativas

set.seed(234)
n <- 20
X <- rnorm(n)
theta0 <- 1
R_wilcox <- wilcox.test(X, alternative="two.sided", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="two.sided", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

R_wilcox <- wilcox.test(X, alternative="less", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="less", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)


# Ejercicio 17

png(file="ej17.png")

n_values <- c(4, 10, 20)

par(mfrow = c(1, length(n_values)))

for (n in n_values) {
  # Calcular los valores posibles de T+
  x <- 0:(n * (n + 1) / 2)
  prob <- dTmas(x, n) # Probabilidades puntuales
  
  # Reacomodando la distribución asintótica para cada n en n_values, podemos l
  #legar a los siguientes parámetros para la Normal
  mu <- (n*(n+1))/4 # Esperanza de T+
  sigma2 <- n*(n+1)*(2*n+1) / 24 
  sigma <- sqrt(sigma2)
  
  # Generamos los valores para la Normal
  x_normal <- seq(0, n*(n+1)/2, length.out = 100)
  y_normal <- dnorm(x_normal, mean = mu, sd = sigma)
  
  # Escalamos la curva Normal para que sea comparable con las probabilidades
  y_normal <- y_normal * max(prob) / max(y_normal)
  
  plot(
    x, prob, type = "h", lwd = 2, col = "skyblue",
    main = paste("Dis. exacta de T+ para n =", n),
    xlab = "T+", ylab = "Probabilidad",
    ylim = c(0, max(prob, y_normal)) # Asegura que ambas tengan el mismo rango en el eje y
  )
  lines(x_normal, y_normal, col = "red", lwd = 2) # Superponemos la curva Normal
}

dev.off()

# Hacemos lo mismo para otros valores de n, con el fin de determinar el n tal que la densidad
# de la distribución asintótica es suficientemente buena para aproximar la probabilidad puntual de T+

n_values_2 <- c(6,7,8,9,10,11,12)

par(mfrow = c(1, length(n_values)))

for (n in n_values_2) {
  # Calcular los valores posibles de T+
  x <- 0:(n * (n + 1) / 2)
  prob <- dTmas(x, n) # Probabilidades puntuales
  
  # Reacomodando la distribución asintótica para cada n en n_values, podemos l
  #legar a los siguientes parámetros para la Normal
  mu <- (n*(n+1))/4 # Esperanza de T+
  sigma2 <- n*(n+1)*(2*n+1) / 24 
  sigma <- sqrt(sigma2)
  
  # Generamos los valores para la Normal
  x_normal <- seq(0, n*(n+1)/2, length.out = 100)
  y_normal <- dnorm(x_normal, mean = mu, sd = sigma)
  
  # Escalamos la curva Normal para que sea comparable con las probabilidades
  y_normal <- y_normal * max(prob) / max(y_normal)
  
  plot(
    x, prob, type = "h", lwd = 2, col = "skyblue",
    main = paste("Dis. exacta de T+ para n =", n),
    xlab = "T+", ylab = "Probabilidad",
    ylim = c(0, max(prob, y_normal)) # Asegura que ambas tengan el mismo rango en el eje y
  )
  lines(x_normal, y_normal, col = "red", lwd = 2) # Superponemos la curva Normal
}

# Ejercicio 18
set.seed(1984)

n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))

# Computamos el test de Wilcoxon para esta muestra usando nuestra implementación del test
test_wilcox <- mi.wilcox.test(X, alternative = "greater", mu = 0)

# Calculamos la potencia del test usando Bootstrap
m <- 10000
tMas <- vector(length = m)

# Eliminamos la semilla porque sino todas las muestras dan iguales 
set.seed(NULL)

for(i in 1:m){
  # Seteamos una semilla en cada repetición
  set.seed(i)
  
  # Generamos la iesima muestra
  Y <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  
  # Calculamos tMas para cada muestra
  diferencias <- Y - theta1
  rangos <- rank(abs(diferencias))
  tMas[i] <- sum(rangos[diferencias > 0])
  
}

# Calculamos la potencia en theta1 usando el resultado del enunciado
qnorm(1-0.05,39,sqrt(162.5))
potencia_wilcox <- (1/m) * sum(ifelse(tMas > 59.97, 1, 0))

# Ejercicio 19
set.seed(1984)

n <- 12
theta0 <- 0
theta1 <- 1
sigma_sq <- 1
m <- 10000
alpha <- 0.05
z_alpha <- qnorm(1 - alpha)
cat("El valor crítico z_alpha es:", z_alpha, "\n")

umbral <- z_alpha / sqrt(n)

rechazos <- replicate(m, {
  x <- rnorm(n, mean=1, sd=1)
  mean_x <- mean(x)
  as.integer(mean_x >= umbral)
})

potencia_estimada <- mean(rechazos)
cat("La potencia estimada es:", potencia_estimada, "\n")

n <- 12
alpha <- 0.05
m <- 10000
x <- 0:(n * (n + 1) / 2)
acc <- pTmas(x, n)
k_alpha <- length(acc[acc <= 1-alpha])+1
k_alpha

cat("El valor crítico k_alpha es:", k_alpha, "\n")

alpha_real <- 1-acc[k_alpha]

cat("El valor real de alpha es:", alpha_real, "\n")

Tplus_calc <- function(d) {
  rnk <- rank(abs(d))
  sum(rnk[d > 0])
}

# Potencia bajo H1: theta_1 = 1 y según los datos de D
set.seed(1984)
rechazos <- replicate(m, {
  d <- rnorm(n, mean = 1, sd = 1)
  Tplus <- Tplus_calc(d)
  as.integer(Tplus >= k_alpha)
})

potencia_estimada <- mean(rechazos)
cat("La potencia estimada del test es:", potencia_estimada, "\n")