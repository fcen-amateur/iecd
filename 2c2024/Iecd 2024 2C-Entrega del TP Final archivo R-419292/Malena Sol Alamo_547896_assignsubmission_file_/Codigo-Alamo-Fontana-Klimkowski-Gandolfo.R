# TP Estadística
# Fecha: 2024-11-29


# ----------------------------
# Ejercicio 1
# ----------------------------
print(c(T, F))
print(c(T, F, 1))
print(c(T, F, 1, "1"))
class(c(T, F))
class(c(T, F, 1))
class(c(T, F, 1, "1"))

# El elemento c(T,F) tiene clase LOGICAL, el elemento c(T,F,1) tiene clase numérica 
# y el elemento c(T,F,1,"1") tiene clase CHARACTER. Esto se debe a que la función c() 
# requiere tipos de datos homogéneos. Convierte en el tipo más inclusivo según:
# 1. Character
# 2. Numeric
# 3. Logical

# ----------------------------
# Ejercicio 2
# ----------------------------
print(class(density))
print(class(density(1:500)))

# density tiene la clase function, y density(1:500) tiene la clase density.
# La diferencia es que density es la función en general, y density(1:500) es 
# el resultado de evaluar la función en un rango.

# ----------------------------
# Ejercicio 3
# ----------------------------
print_methods <- methods("print")
length(print_methods)

# El genérico print puede despachar 258 clases.

density_methods <- methods(class = "density")
print(density_methods)
length(density_methods)

# density cuenta con 5 métodos adicionales además de plot.

# ----------------------------
# Ejercicio 4
# ----------------------------
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))

# Ejecuto el test
mu_0 <- 0
alfa <- 0.05

test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu_0,
  conf.level = 1 - alfa
)

class(unclass(test_t))

# La función class devuelve la clase del objeto test_t, que es htest. 
# unclass elimina el atributo de clase, dejando solo el contenido interno 
# (se comporta como una lista básica).

# ----------------------------
# Ejercicio 12
# ----------------------------
particiones <- function(t, n) {
  if ((t < 0) || (t > n * (n + 1) / 2)) {
    res <- 0
  } else if (t == 0) {
    res <- 1
  } else {
    res <- particiones(t, n - 1) + particiones(t - n, n - 1)
  }
  return(res)
}

stopifnot(
  particiones(t = 3, n = 4) == 2,
  particiones(t = 24, n = 12) == 67,
  particiones(t = 55, n = 10) == 1,
  particiones(t = 45, n = 30) == 1938
)

# ----------------------------
# Ejercicio 13
# ----------------------------
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- particiones(x[i], n) / (2^n)
  }
  return(ret)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- sum(dTmas(0:x[i], n))
  }
  return(ret)
}

stopifnot(
  dTmas(24, 12) == 67 / 2^12,
  all(dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16),
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2^(-55),
  dTmas(34, 15) == dTmas(15 * (15 + 1) / 2 - 34, 15),
  pTmas(34, 15) == 1 - pTmas(15 * (15 + 1) / 2 - (34 + 1), 15)
)

# ----------------------------
# Ejercicio 14
# ----------------------------
mi.wilcox.test <- function(x, alternative = "two.sided", mu = 0) {
  x <- x - mu
  rangos <- rank(abs(x))
  T_mas <- sum(rangos[x > 0])
  n <- length(x)
  max_rango <- n * (n + 1) / 2
  
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  
  if (alternative == "greater") {
    p.value <- sum(dTmas((T_mas:max_rango), n))
  } else if (alternative == "less") {
    p.value <- sum(dTmas((0:T_mas), n))
  } else if (alternative == "two.sided") {
    T_menos <- max_rango - T_mas
    p_derecha <- sum(dTmas((T_mas:max_rango), n))
    p_izquierda <- sum(dTmas((0:T_mas), n))
    p.value <- 2 * min(p_derecha, p_izquierda)
  }
  
  result <- list(
    statistic = c(V = T_mas),
    p.value = p.value,
    alternative = alternative,
    method = "Test Wilcoxon",
    data.name = deparse(substitute(x))
  )
  
  class(result) <- "htest"
  return(result)
}

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1
R_wilcox <- wilcox.test(X, alternative = "two.sided", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative = "two.sided", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

# ----------------------------
# Ejercicio 17
# ----------------------------
dist_asintotica <- function(x, n) {
  mu <- n * (n + 1) / 4
  sigma2 <- n * (n + 1) * (2 * n + 1) / 24
  dnorm(x + 0.5, mean = mu, sd = sqrt(sigma2))  # Ajuste de continuidad
}

plot_distributions <- function(n) {
  max_T <- n * (n + 1) / 2
  T_vals <- 0:max_T
  barplot(
    dTmas(T_vals, n),
    names.arg = T_vals,
    main = paste("Distribución Exacta vs Asintótica (n =", n, ")"),
    xlab = "T^+",
    ylab = "Probabilidad",
    col = "lightblue"
  )
  lines(T_vals, dist_asintotica(T_vals, n), col = "red", lwd = 2, type = "b")
  legend("topright", legend = c("Exacta", "Asintótica"), fill = c("lightblue", "red"))
}

plot_distributions(4)
plot_distributions(10)
plot_distributions(20)

# Si bien la asintótica se ajusta con la exacta a medida que n crece, parecería 
# que está corrida a la izquierda. Esto debe ser un error en nuestra implementación.


# ----------------------------
# Ejercicio 18
# ----------------------------

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

T_max <- n*(n+1)/2

T_plus <- sum(rank(abs(X)) * (X > 0))

estadistico <- mi.wilcox.test(X,alternative="greater",mu=theta1)$statistic

valores <- 1:T_max
dist <- pTmas(valores,n)
indice <- which(dist < 0.95)[1]
k_estrella <- valores[indice]


# Estimamos potencia
m <- 10000
muestrasbootstrap <- vector(mode="numeric",length=m)
for(i in 1:m){
  remuestreo = rnorm(n,1,1)
  
  # Calculo Tmas
  remuestreo <- remuestreo - mu
  rangos <- rank(abs(remuestreo))
  T_mas <- sum(rangos[remuestreo > 0])
  
  muestrasbootstrap[i] = T_mas
}

potencia_estimada <- mean(muestrasbootstrap > k_estrella)
print(potencia_estimada)


# ----------------------------
# Ejercicio 19
# ----------------------------

set.seed(1984)
n <- 12
X <- rnorm(n, 1, 1)

# Potencia analitica media
potencia_media <- function(n, sigma, alpha) {
  z_alpha <- qnorm(1 - alpha)
  potencia <- 1 - pnorm(z_alpha/ (sigma / sqrt(n)))
  return(potencia)
}

potencia_phi_n <- potencia_media(n, 1, 0.95)
print(potencia_phi_n)

test_signo <- function(x) {
  S <- sum(x > 0)  
  return(S)
}

valores <- 1:T_max
dist <- pbinom(valores,n,0.5)
indice <- which(dist > 0.95)[1]
k_estrella <- valores[indice]


# Estimamos potencia
m <- 10000
muestrasbootstrap <- vector(mode="numeric",length=m)
for(i in 1:m){
  remuestreo = rnorm(n,1,1) # El estadistico se distribuye como una binomial
  res = test_signo(remuestreo)
  muestrasbootstrap[i] = res
}

potencia_estimada <- mean(muestrasbootstrap > k_estrella)
print(potencia_estimada)
