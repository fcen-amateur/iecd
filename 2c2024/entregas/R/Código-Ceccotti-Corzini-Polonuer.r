# Ejercicio 1
class(c(T, F))
# c(T, F): logical
class(c(T, F, 1))
# c(T, F, 1): numeric
class(c(T, F, 1, "1"))
# c(T, F, 1, "1"): character

# evidentemente esta devolviendo la clase a la que pertenecen los elementos de la lista
# R tipa los datos a la clase mas general posible para que haya coherencia en las operaciones

# Ejercicio 2

print(class(density))
# "function"

print(class(density(1: 500)))
# "density"

install.packages("sloop") 
library(sloop)

# vamos a ver la diferencia
sloop::s3_dispatch(class(density))
# class.function > class.default
sloop::s3_dispatch(class(density(1:500)))
# class.density > class.default

# evidentemente al no pasarle parametros R interpreta que density es una funcion pero al pasarle los parametros
# la funcion retorna una clase density que es la que R imprime

# Ejercicio 3
length(methods(print))
# 233
methods(class = 'density')
# sin contar "plot" son 5

# Ejercicio 4

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

uncls <- unclass(test_t)
uncls
cls <- class(uncls)
# "list"

# explicacion:
# luego R nos dice que "la copia" de los argumentos que devuelve el llamado t.test, compone una lista.

# ejercicio 12
# al algoritmo recursivo propuesto por la catedra le añadimos "memoización" para hacer el computo mas eficiente
memo <- matrix(-1, nrow = 10000, ncol = 10000)
particiones <- function(t, n) {
  if (n == 0 && t == 0) {
    return(1)
  }
  if (n == 0) {
    return(0)
  }
  if (t < 0 || t > (n * (n + 1) / 2)) {
    return(0)
  }
  
  if (memo[t + 1, n + 1] == -1) {
    memo[t + 1, n + 1] <<- particiones(t, n - 1) + particiones(t - n, n - 1)
  }
  return(memo[t + 1, n + 1])
}

stopifnot(
  particiones(t = 3, n = 4) == 2,
  particiones(t = 24, n = 12) == 67,
  particiones(t = 55, n = 10) == 1,
  particiones(t = 45, n = 30) == 1938
)

# ejercicio 13
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
    for (j in 0:x[i]) {
      ret[i] <- ret[i] + particiones(j, n) / (2^n)
    }
  }
  return(ret)
}

n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2^12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2^-55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)

# ejercicio 14
# calcular_t_mas recibe una lista y devuelve el computo del estadistico T+ 
calcular_t_mas <- function(x) {
  s <- 0
  R <- rank(abs(x))
  n <- length(x)
  for (i in 1:n) {
    if (x[i] > 0) {
      s <- s + R[i]
    }
  }
  return(s)
}

mi.wilcox.test <- function(x, alternative, mu) {
  n <- length(x)
  rv <- list(
    null.value = c(mean = mu),
    alternative = alternative,
    method = "Mi test de Wilcoxon",
    data.name = deparse(substitute(x))
  )
  x <- x - mu
  t_mas_obs <- calcular_t_mas(x)
  rv$statistic <- setNames(t_mas_obs, "V")
  
  if (alternative == "two.sided") {
    if (t_mas_obs > n * (n + 1) / 4) {
      rv$p.value <- 2 * (1 - pTmas(t_mas_obs - 1, n))
    } else {
      rv$p.value <- 2 * pTmas(t_mas_obs, n)
    }
  } else if (alternative == "greater") {
    rv$p.value <- (1 - pTmas(t_mas_obs - 1, n))
  } else if (alternative == "less") {
    rv$p.value <- pTmas(t_mas_obs, n)
  }
  structure(rv, class = "htest")
}

set.seed(1234)
n <- 20

X <- rnorm(n)
theta0 <- -1
R_wilcox <- wilcox.test(X, alternative = "greater", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative = "greater", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)


# ejercicio 17
# la siguiente funcion grafica, dado un n, un plot de barras con la probabilidad exacta
# y sobre ella, la densidad de convergencia asintotica
plot_pdf <- function(n) {
  mean_T <- n * (n + 1) / 4
  sd_T <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  max_n <- n * (n + 1) / 2
  
  exact_pdf <- dTmas(1:max_n, n)
  asympt_pdf <- dnorm(1:max_n, mean_T, sd_T)
  
  y_max <- max(c(exact_pdf, asympt_pdf))
  
  barx <- barplot(exact_pdf,
                  col = "lightblue",
                  main = paste("Exact vs Asymptotic PDF (n =", n, ")"),
                  xlab = "T+", ylab = "Density",
                  ylim = c(0, y_max)
  )
  
  lines(barx, asympt_pdf, col = "red", lwd = 2)
  
  legend("topleft",
         legend = c("Exact", "Asymptotic"), 
         fill = c("lightblue", NA),
         border = c("black", NA), lty = c(NA, 1), col = c("black", "red")
  )
}

par(mfrow = c(1, 3))
plot_pdf(4)
plot_pdf(10)
plot_pdf(20)
par(mfrow = c(1, 1))

# ejercicio 18
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

wilcox.test(X, alternative = "greater", mu = 0)$statistic

estimador_potencia <- function (theta_1) {
  m <- 10000
  muestras <- vector(mode="numeric", length = m)
  for (i in 1:m) {
    muestra <- rnorm(n, mean = theta_1, sd = 1)
    muestras[i] <- calcular_t_mas(muestra)
  }
  
  max_T <- n*(n+1)/2
  vals <- 0:max_T
  
  cdf_vals <- pTmas(vals, n)
  k_estrella <- vals[which(cdf_vals >= 0.95)[1]]
  
  
  potencia_estimada <- mean(muestras > k_estrella)
  return(potencia_estimada)
}

theta_values <- 0:5
y <- sapply(theta_values, estimador_potencia)

plot(theta_values, y, type="b", pch=19, xlab="tita", ylab="Estimacion",
     main="Potencia via Bootstrap")

legend("right", legend="Estimacion", pch=19, col="black")
text(theta_values, y, labels=round(y, 3), pos=4)

#Ejercicio 19:
#Como la mediana estimada es 0, uso directo X
positivos <- sum(X>0)
negativos <- sum(X<0)
resultado <- binom.test(c(positivos,negativos),p = 0.5, alternative = "greater",conf.level=0.95)
resultado

resultados_test<-c()
for (i in 1:10000){
  muestra <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  positivos <- sum(muestra>0)
  negativos <- sum(muestra<0)
  test <- binom.test(c(positivos,negativos),p = 0.5, alternative = "greater")
  p_valor <- test$p.value
  resultados_test[i]<-(p_valor<0.05)
}

total_rechazos<- sum(resultados_test)
potencia_estimada_signo <- total_rechazos / 10000

#Habíamos visto analíticamente la potencia del test de la mediana: 
#Veamos cuánto vale si theta = 1:
#Sabíamos que sigma era conocida y valía 1:
q_0.05<-qnorm(0.95)
t<-q_0.05-(1/(1/sqrt(12)))
potencia_test_de_la_media<-1-pnorm(t)

potencia_test_de_la_media
potencia_estimada_signo