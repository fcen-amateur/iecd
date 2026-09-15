# ejercicio 12 ----

particiones <- function(t, n){
  if (n == 0 & t == 0) {
    return(1)
  } else if ((n == 0 & t != 0) | t < 0 | t > n * (n + 1) / 2){
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

# ejercicio 13 ----

dTmas <- function(x, n) {
  res <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    res[i] <- particiones(x[i], n) / 2**n
  }
  return(res)
}

pTmas <- function(t, n) {
  res <- vector(mode = "numeric", length = length(t))
  for (i in seq_along(t)) {
    res[i] <- sum(dTmas(0:t[i], n))
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

# ejercicio 14 ----

rango <- function(i, x) {
  x <- abs(x)
  return(sum(x <= x[i]))
}

R <- function(x) {
  R <- vector(mode = 'numeric', length = length(x))
  for (i in 1:length(x)) {
    R[i] <- rango(i,x)
  }
  return(R)
}
Tmas <- function(x) {
  R <- R(x)
  return(sum((x > 0) * R))
}

mi.wilcox.test <- function(x, alternative, mu) {
  stopifnot(is.numeric(x))
  stopifnot(is.character(alternative))
  alternative <- match.arg(alternative, c('two.sided','greater','less'))
  stopifnot(is.numeric(mu) && length(mu) == 1)

  n <- length(x)
  Tmas_obs <- Tmas(x-mu)

  if (alternative == 'two.sided') {
    p <- 1 - pTmas(abs(Tmas_obs), n) + dTmas(abs(Tmas_obs), n) + pTmas(-abs(Tmas_obs), n) # Tmas > |Tobs| + Tmas == Tobs y Tmas <= -|Tobs|
  } else if (alternative == 'greater') {
    p <- 1 - pTmas(Tmas_obs, n) + dTmas(abs(Tmas_obs), n) # Tmas > Tobs + Tmas == Tobs
  } else {
    p <- pTmas(Tmas_obs, n) # Tmas <= Tobs
  }

  structure(
    list(
      statistic = Tmas_obs,
      p.value = p,
      alternative = alternative,
      data.name = deparse(substitute(x))
    ),
    class = 'htest'
  )
}

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

# ejercicio 18 ----

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

mi.wilcox.test_nivel <- function(x,nivel) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(nivel) && 0<nivel && nivel<1)
  n <- length(x)
  #En este caso, alternative = 'greater'
  #Quiero hallar el k*
  k=0
  while(k <= n*(n+1)/2){
    pr = 1 - pTmas(k,n) + dTmas(k,n)
    if(pr <= nivel){
      break
    }else{
      k = k+1
    }
  }
  res <-
    list(
      statistic = c(critical_value = k),
      parameter = c(level = nivel),
      alternative = 'greater',
      data.name = deparse(substitute(x))
    )
  structure(res,class="htest")
}
mi.wilcox.nivel <- mi.wilcox.test_nivel(X,nivel = 0.05)
mi.wilcox.nivel

#Entonces k* = 61. Quiero ahora aplicar bootstrap para estimar la funcion de potencia.
kcrit = mi.wilcox.nivel$statistic
estimates = c()
m = 10000
for(i in 1:m){
  n <- 12
  theta1 <- 1
  sigma_sq <- 1
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  estimates = c(estimates,Tmas(X))
}
fp = sum(estimates > kcrit)/m   # 0.9138

# ejercicio 19 ----
set.seed(1984)
#Test para datos distribuidos normalmente

n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

mi.wilcox.test_normal <- function(x,nivel) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(nivel) && 0<nivel && nivel<1)
  n <- length(x)
  Tes <- sqrt(n) * mean(x)
  k_value = qnorm(nivel,lower.tail=FALSE)
  #En este caso, alternative = 'greater'
  res <-
    list(
      statistic = c("critical_value" = k_value),
      parameter = c(level = nivel),
      estimate = c(statistic = Tes),
      alternative = 'greater',
      data.name = deparse(substitute(x))
    )
  structure(res,class="htest")
}
nivel = 0.05
mi.wilcox.normal <- mi.wilcox.test_normal(X,nivel)
mi.wilcox.normal


#Test del signo

mi.wilcox.test_signo <- function(x,nivel) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(nivel) && 0<nivel && nivel<1)
  n <- length(x)
  Tvariables <- (x >= 0)
  Tes <- sqrt(n)*(mean(Tvariables) - 0.5)/sqrt(0.25)
  k_value = qnorm(nivel/2,lower.tail=FALSE)
  #En este caso, alternative = 'two.sided'
  res <-
    list(
      statistic = c("critical_value" = k_value),
      parameter = c(level = nivel),
      estimate = c(statistic = Tes),
      alternative = 'two.sided',
      data.name = deparse(substitute(x))
    )
  structure(res,class="htest")
}
nivel = 0.05
mi.wilcox.signo = mi.wilcox.test_signo(X,nivel)
mi.wilcox.signo
#Funciones de Potencia estimadas por Bootstrap

#Test normal

#Formula cerrada para la función de potencia calculada analiticamente:
n=12
fpExactNorm = 1 - pnorm(1.645 - sqrt(n))  #0.9655

#Bootstrap
kcritNorm = mi.wilcox.normal$statistic["critical_value"]
estimatesNorm = c()
m = 10000
TNorm <- function(x){return(sqrt(length(x)) * mean(x))}
for(i in 1:m){
  n <- 12
  theta1 <- 1
  sigma_sq <- 1
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  estimatesNorm = c(estimatesNorm,TNorm(X))
}
fpNorm = sum(estimatesNorm > kcritNorm)/m     #0.9645


#Test signo

kcritSigno = mi.wilcox.signo$statistic["critical_value"]
estimateSigno = c()
m = 10000
TSigno <- function(x){return(sqrt(length(x))*(mean((x >= 0)) - 0.5)/sqrt(0.25))}
for(i in 1:m){
  n <- 12
  theta1 <- 1
  sigma_sq <- 1
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  estimateSigno = c(estimateSigno,TSigno(X))
}
fpSigno = sum(estimateSigno > kcritSigno)/m  #0.7046

#El test T es efectivamente más potente que el test de signos,
#al tener una diferencia de aproximadamente 0.261 al evaluar
#las funciones de potencia de ambos tests en tita_1 = 1, siendo la del
#test normal la que posee un valor mayor (0.9655).
