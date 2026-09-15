# prg 1
class(c(T, F))
class(c(T, F, 1))
class(c(T, F, 1, "1"))

# prg 2
class(density)
class(density(1:500))

# prg 3
library(sloop)
length(methods("print"))
nrow(sloop::s3_methods_generic("print"))
library(tidyverse)
methods(class="density")
sloop::s3_methods_class("density")
methods(class="pato")

# prg 4
class(unclass(t.test(1:500)))

# prg 12
particiones <- function(t, n) {
  if (n == 0) {
    return(ifelse(t == 0, 1, 0))
  } else if ((t < 0) | (t > n * (n + 1) / 2)) {
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

dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- particiones(x[i], n)
  }
  return(ret / 2 ^ n)
}
pTmas <- function(x, n) {
  dTmas_n <- dTmas(0:(max(x)), n)
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- sum(dTmas_n[1:(x[i] + 1)])
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
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n),
  pTmas(c(21, 13, 8, 5, 3), 13) * 2^13 == c(386, 88, 25, 10, 5)
)

# prg 14
is.scalar <- function(x) { is.numeric(x) && length(x) == 1 }
mi.wilcox.test <- function(x, alternative = c("two.sided", "less", "greater"), mu = 0) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  n <- length(x)
  x <- x - mu  # centro los datos en la mediana bajo H0
  rv <- list(
    alternative = match.arg(alternative),
    statistic = setNames(sum(rank(abs(x))[x > 0]), "V")
  )
  pval.izq <- pTmas(rv$statistic, n)
  pval.der <- 1 - pTmas(rv$statistic - 1, n)
  if (alternative == "greater") {
    rv$p.value <- pval.der
  } else if (alternative == "less") {
    rv$p.value <- pval.izq
  } else { # alternative == "two.sided"
    rv$p.value <- 2 * min(pval.izq, pval.der)
  }
  structure(rv, class = "htest")
}

## test greater
set.seed(1234)
n <- 11
X <- rnorm(n)
theta0 <- -1
alternative <- "greater"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)


## test less
set.seed(7645)
n <- 14
X <- runif(n, -5, 0)
theta0 <- -1
alternative <- "less"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)

## test two.sided
set.seed(7645)
n <- 13
X <- rcauchy(n)
theta0 <- 0
alternative <- "two.sided"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)


## prg 17
sumaN <- function(n) {
  n * (n + 1) / 2
}
soporteN <- function(n) {0:sumaN(n)}
ETmas <-function(n) {sumaN(n) /2}
VarTmas <- function(n) {n*(n+1)*(2*n+1)/24}
dTmasN <- function(n) {dTmas(soporteN(n), n)}
soporteAsint <- function(n) {(soporteN(n) - ETmas(n)) / sqrt(VarTmas(n))}
par(
  mfrow=c(1, 3),
  mar=c(4,2,3,0),
  ann=FALSE
)
for (n in c(4, 10, 20)) {
  soporteAsintN <- soporteAsint(n)
  delta <- soporteAsintN[2] - soporteAsintN[1]
  plot(
    x=soporteAsintN,
    y=dTmasN(n),
    ylim=c(0, max(dTmasN(n)) * 1.05),
    xlim=c(-1, 1) * max(soporteAsintN) * 1.05,
    type="h",
    col="blue",
  )
  title(xlab=paste0("n=", n))
  grilla <- seq(-4, 4, by=0.01)
  lines(x=grilla, y=dnorm(grilla) * delta, type="l", col="red", cex=2)
}
# install.packages("latex2exp")
library(latex2exp)
mtext(TeX("Probabilidad Puntual para $T^{+}_n$ normalizada, $n \\in \\{4, 10, 20\\}$"), side = 3, line = - 2, outer = TRUE)

# prg 18
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
# constantes útiles
m <- 10000
theta0 <- 0
alfa <- 0.05
Tmas <-function(x, theta0) {
    setNames(sum(rank(abs(x - theta0))[x > theta0]), "Tmas")
  }
boot.Tmas <- function(alfa, mean, sd, m, seed = NULL) {
  set.seed(seed)
  boots <- vector(mode = "numeric", length = m)
  for (i in seq.int(m)) {
    Y <- rnorm(n, mean = mean, sd = sd)
    boots[i] <- Tmas(Y, theta0)
  }
  return(boots)
}
suma.n <- function(n) { n * (n + 1) / 2 }
k.Tmas <- function(
    alfa, n, alternative = c("two.sided", "less", "greater")
){
  alternative <- match.arg(alternative)
  suma <- suma.n(n)
  pTobs <- cumsum(dTmas(0:suma, n))  # mucho más rápido que pTmas(0:(n*(n+1)/2), n)
  if (alternative == "less") {
    # `idx` es el índice del primer valor $k$ tal que la proba acumulada A IZQ es > 0.05,
    # Como el soporte de Tmas empieza en 0 y en R los vectores se indexan desde 1,
    # min j : pTmas(j, n) > alfa es `j <- dx - 1`
    # Si queremos definir la RR con un límite _inclusivo_, max k : pTmas(k, n) < alfa es k = j - 1
    idx <- which.max(pTobs > alfa)
    k <- idx - 2 # k = j - 1 = (idx - 1) - 1 = idx - 2
    RR <- 0:k
    alfa.star <- pTmas(k, n)
  } else if (alternative == "greater") {
    # Por la definición de función de distribución y `which.max`, `idx` es el primer (mínimo) índice
    # que acumula más de 1 - alfa a A IZQ. Luego, `j = idx - 1` y pTmas(j, n) > 1 - alfa, y la RR con límite inclusivo es
    # [k, +oo), con `k = j + 1 = (idx - 1) + 1 = idx
    idx <- which.max(pTobs > 1 - alfa)
    k <- idx
    RR <- k:suma
    alfa.star <- 1 - pTmas(k - 1, n)
  } else { # alternative == 'two.sided'
    # La distribución es simétrica, usamos la lógica de `"less"` con alfa.two.sided <- 1/2 * alfa
    # y tomamos el simétrico a derecha
    alfa.cola.izq <- alfa / 2
    idx <- which.max(pTobs > alfa.cola.izq)
    k <- idx - 2 # k = j - 1 = (idx - 1) - 1 = idx - 2
    RR <- c(0:k, (suma - k):suma)
    alfa.star <- pTmas(k, n) * 2
  }
  stopifnot(alfa.star <= alfa)
  return(list(k=k, RR=RR, alfa.star=alfa.star))
}

pot.boot.Tmas <- function(alfa, mean, sd, m, n, alternative) {
  boots <- boot.Tmas(alfa, mean, sd, m)
  RR <- k.Tmas(alfa, n, alternative)$RR
  return(mean(boots %in% RR))
}
pot.boot.Tmas(alfa=0.05, mean=theta1 - theta0, sd=sigma_sq, m=as.integer(1e5), n=n, alternative="greater")


# === sencillito para el TP ===
m <- 10000
theta0 <- 0
alfa <- 0.05
Tmas <-function(x) { sum(rank(abs(x))[x > 0]) }
boot.Tmas <- vector(mode = "numeric", length = m)
for (i in seq.int(m)) {
  Y <- rnorm(n, mean = theta1, sd = sigma_sq)
  boot.Tmas [i] <- Tmas(Y)
}
suma.n <- (n*(n+1)/2)
pTobs <- cumsum(dTmas(0:suma.n, n))  # mucho más rápido que pTmas(0:(n*(n+1)/2), n)
idx <- which.max(pTobs > 1 - alfa)
# idx es el índice del primer elemento tal que pTobs > 1 - alfa
# pTobs[idx] es la acumulada hasta k = idx-1; pTmas(idx-1, n) > 1 - alfa
# la región de rechazo comenzará - inclusive - en el entero siguiente a (idx - 1), que es `idx`
k.star <- idx
alfa.star <- sum(dTmas(k.star:suma.n, n))
pot.boot.wil <- mean(boot.Tmas >= k.star)
round(c(k.star=k.star, alfa.star=alfa.star, pot.boot.wil=pot.boot.wil), 4)

# El punto de corte del test a izquierda, es el último k : p_n(k) <= 0.05
# prg 19
# set.seed(1984)
# n <- 12
# theta1 <- 1
# sigma_sq <- 1
# X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq)) 
# alfa = 0.05
k.signo <- which.max(pbinom(0:n, n, 1/2, lower.tail=FALSE) <= alfa)
alfa.signo <- sum(dbinom(k.signo:n, n, 1/2))
Tsigno <- function(x, theta0) { setNames(sum(x > theta0), "S") }
boot.Signo <- vector(mode = "numeric", length = length(m))
for (i in seq.int(m)) {
  Y <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  boot.Signo[i] <- Tsigno(Y, theta0)
}
pot.boot.sgn <- mean(boot.Signo >= k.signo)
# El punto de corte del test a izquierda, es el último k : p_n(k) <= 0.05
pTsigno <- pbinom(0:n, size=n, prob=1/2, lower.tail=FALSE)  # acumulada de Tmas con n=12
alfa <- 0.05
idx <- which.max(pTsigno <= alfa)
# idx es el índice del primer valor $k$ tal que la proba acumulada A DERECHA es <= 0.05
# Como el soporte de Tmas empieza en 0 y en R los vectores se indexan desde 1,
# el índice idx corresponde al k=idx - 1
ks <- idx - 1
stopifnot(1 - pbinom(ks, size=n, prob=1/2) <= alfa)
stopifnot(1 - pbinom(ks - 1, size=n, prob=1/2) > alfa)
pothat_phis <- mean(boot.Signo > ks)
k.star <- which.max(pbinom(0:n, size=n, prob=1/2, lower.tail=FALSE) <= alfa) + 1
alfa.signo <- sum(dbinom(ks:n, size=n, prob=1/2))
# test N para normales con varianza conocida
# theta0 <- 0
kn <- qnorm(alfa, lower.tail=FALSE)
delta <- sqrt(n) / sqrt(sigma_sq) * (theta0 - theta1)
pot_phin <- pnorm(kn + delta, lower.tail=FALSE)

# test T para normales con varianza _des_conocida
# potencia con t no central
R.pot.t <- power.t.test(
  n=n,
  delta = theta1,
  sd=sqrt(sigma_sq),
  sig.level = alfa,
  type = "one.sample",
  alternative = "one.sided"
)
# my way
k.t <- qt(alfa, df=n-1, ncp=0, lower.tail=FALSE)
ncp <- sqrt(n) * (theta1 - theta0) / sqrt(sigma_sq)
mi.pot.t <- pt(k.t, ncp=ncp, df=n-1, lower.tail=FALSE)
stopifnot(isTRUE(all.equal(R.pot.t$power, mi.pot.t)))
potencias <- c(wil=pot.boot.wil, sgn=pot.boot.sgn, norm=pot_phin, tstu=mi.pot.t)
sort(potencias)

