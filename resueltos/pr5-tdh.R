shape <-  # k on shape, scale param
scale <-  # invesa de rate lambda en Exp (lambda)

sigmasq_hat <- 0.6
n <- 21
pchisq(q=0.6 * n, df=n)
T <- sigmasq_hat * n
pchisq(q=T, df=n)
qchisq(0.01, df=n)
qgamma(0.01)

## Clase 14 - Tira Practica
# https://en.wikipedia.org/wiki/Pareto_distribution
dens <- function(x, tita, x0) {
  return(tita * (x0 ^ tita) / (x ^ (tita + 1)))
}
plot(function(x) {dens(x, tita=10, x0=2)})
xs <- 2 + seq.int(1, 10000) / 100
plot(xs, dens(xs, tita=3, x0=2), type="l")
abline(v=2, col="red")
xs
sum(dens(xs, tita=3, x0=2))
exp(1) ^ (5.4 * log(2))
2^5.4    

alfa <- 0.05
n <- 25
T <- 22.11  # sum(log(X))
Q <- T - n * log(2)
tita0 <- 4
pval <- 1-pgamma(Q, shape=n, rate=tita0)
k <- qgamma(alfa, shape=n, rate=tita0)
c(pval=pval, k_alfa=k)

pot <- function(tita) {
  return(pgamma(k, shape=n, rate=tita))
}
plot(pot, xlim=c(0, 12))
abline(v=tita0)
abline(h=alfa)

# Promedio de n exp(lamda) v.a.
n.sims <- 10000
n.x <- 37
lambda <- 1.26
exps <- replicate(n.sims, mean(rexp(n=n.x, rate=lambda)))
plot(density(exps), col="blue", lty=2)
# exps <- replicate(n.sims, mean(rexp(n=n.x, rate=lambda*2)))
# lines(density(exps), col="orange", lty=2)

gammas <- rgamma(n.sims, shape=n.x, rate =n.x*lambda)
lines(density(gammas), col="red", lty=2)
gammas <- rgamma(n.sims, shape=n.x, rate =n.x*lambda*1.2)
lines(density(gammas), col="green", lty=2)


# https://en.wikipedia.org/wiki/Incomplete_gamma_function
# Se la ve decreciente en x para todo s, pero no la estoy sacando...
inf_inc_gamma <- function(alfa, beta) { 
  return(pgamma(beta, shape=alfa, scale=1) * gamma(alfa))
}

k <- qgamma(0.95, shape=n.x, rate=n.x*lambda)
abline(v=k)
pgamma(k, shape=n.x, rate=n.x*lambda)
pgamma(k, shape=n.x, rate=n.x*lambda*1.2)

# Pr5Ej8
x <- c(3, 4, 5, 3, 2, 7, 6, 4, 6, 4, 7, 2, 3, 4, 6, 7, 3, 5, 6, 6, 4, 5, 5, 7, 3, 2, 2, 4, 3, 5)
x_ <- mean(x)
n <- length(x)
mu0 <- 4
lambda0 <- 1 / mu
sigma0 <- sqrt(1 / lambda0^2)
pnorm(sqrt(n) * (x_ - mu0) / sigma0)

