alfa <- 0.05
n <- 100
lambda0 <- 1
lambda1 <- 2
k <- qgamma(alfa, shape = n, rate = lambda0)
Tobs <- 87.9
pval <- pgamma(Tobs, shape = n, rate = lambda0)
pot <- pgamma(k, shape = n, rate = lambda1)

pot_fun <- function(lambda) {
  pgamma(k, shape = n, rate = lambda)
}
lambdas <- seq(0.8, 3, by = 0.05)
plot(lambdas, pot_fun(lambdas), type = "l")
abline(v = 1, col = "red")
abline(h = 0.05, col = "red")
abline(v = 2, col = "blue")
abline(h = 1, col = "blue")


# ====== ej 2 ======

make_pot_fun2 <- function(mu0, n, alfa) {
  ret <- function(mu) {
    z_alfa <- qnorm(alfa, lower.tail = FALSE)
    1 - pnorm(z_alfa - (mu - mu0) * sqrt(sum(1:n)))
  }
  ret
}
mu0 <- 1
mu1 <- 1.5
n <- 10
alfa <- 0.05
pot_fun2 <- make_pot_fun2(mu0, n, alfa)
mus <- seq(mu0 - 0.5, mu1 + 0.5, by = 0.025)
plot(mus, pot_fun2(mus), type = "l")
abline(v = mu0, col = "red")
abline(h = alfa, col = "red")
abline(v = mu1, col = "blue")
abline(h = pot_fun2(mu1), col = "blue")

# ==== p5 ej 3.i ====
n <- 21
sigmasq_0 <- 1
sigmasq_obs <- 0.6
alfa <- 0.01
k <- qchisq(alfa, df = n) * sigmasq_0 / n
phi <- (sigmasq_obs < k)
pval <- pchisq(sigmasq_obs * n / sigmasq_0, df = n)
paste0(
  ifelse(phi, "", "NO"),
  " RECHAZO H0 (alfa=",
  round(alfa, 3),
  ", p-val=",
  round(pval, 4),
  ")"
)

make_pot_fun3 <- function(sigmasq0, n, alfa) {
  ret <- function(sigmasq1) {
    pchisq(qchisq(alfa, df=n) * sigmasq0 / sigmasq1, df=n)
  }
  ret
}
alfa <- 0.01
n <- 21
sigmasq0 <- 1
sigmasq1 <- 0.8
pot_fun3 <- make_pot_fun3(sigmasq0, n, alfa)
sigmas <- seq(sigmasq1 - 0.1, sigmasq0 + 0.1, by = 0.01)
plot(sigmas, 1 - pot_fun3(sigmas), type = "l")
abline(v = 1, col = "red")
abline(h = 1 - alfa, col = "red")
abline(v = sigmasq1, col = "blue")
PET2 <- 1 - pot_fun3(sigmasq1)
abline(h = PET2, col = "blue")
PET2
