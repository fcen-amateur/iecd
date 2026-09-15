# ej 1
tita <- 0.1337
alfa <- 1/tita
beta <- 1
n <- 10000000
x <- rbeta(n, alfa, beta)
mean(x)
titahat <- 1 / mean(x) - 1

# ej 3
n <- 100
mu <- 5
sigma <- 8
alfa <- 0.05
x <- rnorm(n, mu, sigma)

ic1 <- function(x, mu, alfa=0.05) {
  n <- length(x)
  a1 <- qchisq(alfa / 2, df=n)
  b1 <- qchisq(1 - alfa / 2, df=n)
  denom <- sum((x - mu)**2)
  denom / c(b1, a1)
}

ic2 <- function(x, mu, alfa=0.05) {
  n <- length(x)
  a2 <- qchisq(alfa / 2, df=1)
  b2 <- qchisq(1 - alfa / 2, df=1)
  denom <- n * ((mean(x) - mu) **2)
  return(denom / c(b2, a2))
}
ic1(x, mu)
ic2(x, mu)

Nrep <- 1000
intervalos1 <- matrix(nrow=Nrep, ncol=2)
intervalos2 <- matrix(nrow=Nrep, ncol=2)
for (i in 1:Nrep) {
  x <- rnorm(n, mu, sigma)
  intervalos1[i,] <- ic1(x, mu)
  intervalos2[i,] <- ic2(x, mu)
}
long1 <- intervalos1[,2] - intervalos1[,1]
long2 <- intervalos2[,2] - intervalos2[,1]
c(mean(long1), mean(long2))
