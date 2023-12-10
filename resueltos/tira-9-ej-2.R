n.x <- 30
lambda0 <- 3
n.sims <- 10000
lambdones <- vector("numeric", n.sims)
for (i in 1:n.sims) {
  
  x_ <- mean(rpois(n.x, lambda0))
  lambdones[i] <- 2 * n.x * (lambda0 - x_ - x_ * log(lambda0 / x_))
}
dens <- density(lambdones)
plot(dens, col="blue")
curve(dchisq(x, df=1), add=TRUE, col="red")
legend(5, 1, legend=c("KDE Lambdon", "Chisq 1GL"), col=c("blue", "red"), lty=1)

Lobs <- 4.148
1 - pchisq(Lobs, df=1)
1 - ecdf(lambdones)(Lobs)
mean(lambdones > Lobs)
alfa <- 0.05
crit <- qchisq(1 - alfa, df=1)
mean(lambdones > 3.69)
