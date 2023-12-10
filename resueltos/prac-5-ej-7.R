n <- 10
l0 <- 3
alfa <- 0.05
elegir_crit <- function(lambda0, n) {qgamma(1 - alfa, shape=n, rate=n*lambda0)}
crit0 <- elegir_crit(l0, n)
pgamma(crit0, shape=n, rate=n*l0)
crear_potencia <- function(crit, n) {
  pot <- function(lambda) { 1 - pgamma(crit, shape=n, rate=n*lambda) }
  return(pot)
}
plot(crear_potencia(crit0, n), xlim=c(0, 5))
abline(v=l0, col="red")
abline(h=alfa, col="gray")
curve(crear_potencia(elegir_crit(0.95*l0, n), n)(x), col="blue", add=TRUE)