IC_mu_var_conocida <- function(datos, var_0, nivel) {
  alfa <- 1 - nivel
  zetas <- qnorm(c(alfa / 2, 1 - alfa / 2))
  Xraya <- mean(datos)
  Xraya + zetas * sqrt(var_0 / length(datos))
}

IC_0 <- IC_mu_var_conocida(rep(3.48, 9), 3, 0.95)
IC_1 <- IC_mu_var_conocida(rep(3.48, 36), 3, 0.95)
len_0 <- IC_0[2] - IC_0[1]
len_1 <- IC_1[2] - IC_1[1]
c(len_1, len_0)

IC_0 <- IC_mu_var_conocida(rnorm(n=9, mean=3.48, sd=sqrt(3)), 3, 0.95)
IC_1 <- IC_mu_var_conocida(rnorm(n=36, mean=3.48, sd=sqrt(3)), 3, 0.95)
len_0 <- IC_0[2] - IC_0[1]
len_1 <- IC_1[2] - IC_1[1]
c(len_1, len_0)
IC_0
IC_1

IC_0 <- IC_mu_var_conocida(3.48 + rnorm(n=9, mean=0, sd=sqrt(3)), 3, 0.95)
IC_1 <- IC_mu_var_conocida(3.48 + rnorm(n=36, mean=0, sd=sqrt(3)), 3, 0.95)
len_0 <- IC_0[2] - IC_0[1]
len_1 <- IC_1[2] - IC_1[1]
c(len_1, len_0)
IC_0
IC_1


#Ej. 3
n.x <- 5
mu <- 4
var_0 <- 9
nivel <- 0.95
n.sims <- 10000
simular_ICs <- function(n.x, mu, var_0, nivel, n.sims) {
  ICs <- matrix(nrow=n.sims, ncol=2)
  for (i in 1:n.sims) {
    datos_normales <- rnorm(n.x, mu, sqrt(var_0))
    ICs[i,] <- IC_mu_var_conocida(datos_normales, var_0, nivel)
  }
  return(ICs)
}
ICs <- simular_ICs(n.x, mu, var_0, nivel, 1000)
mean((mu > ICs[, 1]) & (mu < ICs[, 2]))
