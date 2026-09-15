# EJERCICIO 1
class(c(T, F))
# "logical"

class(c(T, F, 1))
#"numeric" 

class(c(T, F, 1, "1"))
# "character"

# EJERCICIO 2

class(density)
# "function"

class(density(1:500))
# "density"

# EJERCICIO 3

length(methods(print))
#191

length(methods(class = "density")) - 1
# 5

#EJERCICIO 4

# Genero la muestra
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
# "list"

# EJERCICIO 12
particiones <- function(t, n) {
  if(n == 0){
    return (ifelse(t == 0, 1, 0));
  }
  if(t < 0 || t > n*(n+1)/2){
    return (0);
  }
  return(particiones(t, n-1) + particiones(t-n, n-1))
}

# Test
stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)


# EJERCICIO 13

dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i]<- particiones(x[i],n)/2**n
  }
  return(ret)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    for(j in 0:x[i]){
      ret[i]<- ret[i] + particiones(j, n)
    }
    ret[i]<- ret[i]/2**n
  }
  return(ret)
}
# Test 

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





# EJERCICIO 14

mi.wilcox.test <- function(x, alternative = c("two.sided", "greater", "less"), mu){
  alternative <- match.arg(alternative)
  n <- length(x)
  diffs <- x - mu
  abs_diffs <- abs(diffs)
  rangos <- rank(abs_diffs)
  tmas <- sum(rangos[diffs > 0])
  if (alternative == "two.sided") {
    p_valor <- 2 * (pTmas(rangos, n))
  } else if (alternative == "greater") {
    p_valor <- pTmas(rangos, n)
  } else if (alternative == "less") {
    p_valor <- 1-pTmas(rangos, n)
  }
  resultado <- list(
    statistic = tmas,
    p.value = p_valor[n],
    null.value = c(mean = mu),
    alternative = alternative,
    method = "Wilcoxon signed rank exact test",
    data.name = deparse(substitute(x))
  )
  class(resultado) <- "htest"
  return(resultado)
} 

# Test

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



# EJERCICIO 17

n1 <- 4
n2 <- 10
n3 <- 20
grafico_proba_puntual <- function(n) {
  valores_t <- 0:((n * (n + 1)) / 2)  # Posibles valores de T+
  
  probabilidades <- dTmas(valores_t, n)
  
  # Media y desviación estándar para la densidad asintótica calculado en ejercicio 16
  mu_w <- n * (n + 1) / 4
  sigma_w <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  
  densidad_asintot <- dnorm(valores_t, mean = mu_w, sd = sigma_w)
  
  barplot(
    probabilidades, space=0, names.arg = valores_t, col = "pink",
    main = paste("P.p. de T⁺ vs dens. asintótica (n =", n, ")"),
    xlab = "Valores de T⁺", ylab = "Probabilidad",
    ylim=c(0,max(probabilidades)*1.2), cex.lab=1.5, cex.main=2
  )
  
  lines(valores_t+0.5 , densidad_asintot, col = "red", lwd = 2, type = "l")
}


grafico_proba_puntual(n1)
grafico_proba_puntual(n2)
grafico_proba_puntual(n3)


# EJERCICIO 18
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sigma_sq)

res <- wilcox.test(X, mu = 0, alternative = "greater")

m <- 10000
tmas <- numeric(m)

for(i in 1:m){
  Y <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  test <- wilcox.test(Y, alternative = "greater", mu = 0, conf.level = 0.95)
  tmas[i] <- test$statistic
}

k <- qnorm(0.95, mean = n*(n+1)/4, sd = sqrt(n*(n+1)*(2*n+1)/24))
est_potencia <- (1/m)*sum(tmas > k)
est_potencia #0.9047


# EJERCICIO 19
set.seed(1984)
n <- 12
theta1 <- 1
sigma <- 1
alpha <- 0.05
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

z_critico <- qnorm(1 - alpha)
delta <- sqrt(n) * (theta1 - 0) / sigma  
potencia_z <- 1 - pnorm(z_critico - delta)
potencia_z  # 0.9655632 potencia analítica


m <- 10000
pvalores_s <- numeric(m)
ss <- numeric(m)
for (i in 1:m) {
  Y <-rnorm(n,mean=theta1, sd= sd(X))
  signos <- sum(Y>0)
  ss[i] <- signos
  pvalores_s[i] <- binom.test(signos, n, p = 0.5, alternative = "greater")$p.value
}
est_potencia <- mean(pvalores_s <= 0.05) 
est_potencia  # 0.7689
mean(signos >= 10)
table(signos)
