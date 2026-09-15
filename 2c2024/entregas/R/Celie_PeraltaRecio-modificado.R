# Nicolás Celie - Reynaldo Martín Peralta Recio

# PREGUNTA I.
objs <- list (c(T, F), c(T, F, 1), c(T, F, 1, "1"))
for (obj in objs) { print (class (obj)) }
# Me imagino que hay una jerarquía del estilo: logical < numeric < character.
# Habría que averiguar.

# PREGUNTA II.
objs1 <- list (density, density (1:500))
for (obj in objs1) { print (class (obj)) }
density (1:500)
help (density)
boxplot (1:500)
# La diferencia está en que "density" no genera datos por sí sola.

# PREGUNTA III.
length (methods ("print")) 
# Esto me devuelve 31 -> Luego, sabe despachar 31 clases.
length (methods (class="density"))
# Y esto me devuelve 6 -> Luego, además de "plot", está asociada a 5 clases

# PREGUNTA IV.
help (unclass)
# unclass (test_t)
# Veo que devuelve una lista con pseudonombres para cada valor que tiene.
# class (unclass (test_t))
# Es como si redujera la clase 'test_t' a su forma más básica.
# Es decir, agarra esos valores que devuelve el test y los mete en una lista, sin más.
# Dejan de tener esa estructura, y pasan a formar parte de una simple lista.
# Por ejemplo, la presentación informativa de los datos relevantes es parte de la clase 'test_t'.
# Si hago 'unclass' la pierdo.

mi.t.test <- function (x, mu_0 = 0, alfa = 0.05) {
  n <- length(x)
  parameter <- n - 1
  estimate <- mean (x)
  stderr <- sd (x) / sqrt (n)
  statistic <- (estimate - mu_0) / stderr
  conf.int <- estimate + qt(c(alfa / 2, 1 - (alfa / 2)), df = parameter) * stderr
  p.value.izq <- pt (statistic, df = parameter)
  p.value <- 2 * min (p.value.izq, 1 - p.value.izq)
  list (
    parameter = parameter,
    estimate = estimate,
    stderr = stderr,
    statistic = statistic,
    conf.int = conf.int,
    p.value = p.value
  )
}
mi.t.test (rnorm(10), mu_0 = 0, alfa = 0.05)
# Da lo mismo que la función de R 't.test'.
# También se puede transformar esta función como para que devuelva algo
# exactamente igual a lo que devuelve 't.test'.

# Pregunta XII.
particiones <- function (t, n) {
  # Uso programación dinámica para agilizar el calculo.
  dp <- matrix(0, nrow = t + 1, ncol = n + 1)
  for (j in 0:n) {
    dp[1, j + 1] <- 1
  }
  for (j in 1:n) {
    for (i in 0:t) {
      dp[i + 1, j + 1] <- dp[i + 1, j]
      if (i >= j) {
        dp[i + 1, j + 1] <- dp[i + 1, j + 1] + dp[i - j + 1, j]
      }
    }
  }
  return (dp[t + 1, n + 1])
}
  
stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

# Pregunta XIII.
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] <- (particiones (x[i], n) / (2^n))
  }
  return(ret)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    t <- x[i]
    acumulada <- 0
    for (j in 0:t) {
      acumulada <- acumulada + (particiones (j, n) / (2^n))
    }
    ret[i] <- acumulada
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
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)

# Pregunta XIV.
mi.wilcox.test <- function (x, alternative = "two.sided", mu = 0) {
  # Resto 'mu'.
  y <- x - mu
  
  # La función 'rank' con 'abs' de R me devuelve un vector que, en cada posición "i", guarda la cantidad de elementos
  # con un módulo menor o igual al módulo de y[i]. Esto vendría a ser lo mismo que el vector 'R' definido en
  # el enunciado del tp.
  R <- rank (abs (y))
  
  # Ahora sí, puedo calcular T+.
  Tmas <- sum (ifelse (y > 0, R, 0))
  
  n <- length (y)
  
  # Calculo el p-valor usando 'pTmas'.
  if (alternative == "two.sided") {
    p_valor <- 2 * min (pTmas (Tmas - 1, n), 1 - pTmas (Tmas, n))
  }
  if (alternative == "greater") p_valor <- 1 - pTmas (Tmas - 1, n)
  if (alternative == "less") p_valor <- pTmas (Tmas, n)
  
  rv <- list(
    method = "Wilcoxon signed rank test",
    statistic = setNames (Tmas, "V"),
    p.value = p_valor,
    alternative = alternative,
    data.name = deparse(substitute(x))
  )
  # rv$statistic <- Tmas
  structure (rv, class = "htest")
  class (rv) <- "htest"
  return (rv)
}

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1
alternative <- "greater"
R_wilcox <- wilcox.test (X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test (X, alternative=alternative, mu = theta0)

stopifnot (
  identical(mi_wilcox$statistic, R_wilcox$statistic),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)

# Pregunta XVII.
comparar_distribuciones <- function (n) {
  mu <- (n * (n + 1)) / 4
  sigma2 <- (n * (n + 1) * (2 * n + 1)) / 24
  sigma <- sqrt (sigma2)
  t_vals <- 0 : (n * (n + 1) / 2)
  probs_exactas <- sapply (t_vals, function(t) dTmas (t, n))
  densidad_normal <- dnorm (t_vals, mean = mu, sd = sigma)
  barplot(
    probs_exactas, names.arg = t_vals, col = "lightblue",
    main = paste ("Comparación distribución exacta vs asintótica (n =", n, ")"),
    xlab = "T+", ylab = "Probabilidad",
    ylim = c(0, t_vals[n*(n+1)/4] * probs_exactas[n*(n+1)/4] * 1.2 / n)
  )
  lines(t_vals, densidad_normal, col = "red", lwd = 2)
  legend("topright", legend = c("Exacta", "Asintótica"),
         fill = c("lightblue", "red"), border = c("black", NA))
}

set.seed(1234)
n_vals <- c(4, 10, 20)
for (n in n_vals) {
  comparar_distribuciones (n)
}

# Pregunta XVIII.

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))

alpha <- 0.05
m <- 10000

# La función 'replicate' lo que hace es:
# corre el pedazo de código entre los corchetes 'm' veces y
# guarda el resultado de cada ejecución en un vector.
# En este caso, en el vector 'bootstrap_H0'.
# Vale aclarar que usamos directo la función 'wilcox.test' de R
# porque funciona mucho más rápido.

bootstrap_H0 <- replicate (m, {
  X_null <- rnorm (n, mean = 0, sd = sqrt (sigma_sq))
  wilcox.test(X_null, alternative = "greater", mu = 0)$statistic
  # Simulo y me guardo el estadístico de Wilcoxon.
})

k_estrella <- quantile (bootstrap_H0, probs = 1 - alpha)
print(k_estrella)

# Repetimos el procedimiento sobre la hipótesis alternativa.
bootstrap_Halt <- replicate (m, {
  Y <- rnorm (n, mean = theta1, sd = sqrt (sigma_sq))
  wilcox.test(Y, alternative = "greater", mu = 0)$statistic
})

potencia_estimada <- mean (bootstrap_Halt > k_estrella)
print (potencia_estimada)