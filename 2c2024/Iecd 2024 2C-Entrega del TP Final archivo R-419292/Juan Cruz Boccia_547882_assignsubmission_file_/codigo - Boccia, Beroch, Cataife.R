# Pregunta 1
class(c(T,F))
class(c(T,F,1))
class(c(T,F,1,"1"))

# Pregunta 2
class(density)
class(density(1:100))

# Pregunta 3
length(methods(print))
length(methods(class="density"))

# Pregunta 4
help(unclass)

ttest <- t.test(1:10, 11:20)
class(ttest)
class(unclass(ttest))

# Pregunta 10
calcular_distribucion_T_mas <- function(n) {
  subconjuntos <- list()
  sumas <- integer()
  
  for (i in 0:n) {
    if (i == 0) {
      subconjuntos[[1]] <- list("{}")  # Vacio
      sumas[1] <- 0
    } else {
      current_subconjuntos <- combn(n, i, simplify = FALSE)
      current_sumas <- sapply(current_subconjuntos, sum)
      subconjuntos <- c(subconjuntos, current_subconjuntos)
      sumas <- c(sumas, current_sumas)
    }
  }
  
  sumas_unicas <- sort(unique(sumas))
  frecuencias <- table(sumas)
  
  subconjuntos_format <- sapply(sumas_unicas, function(t) {
    indices <- which(sumas == t)
    formateados <- sapply(indices, function(idx) {
      paste0("{", paste(subconjuntos[[idx]], collapse = ", "), "}")
    })
    paste(formateados, collapse = ", ")
  })
  
  probabilidades <- frecuencias / length(sumas)
  
  resultado <- data.frame(
    t <- sumas_unicas,
    S_n_t <- subconjuntos_format,
    `#S_n,t` <- as.integer(probabilidades * length(sumas)),
    `p_n(t)` <- as.numeric(probabilidades)
  )
  
  rownames(resultado) <- NULL
  resultado
}

n <- 5
tabla_distribucion <- calcular_distribucion_T_mas(n)
print(tabla_distribucion)

# Pregunta 12
particiones <- function(t, n) {
  if (t < 0 || t > n * (n + 1) / 2) { # Caso base
    return(0)
  }
  if (n == 0) {
    return(ifelse(t == 0, 1, 0))
  }
  return(particiones(t, n - 1) + particiones(t - n, n - 1)) # Paso Recursivo
}

stopifnot(
  particiones(3, 4) == 2,
  particiones(24, 12) == 67,
  particiones(55, 10) == 1,
  particiones(45, 30) == 1938
)

# Pregunta 13
dTmas <- function(x, n) { # Funcion probabilidad puntual dTmas
  probabilidades <- numeric(length(x))
  
  for (i in seq_along(x)) {
    t <- x[i]
    probabilidades[i] <- particiones(t, n) / 2^n
  }
  
  return(probabilidades)
}

pTmas <- function(x, n) { # Funcion distribucion acumulada pTmas
  proba_acumulada <- numeric(length(x))
  
  for (i in seq_along(x)) {
    t <- x[i]
    proba_acumulada[i] <- sum(dTmas(0:t, n))
  }
  
  return(proba_acumulada)
}

n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2^12,
  all(dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16),
  abs(sum(dTmas(0:21, 6)) - 1) < 1e-8,
  dTmas(0:2, 55) == 2^-55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)
)

# Pregunta 14
mi.wilcox.test <- function(x, alternative = "two.sided", mu = 0) {
  stopifnot(is.numeric(x), is.character(alternative), is.numeric(mu))
  alternative <- match.arg(alternative, choices = c("two.sided", "greater", "less"))
  
  x_centered <- x - mu
  
  rangos <- rank(abs(x_centered))
  signos <- ifelse(x_centered > 0, 1, 0)
  T_plus <- sum(signos * rangos)
  
  # T+ bajo H0
  n <- length(x)
  all_possible_T_plus <- 0:(n * (n + 1) / 2)
  prob_T_plus <- dTmas(all_possible_T_plus, n)
  
  if (alternative == "two.sided") {
    p_value <- 2 * min(
      sum(prob_T_plus[T_plus + 1:length(prob_T_plus)]),
      sum(prob_T_plus[1:(T_plus + 1)])
    )
    
  } else if (alternative == "greater") {
    p_value <- sum(prob_T_plus[(T_plus + 1):length(prob_T_plus)])
  } else if (alternative == "less") {
    p_value <- sum(prob_T_plus[1:(T_plus + 1)])
  }
  
  result <- list(
    statistic = T_plus,
    p.value = p_value,
    alternative = alternative,
    method = "Wilcoxon Signed-Rank Test",
    data.name = deparse(substitute(x))
  )
  class(result) <- "htest"
  return(result)
}

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1

R_wilcox <- wilcox.test(X, alternative="greater", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="greater", mu = theta0)

stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  abs(mi_wilcox$p.value - R_wilcox$p.value) < 1e-6,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

print(mi_wilcox)

# Pregunta 17
# Función distribución exacta
graficar_distribuciones <- function(n) {
  max_T_plus <- n * (n + 1) / 2
  posibles_T_plus <- 0:max_T_plus
  
  prob_exacta <- dTmas(posibles_T_plus, n)
  
  media <- n * (n + 1) / 4
  varianza <- n * (n + 1) * (2 * n + 1) / 24
  sd <- sqrt(varianza)
  
  prob_asintotica <- dnorm(posibles_T_plus, mean = media, sd = sd)
  prob_asintotica <- prob_asintotica / sum(prob_asintotica) * sum(prob_exacta)
  
  barplot(
    prob_exacta,
    names.arg = posibles_T_plus,
    col = "lightblue",
    ylim = c(0, max(prob_exacta, prob_asintotica)),
    main = paste("Distribución de T+ para n =", n),
    xlab = "T+",
    ylab = "Probabilidad"
  )
  lines(posibles_T_plus, prob_asintotica, col = "red", lwd = 2)
  legend("topright", legend = c("Exacta", "Asintótica"), fill = c("lightblue", "red"), bty = "n")
}

n1 <- 4
n2 <- 10
n3 <- 20

par(mfrow = c(1, 3))
graficar_distribuciones(n1)
graficar_distribuciones(n2)
graficar_distribuciones(n3)
par(mfrow = c(1, 1))

# Pregunta 18
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
alpha <- 0.05
X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))

# H0: theta = 0 vs H1: theta > 0
wilcox_test <- wilcox.test(X, alternative = "greater", mu = 0, exact = FALSE)
cat("Resultado del test de Wilcoxon:\n")
print(wilcox_test)

# Bootstrap
m <- 10000

set.seed(1984)
rechazos <- replicate(m, {
  X_boot <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  wilcox_test <- wilcox.test(X_boot, alternative = "greater", mu = 0, exact = FALSE)
  wilcox_test$p.value <= alpha
})

# Potencia estimada
potencia_estimada <- mean(rechazos)
cat("\nLa potencia estimada del test es:", potencia_estimada, "\n")

