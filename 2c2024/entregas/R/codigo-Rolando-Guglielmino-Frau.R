# -----------------------------------------------------------------------------
# TRABAJO PRÁCTICO FINAL
#
# TEST DE WILCOXON DE RANGO SIGNADO
#
# Adrian Guglielmino, LU: 39/18
# Kenneth Andrés Frau, LU: 189/22
# Mathias Rolando, LU: 250/22
# -----------------------------------------------------------------------------

library(sloop)

############
# Pregunta 1
############

class(c(T,F)) 
# "logical" pues T y F son los booleanos
class(c(T,F,1)) 
# "numeric" pues al haber un número, los booleanos T y F se codifican 
# respectivamente como los números 1 y 0
class(c(T,F,1,"1")) 
# "character" pues al haber un elemento de tipo character, los primeros tres
# elementos se codifican como tales también. Podría decirse que hay cierta 
# jerarquía en las distintas clases dependiendo qué contiene cada vector.


############
# Pregunta 2
############

class(density) 
# "function"
class(density(1:500)) 
# "density"
# Por si sola, density es clasificada como function en R base, pues es un objeto
# que define un cómputo. Al evaluar esta función con este vector de elementos 
# del  1 al 500, tenemos un nuevo objeto con la clase asociada a la operación
# hecha sobre el mismo, que es density.


############
# Pregunta 3
############

sloop::s3_methods_generic("print")
# El output es una tibble de 240x4, donde cada fila es una clase distinta que 
# despacha "print". Así que podemos decir que hay 240 clases distintas con las
# que "trabaja" esta función.

methods(class = "density")
# El output son los seis métodos que cuenta "density", estos son: 
# coerce, initialize, plot, print, show, slotsFromS3


############
# Pregunta 4
############
help(unclass)
# Parte de la documentación dice que:

# unclass returns (a copy of) its argument with its class attribute removed. (It
# is not allowed for objects which cannot be copied, namely environments and 
# external pointers.)

# Entonces unclass(test_t) remueve los atributos de la clase de test_t y de 
# hecho devuelve como lista los atributos removidos de test_t enumerados
# entonces al hacer class(unclass(test_t)) devuelve una lista, que es una clase 
# en R implementadas sobre vectores. En particular es una pairlist porque tiene
# una "tag" para cada valor.

#############
# Pregunta 12
#############

memo <- list()

# Lo implementamos con memoización porque hay superposicion de problemas,
# es decir hay más llamados recursivos que estados en el algoritmo

particiones <- function(t, n) {

  if (exists(paste(t, n, sep = ","), where = memo)) {
    return(memo[[paste(t, n, sep = ",")]])
  }
  
  if (n == 0) {  # caso base
    result <- ifelse(t == 0, 1, 0)
  } else {
    if (t < 0 || t > n * (n + 1) / 2) { # otro caso
      result <- 0
    } else { # paso recursivo
      result <- particiones(t, n - 1) + particiones(t - n, n - 1)
    }
  }

  memo[[paste(t, n, sep = ",")]] <- result # almacenamos el resultado en la lista memo
  
  return(result)
}


#############
# Pregunta 13
#############

dTmas <- function(x, n) {
  ret <- vector(mode="numeric",length = length(x))
  for (i in seq_along(x)) {
    if(length(x)==1){
      ret <- particiones(x,n)/2^n     # Proba puntual si queremos un valor
    }
    ret[i] <- particiones(x[i], n) / (2^n) # Proba puntual de varios valores
  }
  return(ret)
}


pTmas <- function(x, n) {
  ret <- vector(mode="numeric",length = length(x))
  for (i in seq_along(x)) {
    if(length(x)==1){
      ret <- sum(dTmas(0:x,n))    # Distribución hasta un solo valor
    }
    ret[i] <- sum(dTmas(0:x[i],n))   # Distribución para varios valores
  }
  return(ret)
}


#############
# Pregunta 14
#############

mi.wilcox.test<-function(X,alternative,mu){
  #Calculamos el estadístico
  D = X - mu
  signos = numeric(length(D))       # Cálculo del vector de signos
  for(i in seq_along(D)){
    if(D[i]>0){
      signos[i]<-1
    }
  }
  rangos = rank(abs(D))             # Cálculo del vector de rangos
  statistic = sum(signos*rangos)    # Cálculo de T+
  
  if(alternative=="greater"){
    # p-valor para un test de cola derecha
    p.value = 1 - pTmas(statistic-1,n)
  }else if(alternative=="less"){
    # p-valor para un test de cola izquierda
    p.value = pTmas(statistic,n)
  }else if(alternative=="two.sided"){
    # p-valor para un test de dos colas
    p.value = 2*min(pTmas(statistic,n),1 - pTmas(statistic-1,n))   
  }
  
  # Construimos el objeto de clase "htest"
  result <- list(
    statistic = statistic,
    p.value = p.value,
    alternative = alternative,
    method = "Wilcoxon signed-rank test",
    data.name = deparse(substitute(X))
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



#############
# Pregunta 17
#############

grafico_proba_puntual <- function(n){
  mu <- n*(n+1)/4
  sigma = sqrt(n*(n+1)*(2*n+1)/24)

  valores_exactos <- 0:(n*(n+1)/2)
  prob_exacta <- dTmas(valores_exactos, n)
  
  barplot(
    prob_exacta,
    names.arg = valores_exactos,
    col = "skyblue",
    main = paste("Distribución de probabilidades para n =", n),
    xlab = "t",
    ylab = "Probabilidad",
    space = 0,
    ylim = c(0, max(prob_exacta) * 1.2)
  )
  
  x_continuo <- seq(0, (n*(n+1)/2), length.out = 100)
  densidad_normal <- dnorm(x_continuo, mean = mu, sd = sigma)
  
  lines(x_continuo, densidad_normal, col = "red", lwd = 2)
}
par(mfrow = c(1, 3))


for (n in c(4, 10, 20)) {
  grafico_proba_puntual(n)
}




#############
# Pregunta 18
#############

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
alpha <- 0.05
m <- 10000

bootstrap <- function(data, n, theta1, sigma_sq, mu, alpha, m) {
  valor_critico <- mi.wilcox.test(data, alternative = "greater", mu = mu)$statistic
  estadisticos <- numeric(m)
  for (i in 1:m) {
    # Generamos una muestra de tamaño n de la distribución H:
    bootstrap_mta <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
    
    # Computamos T+(Yi)
    estadisticos[i] <- mi.wilcox.test(bootstrap_mta, alternative = "greater", mu = mu)$statistic
  }
  return(sum(estadisticos > valor_critico)/m)
}

potencia_estimado <- bootstrap(X, n = n, theta1 = theta1, sigma_sq = sigma_sq, mu = 0, alpha = alpha, m = m)
cat("Potencia estimada:", potencia_estimado)


#############
# Pregunta 19
#############

# Test t para muestras con distribución normal para las hipótesis
# H_0 : theta = 0 vs H_1 : theta > 0 

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))


mi.t.test.der <- function(x, mu0 = 0, alfa = 0.05,alternative = "greater") {
  n <- length(x)
  parameter <- n - 1
  estimate <- mean(x)
  stderr <- sd(x) / sqrt(n)
  statistic <- (estimate - mu0) / stderr
  p.value <- 1-pt(statistic, df = parameter)
  result <- list(
    statistic = statistic,
    p.value = p.value,
    parameter = parameter,
    estimate = estimate,
    stderr = stderr,
    null.value = mu0,
    alternative = alternative,
    method = "t-test",
    data.name = deparse(substitute(x))
  )
  class(result) <- "htest"
  return(result)
}

resultado <- mi.t.test.der(X, mu0 = 0, alfa = 0.05, alternative = "greater")
print(resultado)


# Test del signo (Ejercicio 22 - Práctica 5) para las hipótesis
# H_0 : theta = 0 vs H_1 : theta > 0 

bootstrap_signo <- function(data,n,theta1,sigma_sq,alpha,m) {
  valor_critico <- qnorm(1-alpha)
  T_medio <- numeric(m)
  estadisticos <- numeric(m)
  for (i in 1:m) {
    # Generamos una muestra de tamaño n de la distribución:
    bootstrap_mta <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
    
    # Computamos T_medio y el estadístico de prueba Z para cada muestra
    
    T_medio[i] <- sum(bootstrap_mta[bootstrap_mta > 0])/n
    estadisticos[i] <- sqrt(n)*(T_medio[i]-1/2)/sqrt(1/4)
  }
  return(sum(estadisticos > valor_critico)/m)
}

potencia_estimado <- bootstrap_signo(X, n = n, theta1 = theta1, sigma_sq = sigma_sq, alpha = 0.05, m = 10000)
cat("Potencia estimada:", potencia_estimado)


