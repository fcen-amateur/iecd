
# IECD 2C2024 - Trabajo Práctico Final - Archivo de R

#Integrantes:
# Pilar Arévalo, LU 140/22, mail: arevalo.pilu@gmail.com
# Lara Calderón, LU 3334/22, mail: lc.calderon.lara@gmail.com
# Bautista Gilardón, LU 742/21, mail: bautistagilardon@gmail.com


#### EJERCICIO 1 ####

class(c(T, F))  # ----> "logical"
class(c(T, F, 1)) # ----> "numeric"
class(c(T, F, 1, "1")) # ----> "character"

#### EJERCICIO 2 ####

class(density) # ----> "function"
class(density(1:500)) # ----> "density"
?density

#### EJERCICIO 3 ####

length(methods(print)) # ----> 191, 238, 246
methods(class = "density")
length(setdiff(methods(class = "density"), "plot.density")) # ----> 5

#### EJERCICIO 4 ####

# generamos la muestra
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))

# ejecutamos el test
mu_0 <- 0
alfa <- 0.05

test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu_0,
  conf.level = 1 - alfa
)

unclass(test_t)
help(unclass)
class(unclass(test_t))

#### EJERCICIO 12 ####

particiones <- function(t,n) {
  
  if (n == 0) {
    return (ifelse(t == 0, 1, 0))
  }
  
  if (t < 0 || t > n * (n + 1) / 2) {
    return(0)
  }
  
  return(particiones(t, n - 1) + particiones(t - n, n - 1))
  
}

#### EJERCICIO 13 ####

# función de probabilidad puntual
dTmas <- function(x, n) {
  
  ret <- vector(mode = "numeric", length = length(x))
  
  for (i in seq_along(x)) {
    ret[i] <- particiones(x[i], n) / 2^n
  }
  
  return(ret)
  
}

# función de distribución
pTmas <- function(x, n) { 
  
  ret <- vector(mode = "numeric", length = length(x))
  
  for (i in seq_along(x)) {
    ret[i] <- sum(dTmas(0 : x[i], n))
  }
  
  return(ret)
  
}

#### EJERCICIO 14 ####

mi.wilcox.test <- function(x, alternative = 'two.sided', mu = 0) {
  
  diferencias <- x -mu
  rangos <- rank(abs(diferencias))
  Tmas <- sum(rangos[diferencias > 0])
  
  n <- length(x)
  
  if(alternative == 'two.sided') {
    p_valor <- 2 * min(pTmas(Tmas, n), 1 - pTmas(Tmas - 1, n)) 
  }
  if(alternative == 'greater') {
    p_valor <- 1 - pTmas(Tmas - 1, n)
  }
  if(alternative == 'less') {
    p_valor <- pTmas(Tmas, n)
  }
  
  structure(
    list(
      alternative = alternative,
      statistic = Tmas,
      p.value = p_valor
    ),
    class = 'htest'
  )

}

#### EJERCICIO 17 ####

par(mfrow = c(1,3))

esperanza_Tmas <- function(n) {
  return((n * (n + 1)) / 4)
}

varianza_Tmas <- function(n) {
  return((n * (n + 1) * (2 * n + 1)) / 24)
}

enes <- c(4,10,20)

for (n in enes) {
  valores_t <- 0 : (n * (n + 1) / 2)
  
  dist_exacta <- dTmas(valores_t, n)
  
  esperanza <- esperanza_Tmas(n)
  desvio <- sqrt(varianza_Tmas(n))
  
  normal <- dnorm(valores_t, mean = esperanza, sd = desvio)
  
  barplot(
    dist_exacta, space = 0, names.arg = valores_t, col = '#dfbfdf', border = '#9E6D9E',
    main = paste("Distribución de T+ para n =", n),
    ylab = "Probabilidad", xlab = "T+"
  )
  lines(valores_t, normal, col = '#1B340E', lwd = 2)
  legend(
    "topright", legend = c("Distribución exacta", "Aproximación normal"),
    fill = c('#dfbfdf', NA), border = c('#9E6D9E', NA),
    col = c(NA, '#1B340E'), lty = c(0, 1), lwd = c(NA, 2)
  )
  
}

# Copiamos el codigo que utilizamos para exportar las imagenes

#for (n in enes) {
#  valores_t <- 0 : (n * (n + 1) / 2)
#  
#  dist_exacta <- dTmas(valores_t, n)
#  
#  esperanza <- esperanza_Tmas(n)
#  desvio <- sqrt(varianza_Tmas(n))
#  
#  normal <- dnorm(valores_t, mean = esperanza, sd = desvio)
#  
#  png(paste0("grafico_n_", n, ".png"))
#  
#  xlim_range <- c(0, max(valores_t) * 1.2) 
#  ylim_range <- c(0, max(c(dist_exacta, normal)) * 1.1)  
#
#  barplot(
#    dist_exacta, space = 0, names.arg = valores_t, col = '#dfbfdf', border = '#9E6D9E',
#    main = paste("Distribución de T+ para n =", n),
#    ylab = "Probabilidad", xlab = "T+",
#    xlim = xlim_range, ylim = ylim_range
#  )
#  
#  lines(valores_t, normal, col = '#1B340E', lwd = 2)
#  
#  legend(
#    "topright", legend = c("Distribución exacta", "Aproximación normal"),
#    fill = c('#dfbfdf', NA), border = c('#9E6D9E', NA),
#    col = c(NA, '#1B340E'), lty = c(0, 1), lwd = c(NA, 2)
#  )
#  
#  dev.off()
#}

#### EJERCICIO 18 ####

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1

# Computamos el test de wilcoxon
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
test_w=mi.wilcox.test(X, alternative = 'greater', mu = 0)
test_w$p.value

# Realizamos bootstrap para estimar la potencia del test de Wilcoxon
theta1 = 1
sigma_sq <- 1
m = 10000
n=12

resultados_pv <-c()
for(i in 1:m){
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  #test_w=mi.wilcox.test(X, alternative = 'greater', mu = 0)
  test_w=wilcox.test(X, alternative = 'greater', mu = 0)
  #nuestro test tarda mucho para m muy grande, asi que realizamos bootstrap con el implementado en R
  resultados_pv <- c(resultados_pv,test_w$p.value) 
}
resultados_pv
mean(resultados_pv < 0.05)

#### EJERCICIO 19 ####

#creamos un test normal
n.test<-function(x,mu=0,sigma=1,nivel=0.05,alternative="greater"){
  n=length(x)
  T_obs=(sqrt(n)*(mean(x)-mu)/sigma)
  
  if(alternative=="greater"){
    p_valor=pnorm(T_obs,lower.tail = FALSE)
  }else if(alternative=="less"){
    p_valor=pnorm(T_obs,lower.tail = TRUE)
  }else if(alternative=="two.sided"){
    p_valor=2*pnorm(abs(T_obs),lower.tail = FALSE)
  }else{
    print("alternative invalid")
  }
  structure(
    list(
      alternative = alternative,
      statistic = T_obs,
      p.value = p_valor
    ),
    class = 'htest'
  )
}
#computamos el test normal
n=12
X=rnorm(n,mean=1,sd=1)
n.test(X,mu=0,alternative="greater")

#Realizamos los cálculos necesarios para obtener la potencia del test n de nivel 0.05
n_095<-qnorm(0.05,lower.tail=FALSE)
t<-n_095-sqrt(12)
prob<-pnorm(t,lower.tail = FALSE)
prob

#Creamos el test del signo
test_signo<-function(x,mu=0,alternative="greater"){
  n=length(x)
  T_obs=sum(x>0)
  if(alternative=="greater"){
    pvalor=1-pbinom(T_obs-1,n,0.5)
  }else if(alternative=="less"){
    pvalor=pbinom(T_obs,n,0.5)
  }else if(alternative=="two.sided"){
    pvalor=2*(1-pbinom(T_obs-1,n,0.5))
  }else{
    print("invalid altenative")
  }
  structure(
    list(
      alternative = alternative,
      statistic = T_obs,
      p.value = pvalor
    ),
    class = 'htest'
  )
}  
#computo test del signo
X=rnorm(12,mean=1)
test_signo(X, mu = 0, alternative = "greater")

#Realizamos bootstrap para estimar la potencia del test del signo
theta1 = 1
sigma_sq <- 1
m = 10000
n=12

resultados_pv_s <-c()
for(i in 1:m){
  X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  sd(X)
  mean(X)
  #test_signo <- SignTest(X, mu = 0, alternative = "greater") 
  test_signoo <- test_signo(X, mu = 0, alternative = "greater")
  
  resultados_pv_s <- c(resultados_pv_s,test_signoo$p.value) 
}
resultados_pv_s
mean(resultados_pv_s < 0.05)

# la potencia estimada de w dio 0.927
#la anallitica de n dio 0,9655
#la estimada de s dio 0.7082
