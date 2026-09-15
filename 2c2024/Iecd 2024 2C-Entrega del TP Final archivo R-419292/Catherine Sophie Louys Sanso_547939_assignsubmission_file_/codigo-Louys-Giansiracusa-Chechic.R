#Pregunta 1:

vec1 <- c(T,F)
vec2 <- c(T,F,1)
vec3 <- c(T,F,1,"1")

# vemos las clases:

class(vec1) #logical
class(vec2) #numeric
class(vec3) #character

#Pregunta 2: 
class(density) #function
class(density(1:500)) #density

#Pregunta 3

length(methods("print"))
length(methods(class="density"))-1 #pues es sin contar plot

#Pregunta 4: 
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

class(test_t)

#Pregunta 12:
particiones <- function(t,n){
  if (n==0){
    u <- as.integer(t==0)
    return(u)
  } else if (t<0 || t > (n+1)*n /2){
    return(0)
  } else{
    return(particiones(t,n-1)+particiones(t-n,n-1))
  }
}

stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)



#Pregunta 13:
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    numerador <- particiones(x[i],n)
    ret[i] <- numerador/(2^n)
  }
  return(ret)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    proba <- 0
    for(j in 0:x[i]){
      proba <- proba + dTmas(j,n)
    }
    ret[i] <- proba
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



#Pregunta 14:
t_mas_vec <- function(muestra, theta1){
  n <- length(muestra)
  xs <- muestra
  dif <- xs - theta1
  absoluto <- abs(dif)
  signos <- sign(dif)
  ordenados <- sort(absoluto)
  rango <- c()
  for (i in 1:n){
    rango <- c(rango, which(ordenados == absoluto[i])[1]) # devolvemos el primero porque la probabilidad de que haya empate es nula
  }
  t_mas <- sum(rango[signos == 1]) 
  t_mas
}

mi.wilcox.test <- function(x, alternative, mu){
  statistic<- t_mas_vec(x,mu)
  if (alternative == "greater"){
    p.value <- 1-pTmas(statistic-1,n) 
    # hacemos P(T>Tobs) estricta, luego como son numeros enteros, P(T>Tobs) = P(T>=Tobs-1) (distr discreta...)
  } 
  if (alternative == "less"){
    p.value <- pTmas(statistic,n)
  } 
  if (alternative == "two.sided"){
    p.value <- 2*min(pTmas(statistic,n), 1-pTmas(statistic,n)) 
  }
  rv <- list(statistic = statistic, p.value = p.value, alternative = alternative )
  structure(rv, class = "htest")
}


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


#Pregunta 17
enes <- c(4,10,20)
for (n in enes) {
  soporte <- 0:(n * (n + 1) / 2)
  probas <- dTmas(soporte, n)
  
  #distribucion asintotica segun lo calculado en el ej 16
  asint <- dnorm(soporte, mean = n*(n+1)/4, sd = sqrt(n*(n+1)*(2*n+1)/24))
  
  plot.new()
  plot.window(xlim = c(min(soporte), max(soporte)), ylim = c(0, max(probas, asint)))
  
  for (i in 1:length(soporte)) {
    rect(soporte[i] - 0.4, 0, soporte[i] + 0.4, probas[i], col = rgb(0.1, 0.6, 1, 0.6), border = "white")
  }

  lines(soporte, asint, col = "red", lwd = 2)
  title(xlab = "t", ylab = "Probabilidad")
  axis(1)
  axis(2) 
  legend("topright", legend = c("Distribución exacta", "Distribución asintótica"),
         fill = c(rgb(0.1, 0.6, 1, 0.6), "red"), border = "white", bty = "n", cex = 0.8)
}

#Pregunta 18:
set.seed(1984)
n <- 12
theta0 <- 0
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
alpha <- 0.05

# Estimamos k*
acum <- 0
xactual <- n*(n+1)/2
while (acum < alpha){
  acum <- acum + dTmas(xactual,n)
  xactual <- xactual - 1
}

kestrella <- xactual + 1 

contador <- 0
m<-10000
for (i in 1:m){
  muestra <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq)) 
  t_mas_muestra <- t_mas_vec(muestra, theta0)
  contador <- contador + as.integer(t_mas_muestra > kestrella)
}

print(m^-1 *contador)



#Ejercicio 19:
#Calculo analítico de la potencia de phi_t : 

cuantil <- qnorm(1-0.05 )
pot <- 1 - pnorm(cuantil+ sqrt(n) *(0-1)/sigma_sq)
print(pot)

cuantil_z <- qnorm(1-0.05/2)
m<-10000
contador <- 0
for (i in 1:m){
  muestra <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  z<- sqrt(n) * (mean(muestra) - 0.5) / sqrt(0.25)
  
  if (z <= -cuantil_z | z >= cuantil_z) {
    contador <- contador + 1
  }
}

pot_signo <-contador/m
print(pot_signo)


diferencia <- pot - pot_signo
print(diferencia)
