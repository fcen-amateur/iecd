# Punto 1 --------------------------------------------------------------------------
# Que clase tiene los siguientes vectores? Que esta sucediendo?
v_1 <- c(T, F)
v_2 <- c(T, F, 1)
v_3 <- c(T, F, 1, "1")

class(v_1) # rta: logical
class(v_2) # rta: numeric
class(v_3) # rta: character

# parece ser que se queda con la clase mas flexible que pueda incluir a las otras
# los logical se pueden considerar numericos si se convierte a 1s y 0s
# los logical y numeric se los puede considerar character al agregarle las comillas

# Punto 2 --------------------------------------------------------------------------
# Que clase tiene density? Y density(1:500)? Donde esta la difernecia?
class(density) # rta: function
class(density(1:500)) #rta: density

help(density)
# Al pedir la class de density solo, no esta devolviendo nada entonces queda la funcion,
# pero al pasarle unos datos a denstity (en este caso el vector de numeros correlativos 1:500 )
# pasa a devolver algo de tipo density

# Punto 3 --------------------------------------------------------------------------
# A cuantas clases despacha el generico print? con cuantos metodos cuenta density?
methods("print") #rta: sabe despachar a 191
methods(class="density") # rta: cuenta con 5, ademas de plot

# Punto 4 --------------------------------------------------------------------------
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
#Rta: Devuelve lista, lo que ocurre es que unclass devuelve el tipo de datos
#mas basico del cual htest hereda atributos

# Punto 12 --------------------------------------------------------------------------
particiones <- function(t, n){
  if(n==0){
      return (as.numeric(t==n))
    } else if (t<0 || t>(n*(n+1)/2)) {
      return(0)
  } else {
    return ((particiones(t,n-1) + particiones(t-n,n-1)))
  }
}


stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

# Punto 13 --------------------------------------------------------------------------
dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i]<- particiones(x[i],n)/(2**n)
  }
  return(ret)
}

pTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  vec_puntuales <- dTmas(0:max(x), n)
  for (i in seq_along(x)) {
    ret[i] <- sum(vec_puntuales[0:x[i]+1])
  }
  return(ret)
}

n<-15
t<-34

stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1-pTmas(n * (n + 1) / 2 - (t + 1), n)
)

# Punto 14 --------------------------------------------------------------------------
mi.wilcox.test <- function(x, mu = 0, alternative = "two.sided") {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(mu))
  alt <- match.arg(alternative, c("two.sided", "greater", "less"))
  n <- length(x)
  rv <- list(
    null.value = c(median = mu),
    alternative = alt
  )
  estadistico<-0
  vec_rangos<- rank(abs(x-mu))
  for (i in c(1:length(x))) {
    estadistico<- estadistico + as.numeric(x[i]-mu>0)*vec_rangos[i] 
  }
  rv$statistic <- setNames(estadistico, "statistic")
  valores_T <- 0:(n * (n + 1) / 2)
  probabilidades <- dTmas(valores_T, n)
  p_value <- 0
  if (alternative == "two.sided") {
    p_value <- 1-pTmas(estadistico, n) + pTmas((sum(valores_T) - estadistico), n)
  } else if (alternative == "greater") {
    p_value<- 1-pTmas(estadistico,n)
    #P(T>=Estadistico)
  } else if (alternative == "less") {
    p_value<- pTmas(estadistico,n)
    #P(T<=Estadistico)
  }
  rv$p_value <- p_value
  structure(rv, class = "htest")
}

set.seed(1234)
n <- 20
X <- rnorm(n)
theta0 <- -1
R_wilcox <- wilcox.test(X, alternative="two.sided", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="two.sided", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

# Punto 17 --------------------------------------------------------------------------
set.seed(1984)

graficar_probabilidades <- function(n) {
  t_vals <- 0:(n * (n + 1) / 2)
  mu <- n * (n + 1) / 4
  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  
  prob_exactas <- dTmas(t_vals, n)
  
  dens_asintotica <- dnorm(t_vals,mu,sigma)
  
  
  plot(t_vals, dens_asintotica, type = "l",col = "purple", lwd = 2,
       main = paste("Distribución de T+ para n =", n),
       xlab = "Valor de T+", ylab = "Probabilidad")
  
  lines(t_vals, prob_exactas, type = "h", col = "plum", lwd = 2)
  
  legend("topright", legend = c("Probabilidad Exacta", "Densidad Asintótica"),
         col = c("plum", "purple"), lwd = 2)
}


graficar_probabilidades(4)
graficar_probabilidades(10)
graficar_probabilidades(20)

# Punto 18 --------------------------------------------------------------------------
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
m<-10000
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
test <- wilcox.test(X)

vec_probabilidades<-pTmas(c(0:(n*(n+1)/2)),12)
k <- 1
for (i in c(0:(length(vec_probabilidades)-1))) {
  if(vec_probabilidades[i+1]<=0.95){
    k<-i
  }
}

#Ahora genero muestras bajo H1 para estimar la potencia
vec_Tmas_H1 <- vector(mode = "numeric", length = m)
for (i in c(1:m)) {
  Y <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  if(i<=100){
    stopifnot(mi.wilcox.test(Y)$statistic == wilcox.test(Y)$statistic)
  }
  vec_Tmas_H1[i]<-(wilcox.test(Y))$statistic
}

est_bootstrap<- (1/m)*sum(vec_Tmas_H1>k)
# Punto 19 --------------------------------------------------------------------------
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
m<-10000

k_tn<-qnorm(0.95)

potencia_analitica<- 1-pnorm(k_tn - (theta1)*sqrt(n)/sigma_sq, mean = 0, sd = 1)

test_signo <- function(X, theta0=0){
  signos<-sign(X-theta0)
  return (sum(signos==1)/length(X))
}

est_test_signo<-test_signo(X)

#Para calcular k*, necesitamos que el cuantil 0.95 <= k*
vec_probas_bin <- vector(mode = "numeric", length = n+1)
for (i in c(0:n)) {
  vec_probas_bin[i+1]<-pbinom(i,size=n, prob=0.5)
}

k_signo <- 1
for (i in c(0:(length(vec_probas_bin)-1))) {
  if(vec_probas_bin[i+1]<=0.95){
    k_signo<-i/n
  }
}


#Ahora genero muestras bajo H1 para estimar la potencia
vec_Tmas_H1_s <- vector(mode = "numeric", length = m)
for (i in c(1:m)) {
  Y <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  vec_Tmas_H1_s[i]<-test_signo(Y)
}

est_bootstrap_signo<- (1/m)*sum(vec_Tmas_H1_s>k_signo)

#test_t <- t.test(X)

#t <- qt(0.95, n-1)

#delta = theta1*sqrt(n)/sd(X)

#potencia_analitica <- 1-pt(t, df=n-1, ncp = delta)
