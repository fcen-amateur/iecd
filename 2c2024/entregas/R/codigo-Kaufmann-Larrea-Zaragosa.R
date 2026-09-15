
objs <- list(mtcars, 1:5, sum, lm(mpg ~ cyl, mtcars), t.test)
for (obj in objs) { print(class(obj)) }

#Pregunta 1 (3 pts.): ¿Qué clase tienen los siguientes vectores: c(T, F), c(T, F, 1) y
#c(T, F, 1, "1")? ¿Qué cree que está sucediendo?

A <- c(T, F) 
B <- c(T, F, 1)
C <- c(T, F, 1, "1")

print(class(A)) #logical  La lista tiene 2 elementos logicos, entonces la lista es logical
print(class(B)) #numeric T y F son 1 y 0 entonces lo transforma a numeric
print(class(C)) #character, todos los elementos pueden transformarse a char

#############Pregunta 2 (3 pts.)#################
#¿Qué clase tiene density? ¿Y density(1:500)? ¿Dónde está la diferencia?

class(density) #Funcion!
class(density(1:500)) #Density!

#density es una función que al ser evaluada
#como por ejemplo en 1:500 da una estructura del
#tipo density

#########################################
#Para conocer los métodos a los que sabe despachar cierto genérico gen, basta con llamar a
#methods("gen"). Si se quiere conocer todos los métodos asociados con la clase "cls", se invoca
#methods(class="cls"):

methods("plot")[1:9]
methods(class="density")

#sloop::s3_methods_generic(gen) y s3_methods_class(cls)
#############################################################

#Pregunta 3 (3 pts.): ¿A cuántas clases sabe despachar el genérico print? ¿Con cuántos
#métodos cuenta density, además de plot?
methods("print") #236 metodos print.xtabs => tiene 236 clases
methods(class="density") #tiene 6 metodos


##########################################

#Pregunta 4 (3pts.): Lea help(unclass) y conteste: ¿Qué devuelve
#class(unclass(test_t))?¿Por qué?


mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))

mu_0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu_0,
  conf.level = 1 - alfa
)
test_t # p valor muy chico, rechazo u = 0 => u != 0

help(unclass)

class(t.test)
class(test_t)
class(unclass(test_t))

#test es una lista de cosas, una vez que le sacas la clase t.test se transforma en una lista

#Pregunta 10 (5pts.): Reproduzca la tabla de Observación 6 para 𝑛 = 5

# Crear los datos para cada columna
t <- 0:15
S5t <- c(
  "∅", 
  "{5}", 
  "{4}", 
  "{3}", 
  "{2,4}", 
  "{1,3}", 
  "{5,1,2}", 
  "{3,4,1}", 
  "{5,2,3,4}", 
  "{5,1,2,4,3}", 
  "{1,2,3,4,5}", 
  "{2,4,5,1,2,3}", 
  "{3,4,4,5,1,2,4}", 
  "{5,1,3,4}", 
  "{5,2,3,4}", 
  "{5,1,2,3,4}"
)
num_S5t <- c(1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1)
p5t <- c(1/32, 1/32, 1/32, 1/32, 2/32, 2/32, 3/32, 3/32, 3/32, 3/32, 3/32, 2/32, 2/32, 1/32, 1/32, 1/32)

# Crear un dataframe
tabla <- data.frame(
  t = t,
  S5t = S5t,
  num_S5t = num_S5t,
  p5t = p5t
)

###############
#Pregunta 12 (8 pts.): Programe la rescursión 𝑢𝑛(𝑡) en R. Llámela particiones, y dele
#dos argumentos, 𝑡, 𝑛, ambos enteros

particiones <- function(t, n) {
  res = 0
  if(n== 0){
    if(t==0){
      res = 1
    }
    else{
      res = 0
    }
  }
  else{
    if(t<0 || t>n*(n+1)/2){
      res = 0
    }
    else{
      res = particiones(t,n-1) + particiones(t-n,n-1)
    }
  }
}

stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

#Pregunta 13: Usando particiones, implemente dTmas(x, n) y pTmas(x, n) que toman un vector de enteros 
#x y un escalar n, y den, respectivamente, la función de probabilidad puntual y la función de distribución acumulada de 𝑇+ bajo 𝐻0 en cada valor de x.

dTmas <- function(x, n) {
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] = particiones(x[i],n) /(2**n)
  }
  return(ret)
}
pTmas <- function(x, n) { 
  ret <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    for (j in 0:x[i]) {
      ret[i] = ret[i] + particiones(j,n)/(2**n)
    }
  }
  return(ret)
}


#Deben pasar los siguientes casos de test

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

mi.t.test <- function(x, mu = 0, conf.level = 0.95) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  stopifnot(is.scalar(conf.level), conf.level > 0, conf.level < 1)
  
  alfa <- 1 - conf.level
  n <- length(x)
  rv <- list(
    parameter = c(df = n - 1),
    estimate = c(`mean of x` = mean(x)),
    stderr = sd(x) / sqrt(n),
    null.value = c(mean = mu),
    alternative = "two.sided",
    method = "One Sample t-test",
    data.name = deparse(substitute(x))
  )
  
  rv$statistic <- setNames((rv$estimate - mu) / rv$stderr, "t")
  rv$conf.int <- rv$estimate + qt(c(alfa / 2, 1 - alfa / 2), df = rv$parameter) * rv$stderr
  attr(rv$conf.int, "conf.level") <- conf.level
  pval_izq <- pt(rv$statistic, df = rv$parameter)
  rv$p.value <- 2 * min(pval_izq, 1 - pval_izq)
  
  structure(rv, class = "htest")
}
#####14#########

####
#quiero ver para el test twosided el k** para ver si es > o >= tobs

####
mi.wilcox.test <- function(x, alternative = "two.sided", mu = 0) {
  n <- length(x)
  x <- x - mu #Observación 2: Para testear 𝐻0: 𝜃 ≤ 𝜃0, basta con definir 𝑌𝑖 = 𝑋𝑖 − 𝜃0 y realizar los test
  rangos <- rank(abs(x))
  Tmasobservado <- sum(rangos[x > 0])
  
  if (alternative == "two.sided") {
    #p.value <- 2 * (1 - pTmas(Tmas - 1, n))
    T <-2*(Tmasobservado-n*(n+1)/4)
    p.value <- 2* (1-pTmas(abs(T)/2 +n*(n+1)/4 -1 ,n)) 
    #el -1 podria no estar
    
  } 
  else if (alternative == "greater") {
    p.value <- 1 - pTmas(Tmasobservado - 1, n)
  } 
  else if (alternative == "less") {
    p.value <- pTmas(Tmasobservado, n)
  }
  rv <- list(
    statistic = c(Tmasobservado),
    p.value = p.value,
    alternative = alternative,
    method = "Wilcoxon Test",
    data.name = deparse(substitute(x)) #Lo mismo que en t.test del pdf
  )
  structure(rv, class = "htest")
}

#Test 1
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

# Test 2
set.seed(1234)
n <- 10
X <- rnorm(n)
theta0 <- 1
R_wilcox <- wilcox.test(X, alternative="less", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="less", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest"
)

# Test 3
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
2*(pt(1.422,988,lower.tail = FALSE))


########17 #########


esperanza <- function(n){
  return ((n*(n+1))/4)
}

sigma_a <- function(n){
  return (sqrt((n*(n+1)*(2*n+1))/24))
}


n_values <- c(4,10,20) 

par(mfrow = c(1, length(n_values))) 

for (n in n_values) {
  t_values <- seq(0, n*(n+1)/2)
  probabilities <- numeric(length(t_values))
  
  for (i in 1:length(t_values)) {
    t <- t_values[i]
    probabilities[i] <- dTmas(t, n) 
  }
  
  posiciones <- barplot(probabilities, names.arg = t_values, col = "skyblue",
                        main = paste("Distribución exacta para n =", n),
                        xlab = "T+", ylab = "Probabilidad", ylim = c(0, max(probabilities)))
  
  
  #lines(t_values, normal_density, col = "red", lwd = 2)
  x <- seq(0, n*(n+1)/2, length.out = 1000)
  densidad <- dnorm(x, mean = esperanza(n), sd = sigma_a(n))
  
  # Escalar la densidad para ajustarla a las alturas del gráfico
  #densidad_scaled <- densidad* (posiciones[2] - posiciones[1])
  
  # Ajustar la escala del eje X de la curva normal al eje X del barplot
  x_adjusted <- seq(posiciones[1], posiciones[length(posiciones)], length.out = 1000)
  
  # Superponer la curva de densidad
  lines(x_adjusted, densidad, col = "red", lwd = 2)
  
}

##########18#########



#Ejercicio 18

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

alpha <- 0.05
wilcox_test <- wilcox.test(X, mu = 0, alternative = "greater", exact = FALSE)
wilcox_test
rechazo_H0 <- wilcox_test$p.value < alpha
#rechazo_H0 = True

# 3. Estimar la potencia por bootstrap
m <- 10000  # Número de simulaciones bootstrap
rechazos <- 0

for (i in 1:m) {
  # Generar una muestra bootstrap bajo θ1 = 1
  muestras_bootstrap <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
  
  # Test de Wilcoxon para la muestra bootstrap
  test_bootstrap <- wilcox.test(muestras_bootstrap, mu = 0, alternative = "greater", exact = FALSE)
  
  if (test_bootstrap$p.value < alpha) {
    rechazos <- rechazos + 1
  }
}

# 4. Calcular la potencia estimada
potencia_estimada <- rechazos / m     # 1/m sum (I{T+ > k*})

cat("Potencia estimada por bootstrap: ", round(potencia_estimada, 4), "\n")


##########19 ###############


set.seed(1984)
mu <- 1
sigma_sq <- 1
n <- 12
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
# Ejecuto el test_n
alpha <- 0.05
mu_0 <- 0
z_alpha = qnorm(alpha,lower.tail = FALSE)

Tobs = sqrt(n)*(mean(X)-mu_0)/sigma_sq 
rechazo_H0 = Tobs > z_alpha

potencia = 1 - pnorm(z_alpha - mu * sqrt(n) * sigma_sq)
#0.96556


############t.test#############

test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu_0,
  conf.level = 1 - alpha
)

#Potencia analitica

t_alpha <- qt(1 - alpha, n-1)

# Parámetro de no centralidad
delta <- mu * sqrt(n) / sigma_sq

# Potencia del test
potencia <- 1 - pt(t_alpha, n-1, ncp = delta)
cat("La potencia del test t es:", potencia, "\n")
#0.9446

########Test de signo############
m = 1000
# Valor crítico para el test del signo
s_crit <- qbinom(1 - alpha, size = n, prob = 0.5)  # Valor crítico exacto para Binomial
s_crit = 10
sum(dbinom(10:12,n,1/2))
# Simulaciones bootstrap
rechazos <- 0
for (i in 1:m) {
  # Generar muestra bajo H1
  muestra <- rnorm(n, mean = mu, sd = sigma_sq)
  
  # Calcular el número de valores positivos
  S <- sum(muestra > 0)
  
  # Verificar si rechazamos H0
  if (S >= s_crit) {
    rechazos <- rechazos + 1
  }
}

# Estimar potencia
potencia_estimada <- rechazos / m
cat("Potencia estimada del test del signo:", round(potencia_estimada, 4), "\n")



#Potencia 0.965 < 0.944 < 0.927 < 0.712





