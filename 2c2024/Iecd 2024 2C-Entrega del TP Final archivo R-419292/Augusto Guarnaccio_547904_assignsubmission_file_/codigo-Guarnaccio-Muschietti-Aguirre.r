# Título: Trabajo práctico IEyCD
# Autores: Victoria Aguirre, Augusto Guarnaccio, Bruno Muschietti

# En el script van a estar todo el código que haya sido necesario utilizar para
# responder las preguntas de programación del trabajo. Las respuestas a las preguntas
# se las puede encontrar en el informe. Esto con motivo de no sobrecargar de cosas
# el archivo.
# A su vez, cada vez que en el tp se reinicie la seed nosotros así lo haremos.

#-------------------------------------------------------------------------------#
# Importamos librerías 

install.packages("sloop")
library(sloop)

#-------------------------------------------------------------------------------#
# Pregunta 1

class(c(T,F))
class(c(T,F,1))
class(c(T,F,1,"1"))

#-------------------------------------------------------------------------------#
# Pregunta 2

class(density)
class(density(1:500))

#-------------------------------------------------------------------------------#
# Pregunta 3

# Primero nos fijamos la cantidad de clases a las que puede despachar "print"
sloop::s3_methods_generic("print")


# Ahora la cantidad de métodos de "density"

sloop::s3_methods_class("density") # métodos s3 para la clase density
methods(class="density") # todos los métodos
sloop::s4_methods_class("density") # métodos s4 para la clase density

#-------------------------------------------------------------------------------#
# Pregunta 4

# Pegamos el código del test

## Genero la muestra
mu = 1
sigma_sq = 1
n = 30
X = rnorm(n, mean = mu, sd = sqrt(sigma_sq)) # generamos una muestra de n observaciones

## Ejecuto el test
mu0 = 0
alfa = 0.05
test_t = t.test(
  X,
  alternative = "two.sided", # Prueba de dos colas
  mu = mu0,
  conf.level = 1 - alfa
)

class(unclass(test_t))

#-------------------------------------------------------------------------------#
# Pregunta 12

# Implementamos la función particiones

particiones = function(t, n) {
  
  #casos base
  if(n==0){
    if(t==0){
      return (1)
    }else{
      return (0)
    }
  }
  
  if(t<0 || t > n*(n+1)/2){
    return(0)
  }
  
  #recursiones
  u1 = particiones(t,n-1)
  u2 = particiones(t-n, n-1) 
  
  return ((u1+u2))
}

# Corremos los test provistos

stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

#-------------------------------------------------------------------------------#
# Pregunta 13

# Implementamos la función de probabilidad puntual
dTmas = function(x, n) {
  ret = vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] = particiones(x[i], n)/2**n
  }
  return(ret)
}

# Implementamos la función de probabilidad acumulada
pTmas = function(x, n) {
  ret = vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    ret[i] = sum(dTmas(0:x[i], n)) # Como el vector puede no estar en orden para cada x[i]
    # debemos hacer el calculo de la acumulada desde 0 hasta x[i]
  }
  return(ret)
}

# Corremos los tests provistos

n = 15
t = 34
stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n)

)

#-------------------------------------------------------------------------------#
# Pregunta 14

# Realizamos una función para calcular el estadístico T+

estadistico = function(x){
  rangos = rank(abs(x))
  sum = 0
  for(i in seq_along(x)){
    if(x[i]>0){
      sum = sum + rangos[i]
    }
  }
  return (sum)
}

# Implementamos nuestro wilcox test
mi.wilcox.test = function(x, alternative = c("two.sided", "greater", "less"), mu ){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(mu))
  alternative = match.arg(alternative)
  # Nos aseguramos de que lo que nos hayan pasado respete los tipos
  n = length(x)
  # Armamos el objeto lista al que luego le asignaremos la clase correspondiente
  rv = list(
    null.value = c(location = mu),
    alternative = alternative,
    method = "Wilcoxon signed rank exact test",
    data.name = deparse(substitute(x))
  )
  t_mas = estadistico(x-mu)
  # T+ testea bajo H0 simetría respecto del 0, por lo tanto debemos hacer el test para Y = x-mu
  rv$statistic <- setNames(t_mas, "V")
  
  # Calculamos el pvalor según la alternativa especificada
  if(alternative =="two.sided"){
    pvalor = 2*(1- pTmas(t_mas-1, length(x))) 
    # El -1 se debe a que estamos trabajando con v.a.s discretos y ya no da lo mismo cuando tomamos el complemento
  }
  if(alternative =="greater"){
    pvalor = (1- pTmas(t_mas-1, length(x)))
  }
  if(alternative =="less"){
    pvalor = pTmas(t_mas, length(x))
  }
  
  rv$p.value = pvalor
  structure(rv, class = "htest") # Estructuramos la lista como un objeto de clase htest
}

# Corremos los tests

set.seed(1234)
n = 20
X = rnorm(n)
theta0 = -1
R_wilcox <- wilcox.test(X, alternative="greater", mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative="greater", mu = theta0)
stopifnot(
  mi_wilcox$statistic == R_wilcox$statistic,
  mi_wilcox$p.value == R_wilcox$p.value,
  mi_wilcox$alternative == R_wilcox$alternative,
  class(mi_wilcox) == "htest" )

#-------------------------------------------------------------------------------#
# Pregunta 17

n1 = 4
n2 = 10
n3 = 20

# n1 = 4

muestra_n1 = dTmas(0:(n1*(n1+1)/2), n1)

bp1 = barplot(muestra_n1, ylim = c(0, 1),
               main = "T+ vs Distribución asintótica con n = 4")

x_seq = seq(-4, 4, length.out = length(bp1))
y_norm = dnorm((0:(n1*(n1+1)/2)), mean = n1*(n1+1)/4, sd = sqrt(n1*(n1+1)*(2*n1+1)/24))  # Acumulada de la normal estándar)  # Acumulada de la normal estándar


lines(bp1, y_norm, col = "green", lwd = 4)

# n2 = 10

muestra_n2 = dTmas(0:(n2*(n2+1)/2), n2)

bp2 = barplot(muestra_n2, ylim = c(0, 0.045),
               main = "T+ vs Distribución asintótica con n = 10")
y_norm = dnorm((0:(n2*(n2+1)/2)), mean = (n2*(n2+1))/4, sd = sqrt((n2*(n2+1)*(2*n2+1))/24))  # Acumulada de la normal estándar
lines(bp2, y_norm, col = "green", lwd = 4)

# n3 = 20

muestra_n3 = dTmas(0:(n3*(n3+1)/2), n3)

bp3 = barplot(muestra_n3, ylim = c(0, 0.015),
               main = "T+ vs Distribución asintótica con n = 20")

y_norm = dnorm((0:(n3*(n3+1)/2)), mean = (n3*(n3+1))/4, sd = sqrt((n3*(n3+1)*(2*n3+1))/24))  # Acumulada de la normal estándar


lines(bp3, y_norm, col = "green", lwd = 4)

#-------------------------------------------------------------------------------#
# Pregunta 18

set.seed(1984)
n = 12
theta1 = 1
sigma_sq = 1
X = rnorm(n, mean=theta1, sd=sqrt(sigma_sq))


# Hacemos el test de wilcoxon usando el nuestro y el implementado por R para checkear que estamos haciendo bien las cosas
nuestro.test = mi.wilcox.test(X, alternative="greater", mu = 0)
r.test = wilcox.test(X, alternative = "greater", mu = 0, conf.int = TRUE, conf.level = 0.95)
print(nuestro.test)
print(r.test)

# Ahora buscaremos el k, para eso recorreremos el soporte de T+ y nos quedaremos con el k que maximize su acumulada sin pasarse de 0.95


sopw = c(1:78)

# Redefinimos otra pTmas2 para cuando el vector está ordenado pues la otra tiene una complejidad
# temporal elevada y si la precondición del algoritmo asegura un vector ordenado se puede reducir considerablemente

pTmas2 = function(x, n) {
  ret = vector(mode = "numeric", length = length(x))
  puntuales = dTmas(x,n)
  acum = 0
  for (i in seq_along(x)) {
    acum = acum + puntuales[i]
    ret[i] = acum
  }
  return(ret)
}

acumuladas = pTmas2(sopw,12)
kw = 0
for (i in seq_along(sopw)){
  if(acumuladas[i] <= 0.95){
    kw = i
  }
}


# Printeamos el valor
print(kw)

# Hacemos bootstrap para el test de Wilcoxon
m = 10000
muestras = vector(mode = "numeric", length = length(m))
sum = 0

for (i in 1:m){
  #generamos la i-esíma muestra de tamaño n
  Y_i = rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  #calculamos el estadistico para estos datos
  est = estadistico(Y_i)
  muestras[i] = est
  if(est > kw){
    sum = sum+1
  }
}
pot_w = sum/m
print(pot_w)

#-------------------------------------------------------------------------------#
# Pregunta 19

# Como la muestra a usar es la misma no la reiniciaremos
# Definimos el test z y calculamos su potencia

mi.z.test = function(x, mu0, sigma) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(mu0))
  stopifnot(is.numeric(sigma))
  n = length(x)
  rv = list(
    null.value = c(mean = mu0),
    estimate = c('mean of X' = mean(x)),
    varderr = sigma/sqrt(n),
    alternative = "greater",
    method = "One Sample z-test",
    data.name = deparse(substitute(x))
  )
  rv$statistic = setNames((rv$estimate - mu0)/rv$varderr , "Z")
  rv$p.value = pnorm(rv$statistic, mean = 0, sd = 1, lower.tail = FALSE)
  structure(rv, class = "htest")
}

ztest = mi.z.test(X, 0, 1)
print(ztest)

# Calculamos la región de rechazo
kz = qnorm(0.95, mean=0, sd=sqrt(sigma_sq))
print(kz)


# Calculamos la potencia
pot_z = pnorm(kz -(sqrt(12)/sigma_sq),mean=0, sd=sqrt(sigma_sq) ,lower.tail = FALSE)
print(pot_z)

# Definimos el test del signo

mi.sign.test = function(x, med = 0) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(med))
  n = length(x)
  rv = list(
    null.value = c(median = med),
    alternative = "greater",
    method = "One Sample sign-test",
    data.name = deparse(substitute(x))
  )
  rv$statistic = setNames(mean(x>=med), "Tn")
  pval_izq = pbinom(rv$statistic*n, n, 1/2)
  rv$p.value = 1 - pbinom(rv$statistic*n-1, n, 1/2)
  structure(rv, class = "htest")
}


# Hacemos el test
signo = mi.sign.test(X, 0)
print(signo)

# Despejamos el k de la región de rechazo
for (i in 0:n){
  if(pbinom(i,12,1/2) <= 0.95){
    k_n_s = i
  }
}
k_s = k_n_s/12

print(k_s)

m = 10000
muestras = vector(mode = "numeric", length = length(m))
sum = 0


# Hacemos bootstrap para el test del signo
for (i in 1:m){
  #generamos la i-esíma muestra de tamaño n
  Y_i <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
  #calculamos el estadistico para estos datos
  est = mean(Y_i>=0)
  muestras[i] = est
  if(est > k_s){
    sum = sum+1
  }
}

pot_s = sum/m
print(pot_s)

# Distancia entre wilcoxon y test de normal
dist_w_z = pot_z - pot_w
print(dist_w_z)

# Distancia entre wilcoxon y test del signo
dist_w_s = pot_w - pot_s
print(dist_w_s)
# Distancia entre test normal y test del signo
dist_z_s = pot_z - pot_s
print(dist_z_s)

