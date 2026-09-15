
#---------------------------------------------------------------
# Ejercicio 1: ¿Qué clase tienen los siguientes vectores: c(T, F), c(T, F, 1) y c(T, F, 1, "1")? 
# ¿Qué cree que está sucediendo?
#---------------------------------------------------------------

v1 = c(T, F)
class(v1)
#En v1 ambos valores son logicos, por lo tanto la clase sera logica.
v2 = c(T, F, 1)
class(v2)
#En v2 se añade un 1, y los valores T y F pueden ser representados como valores numericos 0 y 1, por lo tanto la clase es numeric.
v3 = c(T, F, 1 , "1")
class(v3)
#En v3 el "1" es un caracter, y T y F pueden ser escritos como caracteres "True" y "FALSE", por lo tanto la clase es character.

#Creemos que lo que aqui esta pasando es que T y F son valores logicos o booleanos
# y el 1 es tomado como un valor numerico, salvo que lo escribamos de esta manera "1"
#ya que es tomado como un caracter.

#---------------------------------------------------------------
# Ejercicio 2: ¿Qué clase tiene density? ¿Y density(1:500)? ¿Dónde está la
# diferencia?
#---------------------------------------------------------------


class(density) #La clase de density es function 
class(density(1:500)) #La clase de la llamada de density es density.

#La diferencia de clases es dada porque density es una funcion que se toma sobre una muestra 
# y lo que devuelve llamada de density sobre un conjunto de parametros valido es un objeto con
#atributos pertinentes al estudio de la densidad estimada, como el minimo, maximo (de la muestra), entre otros. 

#---------------------------------------------------------------
# Ejercicio 3: ¿A cuántas clases sabe despachar el genérico print? ¿Con cuántos
# métodos cuenta density, además de plot?
#---------------------------------------------------------------
  
methods("print")
#Despacha a 301 clases el metodo print.

methods(class = "density")
#Cuenta con los metodos coerce, initialize, print, show,slotsFromS3 ademas de plot

#---------------------------------------------------------------
# Ejercicio 4: Lea help(unclass) y conteste: ¿Qué devuelve
# class(unclass(test_t))?¿Por qué?
#---------------------------------------------------------------
help(unclass)

#Test
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
mu_0 <- 0
alfa <- 0.05
test_t <- t.test(
 X,
 alternative = "two.sided",
 mu = 0,
 conf.level = 1 - alfa
)

class(unclass(test_t))

#Observamos que devuelve la clase lista

#El resultado de la función es una lista con los datos calculados. Luego, para presentarlos de modo más estético, se define una
# clase para definir sobre esta un propio método print. El resultado entonces no es más que una lista con un print nuevo, luego al
# unclassear obtenemos como resultado 'list'.


#-----------------------------------------------------------------------
# Ejercicio 10: Replicar la tabla de observaciones para n=5
#-----------------------------------------------------------------------

computarS_n_t <- function(n, t) {
    # Si t es 0, devolvemos directo el vacío
    if (t==0){
        return(list("vacío"))
    }

    # Generamos tooodas las posibles combinaciones de listas binarias
    combinaciones_binarias <- expand.grid(rep(list(c(0, 1)), n))
    
    resultados <- list()
    
    # Pasamos por cada combinacion para ver si cumple T+
    for (i in 1:nrow(combinaciones_binarias)) {
        sec_binaria <- as.numeric(combinaciones_binarias[i, ])
        indexes <- which(sec_binaria == 1)
        if (sum(indexes) == t) {
        resultados <- append(resultados, list(indexes))
        }
    }
  
  return(resultados)
}


calcularP_n_t <- function (n,t, s_n_t){
    # Calculamos la proba tal cual como está en el pdf

    numerador = length(s_n_t)
    denominador = 2^n
    return (numerador/denominador)
}

n_ej10 = 5

vector_t = list()
vector_probas = list()
vector_cardinales = list()
vector_S = list()

# Creamos las columnas de la tabla
for (t in 0:10){
    s_n_t = computarS_n_t(n_ej10, t)
    cardinal= length(s_n_t)
    proba = calcularP_n_t(n_ej10, t, s_n_t)

    vector_t = append(vector_t, t)
    vector_probas = append(vector_probas, proba)
    vector_cardinales = append(vector_cardinales, cardinal)
    vector_S[[as.character(t)]] = I(s_n_t)
}

# Generamos la tabla
# La separación de cada S_n se puede ver cada vez que la secuencia rompe su tendencia creciente.
tabla_observaciones_10 <- data.frame(
    "t" = I(vector_t), 
    "S(n,t)" = I(vector_S) , 
    "#S(n,t)" = I(vector_cardinales),
    "p(n,t)" = I(vector_probas)
)


#-----------------------------------------------------------------------
# Ejercicio 12: Programar la recursión u_n(t)
#-----------------------------------------------------------------------

particiones <- function(t,n){

    # Casos base
    if (n==0){
        if (t==0){
            return (1)
        } else{
            return(0)
        }
    }
    if (t<0 | t > n*(n+1)/2){
        return (0)
    }

    # Caso recursivo
    return(particiones(t,n-1)+ particiones(t-n,n-1))
}

#-----------------------------------------------------------------------
# Ejercicio 13: implementamos dTmas y pTmas
#-----------------------------------------------------------------------

#Obs: tenemos que calcular p(t) para todo t entre 1 y x.

# Puntual
dTmas <- function(x,n){
    ret <- vector(mode= 'numeric', length = length(x))
    for (i in seq_along(x)){
        # Calculamos la puntual para ese valor de x
        u_t = particiones(x[i],n)
        proba = u_t / (2^n)

        # La añadimos al vector resultados
        ret[i] = proba
    }
    return (ret)
}

# Acumulada
pTmas <- function(x,n){
    ret <- vector(mode= 'numeric', length = length(x))
    for (i in seq_along(x)){
        # Calculamos las puntuales hasta x[i] y luego las sumamos (def acumulada).
        acumulada = sum(dTmas(0:x[i], n))
        ret[i] = acumulada
    }
    return(ret)
}


#-------------------------------------------------------------------------------------------------------------------------------
# Ejercicio 14: : Programe mi.wilcox.test
#-------------------------------------------------------------------------------------------------------------------------------

#Funcion para calcular el estadistico T+ 
Tmas <- function(X, mu){
    X = X-mu  #Trasladamos al origen para facilitar cuentas.
    X_abs = abs(X) #Modulo de observaciones  
    rangos_absolutos = rep(-1,length(X)) #Rango de los modulos 
    indicadora = rep(FALSE,length(X)) # indicadora[i] = 1 sii xi>0.
    for(i in seq(1:length(X))){ 
        if(X[i] > 0){ indicadora[i] = TRUE}
    }
    X_abs = sort(X_abs)
    for(i in 1:length(X)){ #Para cada observacion xi.
        for(j in 1:length(X_abs)){ #Itero sobre los valores absolutos ordenados
            if( abs(X[i]) == abs(X_abs[j])){ #hasta encontrar el rango(|xi|)
                rangos_absolutos[i] = j
            }
        }
    }
    T_observado = 0 #Finalmente,calculamos la serie que es igual a T+.
    for(i in 1:length(X)){
        if(indicadora[i] == TRUE){
            T_observado = T_observado + rangos_absolutos[i]
        }
    }
    return(T_observado)
}

#Funcion de cuantiles para el test
cuantil_mas <- function(k,n){
  x = 1
  acumulada = dTmas(1,n)
  while(acumulada < k){
    x = x +1
   acumulada = acumulada + dTmas(x,n) 
  }
  return (x)
}

#Nuestro Test de Wilcoxon
mi.wilcox.test = function(X,  alternative = "two.sided", mu){    
  
   match.arg(alternative, c("two.sided","less","greater"))
    statistic = Tmas(X,mu)
    n = length(X)
    alpha = 0.05
    alpha_medio = 0.05/2
    if(alternative == "two.sided"){
        k1 = cuantil_mas( alpha_medio,n) 
        k2 = cuantil_mas(1-alpha_medio,n)
       #Si el estadistico esta mas cerca del cuantil inferior o superior,
       #la probabilidad de obtener un valor peor del observado es dos veces
        #P(T+ < cuantil_inferior) y P(T+ > cuantil_superior) respectivamente. 
        if(abs(statistic-k1) < abs(statistic-k2) ){
            p.value = 2*pTmas(statistic,n)
        }
        if(abs(statistic-k1) > abs(statistic-k2)){
            p.value = 2*(1- pTmas(statistic-1,n) )
        }
    }

if(alternative == "greater"){
    k = cuantil_mas(1-alpha,n)
    p.value = 1-pTmas(statistic-1,n)
    }

if(alternative == "less"){
    k = cuantil_mas(alpha,n)
    p.value = pTmas(statistic,n)
}
    X = deparse(substitute(X))
    lista = list(
      data.name = X,
      statistic = c(V = statistic),
    p.value = p.value,
    alternative = alternative,
    method = "Wilcoxon signed rank exact test"
    )
    class(lista) = "htest"
return (lista)
 }


#-----------------------------------------------------------------------
# Ejercicio 17: barplot de distribución exacta y asintótica para T+
#----------------------------------------------------------------------- 

n_1 = 4
n_2 = 10
n_3 = 20

enes = c(n_1,n_2,n_3)

for (n in enes){
    max = n*(n+1)/2
    valores_t = 0:max
    probas = dTmas(valores_t,n)
    file_name <- paste0("grafico_ej17_n=", n, ".png")
    png(file_name, width = 800, height = 600) 
    bars <- barplot(
            probas,                         
            names.arg = valores_t,          
            col = "skyblue",               
            xlab = "t",              
            ylab = "Probabilidad",          
            main = sprintf("Distribución exacta vs. asintótica para T+, con n= %d",n), 
            ylim = c(0, max(probas) + 0.1)
        )
    normal <- dnorm(valores_t, mean =  n*(n+1)/4, sd = sqrt(n*(n+1)*(2*n+1)/24))

    lines(
            bars,                  
            normal,          
            type = "o",                     
            col = "red",                   
            lwd = 2                        
        )
    legend(
            "topright", 
            legend = c("Distribución exacta", "Distribución asintótica"),
            col = c("skyblue", "red"), 
            lwd = c(NA, 2), 
            pch = c(15, NA), 
            bty = "n"
        )
    dev.off()
}

# Podemos ver que coinciden razonablemente a partir de n = 10. Coinciden para todos los n en el centro y en las colas.
# Tal vez, para n = 4, la coincidencia en las colas es cuestionable, pero el comportamiento es similar.

#-----------------------------------------------------------------------
# Ejercicio 18: computar el test de Wilcoxon para datos generados.
#----------------------------------------------------------------------- 

set.seed(1984)
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
mi.wilcox.test(X, "greater", 0)

#Función para estimar la potencia del test
potencia_estimada <- function(m,nivel){
    estadisticos_bt = rep(-1,m) 
    n <- 12
    for(i in 1:m){ #Almaceno los estadisticos de cada remuestra.
        Yi <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
        estadisticos_bt[i] = Tmas(Yi,0)   
    }
    k = cuantil_mas(1-nivel,n)
    potencia = 0 
    for(i in 1:m){ #Cada estadistico con el cual se rechaze la hipotesis nula suma 1.
      if(estadisticos_bt[i] > k){
        potencia = potencia + 1
      }
    }
    potencia = potencia / m
    return (potencia)
}

m = 10000
nivel = 0.05
k = cuantil_mas(1-nivel,n)
1-pTmas(k,n)
# Aqui el cuantil k tal que P(T+>k) <= 0.05 es igual a 60.
1-pTmas(k-1,n)
#Es el cuantil en el cual se maximiza la potencia del test
#(respetando el nivel <= 0.05 ), ya que P(T+>k) es decreciente en k
#y con el cuantil k = 59 el nivel es 0.054, mayor a la cota pedida.
potencia_en_1 = potencia_estimada(m,nivel)
potencia_en_1
#La probabilidad de rechazar la hipotesis nula dado que la distribucion real es simetrica y con mediana
# igual a 1 es de 0.927, lo cual es esperable ya que theta1 pertenece a la region de 
#rechazo del test. 

#-----------------------------------------------------------------------
# Ejercicio 19: test phi_n
#----------------------------------------------------------------------- 

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))

promedio = mean(X)
estadistico = sqrt(12)*promedio/sigma_sq
pvalor = pnorm(estadistico, lower.tail = FALSE)
X_d = deparse(substitute(X))
lista = list(
  data.name = X_d,
  statistic = c(V = estadistico),
  p.value = pvalor,
  alternative = "greater",
  method = "Test para normales"
)
class(lista) = "htest"
print(lista)


# sqrt(n)*(mean(X)/sigma_sq) = sqrt(n)*(mean(X)-1+1/sigma_sq) y luego, asumiendo
# theta = 1, tenemos que P(sqrt(n)*(mean(X)-1+1/sigma_sq) >= z_a) = 
# P(sqrt(n)*(mean(X)-1/sigma_sq) >= z_(1-a) - sqrt(n)/sigma_sq)
# Luego la función de ponencia es 1 - F_Z(z_(1-a) - sqrt(n)/sigma_sq))

potencia = pnorm(qnorm(0.95) - sqrt(n), lower.tail = FALSE)
print(potencia)


testSigno = function(X){
  T <- ifelse(X > 0.5, 1, 0)
  
  estadistico_signo <- sqrt(12)*(mean(T)-0.5)/0.5
  alpha <- qbinom(0.05, size = 12, prob = 0.5, lower.tail=FALSE)
  
  # Si bien el estadístico es otro, podemos calcular directamente los p-valores
  # con la suma de T
  
  suma <- sum(T)
  pvalor_signo <-  1 - pbinom(suma, size = 12, prob = 0.5)
  X_d = deparse(substitute(X))
  lista = list(
    data.name = X_d,
    statistic = c(V = estadistico_signo),
    p.value = pvalor_signo,
    alternative = "two.sided",
    method = "Test del signo para normales"
  )
  class(lista) = "htest"
  return (lista)
  
}
res <- testSigno(X)
print(res)

potencia_estimada_signo= function(m){
  estadisticos_bt = rep(-1,m) 
  set.seed(1984)
  n <- 12
  theta1 <- 1
  sigma_sq <- 1
  for(i in 1:m){ #Almaceno los estadisticos de cada remuestra.
    Yi <- rnorm(n, mean=theta1, sd=sqrt(sigma_sq))
    T <- ifelse(Yi > 0.5, 1, 0)
    estadisticos_bt[i] <- sum(T)
  }
  k = qbinom(0.05, size = 12, prob = 0.5, lower.tail=FALSE)
  potencia = 0 
  for(i in 1:m){ #Cada estadistico con el cual se rechaze la hipotesis nula suma 1.
    if(estadisticos_bt[i] > k){
      potencia = potencia + 1
    }    
  }
  potencia = potencia /m
  return (potencia)
}

print(potencia_estimada_signo(10000))

#El test con mayor potencia es el phi_n con potencia = 0.96, que cumlple con lo esperado peus en UPM.
#El test del signo dio 0.25 (la potencia) con lo cual probablemente tenga algún error
